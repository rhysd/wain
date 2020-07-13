use crate::cast;
use crate::globals::Globals;
use crate::import::{ImportInvalidError, ImportInvokeError, Importer};
use crate::memory::Memory;
use crate::stack::{CallFrame, Stack, StackAccess};
use crate::table::Table;
use crate::trap::{Result, Trap, TrapReason};
use crate::value::{Float, LittleEndian, Value};
use std::mem;
use wain_ast as ast;
use wain_ast::AsValType;

// Note: This implementation currently ignores Wasm's thread model since MVP does not support multiple
// threads. https://webassembly.github.io/spec/core/exec/runtime.html#configurations

// TODO: Handle external values for imports and exports

// https://webassembly.github.io/spec/core/exec/numerics.html?highlight=ieee#xref-exec-numerics-op-fmin-mathrm-fmin-n-z-1-z-2
fn fmin<F: Float>(l: F, r: F) -> F {
    // f32::min() cannot use directly because of NaN handling divergence.
    // For example, 42f32.min(f32::NAN) is 42 but (f32.min (f32.const 42) (f32.const nan)) is nan.
    if l.is_nan() {
        l.to_arithmetic_nan()
    } else if r.is_nan() {
        r.to_arithmetic_nan()
    } else if l == r {
        F::from_bits(l.to_bits() | r.to_bits())
    } else {
        l.min(r)
    }
}

// https://webassembly.github.io/spec/core/exec/numerics.html?highlight=ieee#xref-exec-numerics-op-fmax-mathrm-fmax-n-z-1-z-2
fn fmax<F: Float>(l: F, r: F) -> F {
    // f32::max() cannot use directly for the same reason as f32::min() and f32.min
    if l.is_nan() {
        l.to_arithmetic_nan()
    } else if r.is_nan() {
        r.to_arithmetic_nan()
    } else if l == r {
        F::from_bits(l.to_bits() & r.to_bits())
    } else {
        l.max(r)
    }
}

enum ExecState {
    Breaking(u32), // Breaking
    Ret,           // Returning from current function call
    Continue,      // Continuing execution
}

type ExecResult = Result<ExecState>;

// https://webassembly.github.io/spec/core/exec/runtime.html#module-instances
pub struct ModuleInstance<'module, 'source> {
    ast: &'module ast::Module<'source>,
    table: Table,   // Only one table is allowed for MVP
    memory: Memory, // Only one memory is allowed for MVP
    globals: Globals,
}

// State of abtract machine to run wasm code. This struct contains both store and stack
// https://webassembly.github.io/spec/core/exec/runtime.html
pub struct Runtime<'module, 'source, I: Importer> {
    module: ModuleInstance<'module, 'source>,
    stack: Stack,
    importer: I,
    // Call frame has param types and local types as slices. The slices' lifetimes were derived
    // from module.
    frame: CallFrame<'module>,
}

impl<'m, 's, I: Importer> Runtime<'m, 's, I> {
    /// Initialize states of execution (stack, memory, ...) and instantiate a given module. It means
    /// that 'start function' is invoked in this function if presents.
    /// The module is assumed to be validated. If an invalid module is given, the behavior is
    /// unspecified, meaning that it may crash or the result may be incorrect.
    ///
    /// https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    pub fn instantiate(module: &'m ast::Module<'s>, importer: I) -> Result<Self> {
        // TODO: 2., 3., 4. Validate external values before instantiate globals

        fn unknown_import<'s>(import: &ast::Import<'s>, at: usize) -> Box<Trap> {
            Trap::new(
                TrapReason::UnknownImport {
                    mod_name: import.mod_name.0.to_string(),
                    name: import.name.0.to_string(),
                    kind: "function",
                },
                at,
            )
        }

        for func in module.funcs.iter() {
            match &func.kind {
                ast::FuncKind::Body { .. } => break, // All imports precedes other definitions
                ast::FuncKind::Import(i) => {
                    let mod_name = &i.mod_name.0;
                    if mod_name != I::MODULE_NAME {
                        return Err(unknown_import(i, func.start));
                    }

                    let fty = &module.types[func.idx as usize];
                    let name = &i.name.0;
                    match importer.validate(name, &fty.params, fty.results.get(0).copied()) {
                        Some(ImportInvalidError::NotFound) => {
                            return Err(unknown_import(i, func.start));
                        }
                        Some(ImportInvalidError::SignatureMismatch {
                            expected_params,
                            expected_ret,
                        }) => {
                            return Err(Trap::new(
                                TrapReason::FuncSignatureMismatch {
                                    import: Some((mod_name.to_string(), name.to_string())),
                                    expected_params: expected_params.to_vec(),
                                    expected_results: expected_ret.into_iter().collect(),
                                    actual_params: fty.params.to_vec(),
                                    actual_results: fty.results.clone(),
                                },
                                func.start,
                            ))
                        }
                        None => { /* do nothing */ }
                    }
                }
            }
        }

        // 5. global initialization values determined by module and externval
        let globals = Globals::instantiate(&module.globals)?;

        // 6. a new module instance allocated from module in store S
        // https://webassembly.github.io/spec/core/exec/modules.html#alloc-module

        // 6.2 allocate functions (nothing to do since we run abstract tree directly)

        // 6.3 allocate table
        let mut table = Table::allocate(&module.tables)?;
        // 6.4 allocate memory
        let mut memory = Memory::allocate(&module.memories)?;

        // 7. and 8. push empty frame
        let frame = CallFrame::default();
        let stack = Stack::default();

        // 9. add element segments to table
        for elem in module.elems.iter() {
            table.new_elem(elem, &globals)?;
        }

        // 10. add data segments to memory
        for data in module.data.iter() {
            memory.new_data(data, &globals)?;
        }

        // 11. and 12. pop frame (unnecessary for now)

        let mut runtime = Self {
            module: ModuleInstance {
                ast: module,
                table,
                memory,
                globals,
            },
            stack,
            importer,
            frame,
        };

        // 15. If the start function is not empty, invoke it
        if let Some(start) = &runtime.module.ast.entrypoint {
            // Execute entrypoint
            runtime.invoke_by_funcidx(start.idx)?;
        }

        Ok(runtime)
    }

    pub fn module(&self) -> &'m ast::Module<'s> {
        &self.module.ast
    }

    pub fn memory(&self) -> &Memory {
        &self.module.memory
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.module
            .ast
            .exports
            .iter()
            .find_map(|e| match e.kind {
                ast::ExportKind::Global(idx) if e.name.0 == name => Some(idx),
                _ => None,
            })
            .map(|idx| {
                let ty = self.module.ast.globals[idx as usize].ty;
                self.module.globals.get_any(idx, ty)
            })
    }

    fn push_frame(&mut self, new_frame: CallFrame<'m>) -> CallFrame<'m> {
        mem::replace(&mut self.frame, new_frame)
    }

    fn pop_frame(&mut self, prev_frame: CallFrame<'m>) {
        self.stack
            .restore(self.frame.base_addr, self.frame.base_idx);
        self.frame = prev_frame;
    }

    // Returns if it has return value on stack or not
    fn invoke_import(
        &mut self,
        import: &ast::Import<'s>,
        has_ret: bool,
        pos: usize,
    ) -> Result<bool> {
        if import.mod_name.0 == I::MODULE_NAME {
            match self
                .importer
                .call(&import.name.0, &mut self.stack, &mut self.module.memory)
            {
                Ok(()) => return Ok(has_ret),
                Err(ImportInvokeError::Fatal { message }) => {
                    return Err(Trap::new(
                        TrapReason::ImportFuncCallFail {
                            mod_name: import.mod_name.0.to_string(),
                            name: import.name.0.to_string(),
                            msg: message,
                        },
                        pos,
                    ))
                }
            }
        }
        unreachable!(
            "fatal: invalid import at runtime: {}::{}",
            import.mod_name.0, import.name.0
        );
    }

    // https://webassembly.github.io/spec/core/exec/modules.html#invocation
    // https://webassembly.github.io/spec/core/exec/instructions.html#function-calls
    // Returns if it has return value on stack or not
    fn invoke_by_funcidx(&mut self, funcidx: u32) -> Result<bool> {
        let func = &self.module.ast.funcs[funcidx as usize];
        let fty = &self.module.ast.types[func.idx as usize];

        // Call this function with params
        let (locals, body) = match &func.kind {
            ast::FuncKind::Import(i) => {
                return self.invoke_import(i, !fty.results.is_empty(), func.start)
            }
            ast::FuncKind::Body { locals, expr } => (locals, expr),
        };

        let prev_frame = self.push_frame(CallFrame::new(&self.stack, &fty.params, locals));

        self.stack.extend_zero_values(&locals);

        for insn in body {
            match insn.execute(self)? {
                ExecState::Continue => {}
                // When using br or br_if outside control instructions, it unwinds execution in
                // the function body. Label with empty continuation is put before invoking the
                // function body (11.). It means that breaking outside control instructions will be
                // caught by this label.
                ExecState::Ret | ExecState::Breaking(_) => break,
            }
        }

        if fty.results.is_empty() {
            self.pop_frame(prev_frame);
            Ok(false)
        } else {
            // Push 1st result value since number of result type is 1 or 0 for MVP
            let v: Value = self.stack.pop();
            self.pop_frame(prev_frame);
            self.stack.push(v); // push result value
            Ok(true)
        }
    }

    // Invoke function by name
    pub fn invoke(&mut self, name: impl AsRef<str>, args: &[Value]) -> Result<Option<Value>> {
        fn find_func_to_invoke<'s>(
            name: &str,
            exports: &[ast::Export<'s>],
        ) -> Result<(u32, usize)> {
            for export in exports {
                if export.name.0 == name {
                    let actual = match export.kind {
                        ast::ExportKind::Func(idx) => return Ok((idx, export.start)),
                        ast::ExportKind::Table(_) => "table",
                        ast::ExportKind::Memory(_) => "memory",
                        ast::ExportKind::Global(_) => "global variable",
                    };
                    return Err(Trap::new(
                        TrapReason::WrongInvokeTarget {
                            name: name.to_string(),
                            actual: Some(actual),
                        },
                        export.start,
                    ));
                }
            }
            Err(Trap::new(
                TrapReason::WrongInvokeTarget {
                    name: name.to_string(),
                    actual: None,
                },
                0,
            ))
        }

        let name = name.as_ref();
        let (funcidx, start) = find_func_to_invoke(name, &self.module.ast.exports)?;
        let arg_types =
            &self.module.ast.types[self.module.ast.funcs[funcidx as usize].idx as usize].params;

        // Check parameter types
        if args
            .iter()
            .map(Value::valtype)
            .ne(arg_types.iter().copied())
        {
            return Err(Trap::new(
                TrapReason::InvokeInvalidArgs {
                    name: name.to_string(),
                    args: args.to_vec(),
                    arg_types: arg_types.to_vec(),
                },
                start,
            ));
        }

        // Push values to stack for invoking the function
        for arg in args {
            self.stack.push(arg.clone());
        }

        if self.invoke_by_funcidx(funcidx)? {
            Ok(Some(self.stack.pop()))
        } else {
            Ok(None)
        }
    }

    fn mem_addr(&mut self, mem: &ast::Mem) -> usize {
        let addr = self.stack.pop::<i32>() as u32 as usize;
        addr + mem.offset as usize
    }

    fn load<V: LittleEndian>(&mut self, mem: &ast::Mem, at: usize) -> Result<V> {
        let addr = self.mem_addr(mem);
        Ok(self.module.memory.load(addr, at)?)
    }

    fn store<V: LittleEndian>(&mut self, mem: &ast::Mem, v: V, at: usize) -> Result<()> {
        let addr = self.mem_addr(mem);
        self.module.memory.store(addr, v, at)?;
        Ok(())
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#exec-unop
    fn unop<T, F>(&mut self, op: F)
    where
        T: StackAccess + LittleEndian,
        F: FnOnce(T) -> T,
    {
        // Instead of popping value and pushing the result, directly modify stack top for optimization
        let ret = op(self.stack.top());
        self.stack.write_top_bytes(ret);
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#exec-binop
    fn binop<T, F>(&mut self, op: F)
    where
        T: StackAccess + LittleEndian,
        F: FnOnce(T, T) -> T,
    {
        // Instead of popping value and pushing the result, directly modify stack top for optimization
        let c2 = self.stack.pop();
        let c1 = self.stack.top();
        let ret = op(c1, c2);
        self.stack.write_top_bytes(ret);
    }

    fn binop_trap<T, F>(&mut self, op: F) -> Result<()>
    where
        T: StackAccess + LittleEndian,
        F: FnOnce(T, T) -> Result<T>,
    {
        // Instead of popping value and pushing the result, directly modify stack top for optimization
        let c2 = self.stack.pop();
        let c1 = self.stack.top();
        let ret = op(c1, c2)?;
        self.stack.write_top_bytes(ret);
        Ok(())
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#exec-testop
    fn testop<T, F>(&mut self, op: F)
    where
        T: StackAccess + LittleEndian,
        F: FnOnce(T) -> bool,
    {
        let ret = op(self.stack.top());
        self.stack.write_top::<T, i32>(if ret { 1 } else { 0 });
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#exec-relop
    fn relop<T, F>(&mut self, op: F)
    where
        T: StackAccess + LittleEndian,
        F: FnOnce(T, T) -> bool,
    {
        let c2 = self.stack.pop();
        let c1 = self.stack.top();
        let ret = op(c1, c2);
        self.stack.write_top::<T, i32>(if ret { 1i32 } else { 0 });
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#exec-cvtop
    fn cvtop<T, U, F>(&mut self, op: F)
    where
        T: StackAccess,
        U: StackAccess + LittleEndian + AsValType,
        F: FnOnce(T) -> U,
    {
        let ret = op(self.stack.top());
        self.stack.write_top::<T, U>(ret);
    }

    fn cvtop_trap<T, U, F>(&mut self, op: F) -> Result<()>
    where
        T: StackAccess,
        U: StackAccess + LittleEndian + AsValType,
        F: FnOnce(T) -> Result<U>,
    {
        let ret = op(self.stack.top())?;
        self.stack.write_top::<T, U>(ret);
        Ok(())
    }
}

trait Execute<'m, 's, I: Importer> {
    fn execute(&self, runtime: &mut Runtime<'m, 's, I>) -> ExecResult;
}

// https://webassembly.github.io/spec/core/exec/instructions.html#blocks
impl<'m, 's, I: Importer> Execute<'m, 's, I> for Vec<ast::Instruction> {
    fn execute(&self, runtime: &mut Runtime<'m, 's, I>) -> ExecResult {
        // Run instruction sequence as block
        for insn in self {
            match insn.execute(runtime)? {
                ExecState::Continue => {}
                state => return Ok(state), // Stop executing this block on return or break
            }
        }
        Ok(ExecState::Continue)
    }
}

// https://webassembly.github.io/spec/core/exec/instructions.html
impl<'m, 's, I: Importer> Execute<'m, 's, I> for ast::Instruction {
    #[allow(clippy::cognitive_complexity)]
    fn execute(&self, runtime: &mut Runtime<'m, 's, I>) -> ExecResult {
        use ast::InsnKind::*;
        #[allow(clippy::float_cmp)]
        match &self.kind {
            // Control instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-block
            Block { ty, body } => {
                let label = runtime.stack.push_label(*ty);
                match body.execute(runtime)? {
                    ExecState::Continue => {}
                    ExecState::Ret => return Ok(ExecState::Ret),
                    ExecState::Breaking(0) => {}
                    ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                }
                runtime.stack.pop_label(label);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-loop
            Loop { ty, body } => loop {
                // Note: Difference between block and loop is the position on breaking. When reaching
                // to the end of instruction sequence, loop instruction ends execution of subsequence.
                let label = runtime.stack.push_label(*ty);
                match body.execute(runtime)? {
                    ExecState::Continue => {
                        runtime.stack.pop_label(label);
                        break;
                    }
                    ExecState::Ret => return Ok(ExecState::Ret),
                    ExecState::Breaking(0) => continue,
                    ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                }
            },
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-if
            If {
                ty,
                then_body,
                else_body,
            } => {
                let cond: i32 = runtime.stack.pop();
                let label = runtime.stack.push_label(*ty);
                let insns = if cond != 0 { then_body } else { else_body };
                match insns.execute(runtime)? {
                    ExecState::Continue => {}
                    ExecState::Ret => return Ok(ExecState::Ret),
                    ExecState::Breaking(0) => {}
                    ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                }
                runtime.stack.pop_label(label);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-unreachable
            Unreachable => return Err(Trap::new(TrapReason::ReachUnreachable, self.start)),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-nop
            Nop => { /* yay! nothing to do */ }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-br
            Br(labelidx) => return Ok(ExecState::Breaking(*labelidx)),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-br-if
            BrIf(labelidx) => {
                let cond: i32 = runtime.stack.pop();
                if cond != 0 {
                    return Ok(ExecState::Breaking(*labelidx));
                }
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-br-table
            BrTable {
                labels,
                default_label,
            } => {
                let idx: i32 = runtime.stack.pop();
                let idx = idx as usize;
                let labelidx = if idx < labels.len() {
                    labels[idx]
                } else {
                    *default_label
                };
                return Ok(ExecState::Breaking(labelidx));
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-return
            Return => return Ok(ExecState::Ret),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-call
            Call(funcidx) => {
                runtime.invoke_by_funcidx(*funcidx)?;
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-call-indirect
            CallIndirect(typeidx) => {
                let expected = &runtime.module.ast.types[*typeidx as usize];
                let elemidx: i32 = runtime.stack.pop();
                let funcidx = runtime.module.table.at(elemidx as usize, self.start)?;
                let func = &runtime.module.ast.funcs[funcidx as usize];
                let actual = &runtime.module.ast.types[func.idx as usize];
                if expected.params != actual.params || expected.results != actual.results {
                    return Err(Trap::new(
                        TrapReason::FuncSignatureMismatch {
                            import: None,
                            expected_params: expected.params.clone(),
                            expected_results: expected.results.clone(),
                            actual_params: actual.params.clone(),
                            actual_results: actual.results.clone(),
                        },
                        self.start,
                    ));
                }
                runtime.invoke_by_funcidx(funcidx)?;
            }
            // Parametric instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-drop
            Drop => {
                runtime.stack.pop::<Value>();
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-select
            Select => {
                let cond: i32 = runtime.stack.pop();
                if cond != 0 {
                    // pop val2 -> pop val1 -> push val1 (skip pop/push val1)
                    let _val2: Value = runtime.stack.pop();
                } else {
                    // pop val2 -> pop val1 -> push val2
                    let val2: Value = runtime.stack.pop();
                    let _val1: Value = runtime.stack.pop();
                    runtime.stack.push(val2);
                }
            }
            // Variable instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-local-get
            LocalGet(localidx) => {
                let addr = runtime.frame.local_addr(*localidx);
                match runtime.frame.local_type(*localidx) {
                    ast::ValType::I32 => runtime.stack.push(runtime.stack.read::<i32>(addr)),
                    ast::ValType::I64 => runtime.stack.push(runtime.stack.read::<i64>(addr)),
                    ast::ValType::F32 => runtime.stack.push(runtime.stack.read::<f32>(addr)),
                    ast::ValType::F64 => runtime.stack.push(runtime.stack.read::<f64>(addr)),
                }
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-local-set
            LocalSet(localidx) => {
                let addr = runtime.frame.local_addr(*localidx);
                let val = runtime.stack.pop();
                runtime.stack.write_any(addr, val);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-local-tee
            LocalTee(localidx) => {
                // Like local.set, but it does not change stack
                let addr = runtime.frame.local_addr(*localidx);
                let val = runtime.stack.top();
                runtime.stack.write_any(addr, val);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-global-get
            GlobalGet(globalidx) => match runtime.module.ast.globals[*globalidx as usize].ty {
                ast::ValType::I32 => runtime
                    .stack
                    .push(runtime.module.globals.get::<i32>(*globalidx)),
                ast::ValType::I64 => runtime
                    .stack
                    .push(runtime.module.globals.get::<i64>(*globalidx)),
                ast::ValType::F32 => runtime
                    .stack
                    .push(runtime.module.globals.get::<f32>(*globalidx)),
                ast::ValType::F64 => runtime
                    .stack
                    .push(runtime.module.globals.get::<f64>(*globalidx)),
            },
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-global-set
            GlobalSet(globalidx) => runtime
                .module
                .globals
                .set_any(*globalidx, runtime.stack.top()),
            // Memory instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#and
            I32Load(mem) => {
                let v: i32 = runtime.load(mem, self.start)?;
                runtime.stack.push(v);
            }
            I64Load(mem) => {
                let v: i64 = runtime.load(mem, self.start)?;
                runtime.stack.push(v);
            }
            F32Load(mem) => {
                let v: f32 = runtime.load(mem, self.start)?;
                runtime.stack.push(v);
            }
            F64Load(mem) => {
                let v: f64 = runtime.load(mem, self.start)?;
                runtime.stack.push(v);
            }
            I32Load8S(mem) => {
                let v: i8 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as i32);
            }
            I32Load8U(mem) => {
                let v: i8 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as u8 as i32);
            }
            I32Load16S(mem) => {
                let v: i16 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as i32);
            }
            I32Load16U(mem) => {
                let v: i16 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as u16 as i32);
            }
            I64Load8S(mem) => {
                let v: i8 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as i64);
            }
            I64Load8U(mem) => {
                let v: i8 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as u8 as i64);
            }
            I64Load16S(mem) => {
                let v: i16 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as i64);
            }
            I64Load16U(mem) => {
                let v: i16 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as u16 as i64);
            }
            I64Load32S(mem) => {
                let v: i32 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as i64);
            }
            I64Load32U(mem) => {
                let v: i32 = runtime.load(mem, self.start)?;
                runtime.stack.push(v as u32 as i64);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-storen
            I32Store(mem) => {
                let v: i32 = runtime.stack.pop();
                runtime.store(mem, v, self.start)?;
            }
            I64Store(mem) => {
                let v: i64 = runtime.stack.pop();
                runtime.store(mem, v, self.start)?;
            }
            F32Store(mem) => {
                let v: f32 = runtime.stack.pop();
                runtime.store(mem, v, self.start)?;
            }
            F64Store(mem) => {
                let v: f64 = runtime.stack.pop();
                runtime.store(mem, v, self.start)?;
            }
            I32Store8(mem) => {
                let v: i32 = runtime.stack.pop();
                runtime.store(mem, v as i8, self.start)?;
            }
            I32Store16(mem) => {
                let v: i32 = runtime.stack.pop();
                runtime.store(mem, v as i16, self.start)?;
            }
            I64Store8(mem) => {
                let v: i64 = runtime.stack.pop();
                runtime.store(mem, v as i8, self.start)?;
            }
            I64Store16(mem) => {
                let v: i64 = runtime.stack.pop();
                runtime.store(mem, v as i16, self.start)?;
            }
            I64Store32(mem) => {
                let v: i64 = runtime.stack.pop();
                runtime.store(mem, v as i32, self.start)?;
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-memory-size
            MemorySize => runtime.stack.push(runtime.module.memory.size() as i32),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-memory-grow
            MemoryGrow => {
                let pages: i32 = runtime.stack.pop();
                let prev_pages = runtime.module.memory.grow(pages as u32);
                runtime.stack.push(prev_pages);
            }
            // Numeric instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-const
            I32Const(i) => runtime.stack.push(*i),
            I64Const(i) => runtime.stack.push(*i),
            F32Const(f) => runtime.stack.push(*f),
            F64Const(f) => runtime.stack.push(*f),
            // Integer operations
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-iclz
            I32Clz => runtime.unop::<i32, _>(|v| v.leading_zeros() as i32),
            I64Clz => runtime.unop::<i64, _>(|v| v.leading_zeros() as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ictz
            I32Ctz => runtime.unop::<i32, _>(|v| v.trailing_zeros() as i32),
            I64Ctz => runtime.unop::<i64, _>(|v| v.trailing_zeros() as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ipopcnt
            I32Popcnt => runtime.unop::<i32, _>(|v| v.count_ones() as i32),
            I64Popcnt => runtime.unop::<i64, _>(|v| v.count_ones() as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-iadd
            I32Add => runtime.binop::<i32, _>(|l, r| l.wrapping_add(r)),
            I64Add => runtime.binop::<i64, _>(|l, r| l.wrapping_add(r)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-isub
            I32Sub => runtime.binop::<i32, _>(|l, r| l.wrapping_sub(r)),
            I64Sub => runtime.binop::<i64, _>(|l, r| l.wrapping_sub(r)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-imul
            I32Mul => runtime.binop::<i32, _>(|l, r| l.wrapping_mul(r)),
            I64Mul => runtime.binop::<i64, _>(|l, r| l.wrapping_mul(r)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-idiv-s
            // Note: According to i32.wast and i64.wast, integer overflow on idiv_s should be trapped.
            // This is intended behavior: https://github.com/WebAssembly/spec/issues/1185#issuecomment-619412936
            I32DivS => runtime.binop_trap::<i32, _>(|l, r| match l.checked_div(r) {
                Some(i) => Ok(i),
                None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
            })?,
            I64DivS => runtime.binop_trap::<i64, _>(|l, r| match l.checked_div(r) {
                Some(i) => Ok(i),
                None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-idiv-u
            I32DivU => {
                runtime.binop_trap::<i32, _>(|l, r| match (l as u32).checked_div(r as u32) {
                    Some(u) => Ok(u as i32),
                    None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
                })?
            }
            I64DivU => {
                runtime.binop_trap::<i64, _>(|l, r| match (l as u64).checked_div(r as u64) {
                    Some(u) => Ok(u as i64),
                    None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
                })?
            }
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irem-s
            // Note: rem_s should not cause overflow. For example, 0x80000000 % -1 causes overflow
            // in Rust, but Wasm test case says it should return 0. Note that Go has special rule
            // that x % -1 is 0 when x is the most negative value.
            // This is intended behavior: https://github.com/WebAssembly/spec/issues/1185#issuecomment-619412936
            I32RemS => runtime.binop_trap::<i32, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok(l.wrapping_rem(r))
                }
            })?,
            I64RemS => runtime.binop_trap::<i64, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok(l.wrapping_rem(r))
                }
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irem-u
            I32RemU => runtime.binop_trap::<i32, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok((l as u32 % r as u32) as i32) // for unsigned integers overflow never occurs
                }
            })?,
            I64RemU => runtime.binop_trap::<i64, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok((l as u64 % r as u64) as i64) // for unsigned integers overflow never occurs
                }
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-iand
            I32And => runtime.binop::<i32, _>(|l, r| l & r),
            I64And => runtime.binop::<i64, _>(|l, r| l & r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ior
            I32Or => runtime.binop::<i32, _>(|l, r| l | r),
            I64Or => runtime.binop::<i64, _>(|l, r| l | r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ixor
            I32Xor => runtime.binop::<i32, _>(|l, r| l ^ r),
            I64Xor => runtime.binop::<i64, _>(|l, r| l ^ r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ishl
            I32Shl => runtime.binop::<i32, _>(|l, r| l.wrapping_shl(r as u32)),
            I64Shl => runtime.binop::<i64, _>(|l, r| l.wrapping_shl(r as u32)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ishr-s
            I32ShrS => runtime.binop::<i32, _>(|l, r| l.wrapping_shr(r as u32)),
            I64ShrS => runtime.binop::<i64, _>(|l, r| l.wrapping_shr(r as u32)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ishr-u
            I32ShrU => runtime.binop::<i32, _>(|l, r| (l as u32).wrapping_shr(r as u32) as i32),
            I64ShrU => runtime.binop::<i64, _>(|l, r| (l as u64).wrapping_shr(r as u32) as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irotl
            I32Rotl => runtime.binop::<i32, _>(|l, r| l.rotate_left(r as u32)),
            I64Rotl => runtime.binop::<i64, _>(|l, r| l.rotate_left(r as u32)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irotr
            I32Rotr => runtime.binop::<i32, _>(|l, r| l.rotate_right(r as u32)),
            I64Rotr => runtime.binop::<i64, _>(|l, r| l.rotate_right(r as u32)),
            // Float number operations
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fabs
            F32Abs => runtime.unop::<f32, _>(|f| f.abs()),
            F64Abs => runtime.unop::<f64, _>(|f| f.abs()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fneg
            F32Neg => runtime.unop::<f32, _>(|f| -f),
            F64Neg => runtime.unop::<f64, _>(|f| -f),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fceil
            F32Ceil => runtime.unop::<f32, _>(|f| f.ceil()),
            F64Ceil => runtime.unop::<f64, _>(|f| f.ceil()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ffloor
            F32Floor => runtime.unop::<f32, _>(|f| f.floor()),
            F64Floor => runtime.unop::<f64, _>(|f| f.floor()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ftrunc
            F32Trunc => runtime.unop::<f32, _>(|f| f.trunc()),
            F64Trunc => runtime.unop::<f64, _>(|f| f.trunc()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fnearest
            F32Nearest => runtime.unop::<f32, _>(|f| {
                // f32::round() is not available because behavior when two values are equally near
                // is different. For example, 4.5f32.round() is 5.0 but (f32.nearest (f32.const 4.5))
                // is 4.0.
                let fround = f.round();
                if (f - fround).abs() == 0.5 && fround % 2.0 != 0.0 {
                    f.trunc()
                } else {
                    fround
                }
            }),
            F64Nearest => runtime.unop::<f64, _>(|f| {
                // f64::round() is not available for the same reason as f32.nearest
                let fround = f.round();
                if (f - fround).abs() == 0.5 && fround % 2.0 != 0.0 {
                    f.trunc()
                } else {
                    fround
                }
            }),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fsqrt
            F32Sqrt => runtime.unop::<f32, _>(|f| f.sqrt()),
            F64Sqrt => runtime.unop::<f64, _>(|f| f.sqrt()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fadd
            F32Add => runtime.binop::<f32, _>(|l, r| l + r),
            F64Add => runtime.binop::<f64, _>(|l, r| l + r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fsub
            F32Sub => runtime.binop::<f32, _>(|l, r| l - r),
            F64Sub => runtime.binop::<f64, _>(|l, r| l - r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fmul
            F32Mul => runtime.binop::<f32, _>(|l, r| l * r),
            F64Mul => runtime.binop::<f64, _>(|l, r| l * r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fdiv
            F32Div => runtime.binop::<f32, _>(|l, r| l / r),
            F64Div => runtime.binop::<f64, _>(|l, r| l / r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fmin
            F32Min => runtime.binop::<f32, _>(fmin),
            F64Min => runtime.binop::<f64, _>(fmin),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fmax
            F32Max => runtime.binop::<f32, _>(fmax),
            F64Max => runtime.binop::<f64, _>(fmax),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fcopysign
            F32Copysign => runtime.binop::<f32, _>(|l, r| l.copysign(r)),
            F64Copysign => runtime.binop::<f64, _>(|l, r| l.copysign(r)),
            // Integer comparison
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ieqz
            I32Eqz => runtime.testop::<i32, _>(|i| i == 0),
            I64Eqz => runtime.testop::<i64, _>(|i| i == 0),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ieq
            I32Eq => runtime.relop::<i32, _>(|l, r| l == r),
            I64Eq => runtime.relop::<i64, _>(|l, r| l == r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ine
            I32Ne => runtime.relop::<i32, _>(|l, r| l != r),
            I64Ne => runtime.relop::<i64, _>(|l, r| l != r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ilt-s
            I32LtS => runtime.relop::<i32, _>(|l, r| l < r),
            I64LtS => runtime.relop::<i64, _>(|l, r| l < r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ilt-u
            I32LtU => runtime.relop::<i32, _>(|l, r| (l as u32) < r as u32),
            I64LtU => runtime.relop::<i64, _>(|l, r| (l as u64) < r as u64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-igt-s
            I32GtS => runtime.relop::<i32, _>(|l, r| l > r),
            I64GtS => runtime.relop::<i64, _>(|l, r| l > r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-igt-u
            I32GtU => runtime.relop::<i32, _>(|l, r| l as u32 > r as u32),
            I64GtU => runtime.relop::<i64, _>(|l, r| l as u64 > r as u64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ile-s
            I32LeS => runtime.relop::<i32, _>(|l, r| l <= r),
            I64LeS => runtime.relop::<i64, _>(|l, r| l <= r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ile-u
            I32LeU => runtime.relop::<i32, _>(|l, r| l as u32 <= r as u32),
            I64LeU => runtime.relop::<i64, _>(|l, r| l as u64 <= r as u64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ige-s
            I32GeS => runtime.relop::<i32, _>(|l, r| l >= r),
            I64GeS => runtime.relop::<i64, _>(|l, r| l >= r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ige-u
            I32GeU => runtime.relop::<i32, _>(|l, r| l as u32 >= r as u32),
            I64GeU => runtime.relop::<i64, _>(|l, r| l as u64 >= r as u64),
            // Float number comparison
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-feq
            F32Eq => runtime.relop::<f32, _>(|l, r| l == r),
            F64Eq => runtime.relop::<f64, _>(|l, r| l == r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fne
            F32Ne => runtime.relop::<f32, _>(|l, r| l != r),
            F64Ne => runtime.relop::<f64, _>(|l, r| l != r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-flt
            F32Lt => runtime.relop::<f32, _>(|l, r| l < r),
            F64Lt => runtime.relop::<f64, _>(|l, r| l < r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fgt
            F32Gt => runtime.relop::<f32, _>(|l, r| l > r),
            F64Gt => runtime.relop::<f64, _>(|l, r| l > r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fle
            F32Le => runtime.relop::<f32, _>(|l, r| l <= r),
            F64Le => runtime.relop::<f64, _>(|l, r| l <= r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fge
            F32Ge => runtime.relop::<f32, _>(|l, r| l >= r),
            F64Ge => runtime.relop::<f64, _>(|l, r| l >= r),
            // Conversion
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-extend-u
            I64ExtendI32U => runtime.cvtop::<i32, i64, _>(|v| v as u32 as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-extend-s
            I64ExtendI32S => runtime.cvtop::<i32, i64, _>(|v| v as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-wrap
            I32WrapI64 => runtime.cvtop::<i64, i32, _>(|v| v as i32),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-trunc-u
            I32TruncF32U => runtime.cvtop_trap::<f32, i32, _>(|v| match cast::f32_to_u32(v) {
                Some(u) => Ok(u as i32),
                None => Err(Trap::out_of_range(v, "u32", self.start)),
            })?,
            I32TruncF64U => runtime.cvtop_trap::<f64, i32, _>(|v| match cast::f64_to_u32(v) {
                Some(u) => Ok(u as i32),
                None => Err(Trap::out_of_range(v, "u32", self.start)),
            })?,
            I64TruncF32U => runtime.cvtop_trap::<f32, i64, _>(|v| match cast::f32_to_u64(v) {
                Some(u) => Ok(u as i64),
                None => Err(Trap::out_of_range(v, "u64", self.start)),
            })?,
            I64TruncF64U => runtime.cvtop_trap::<f64, i64, _>(|v| match cast::f64_to_u64(v) {
                Some(u) => Ok(u as i64),
                None => Err(Trap::out_of_range(v, "u64", self.start)),
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-trunc-s
            I32TruncF32S => runtime.cvtop_trap::<f32, i32, _>(|v| match cast::f32_to_i32(v) {
                Some(u) => Ok(u),
                None => Err(Trap::out_of_range(v, "i32", self.start)),
            })?,
            I32TruncF64S => runtime.cvtop_trap::<f64, i32, _>(|v| match cast::f64_to_i32(v) {
                Some(u) => Ok(u),
                None => Err(Trap::out_of_range(v, "i32", self.start)),
            })?,
            I64TruncF32S => runtime.cvtop_trap::<f32, i64, _>(|v| match cast::f32_to_i64(v) {
                Some(u) => Ok(u),
                None => Err(Trap::out_of_range(v, "i64", self.start)),
            })?,
            I64TruncF64S => runtime.cvtop_trap::<f64, i64, _>(|v| match cast::f64_to_i64(v) {
                Some(u) => Ok(u),
                None => Err(Trap::out_of_range(v, "i64", self.start)),
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-promote
            F64PromoteF32 => runtime.cvtop::<f32, f64, _>(|v| v as f64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-demote
            F32DemoteF64 => runtime.cvtop::<f64, f32, _>(|v| v as f32),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-convert-u
            F32ConvertI32U => runtime.cvtop::<i32, f32, _>(|v| v as u32 as f32),
            F32ConvertI64U => runtime.cvtop::<i64, f32, _>(|v| v as u64 as f32),
            F64ConvertI32U => runtime.cvtop::<i32, f64, _>(|v| v as u32 as f64),
            F64ConvertI64U => runtime.cvtop::<i64, f64, _>(|v| v as u64 as f64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-convert-s
            F32ConvertI32S => runtime.cvtop::<i32, f32, _>(|v| v as f32),
            F32ConvertI64S => runtime.cvtop::<i64, f32, _>(|v| v as f32),
            F64ConvertI32S => runtime.cvtop::<i32, f64, _>(|v| v as f64),
            F64ConvertI64S => runtime.cvtop::<i64, f64, _>(|v| v as f64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-reinterpret
            // Don't need to modify stack. Just changing type to t2 is enough.
            I32ReinterpretF32 => runtime.stack.write_top_type(i32::VAL_TYPE),
            I64ReinterpretF64 => runtime.stack.write_top_type(i64::VAL_TYPE),
            F32ReinterpretI32 => runtime.stack.write_top_type(f32::VAL_TYPE),
            F64ReinterpretI64 => runtime.stack.write_top_type(f64::VAL_TYPE),
        }
        Ok(ExecState::Continue)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::import::DefaultImporter;
    use std::borrow::Cow;
    use std::env;
    use std::fmt;
    use std::fs;
    use std::io::{self, Read, Write};
    use std::path::PathBuf;
    use std::result;
    use wain_syntax_text::parse;
    use wain_validate::validate;

    struct Discard;

    impl Read for Discard {
        fn read(&mut self, b: &mut [u8]) -> io::Result<usize> {
            Ok(b.len())
        }
    }

    impl Write for Discard {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            Ok(buf.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn hello_world() {
        fn unwrap<T, E: fmt::Display>(res: result::Result<T, E>) -> T {
            match res {
                Ok(x) => x,
                Err(e) => panic!("unwrap failed with error message:\n{}", e),
            }
        }

        fn exec(file: PathBuf) -> Result<Vec<u8>> {
            let source = fs::read_to_string(file).unwrap();
            let ast = unwrap(parse(&source));
            unwrap(validate(&ast));
            let mut stdout = vec![];
            {
                let importer = DefaultImporter::with_stdio(Discard, &mut stdout);
                let mut runtime = unwrap(Runtime::instantiate(&ast.module, importer));
                runtime.invoke("_start", &[])?;
            }
            Ok(stdout)
        }

        let mut dir = env::current_dir().unwrap();
        dir.pop();
        dir.push("examples");
        dir.push("hello");
        let dir = dir;

        let stdout = exec(dir.join("hello.wat")).unwrap();
        assert_eq!(stdout, b"Hello, world\n");

        let stdout = exec(dir.join("hello_global.wat")).unwrap();
        assert_eq!(stdout, b"Hello, world\n");

        let stdout = exec(dir.join("hello_indirect_call.wat")).unwrap();
        assert_eq!(stdout, b"Hello, world\n");

        let stdout = exec(dir.join("hello_struct.wat")).unwrap();
        assert_eq!(stdout, b"Hello, world\n");
    }

    fn exec_insns(ty: ast::ValType, insns: Vec<ast::InsnKind>) -> Result<Option<Value>> {
        let expr = insns
            .into_iter()
            .map(|kind| ast::Instruction { start: 0, kind })
            .collect();

        let mut module = ast::Module::default();
        module.memories.push(ast::Memory {
            start: 0,
            ty: ast::MemType {
                limit: ast::Limits::From(0),
            },
            import: None,
        });
        module.types.push(ast::FuncType {
            start: 0,
            params: vec![],
            results: vec![ty],
        });
        module.funcs.push(ast::Func {
            start: 0,
            idx: 0,
            kind: ast::FuncKind::Body {
                locals: vec![],
                expr,
            },
        });
        module.exports.push(ast::Export {
            start: 0,
            name: ast::Name(Cow::Borrowed("test")),
            kind: ast::ExportKind::Func(0),
        });

        let importer = DefaultImporter::with_stdio(Discard, Discard);
        let mut runtime = Runtime::instantiate(&module, importer)?;
        runtime.invoke("test", &[])
    }

    #[test]
    fn nearest_edge_cases() {
        use ast::InsnKind::*;
        use ast::ValType::*;

        let f = exec_insns(F32, vec![F32Const(4.5), F32Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F32(f) if f == 4.0));

        let f = exec_insns(F32, vec![F32Const(3.5), F32Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F32(f) if f == 4.0));

        let f = exec_insns(F32, vec![F32Const(-0.5), F32Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F32(f) if f == 0.0 && f.is_sign_negative())); // -0.0

        let f = exec_insns(F32, vec![F32Const(0.5), F32Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F32(f) if f == 0.0 && f.is_sign_positive())); // +0.0

        let f = exec_insns(F64, vec![F64Const(4.5), F64Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F64(f) if f == 4.0));

        let f = exec_insns(F64, vec![F64Const(3.5), F64Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F64(f) if f == 4.0));

        let f = exec_insns(F64, vec![F64Const(-0.5), F64Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F64(f) if f == 0.0 && f.is_sign_negative())); // -0.0

        let f = exec_insns(F64, vec![F64Const(0.5), F64Nearest])
            .unwrap()
            .unwrap();
        assert!(matches!(f, Value::F64(f) if f == 0.0 && f.is_sign_positive() /* +0.0 */));
    }

    #[test]
    fn int_overflow() {
        use ast::InsnKind::*;
        use ast::ValType::*;

        let i = exec_insns(I32, vec![I32Const(i32::MAX), I32Const(1), I32Add])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I32(i) if i == i32::MIN));

        let i = exec_insns(I32, vec![I32Const(i32::MIN), I32Const(1), I32Sub])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I32(i) if i == i32::MAX));

        let i = exec_insns(I32, vec![I32Const(i32::MIN), I32Const(-1), I32Mul])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I32(i) if i == i32::MIN));

        let i = exec_insns(I64, vec![I64Const(i64::MAX), I64Const(1), I64Add])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I64(i) if i == i64::MIN));

        let i = exec_insns(I64, vec![I64Const(i64::MIN), I64Const(1), I64Sub])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I64(i) if i == i64::MAX));

        let i = exec_insns(I64, vec![I64Const(i64::MIN), I64Const(-1), I64Mul])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I64(i) if i == i64::MIN));
    }

    #[test]
    fn div_rem_edge_cases() {
        use ast::InsnKind::*;
        use ast::ValType::*;

        let i = exec_insns(I32, vec![I32Const(i32::MIN), I32Const(-1), I32RemS])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I32(0)), "{:?}", i);

        let i = exec_insns(I64, vec![I64Const(i64::MIN), I64Const(-1), I64RemS])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::I64(0)), "{:?}", i);

        let e = exec_insns(I32, vec![I32Const(1), I32Const(0), I32RemS]).unwrap_err();
        assert!(matches!(e.reason, TrapReason::RemZeroDivisor));
        let e = exec_insns(I32, vec![I32Const(1), I32Const(0), I32RemU]).unwrap_err();
        assert!(matches!(e.reason, TrapReason::RemZeroDivisor));
        let e = exec_insns(I64, vec![I64Const(1), I64Const(0), I64RemS]).unwrap_err();
        assert!(matches!(e.reason, TrapReason::RemZeroDivisor));
        let e = exec_insns(I64, vec![I64Const(1), I64Const(0), I64RemU]).unwrap_err();
        assert!(matches!(e.reason, TrapReason::RemZeroDivisor));
    }

    #[test]
    fn fmin_edge_cases() {
        use ast::InsnKind::*;
        use ast::ValType::*;

        let i = exec_insns(F32, vec![F32Const(0.0), F32Const(-0.0), F32Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f.to_bits() == 0x8000_0000));
        let i = exec_insns(F32, vec![F32Const(-0.0), F32Const(0.0), F32Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f.to_bits() == 0x8000_0000));
        let i = exec_insns(F32, vec![F32Const(1.0), F32Const(1.0), F32Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f == 1.0));
        let i = exec_insns(F32, vec![F32Const(-42.0), F32Const(-42.0), F32Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f == -42.0));
        let i = exec_insns(
            F32,
            vec![
                F32Const(f32::NEG_INFINITY),
                F32Const(f32::from_bits(0x7f80_0001)),
                F32Min,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F32(f) if f.is_nan() && f.to_bits() == 0x7fc0_0001));
        let i = exec_insns(
            F32,
            vec![
                F32Const(f32::from_bits(0x7fff_ffff)),
                F32Const(f32::NEG_INFINITY),
                F32Min,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F32(f) if f.is_nan() && f.to_bits() == 0x7fff_ffff));
        let i = exec_insns(
            F32,
            vec![
                F32Const(f32::from_bits(0x7f80_0001)),
                F32Const(f32::from_bits(0x7fff_ffff)),
                F32Min,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F32(f) if f.is_nan() && f.to_bits() == 0x7fc0_0001));

        let i = exec_insns(F64, vec![F64Const(0.0), F64Const(-0.0), F64Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f.to_bits() == 0x8000_0000_0000_0000));
        let i = exec_insns(F64, vec![F64Const(-0.0), F64Const(0.0), F64Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f.to_bits() == 0x8000_0000_0000_0000));
        let i = exec_insns(F64, vec![F64Const(1.0), F64Const(1.0), F64Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f == 1.0));
        let i = exec_insns(F64, vec![F64Const(-42.0), F64Const(-42.0), F64Min])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f == -42.0));
        let i = exec_insns(
            F64,
            vec![
                F64Const(f64::NEG_INFINITY),
                F64Const(f64::from_bits(0x7ff0_0000_0000_0001)),
                F64Min,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F64(f) if f.is_nan() && f.to_bits() == 0x7ff8_0000_0000_0001));
        let i = exec_insns(
            F64,
            vec![
                F64Const(f64::from_bits(0x7fff_ffff_ffff_ffff)),
                F64Const(f64::NEG_INFINITY),
                F64Min,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F64(f) if f.is_nan() && f.to_bits() == 0x7fff_ffff_ffff_ffff));
        let i = exec_insns(
            F64,
            vec![
                F64Const(f64::from_bits(0x7ff0_0000_0000_0001)),
                F64Const(f64::from_bits(0x7fff_ffff_ffff_ffff)),
                F64Min,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F64(f) if f.is_nan() && f.to_bits() == 0x7ff8_0000_0000_0001));
    }

    #[test]
    fn fmax_edge_cases() {
        use ast::InsnKind::*;
        use ast::ValType::*;

        let i = exec_insns(F32, vec![F32Const(0.0), F32Const(-0.0), F32Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f.to_bits() == 0x0000_0000));
        let i = exec_insns(F32, vec![F32Const(-0.0), F32Const(0.0), F32Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f.to_bits() == 0x0000_0000));
        let i = exec_insns(F32, vec![F32Const(1.0), F32Const(1.0), F32Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f == 1.0));
        let i = exec_insns(F32, vec![F32Const(-42.0), F32Const(-42.0), F32Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F32(f) if f == -42.0));
        let i = exec_insns(
            F32,
            vec![
                F32Const(f32::INFINITY),
                F32Const(f32::from_bits(0x7f80_0001)),
                F32Max,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F32(f) if f.is_nan() && f.to_bits() == 0x7fc0_0001));
        let i = exec_insns(
            F32,
            vec![
                F32Const(f32::from_bits(0x7fff_ffff)),
                F32Const(f32::INFINITY),
                F32Max,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F32(f) if f.is_nan() && f.to_bits() == 0x7fff_ffff));
        let i = exec_insns(
            F32,
            vec![
                F32Const(f32::from_bits(0x7f80_0001)),
                F32Const(f32::from_bits(0x7fff_ffff)),
                F32Max,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F32(f) if f.is_nan() && f.to_bits() == 0x7fc0_0001));

        let i = exec_insns(F64, vec![F64Const(0.0), F64Const(-0.0), F64Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f.to_bits() == 0x0000_0000_0000_0000));
        let i = exec_insns(F64, vec![F64Const(-0.0), F64Const(0.0), F64Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f.to_bits() == 0x0000_0000_0000_0000));
        let i = exec_insns(F64, vec![F64Const(1.0), F64Const(1.0), F64Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f == 1.0));
        let i = exec_insns(F64, vec![F64Const(-42.0), F64Const(-42.0), F64Max])
            .unwrap()
            .unwrap();
        assert!(matches!(i, Value::F64(f) if f == -42.0));
        let i = exec_insns(
            F64,
            vec![
                F64Const(f64::INFINITY),
                F64Const(f64::from_bits(0x7ff0_0000_0000_0001)),
                F64Max,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F64(f) if f.is_nan() && f.to_bits() == 0x7ff8_0000_0000_0001));
        let i = exec_insns(
            F64,
            vec![
                F64Const(f64::from_bits(0x7fff_ffff_ffff_ffff)),
                F64Const(f64::INFINITY),
                F64Max,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F64(f) if f.is_nan() && f.to_bits() == 0x7fff_ffff_ffff_ffff));
        let i = exec_insns(
            F64,
            vec![
                F64Const(f64::from_bits(0x7ff0_0000_0000_0001)),
                F64Const(f64::from_bits(0x7fff_ffff_ffff_ffff)),
                F64Max,
            ],
        )
        .unwrap()
        .unwrap();
        assert!(matches!(i, Value::F64(f) if f.is_nan() && f.to_bits() == 0x7ff8_0000_0000_0001));
    }
}
