use crate::cast;
use crate::globals::Globals;
use crate::import::{ImportInvalidError, ImportInvokeError, Importer};
use crate::memory::Memory;
use crate::stack::{CallFrame, Stack, StackAccess};
use crate::table::Table;
use crate::trap::{Result, Trap, TrapReason};
use crate::value::{Float, LittleEndian, Value};
use wain_ast as ast;
use wain_ast::AsValType;

// Note: This implementation currently ignores Wasm's thread model since MVP does not support multiple
// threads. https://webassembly.github.io/spec/core/exec/runtime.html#configurations

// TODO: Handle external values for imports and exports

#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
pub enum Run {
    Success,
    Warning(&'static str),
}

enum ExecState {
    Breaking(u32), // Breaking
    Ret,           // Returning from current function call
    Continue,      // Continuing execution
}

type ExecResult = Result<ExecState>;

fn fmin<F: Float>(l: F, r: F) -> F {
    // f32::min() cannot use directly because of NaN handling divergence.
    // For example, 42f32.min(f32::NAN) is 42
    // but (f32.min (f32.const 42) (f32.const nan)) is nan.
    if l.is_nan() {
        l
    } else if r.is_nan() {
        r
    } else {
        l.min(r)
    }
}

fn fmax<F: Float>(l: F, r: F) -> F {
    // f32::max() cannot use directly for the same reason as f32::min() and f32.min
    if l.is_nan() {
        l
    } else if r.is_nan() {
        r
    } else {
        l.max(r)
    }
}

// State of abtract machine to run wasm code. This struct contains both store and stack
pub struct Machine<'module, 'source, I: Importer> {
    module: &'module ast::Module<'source>,
    table: Table, // Only one table is allowed for MVP
    stack: Stack,
    memory: Memory, // Only one memory is allowed for MVP
    globals: Globals,
    importer: I,
}

impl<'m, 's, I: Importer> Machine<'m, 's, I> {
    // https://webassembly.github.io/spec/core/exec/modules.html#instantiation
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
                    if mod_name == "env" {
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
                                        expected_params: expected_params.iter().copied().collect(),
                                        expected_results: expected_ret.into_iter().collect(),
                                        actual_params: fty.params.iter().copied().collect(),
                                        actual_results: fty.results.clone().into_boxed_slice(),
                                    },
                                    func.start,
                                ))
                            }
                            None => { /* do nothing */ }
                        }
                    } else {
                        return Err(unknown_import(i, func.start));
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

        // 7. and 8. push empty frame (unnecessary for now)
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

        Ok(Self {
            module,
            table,
            stack,
            memory,
            globals,
            importer,
        })
    }

    pub fn module(&self) -> &'m ast::Module<'s> {
        &self.module
    }

    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    pub fn get_global(&self, name: &str) -> Option<Value> {
        self.module
            .exports
            .iter()
            .find_map(|e| match e.kind {
                ast::ExportKind::Global(idx) if e.name.0 == name => Some(idx),
                _ => None,
            })
            .map(|idx| {
                let ty = self.module.globals[idx as usize].ty;
                self.globals.get_any(idx, ty)
            })
    }

    // Returns if it has return value on stack or not
    fn invoke_import(
        &mut self,
        import: &ast::Import<'s>,
        has_ret: bool,
        pos: usize,
    ) -> Result<bool> {
        if import.mod_name.0 == "env" {
            match self
                .importer
                .call(&import.name.0, &mut self.stack, &mut self.memory)
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

    // https://webassembly.github.io/spec/core/exec/instructions.html#function-calls
    // Returns if it has return value on stack or not
    fn invoke_by_funcidx(&mut self, funcidx: u32) -> Result<bool> {
        let func = &self.module.funcs[funcidx as usize];
        let fty = &self.module.types[func.idx as usize];

        // Call this function with params
        let (locals, body) = match &func.kind {
            ast::FuncKind::Import(i) => {
                return self.invoke_import(i, !fty.results.is_empty(), func.start)
            }
            ast::FuncKind::Body { locals, expr } => (locals, expr),
        };

        // Push call frame
        let frame = CallFrame::new(&self.stack, &fty.params, locals);

        self.stack.extend_zero_values(&locals);

        for insn in body.iter() {
            match insn.execute(self, &frame)? {
                ExecState::Continue => {}
                // When using br or br_if outside control instructions, it unwinds execution in
                // the function body. Label with empty continuation is put before invoking the
                // function body (11.). It means that breaking outside control instructions will be
                // caught by this label.
                ExecState::Ret | ExecState::Breaking(_) => break,
            }
        }

        if fty.results.is_empty() {
            self.stack.restore(frame.base_addr, frame.base_idx); // Pop call frame
            Ok(false)
        } else {
            // Push 1st result value since number of result type is 1 or 0 for MVP
            let v: Value = self.stack.pop();
            self.stack.restore(frame.base_addr, frame.base_idx); // Pop call frame
            self.stack.push(v); // push result value
            Ok(true)
        }
    }

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
        let (funcidx, start) = find_func_to_invoke(name, &self.module.exports)?;
        let arg_types = &self.module.types[self.module.funcs[funcidx as usize].idx as usize].params;

        // Check parameter types
        if args
            .iter()
            .map(Value::valtype)
            .ne(arg_types.iter().copied())
        {
            return Err(Trap::new(
                TrapReason::InvokeInvalidArgs {
                    name: name.to_string(),
                    args: args.iter().cloned().collect(),
                    arg_types: arg_types.iter().copied().collect(),
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

    // As the last step of instantiation, invoke start function
    pub fn execute(&mut self) -> Result<Run> {
        // 15. If the start function is not empty, invoke it
        if let Some(start) = &self.module.entrypoint {
            // Execute entrypoint
            return self.invoke_by_funcidx(start.idx).map(|_| Run::Success);
        }

        // Note: This behavior is not described in spec. But current Clang does not emit 'start' section
        // even if a main function is included in the source. Instead, wasm-ld recognizes '_start' exported
        // function as entrypoint. Here the behavior is implemented
        for export in self.module.exports.iter() {
            if export.name.0 == "_start" {
                if let ast::ExportKind::Func(idx) = &export.kind {
                    return self.invoke_by_funcidx(*idx).map(|_| Run::Success);
                }
            }
        }

        Ok(Run::Warning("no entrypoint found. 'start' section nor '_start' exported function is set to the module"))
    }

    fn mem_addr(&mut self, mem: &ast::Mem) -> usize {
        let mut addr = self.stack.pop::<i32>() as usize;
        if let Some(off) = mem.offset {
            addr += off as usize;
        }
        addr
    }

    fn load<V: LittleEndian>(&mut self, mem: &ast::Mem, at: usize) -> Result<V> {
        let addr = self.mem_addr(mem);
        Ok(self.memory.load(addr, at)?)
    }

    fn store<V: LittleEndian>(&mut self, mem: &ast::Mem, v: V, at: usize) -> Result<()> {
        let addr = self.mem_addr(mem);
        self.memory.store(addr, v, at)?;
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
}

trait Execute<'f, 'm, 's, I: Importer> {
    fn execute(&self, machine: &mut Machine<'m, 's, I>, frame: &CallFrame<'f>) -> ExecResult;
}

// https://webassembly.github.io/spec/core/exec/instructions.html#blocks
impl<'f, 'm, 's, I: Importer> Execute<'f, 'm, 's, I> for Vec<ast::Instruction> {
    fn execute(&self, machine: &mut Machine<'m, 's, I>, frame: &CallFrame<'f>) -> ExecResult {
        // Run instruction sequence as block
        for insn in self.iter() {
            match insn.execute(machine, frame)? {
                ExecState::Continue => {}
                state => return Ok(state), // Stop executing this block on return or break
            }
        }
        Ok(ExecState::Continue)
    }
}

// https://webassembly.github.io/spec/core/exec/instructions.html
impl<'f, 'm, 's, I: Importer> Execute<'f, 'm, 's, I> for ast::Instruction {
    #[allow(clippy::cognitive_complexity)]
    fn execute(&self, machine: &mut Machine<'m, 's, I>, frame: &CallFrame<'f>) -> ExecResult {
        use ast::InsnKind::*;
        #[allow(clippy::float_cmp)]
        match &self.kind {
            // Control instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-block
            Block { ty, body } => {
                let label = machine.stack.push_label(*ty);
                match body.execute(machine, frame)? {
                    ExecState::Continue => {}
                    ExecState::Ret => return Ok(ExecState::Ret),
                    ExecState::Breaking(0) => {}
                    ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                }
                machine.stack.pop_label(label);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-loop
            Loop { ty, body } => loop {
                // Note: Difference between block and loop is the position on breaking. When reaching
                // to the end of instruction sequence, loop instruction ends execution of subsequence.
                let label = machine.stack.push_label(*ty);
                match body.execute(machine, frame)? {
                    ExecState::Continue => {
                        machine.stack.pop_label(label);
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
                let cond: i32 = machine.stack.pop();
                let label = machine.stack.push_label(*ty);
                let insns = if cond != 0 { then_body } else { else_body };
                match insns.execute(machine, frame)? {
                    ExecState::Continue => {}
                    ExecState::Ret => return Ok(ExecState::Ret),
                    ExecState::Breaking(0) => {}
                    ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                }
                machine.stack.pop_label(label);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-unreachable
            Unreachable => return Err(Trap::new(TrapReason::ReachUnreachable, self.start)),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-nop
            Nop => { /* yay! nothing to do */ }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-br
            Br(labelidx) => return Ok(ExecState::Breaking(*labelidx)),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-br-if
            BrIf(labelidx) => {
                let cond: i32 = machine.stack.pop();
                if cond != 0 {
                    return Ok(ExecState::Breaking(*labelidx));
                }
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-br-table
            BrTable {
                labels,
                default_label,
            } => {
                let idx: i32 = machine.stack.pop();
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
                machine.invoke_by_funcidx(*funcidx)?;
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-call-indirect
            CallIndirect(typeidx) => {
                let expected = &machine.module.types[*typeidx as usize];
                let elemidx: i32 = machine.stack.pop();
                let funcidx = machine.table.at(elemidx as usize, self.start)?;
                let func = &machine.module.funcs[funcidx as usize];
                let actual = &machine.module.types[func.idx as usize];
                if expected.params.iter().ne(actual.params.iter())
                    || expected.results.iter().ne(actual.results.iter())
                {
                    return Err(Trap::new(
                        TrapReason::FuncSignatureMismatch {
                            import: None,
                            expected_params: expected.params.clone().into_boxed_slice(),
                            expected_results: expected.results.clone().into_boxed_slice(),
                            actual_params: actual.params.clone().into_boxed_slice(),
                            actual_results: actual.results.clone().into_boxed_slice(),
                        },
                        self.start,
                    ));
                }
                machine.invoke_by_funcidx(funcidx)?;
            }
            // Parametric instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-drop
            Drop => {
                machine.stack.pop::<Value>();
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-select
            Select => {
                let cond: i32 = machine.stack.pop();
                if cond != 0 {
                    // pop val2 -> pop val1 -> push val1 (skip pop/push val1)
                    let _val2: Value = machine.stack.pop();
                } else {
                    // pop val2 -> pop val1 -> push val2
                    let val2: Value = machine.stack.pop();
                    let _val1: Value = machine.stack.pop();
                    machine.stack.push(val2);
                }
            }
            // Variable instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-local-get
            LocalGet(localidx) => {
                let addr = frame.local_addr(*localidx);
                match frame.local_type(*localidx) {
                    ast::ValType::I32 => machine.stack.push(machine.stack.read::<i32>(addr)),
                    ast::ValType::I64 => machine.stack.push(machine.stack.read::<i64>(addr)),
                    ast::ValType::F32 => machine.stack.push(machine.stack.read::<f32>(addr)),
                    ast::ValType::F64 => machine.stack.push(machine.stack.read::<f64>(addr)),
                }
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-local-set
            LocalSet(localidx) => {
                let addr = frame.local_addr(*localidx);
                let val = machine.stack.pop();
                machine.stack.write_any(addr, val);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-local-tee
            LocalTee(localidx) => {
                // Like local.set, but it does not change stack
                let addr = frame.local_addr(*localidx);
                let val = machine.stack.top();
                machine.stack.write_any(addr, val);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-global-get
            GlobalGet(globalidx) => match machine.module.globals[*globalidx as usize].ty {
                ast::ValType::I32 => machine.stack.push(machine.globals.get::<i32>(*globalidx)),
                ast::ValType::I64 => machine.stack.push(machine.globals.get::<i64>(*globalidx)),
                ast::ValType::F32 => machine.stack.push(machine.globals.get::<f32>(*globalidx)),
                ast::ValType::F64 => machine.stack.push(machine.globals.get::<f64>(*globalidx)),
            },
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-global-set
            GlobalSet(globalidx) => machine.globals.set_any(*globalidx, machine.stack.top()),
            // Memory instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#and
            I32Load(mem) => {
                let v: i32 = machine.load(mem, self.start)?;
                machine.stack.push(v);
            }
            I64Load(mem) => {
                let v: i64 = machine.load(mem, self.start)?;
                machine.stack.push(v);
            }
            F32Load(mem) => {
                let v: f32 = machine.load(mem, self.start)?;
                machine.stack.push(v);
            }
            F64Load(mem) => {
                let v: f64 = machine.load(mem, self.start)?;
                machine.stack.push(v);
            }
            I32Load8S(mem) => {
                let v: i8 = machine.load(mem, self.start)?;
                machine.stack.push(v as i32);
            }
            I32Load8U(mem) => {
                let v: u8 = machine.load(mem, self.start)?;
                machine.stack.push(v as i32);
            }
            I32Load16S(mem) => {
                let v: i16 = machine.load(mem, self.start)?;
                machine.stack.push(v as i32);
            }
            I32Load16U(mem) => {
                let v: u16 = machine.load(mem, self.start)?;
                machine.stack.push(v as i32);
            }
            I64Load8S(mem) => {
                let v: i8 = machine.load(mem, self.start)?;
                machine.stack.push(v as i64);
            }
            I64Load8U(mem) => {
                let v: u8 = machine.load(mem, self.start)?;
                machine.stack.push(v as i64);
            }
            I64Load16S(mem) => {
                let v: i16 = machine.load(mem, self.start)?;
                machine.stack.push(v as i64);
            }
            I64Load16U(mem) => {
                let v: u16 = machine.load(mem, self.start)?;
                machine.stack.push(v as i64);
            }
            I64Load32S(mem) => {
                let v: i32 = machine.load(mem, self.start)?;
                machine.stack.push(v as i64);
            }
            I64Load32U(mem) => {
                let v: u32 = machine.load(mem, self.start)?;
                machine.stack.push(v as i64);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-storen
            I32Store(mem) => {
                let v: i32 = machine.stack.pop();
                machine.store(mem, v, self.start)?;
            }
            I64Store(mem) => {
                let v: i64 = machine.stack.pop();
                machine.store(mem, v, self.start)?;
            }
            F32Store(mem) => {
                let v: f32 = machine.stack.pop();
                machine.store(mem, v, self.start)?;
            }
            F64Store(mem) => {
                let v: f64 = machine.stack.pop();
                machine.store(mem, v, self.start)?;
            }
            I32Store8(mem) => {
                let v: i32 = machine.stack.pop();
                machine.store(mem, v as i8, self.start)?;
            }
            I32Store16(mem) => {
                let v: i32 = machine.stack.pop();
                machine.store(mem, v as i16, self.start)?;
            }
            I64Store8(mem) => {
                let v: i64 = machine.stack.pop();
                machine.store(mem, v as i8, self.start)?;
            }
            I64Store16(mem) => {
                let v: i64 = machine.stack.pop();
                machine.store(mem, v as i16, self.start)?;
            }
            I64Store32(mem) => {
                let v: i64 = machine.stack.pop();
                machine.store(mem, v as i32, self.start)?;
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-memory-size
            MemorySize => machine.stack.push(machine.memory.size() as i32),
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-memory-grow
            MemoryGrow => {
                let pages: i32 = machine.stack.pop();
                let prev_pages = machine.memory.grow(pages as u32);
                machine.stack.push(prev_pages);
            }
            // Numeric instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-const
            I32Const(i) => machine.stack.push(*i),
            I64Const(i) => machine.stack.push(*i),
            F32Const(f) => machine.stack.push(*f),
            F64Const(f) => machine.stack.push(*f),
            // Integer operations
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-iclz
            I32Clz => machine.unop::<i32, _>(|v| v.leading_zeros() as i32),
            I64Clz => machine.unop::<i64, _>(|v| v.leading_zeros() as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ictz
            I32Ctz => machine.unop::<i32, _>(|v| v.trailing_zeros() as i32),
            I64Ctz => machine.unop::<i64, _>(|v| v.trailing_zeros() as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ipopcnt
            I32Popcnt => machine.unop::<i32, _>(|v| v.count_ones() as i32),
            I64Popcnt => machine.unop::<i64, _>(|v| v.count_ones() as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-iadd
            I32Add => machine.binop::<i32, _>(|l, r| l.wrapping_add(r)),
            I64Add => machine.binop::<i64, _>(|l, r| l.wrapping_add(r)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-isub
            I32Sub => machine.binop::<i32, _>(|l, r| l.wrapping_sub(r)),
            I64Sub => machine.binop::<i64, _>(|l, r| l.wrapping_sub(r)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-imul
            I32Mul => machine.binop::<i32, _>(|l, r| l.wrapping_mul(r)),
            I64Mul => machine.binop::<i64, _>(|l, r| l.wrapping_mul(r)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-idiv-s
            // Note: According to i32.wast and i64.wast, integer overflow on idiv_s should be trapped.
            // This is intended behavior: https://github.com/WebAssembly/spec/issues/1185#issuecomment-619412936
            I32DivS => machine.binop_trap::<i32, _>(|l, r| match l.checked_div(r) {
                Some(i) => Ok(i),
                None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
            })?,
            I64DivS => machine.binop_trap::<i64, _>(|l, r| match l.checked_div(r) {
                Some(i) => Ok(i),
                None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-idiv-u
            I32DivU => {
                machine.binop_trap::<i32, _>(|l, r| match (l as u32).checked_div(r as u32) {
                    Some(u) => Ok(u as i32),
                    None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
                })?
            }
            I64DivU => {
                machine.binop_trap::<i64, _>(|l, r| match (l as u64).checked_div(r as u64) {
                    Some(u) => Ok(u as i64),
                    None => Err(Trap::new(TrapReason::DivByZeroOrOverflow, self.start)),
                })?
            }
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irem-s
            // Note: rem_s should not cause overflow. For example, 0x80000000 % -1 causes overflow
            // in Rust, but Wasm test case says it should return 0. Note that Go has special rule
            // that x % -1 is 0 when x is the most negative value.
            // This is intended behavior: https://github.com/WebAssembly/spec/issues/1185#issuecomment-619412936
            I32RemS => machine.binop_trap::<i32, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok(l.wrapping_rem(r))
                }
            })?,
            I64RemS => machine.binop_trap::<i64, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok(l.wrapping_rem(r))
                }
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irem-u
            I32RemU => machine.binop_trap::<i32, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok((l as u32 % r as u32) as i32) // for unsigned integers overflow never occurs
                }
            })?,
            I64RemU => machine.binop_trap::<i64, _>(|l, r| {
                if r == 0 {
                    Err(Trap::new(TrapReason::RemZeroDivisor, self.start))
                } else {
                    Ok((l as u64 % r as u64) as i64) // for unsigned integers overflow never occurs
                }
            })?,
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-iand
            I32And => machine.binop::<i32, _>(|l, r| l & r),
            I64And => machine.binop::<i64, _>(|l, r| l & r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ior
            I32Or => machine.binop::<i32, _>(|l, r| l | r),
            I64Or => machine.binop::<i64, _>(|l, r| l | r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ixor
            I32Xor => machine.binop::<i32, _>(|l, r| l ^ r),
            I64Xor => machine.binop::<i64, _>(|l, r| l ^ r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ishl
            I32Shl => machine.binop::<i32, _>(|l, r| l.wrapping_shl(r as u32)),
            I64Shl => machine.binop::<i64, _>(|l, r| l.wrapping_shl(r as u32)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ishr-s
            I32ShrS => machine.binop::<i32, _>(|l, r| l.wrapping_shr(r as u32)),
            I64ShrS => machine.binop::<i64, _>(|l, r| l.wrapping_shr(r as u32)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ishr-u
            I32ShrU => machine.binop::<i32, _>(|l, r| (l as u32).wrapping_shr(r as u32) as i32),
            I64ShrU => machine.binop::<i64, _>(|l, r| (l as u64).wrapping_shr(r as u32) as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irotl
            I32Rotl => machine.binop::<i32, _>(|l, r| l.rotate_left(r as u32)),
            I64Rotl => machine.binop::<i64, _>(|l, r| l.rotate_left(r as u32)),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-irotr
            I32Rotr => machine.binop::<i32, _>(|l, r| l.rotate_right(r as u32)),
            I64Rotr => machine.binop::<i64, _>(|l, r| l.rotate_right(r as u32)),
            // Float number operations
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fabs
            F32Abs => machine.unop::<f32, _>(|f| f.abs()),
            F64Abs => machine.unop::<f64, _>(|f| f.abs()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fneg
            F32Neg => machine.unop::<f32, _>(|f| -f),
            F64Neg => machine.unop::<f64, _>(|f| -f),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fceil
            F32Ceil => machine.unop::<f32, _>(|f| f.ceil()),
            F64Ceil => machine.unop::<f64, _>(|f| f.ceil()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ffloor
            F32Floor => machine.unop::<f32, _>(|f| f.floor()),
            F64Floor => machine.unop::<f64, _>(|f| f.floor()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ftrunc
            F32Trunc => machine.unop::<f32, _>(|f| f.trunc()),
            F64Trunc => machine.unop::<f64, _>(|f| f.trunc()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fnearest
            F32Nearest => machine.unop::<f32, _>(|f| {
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
            F64Nearest => machine.unop::<f64, _>(|f| {
                // f64::round() is not available for the same reason as f32.nearest
                let fround = f.round();
                if (f - fround).abs() == 0.5 && fround % 2.0 != 0.0 {
                    f.trunc()
                } else {
                    fround
                }
            }),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fsqrt
            F32Sqrt => machine.unop::<f32, _>(|f| f.sqrt()),
            F64Sqrt => machine.unop::<f64, _>(|f| f.sqrt()),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fadd
            F32Add => machine.binop::<f32, _>(|l, r| l + r),
            F64Add => machine.binop::<f64, _>(|l, r| l + r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fsub
            F32Sub => machine.binop::<f32, _>(|l, r| l - r),
            F64Sub => machine.binop::<f64, _>(|l, r| l - r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fmul
            F32Mul => machine.binop::<f32, _>(|l, r| l * r),
            F64Mul => machine.binop::<f64, _>(|l, r| l * r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fdiv
            F32Div => machine.binop::<f32, _>(|l, r| l / r),
            F64Div => machine.binop::<f64, _>(|l, r| l / r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fmin
            F32Min => machine.binop::<f32, _>(fmin),
            F64Min => machine.binop::<f64, _>(fmin),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fmax
            F32Max => machine.binop::<f32, _>(fmax),
            F64Max => machine.binop::<f64, _>(fmax),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fcopysign
            F32Copysign => machine.binop::<f32, _>(|l, r| l.copysign(r)),
            F64Copysign => machine.binop::<f64, _>(|l, r| l.copysign(r)),
            // Integer comparison
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ieqz
            I32Eqz => machine.testop::<i32, _>(|i| i == 0),
            I64Eqz => machine.testop::<i64, _>(|i| i == 0),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ieq
            I32Eq => machine.relop::<i32, _>(|l, r| l == r),
            I64Eq => machine.relop::<i64, _>(|l, r| l == r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ine
            I32Ne => machine.relop::<i32, _>(|l, r| l != r),
            I64Ne => machine.relop::<i64, _>(|l, r| l != r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ilt-s
            I32LtS => machine.relop::<i32, _>(|l, r| l < r),
            I64LtS => machine.relop::<i64, _>(|l, r| l < r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ilt-u
            I32LtU => machine.relop::<i32, _>(|l, r| (l as u32) < r as u32),
            I64LtU => machine.relop::<i64, _>(|l, r| (l as u64) < r as u64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-igt-s
            I32GtS => machine.relop::<i32, _>(|l, r| l > r),
            I64GtS => machine.relop::<i64, _>(|l, r| l > r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-igt-u
            I32GtU => machine.relop::<i32, _>(|l, r| l as u32 > r as u32),
            I64GtU => machine.relop::<i64, _>(|l, r| l as u64 > r as u64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ile-s
            I32LeS => machine.relop::<i32, _>(|l, r| l <= r),
            I64LeS => machine.relop::<i64, _>(|l, r| l <= r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ile-u
            I32LeU => machine.relop::<i32, _>(|l, r| l as u32 <= r as u32),
            I64LeU => machine.relop::<i64, _>(|l, r| l as u64 <= r as u64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ige-s
            I32GeS => machine.relop::<i32, _>(|l, r| l >= r),
            I64GeS => machine.relop::<i64, _>(|l, r| l >= r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-ige-u
            I32GeU => machine.relop::<i32, _>(|l, r| l as u32 >= r as u32),
            I64GeU => machine.relop::<i64, _>(|l, r| l as u64 >= r as u64),
            // Float number comparison
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-feq
            F32Eq => machine.relop::<f32, _>(|l, r| l == r),
            F64Eq => machine.relop::<f64, _>(|l, r| l == r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fne
            F32Ne => machine.relop::<f32, _>(|l, r| l != r),
            F64Ne => machine.relop::<f64, _>(|l, r| l != r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-flt
            F32Lt => machine.relop::<f32, _>(|l, r| l < r),
            F64Lt => machine.relop::<f64, _>(|l, r| l < r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fgt
            F32Gt => machine.relop::<f32, _>(|l, r| l > r),
            F64Gt => machine.relop::<f64, _>(|l, r| l > r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fle
            F32Le => machine.relop::<f32, _>(|l, r| l <= r),
            F64Le => machine.relop::<f64, _>(|l, r| l <= r),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-fge
            F32Ge => machine.relop::<f32, _>(|l, r| l >= r),
            F64Ge => machine.relop::<f64, _>(|l, r| l >= r),
            // Conversion
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-extend-u
            I64ExtendI32U => machine.cvtop::<i32, i64, _>(|v| v as u32 as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-extend-s
            I64ExtendI32S => machine.cvtop::<i32, i64, _>(|v| v as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-wrap
            I32WrapI64 => machine.cvtop::<i64, i32, _>(|v| v as i32),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-trunc-u
            I32TruncF32U => machine.cvtop::<f32, i32, _>(|v| cast::f32_to_u32(v) as i32),
            I32TruncF64U => machine.cvtop::<f64, i32, _>(|v| cast::f64_to_u32(v) as i32),
            I64TruncF32U => machine.cvtop::<f32, i64, _>(|v| cast::f32_to_u64(v) as i64),
            I64TruncF64U => machine.cvtop::<f64, i64, _>(|v| cast::f64_to_u64(v) as i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-trunc-s
            I32TruncF32S => machine.cvtop::<f32, i32, _>(cast::f32_to_i32),
            I32TruncF64S => machine.cvtop::<f64, i32, _>(cast::f64_to_i32),
            I64TruncF32S => machine.cvtop::<f32, i64, _>(cast::f32_to_i64),
            I64TruncF64S => machine.cvtop::<f64, i64, _>(cast::f64_to_i64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-promote
            F64PromoteF32 => machine.cvtop::<f32, f64, _>(|v| v as f64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-demote
            F32DemoteF64 => machine.cvtop::<f64, f32, _>(|v| v as f32),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-convert-u
            F32ConvertI32U => machine.cvtop::<i32, f32, _>(|v| v as u32 as f32),
            F32ConvertI64U => machine.cvtop::<i64, f32, _>(|v| v as u64 as f32),
            F64ConvertI32U => machine.cvtop::<i32, f64, _>(|v| v as u32 as f64),
            F64ConvertI64U => machine.cvtop::<i64, f64, _>(|v| v as u64 as f64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-convert-s
            F32ConvertI32S => machine.cvtop::<i32, f32, _>(|v| v as f32),
            F32ConvertI64S => machine.cvtop::<i64, f32, _>(|v| v as f32),
            F64ConvertI32S => machine.cvtop::<i32, f64, _>(|v| v as f64),
            F64ConvertI64S => machine.cvtop::<i64, f64, _>(|v| v as f64),
            // https://webassembly.github.io/spec/core/exec/numerics.html#op-reinterpret
            // Don't need to modify stack. Just changing type to t2 is enough.
            I32ReinterpretF32 => machine.stack.write_top_type(i32::VAL_TYPE),
            I64ReinterpretF64 => machine.stack.write_top_type(i64::VAL_TYPE),
            F32ReinterpretI32 => machine.stack.write_top_type(f32::VAL_TYPE),
            F64ReinterpretI64 => machine.stack.write_top_type(f64::VAL_TYPE),
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

        fn exec(file: PathBuf) -> (Run, Vec<u8>) {
            let source = fs::read_to_string(file).unwrap();
            let ast = unwrap(parse(&source));
            unwrap(validate(&ast));
            let mut stdout = vec![];
            let run = {
                let importer = DefaultImporter::with_stdio(Discard, &mut stdout);
                let mut machine = unwrap(Machine::instantiate(&ast.module, importer));
                unwrap(machine.execute())
            };
            (run, stdout)
        }

        let mut dir = env::current_dir().unwrap();
        dir.pop();
        dir.push("examples");
        dir.push("hello");
        let dir = dir;

        let (run, stdout) = exec(dir.join("hello.wat"));
        assert_eq!(run, Run::Success);
        assert_eq!(stdout, b"Hello, world\n");

        let (run, stdout) = exec(dir.join("hello_global.wat"));
        assert_eq!(run, Run::Success);
        assert_eq!(stdout, b"Hello, world\n");

        let (run, stdout) = exec(dir.join("hello_indirect_call.wat"));
        assert_eq!(run, Run::Success);
        assert_eq!(stdout, b"Hello, world\n");

        let (run, stdout) = exec(dir.join("hello_struct.wat"));
        assert_eq!(run, Run::Success);
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
        let mut machine = Machine::instantiate(&module, importer)?;
        machine.invoke("test", &[])
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
}
