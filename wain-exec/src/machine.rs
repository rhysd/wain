use crate::globals::Globals;
use crate::memory::Memory;
use crate::stack::{CallFrame, Stack};
use crate::table::Table;
use crate::trap::{Result, Trap, TrapReason};
use crate::value::Value;
use std::io;
use wain_ast as ast;

// Note: This implementation currently ignores Wasm's thread model since MVP does not support multiple
// threads. https://webassembly.github.io/spec/core/exec/runtime.html#configurations

// TODO: Handle external values for imports and exports

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

// Abtract machine to run wasm code
pub struct Machine<'module, 'source> {
    module: &'module ast::Module<'source>,
    table: Table, // Only one table is allowed for MVP
    stack: Stack,
    memory: Memory, // Only one memory is allowed for MVP
    globals: Globals,
}

impl<'m, 'a> Machine<'m, 'a> {
    // https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    pub fn instantiate(module: &'m ast::Module<'a>) -> Result<Self> {
        // TODO: 2., 3., 4. Validate external values before instantiate globals

        // 5. global initialization values determined by module and externval
        let globals = Globals::instantiate(&module.globals)?;

        // 6. a new module instance allocated from module in store S
        // https://webassembly.github.io/spec/core/exec/modules.html#alloc-module

        // 6.2 allocate functions
        for func in module.funcs.iter() {
            if let ast::FuncKind::Import(i) = &func.kind {
                if i.mod_name.0 != "env" || i.name.0 != "putchar" {
                    return Err(Trap::unknown_import(i, "function", func.start));
                }
            }
        }

        // 6.3 allocate table
        let mut table = Table::allocate(&module.tables)?;
        // 6.4 allocate memory
        let mut memory = Memory::allocate(&module.memories)?;

        // 7. and 8. push empty frame (unnecessary for now)
        let stack = Stack::new();

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
        })
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#function-calls
    fn invoke(&mut self, funcidx: u32) -> ExecResult {
        let func = &self.module.funcs[funcidx as usize];

        // Call this function with params
        let (locals, body) = match &func.kind {
            ast::FuncKind::Import(i) => {
                if i.mod_name.0 == "env" && i.name.0 == "putchar" {
                    return self.putchar(func.start).map(|_| ExecState::Continue);
                } else {
                    return Err(Trap::unknown_import(i, "function", func.start));
                }
            }
            ast::FuncKind::Body { locals, expr } => (locals, expr),
        };

        let fty = &self.module.types[func.idx as usize];

        // Push call frame
        let mut frame = CallFrame::new(&self.stack, &fty.params, locals);

        for insn in body.iter() {
            match insn.execute(self, &mut frame)? {
                ExecState::Continue => {}
                ExecState::Ret => break,
                ExecState::Breaking(_) => unreachable!(), // thanks to validation, this does not occur
            }
        }

        if fty.results.is_empty() {
            self.stack.restore(frame.base_addr, frame.base_idx); // Pop call frame
        } else {
            // Push 1st result value since number of result type is 1 or 0 for MVP
            let v: Value = self.stack.pop();
            self.stack.restore(frame.base_addr, frame.base_idx); // Pop call frame
            self.stack.push(v); // push result value
        }

        Ok(ExecState::Continue)
    }

    // As the last step of instantiation, invoke start function
    pub fn execute(&mut self) -> Result<Run> {
        // 15. If the start function is not empty, invoke it
        if let Some(start) = &self.module.entrypoint {
            // Execute entrypoint
            return self.invoke(start.idx).map(|_| Run::Success);
        }

        // Note: This behavior is not described in spec. But current Clang does not emit 'start' section
        // even if a main function is included in the source. Instead, wasm-ld recognizes '_start' exported
        // function as entrypoint. Here the behavior is implemented
        for export in self.module.exports.iter() {
            if export.name.0 == "_start" {
                if let ast::ExportKind::Func(idx) = &export.kind {
                    return self.invoke(*idx).map(|_| Run::Success);
                }
            }
        }

        Ok(Run::Warning("no entrypoint found. 'start' section nor '_start' exported function is set to the module"))
    }

    fn putchar(&mut self, offset: usize) -> Result<()> {
        use std::io::Write;
        let v: i32 = self.stack.pop();
        let b = v as u8;
        match io::stdout().write(&[b]) {
            Ok(_) => Ok(()),
            Err(e) => Err(Trap::new(TrapReason::IoError(e), offset)),
        }
    }
}

trait Execute<'f, 'm, 'a> {
    fn execute(&self, machine: &mut Machine<'m, 'a>, frame: &mut CallFrame<'f>) -> ExecResult;
}

// https://webassembly.github.io/spec/core/exec/instructions.html#blocks
impl<'f, 'm, 'a> Execute<'f, 'm, 'a> for Vec<ast::Instruction> {
    fn execute(&self, machine: &mut Machine<'m, 'a>, frame: &mut CallFrame<'f>) -> ExecResult {
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
impl<'f, 'm, 'a> Execute<'f, 'm, 'a> for ast::Instruction {
    fn execute(&self, machine: &mut Machine<'m, 'a>, frame: &mut CallFrame<'f>) -> ExecResult {
        use ast::InsnKind::*;
        match &self.kind {
            // Control instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-block
            Block { ty, body } => {
                let label = machine.stack.push_label(ty);
                match body.execute(machine, frame)? {
                    ExecState::Continue => {}
                    ExecState::Ret => return Ok(ExecState::Ret),
                    ExecState::Breaking(0) => {}
                    ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                }
                machine.stack.pop_label(label);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-loop
            Loop { ty, body } => {
                let label = machine.stack.push_label(ty);
                loop {
                    match body.execute(machine, frame)? {
                        ExecState::Continue => {} // next iteration
                        ExecState::Ret => return Ok(ExecState::Ret),
                        ExecState::Breaking(0) => break,
                        ExecState::Breaking(level) => return Ok(ExecState::Breaking(level - 1)),
                    }
                }
                machine.stack.pop_label(label);
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-if
            If {
                ty,
                then_body,
                else_body,
            } => {
                let cond: i32 = machine.stack.pop();
                let label = machine.stack.push_label(ty);
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
            Call(funcidx) => return machine.invoke(*funcidx),
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
                            expected_params: expected.params.clone(),
                            expected_results: expected.results.clone(),
                            actual_params: actual.params.clone(),
                            actual_results: actual.results.clone(),
                        },
                        self.start,
                    ));
                }
                return machine.invoke(funcidx);
            }
            // Parametric instructions
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-drop
            Drop => {
                machine.stack.pop::<Value>();
            }
            // https://webassembly.github.io/spec/core/exec/instructions.html#exec-select
            Select => {
                let cond: i32 = machine.stack.pop();
                let rhs: Value = machine.stack.pop();
                let lhs: Value = machine.stack.pop();
                machine.stack.push(if cond == 0 { lhs } else { rhs });
            }
            // Variable instructions
            LocalGet(idx) => unimplemented!("LocalGet"),
            LocalSet(idx) => unimplemented!("LocalSet"),
            LocalTee(idx) => unimplemented!("LocalTee"),
            GlobalGet(idx) => unimplemented!("GlobalGet"),
            GlobalSet(idx) => unimplemented!("GlobalSet"),
            // Memory instructions
            I32Load(mem) => unimplemented!("I32Load"),
            I64Load(mem) => unimplemented!("I64Load"),
            F32Load(mem) => unimplemented!("F32Load"),
            F64Load(mem) => unimplemented!("F64Load"),
            I32Load8S(mem) => unimplemented!("I32Load8S"),
            I32Load8U(mem) => unimplemented!("I32Load8U"),
            I32Load16S(mem) => unimplemented!("I32Load16S"),
            I32Load16U(mem) => unimplemented!("I32Load16U"),
            I64Load8S(mem) => unimplemented!("I64Load8S"),
            I64Load8U(mem) => unimplemented!("I64Load8U"),
            I64Load16S(mem) => unimplemented!("I64Load16S"),
            I64Load16U(mem) => unimplemented!("I64Load16U"),
            I64Load32S(mem) => unimplemented!("I64Load32S"),
            I64Load32U(mem) => unimplemented!("I64Load32U"),
            I32Store(mem) => unimplemented!("I32Store"),
            I64Store(mem) => unimplemented!("I64Store"),
            F32Store(mem) => unimplemented!("F32Store"),
            F64Store(mem) => unimplemented!("F64Store"),
            I32Store8(mem) => unimplemented!("I32Store8"),
            I32Store16(mem) => unimplemented!("I32Store16"),
            I64Store8(mem) => unimplemented!("I64Store8"),
            I64Store16(mem) => unimplemented!("I64Store16"),
            I64Store32(mem) => unimplemented!("I64Store32"),
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
            // i32 operations
            I32Clz => unimplemented!("I32Clz"),
            I32Ctz => unimplemented!("I32Ctz"),
            I32Popcnt => unimplemented!("I32Popcnt"),
            I32Add => unimplemented!("I32Add"),
            I32Sub => unimplemented!("I32Sub"),
            I32Mul => unimplemented!("I32Mul"),
            I32DivS => unimplemented!("I32DivS"),
            I32DivU => unimplemented!("I32DivU"),
            I32RemS => unimplemented!("I32RemS"),
            I32RemU => unimplemented!("I32RemU"),
            I32And => unimplemented!("I32And"),
            I32Or => unimplemented!("I32Or"),
            I32Xor => unimplemented!("I32Xor"),
            I32Shl => unimplemented!("I32Shl"),
            I32ShrS => unimplemented!("I32ShrS"),
            I32ShrU => unimplemented!("I32ShrU"),
            I32Rotl => unimplemented!("I32Rotl"),
            I32Rotr => unimplemented!("I32Rotr"),
            // i64 operations
            I64Clz => unimplemented!("I64Clz"),
            I64Ctz => unimplemented!("I64Ctz"),
            I64Popcnt => unimplemented!("I64Popcnt"),
            I64Add => unimplemented!("I64Add"),
            I64Sub => unimplemented!("I64Sub"),
            I64Mul => unimplemented!("I64Mul"),
            I64DivS => unimplemented!("I64DivS"),
            I64DivU => unimplemented!("I64DivU"),
            I64RemS => unimplemented!("I64RemS"),
            I64RemU => unimplemented!("I64RemU"),
            I64And => unimplemented!("I64And"),
            I64Or => unimplemented!("I64Or"),
            I64Xor => unimplemented!("I64Xor"),
            I64Shl => unimplemented!("I64Shl"),
            I64ShrS => unimplemented!("I64ShrS"),
            I64ShrU => unimplemented!("I64ShrU"),
            I64Rotl => unimplemented!("I64Rotl"),
            I64Rotr => unimplemented!("I64Rotr"),
            // f32 operations
            F32Abs => unimplemented!("F32Abs"),
            F32Neg => unimplemented!("F32Neg"),
            F32Ceil => unimplemented!("F32Ceil"),
            F32Floor => unimplemented!("F32Floor"),
            F32Trunc => unimplemented!("F32Trunc"),
            F32Nearest => unimplemented!("F32Nearest"),
            F32Sqrt => unimplemented!("F32Sqrt"),
            F32Add => unimplemented!("F32Add"),
            F32Sub => unimplemented!("F32Sub"),
            F32Mul => unimplemented!("F32Mul"),
            F32Div => unimplemented!("F32Div"),
            F32Min => unimplemented!("F32Min"),
            F32Max => unimplemented!("F32Max"),
            F32Copysign => unimplemented!("F32Copysign"),
            // f64 operations
            F64Abs => unimplemented!("F64Abs"),
            F64Neg => unimplemented!("F64Neg"),
            F64Ceil => unimplemented!("F64Ceil"),
            F64Floor => unimplemented!("F64Floor"),
            F64Trunc => unimplemented!("F64Trunc"),
            F64Nearest => unimplemented!("F64Nearest"),
            F64Sqrt => unimplemented!("F64Sqrt"),
            F64Add => unimplemented!("F64Add"),
            F64Sub => unimplemented!("F64Sub"),
            F64Mul => unimplemented!("F64Mul"),
            F64Div => unimplemented!("F64Div"),
            F64Min => unimplemented!("F64Min"),
            F64Max => unimplemented!("F64Max"),
            F64Copysign => unimplemented!("F64Copysign"),
            // i32 comparison
            I32Eqz => unimplemented!("I32Eqz"),
            I32Eq => unimplemented!("I32Eq"),
            I32Ne => unimplemented!("I32Ne"),
            I32LtS => unimplemented!("I32LtS"),
            I32LtU => unimplemented!("I32LtU"),
            I32GtS => unimplemented!("I32GtS"),
            I32GtU => unimplemented!("I32GtU"),
            I32LeS => unimplemented!("I32LeS"),
            I32LeU => unimplemented!("I32LeU"),
            I32GeS => unimplemented!("I32GeS"),
            I32GeU => unimplemented!("I32GeU"),
            // i64 comparison
            I64Eqz => unimplemented!("I64Eqz"),
            I64Eq => unimplemented!("I64Eq"),
            I64Ne => unimplemented!("I64Ne"),
            I64LtS => unimplemented!("I64LtS"),
            I64LtU => unimplemented!("I64LtU"),
            I64GtS => unimplemented!("I64GtS"),
            I64GtU => unimplemented!("I64GtU"),
            I64LeS => unimplemented!("I64LeS"),
            I64LeU => unimplemented!("I64LeU"),
            I64GeS => unimplemented!("I64GeS"),
            I64GeU => unimplemented!("I64GeU"),
            // f32 comparison
            F32Eq => unimplemented!("F32Eq"),
            F32Ne => unimplemented!("F32Ne"),
            F32Lt => unimplemented!("F32Lt"),
            F32Gt => unimplemented!("F32Gt"),
            F32Le => unimplemented!("F32Le"),
            F32Ge => unimplemented!("F32Ge"),
            // f64 comparison
            F64Eq => unimplemented!("F64Eq"),
            F64Ne => unimplemented!("F64Ne"),
            F64Lt => unimplemented!("F64Lt"),
            F64Gt => unimplemented!("F64Gt"),
            F64Le => unimplemented!("F64Le"),
            F64Ge => unimplemented!("F64Ge"),
            // Conversion
            I32WrapI64 => unimplemented!("I32WrapI64"),
            I32TruncF32S => unimplemented!("I32TruncF32S"),
            I32TruncF32U => unimplemented!("I32TruncF32U"),
            I32TruncF64S => unimplemented!("I32TruncF64S"),
            I32TruncF64U => unimplemented!("I32TruncF64U"),
            I64ExtendI32S => unimplemented!("I64ExtendI32S"),
            I64ExtendI32U => unimplemented!("I64ExtendI32U"),
            I64TruncF32S => unimplemented!("I64TruncF32S"),
            I64TruncF32U => unimplemented!("I64TruncF32U"),
            I64TruncF64S => unimplemented!("I64TruncF64S"),
            I64TruncF64U => unimplemented!("I64TruncF64U"),
            F32ConvertI32S => unimplemented!("F32ConvertI32S"),
            F32ConvertI32U => unimplemented!("F32ConvertI32U"),
            F32ConvertI64S => unimplemented!("F32ConvertI64S"),
            F32ConvertI64U => unimplemented!("F32ConvertI64U"),
            F32DemoteF64 => unimplemented!("F32DemoteF64"),
            F64ConvertI32S => unimplemented!("F64ConvertI32S"),
            F64ConvertI32U => unimplemented!("F64ConvertI32U"),
            F64ConvertI64S => unimplemented!("F64ConvertI64S"),
            F64ConvertI64U => unimplemented!("F64ConvertI64U"),
            F64PromoteF32 => unimplemented!("F64PromoteF32"),
            I32ReinterpretF32 => unimplemented!("I32ReinterpretF32"),
            I64ReinterpretF64 => unimplemented!("I64ReinterpretF64"),
            F32ReinterpretI32 => unimplemented!("F32ReinterpretI32"),
            F64ReinterpretI64 => unimplemented!("F64ReinterpretI64"),
        }
        Ok(ExecState::Continue)
    }
}
