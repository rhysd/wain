// Validate instruction sequence for
// - function body
// - subsequence in control instructions like block, loop, if
// - initilization sequences for globals and table elems
//
// Algorithm for validating instructions:
//   https://webassembly.github.io/spec/core/appendix/algorithm.html#algo-valid

use crate::error::{ErrorKind, Ordinal, Result};
use crate::Context as OuterContext;
use std::fmt;
use std::mem;
use wain_ast::source::Source;
use wain_ast::*;

#[derive(Copy, Clone)]
enum Type {
    Known(ValType),
    Unknown,
}
impl Type {
    const I32: Type = Type::Known(ValType::I32);
    const I64: Type = Type::Known(ValType::I64);
    const F32: Type = Type::Known(ValType::F32);
    const F64: Type = Type::Known(ValType::F64);
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => f.write_str("unknown"),
            Type::Known(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Default)]
struct CtrlFrame {
    height: usize,
    offset: usize,
    // Unreachability of current instruction sequence
    unreachable: bool,
}

// https://webassembly.github.io/spec/core/valid/conventions.html#context
struct FuncBodyContext<'outer, 'module: 'outer, 'source: 'module, S: Source> {
    current_op: &'static str,
    current_offset: usize,
    outer: &'outer OuterContext<'module, 'source, S>,
    // Types on stack to check operands of instructions such as unreachable, br, table_br
    op_stack: Vec<Type>,
    // Index of current control frame
    current_frame: CtrlFrame,
    // Label stack to verify jump instructions. None means no type for the label
    label_stack: Vec<Option<ValType>>,
    // The list of locals declared in the current function (including parameters), represented by their value type.
    // It's empty when validating outside function.
    params: &'outer [ValType],
    locals: &'outer [ValType],
    // Return type of the current function if exists
    ret_ty: Option<ValType>,
}

impl<'outer, 'm, 's, S: Source> FuncBodyContext<'outer, 'm, 's, S> {
    fn error<T>(&self, kind: ErrorKind) -> Result<T, S> {
        self.outer.error(kind, self.current_op, self.current_offset)
    }

    fn ensure_ctrl_frame_not_empty(&self) -> Result<(), S> {
        if self.op_stack.len() > self.current_frame.height {
            return Ok(());
        }

        if self.current_frame.unreachable {
            // Reach top of current control frame, but it's ok when unreachable. For example,
            //
            //   unreachable i32.add
            //
            // should be valid. In the case operands of i32.add are not checked. For example,
            //
            //   unreachable (i64.const 0) i32.add
            //
            // should be invalid. In the case one operand of i32.add is i64. To archive this check,
            // popping operand stack has trick. Unknown type is simply ignored on check.
            return Ok(());
        }

        // When not unreachable and stack hits top of current control frame, this instruction
        // sequence is invalid if some value should have been pushed on stack
        self.error(ErrorKind::CtrlFrameEmpty {
            op: self.current_op,
            frame_start: self.current_frame.offset,
            idx_in_op_stack: self.current_frame.height,
        })
    }

    fn ensure_op_stack_top(&mut self, expected: Type) -> Result<Type, S> {
        self.ensure_ctrl_frame_not_empty()?;
        if self.op_stack.len() == self.current_frame.height {
            assert!(self.current_frame.unreachable);
            self.op_stack.push(expected);
            return Ok(expected);
        }

        let actual = self.op_stack[self.op_stack.len() - 1];

        // Note: None here means unknown type due to unreachable
        if let (Type::Known(expected), Type::Known(actual)) = (expected, actual) {
            if actual != expected {
                return self.error(ErrorKind::TypeMismatch {
                    expected: Some(expected),
                    actual: Some(actual),
                });
            }
        }

        Ok(actual)
    }

    fn pop_op_stack(&mut self, expected: Type) -> Result<Type, S> {
        let ty = self.ensure_op_stack_top(expected)?;
        self.op_stack.pop();
        Ok(ty)
    }

    fn push_control_frame(&mut self, offset: usize) -> CtrlFrame {
        let new = CtrlFrame {
            height: self.op_stack.len(),
            offset,
            unreachable: false,
        };
        mem::replace(&mut self.current_frame, new)
    }

    fn pop_control_frame(&mut self, prev: CtrlFrame, ty: Option<ValType>) -> Result<(), S> {
        // control frame top is validated by pop_op_stack
        if let Some(ty) = ty {
            self.pop_op_stack(Type::Known(ty))?;
        }
        let expected = self.current_frame.height;
        let actual = self.op_stack.len();
        assert!(expected <= actual);
        if expected != actual {
            return self.error(ErrorKind::InvalidStackDepth {
                expected,
                actual,
                remaining: format!("{:?}", &self.op_stack[expected..]),
            });
        }
        self.current_frame = prev;
        Ok(())
    }

    fn mark_unreachable(&mut self, stack_top: Option<ValType>) -> Result<(), S> {
        if let Some(ty) = stack_top {
            self.pop_op_stack(Type::Known(ty))?;
        }
        assert!(self.op_stack.len() >= self.current_frame.height);
        self.op_stack.truncate(self.current_frame.height);
        self.current_frame.unreachable = true;
        Ok(())
    }

    fn pop_label_stack(&mut self) {
        assert!(!self.label_stack.is_empty());
        self.label_stack.pop();
    }

    fn validate_label_idx(&self, idx: u32) -> Result<Option<ValType>, S> {
        let len = self.label_stack.len();
        if (idx as usize) >= len {
            return self.error(ErrorKind::IndexOutOfBounds {
                idx,
                upper: len,
                what: "label",
            });
        }
        let ty = self.label_stack[len - 1 - (idx as usize)];
        Ok(ty)
    }

    fn validate_local_idx(&self, idx: u32) -> Result<ValType, S> {
        let uidx = idx as usize;
        if let Some(ty) = self.params.get(uidx) {
            return Ok(*ty);
        }

        if let Some(ty) = self.locals.get(uidx - self.params.len()) {
            Ok(*ty)
        } else {
            self.error(ErrorKind::IndexOutOfBounds {
                idx,
                upper: self.locals.len(),
                what: "local variable",
            })
            .map_err(|e| {
                e.update_msg(format!(
                    "access to {} local variable at {}",
                    Ordinal(uidx - self.params.len()),
                    self.current_op
                ))
            })
        }
    }

    fn validate_memarg(&self, mem: &Mem, bits: u8) -> Result<(), S> {
        self.outer
            .memory_from_idx(0, self.current_op, self.current_offset)?;
        // The alignment must not be larger than the bit width of t divided by 8.
        if let Some(align) = mem.align {
            if align > bits / 8 {
                return self.error(ErrorKind::TooLargeAlign { align, bits });
            }
        }
        Ok(())
    }

    fn validate_load(&mut self, mem: &Mem, bits: u8, ty: ValType) -> Result<(), S> {
        self.validate_memarg(mem, bits)?;
        self.pop_op_stack(Type::I32)?; // load address
        self.op_stack.push(Type::Known(ty));
        Ok(())
    }

    fn validate_store(&mut self, mem: &Mem, bits: u8, ty: ValType) -> Result<(), S> {
        self.validate_memarg(mem, bits)?;
        self.pop_op_stack(Type::Known(ty))?; // value to store
        self.pop_op_stack(Type::I32)?; // store address
        Ok(())
    }

    fn validate_convert(&mut self, from: ValType, to: ValType) -> Result<(), S> {
        self.pop_op_stack(Type::Known(from))?;
        self.op_stack.push(Type::Known(to));
        Ok(())
    }
}

pub(crate) fn validate_func_body<'outer, 'm, 's, S: Source>(
    body: &'outer [Instruction],
    func_ty: &'outer FuncType,
    locals: &'outer [ValType],
    outer: &'outer OuterContext<'m, 's, S>,
    start: usize,
) -> Result<(), S> {
    // Note: FuncType already validated func_ty has at most one result type
    // This assumes a function can have only one return value
    let ret_ty = func_ty.results.get(0).copied();
    let mut ctx = FuncBodyContext {
        current_op: "",
        current_offset: start,
        outer,
        op_stack: vec![],
        label_stack: vec![ret_ty],
        current_frame: CtrlFrame {
            height: 0,
            offset: start,
            unreachable: false,
        },
        params: &func_ty.params,
        locals,
        ret_ty,
    };

    body.validate(&mut ctx)?;

    // No value must not remain in current frame after popping return values
    ctx.current_op = "function return";
    ctx.current_offset = start;
    ctx.pop_control_frame(Default::default(), ret_ty)
}

trait ValidateInsnSeq<'outer, 'm, 's, S: Source> {
    fn validate(&self, ctx: &mut FuncBodyContext<'outer, 'm, 's, S>) -> Result<(), S>;
}

impl<'s, 'm, 'outer, S: Source, V: ValidateInsnSeq<'outer, 'm, 's, S>>
    ValidateInsnSeq<'outer, 'm, 's, S> for [V]
{
    fn validate(&self, ctx: &mut FuncBodyContext<'outer, 'm, 's, S>) -> Result<(), S> {
        self.iter()
            .map(|insn| insn.validate(ctx))
            .collect::<Result<_, _>>()?;
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#instruction-sequences
impl<'outer, 'm, 's, S: Source> ValidateInsnSeq<'outer, 'm, 's, S> for Instruction {
    fn validate(&self, ctx: &mut FuncBodyContext<'outer, 'm, 's, S>) -> Result<(), S> {
        ctx.current_op = self.kind.name();
        ctx.current_offset = self.start;
        let start = self.start;
        use InsnKind::*;
        match &self.kind {
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-block
            Block { ty, body } => {
                let saved = ctx.push_control_frame(start);
                ctx.label_stack.push(*ty);
                body.validate(ctx)?;
                ctx.pop_label_stack();
                ctx.pop_control_frame(saved, *ty)?;
                if let Some(ty) = *ty {
                    ctx.op_stack.push(Type::Known(ty));
                }
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-loop
            Loop { ty, body } => {
                let saved = ctx.push_control_frame(start);
                ctx.label_stack.push(None);
                body.validate(ctx)?;
                ctx.pop_label_stack();
                ctx.pop_control_frame(saved, *ty)?;
                if let Some(ty) = *ty {
                    ctx.op_stack.push(Type::Known(ty));
                }
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-if
            If {
                ty,
                then_body,
                else_body,
            } => {
                // Condition
                ctx.pop_op_stack(Type::I32)?;
                ctx.label_stack.push(*ty);

                let saved = ctx.push_control_frame(start);
                then_body.validate(ctx)?;
                ctx.pop_control_frame(saved, *ty)?;

                let saved = ctx.push_control_frame(start);
                else_body.validate(ctx)?;
                ctx.pop_control_frame(saved, *ty)?;

                ctx.pop_label_stack();
                if let Some(ty) = *ty {
                    ctx.op_stack.push(Type::Known(ty));
                }
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-unreachable
            Unreachable => ctx.mark_unreachable(None)?,
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-nop
            Nop => {}
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-br
            Br(labelidx) => {
                let ty = ctx.validate_label_idx(*labelidx)?;
                ctx.mark_unreachable(ty)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-br-if
            BrIf(labelidx) => {
                // Condition
                ctx.pop_op_stack(Type::I32)?;
                if let Some(ty) = ctx.validate_label_idx(*labelidx)? {
                    ctx.ensure_op_stack_top(Type::Known(ty))?;
                }
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-br-table
            BrTable {
                labels,
                default_label,
            } => {
                ctx.pop_op_stack(Type::I32)?;
                let expected = ctx.validate_label_idx(*default_label)?;
                for (i, idx) in labels.iter().enumerate() {
                    let actual = ctx.validate_label_idx(*idx)?;
                    if expected != actual {
                        return ctx
                            .error(ErrorKind::TypeMismatch { expected, actual })
                            .map_err(|e| {
                                e.update_msg(format!(
                                    "{} label {} at {}",
                                    Ordinal(i),
                                    idx,
                                    ctx.current_op
                                ))
                            });
                    }
                }
                ctx.mark_unreachable(expected)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-return
            Return => {
                ctx.mark_unreachable(ctx.ret_ty)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-call
            Call(funcidx) => {
                let func = ctx.outer.func_from_idx(*funcidx, ctx.current_op, start)?;
                // func.idx was already validated
                let fty = &ctx.outer.module.types[func.idx as usize];
                // Pop extracts parameters in reverse order
                for (i, ty) in fty.params.iter().enumerate().rev() {
                    ctx.pop_op_stack(Type::Known(*ty))
                        .map_err(|e| e.update_msg(format!("{} parameter at call", Ordinal(i))))?;
                }
                for ty in fty.results.iter() {
                    ctx.op_stack.push(Type::Known(*ty));
                }
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-call-indirect
            CallIndirect(typeidx) => {
                ctx.outer.table_from_idx(0, ctx.current_op, start)?;
                // Check table index
                ctx.pop_op_stack(Type::I32)?;
                let fty = ctx.outer.type_from_idx(*typeidx, ctx.current_op, start)?;
                // Pop extracts parameters in reverse order
                for (i, ty) in fty.params.iter().enumerate().rev() {
                    ctx.pop_op_stack(Type::Known(*ty)).map_err(|e| {
                        e.update_msg(format!("{} parameter at call.indirect", Ordinal(i)))
                    })?;
                }
                for ty in fty.results.iter() {
                    ctx.op_stack.push(Type::Known(*ty));
                }
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-drop
            Drop => {
                ctx.pop_op_stack(Type::Unknown)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-select
            Select => {
                ctx.pop_op_stack(Type::I32)?;
                let ty = ctx.pop_op_stack(Type::Unknown)?;
                // 'select' instruction is value-polymorphic. The value pushed here is
                // one of the first or second value. The value is checked dynamically
                ctx.ensure_op_stack_top(ty)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-local-get
            LocalGet(localidx) => {
                let ty = ctx.validate_local_idx(*localidx)?;
                ctx.op_stack.push(Type::Known(ty));
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-local-set
            LocalSet(localidx) => {
                let ty = Type::Known(ctx.validate_local_idx(*localidx)?);
                ctx.pop_op_stack(ty)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-local-tee
            LocalTee(localidx) => {
                let ty = Type::Known(ctx.validate_local_idx(*localidx)?);
                // pop and push the same value
                ctx.ensure_op_stack_top(ty)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-global-get
            GlobalGet(globalidx) => {
                let global = ctx
                    .outer
                    .global_from_idx(*globalidx, ctx.current_op, start)?;
                ctx.op_stack.push(Type::Known(global.ty));
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-global-set
            GlobalSet(globalidx) => {
                let global = ctx
                    .outer
                    .global_from_idx(*globalidx, ctx.current_op, start)?;
                let ty = Type::Known(global.ty);
                if !global.mutable {
                    return ctx.error(ErrorKind::SetImmutableGlobal {
                        ty: global.ty,
                        idx: *globalidx,
                    });
                }
                ctx.pop_op_stack(ty)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-load
            I32Load(mem) => ctx.validate_load(mem, 32, ValType::I32)?,
            I64Load(mem) => ctx.validate_load(mem, 64, ValType::I64)?,
            F32Load(mem) => ctx.validate_load(mem, 32, ValType::F32)?,
            F64Load(mem) => ctx.validate_load(mem, 64, ValType::F64)?,
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-loadn
            I32Load8S(mem) => ctx.validate_load(mem, 8, ValType::I32)?,
            I32Load8U(mem) => ctx.validate_load(mem, 8, ValType::I32)?,
            I32Load16S(mem) => ctx.validate_load(mem, 16, ValType::I32)?,
            I32Load16U(mem) => ctx.validate_load(mem, 16, ValType::I32)?,
            I64Load8S(mem) => ctx.validate_load(mem, 8, ValType::I64)?,
            I64Load8U(mem) => ctx.validate_load(mem, 8, ValType::I64)?,
            I64Load16S(mem) => ctx.validate_load(mem, 16, ValType::I64)?,
            I64Load16U(mem) => ctx.validate_load(mem, 16, ValType::I64)?,
            I64Load32S(mem) => ctx.validate_load(mem, 32, ValType::I64)?,
            I64Load32U(mem) => ctx.validate_load(mem, 32, ValType::I64)?,
            // https://webassembly.github.io/spec/core/valid/instructions.html#id16
            I32Store(mem) => ctx.validate_store(mem, 32, ValType::I32)?,
            I64Store(mem) => ctx.validate_store(mem, 64, ValType::I64)?,
            F32Store(mem) => ctx.validate_store(mem, 32, ValType::F32)?,
            F64Store(mem) => ctx.validate_store(mem, 64, ValType::F64)?,
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-storen
            I32Store8(mem) => ctx.validate_store(mem, 8, ValType::I32)?,
            I32Store16(mem) => ctx.validate_store(mem, 16, ValType::I32)?,
            I64Store8(mem) => ctx.validate_store(mem, 8, ValType::I64)?,
            I64Store16(mem) => ctx.validate_store(mem, 16, ValType::I64)?,
            I64Store32(mem) => ctx.validate_store(mem, 32, ValType::I64)?,
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-memory-size
            MemorySize => {
                if ctx.outer.module.memories.is_empty() {
                    return ctx.error(ErrorKind::MemoryIsNotDefined);
                }
                ctx.op_stack.push(Type::I32);
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-memory-grow
            MemoryGrow => {
                if ctx.outer.module.memories.is_empty() {
                    return ctx.error(ErrorKind::MemoryIsNotDefined);
                }
                // pop i32 and push i32
                ctx.ensure_op_stack_top(Type::I32)?;
            }
            I32Const(_) => {
                ctx.op_stack.push(Type::I32);
            }
            I64Const(_) => {
                ctx.op_stack.push(Type::I64);
            }
            F32Const(_) => {
                ctx.op_stack.push(Type::F32);
            }
            F64Const(_) => {
                ctx.op_stack.push(Type::F64);
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-unop
            // [t] -> [t]
            I32Clz | I32Ctz | I32Popcnt => {
                ctx.ensure_op_stack_top(Type::I32)?;
            }
            I64Clz | I64Ctz | I64Popcnt => {
                ctx.ensure_op_stack_top(Type::I64)?;
            }
            F32Abs | F32Neg | F32Ceil | F32Floor | F32Trunc | F32Nearest | F32Sqrt => {
                ctx.ensure_op_stack_top(Type::F32)?;
            }
            F64Abs | F64Neg | F64Ceil | F64Floor | F64Trunc | F64Nearest | F64Sqrt => {
                ctx.ensure_op_stack_top(Type::F64)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-binop
            // [t t] -> [t]
            I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
            | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotl | I32Rotr => {
                ctx.pop_op_stack(Type::I32)?;
                ctx.ensure_op_stack_top(Type::I32)?;
            }
            I64Add | I64Sub | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or
            | I64Xor | I64Shl | I64ShrS | I64ShrU | I64Rotl | I64Rotr => {
                ctx.pop_op_stack(Type::I64)?;
                ctx.ensure_op_stack_top(Type::I64)?;
            }
            F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign => {
                ctx.pop_op_stack(Type::F32)?;
                ctx.ensure_op_stack_top(Type::F32)?;
            }
            F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64Copysign => {
                ctx.pop_op_stack(Type::F64)?;
                ctx.ensure_op_stack_top(Type::F64)?;
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-testop
            // [t] -> [i32]
            I32Eqz => {
                ctx.ensure_op_stack_top(Type::I32)?;
            }
            I64Eqz => {
                ctx.pop_op_stack(Type::I64)?;
                ctx.op_stack.push(Type::I32);
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-relop
            // [t t] -> [i32]
            I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS
            | I32GeU => {
                ctx.pop_op_stack(Type::I32)?;
                ctx.ensure_op_stack_top(Type::I32)?;
            }
            I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS
            | I64GeU => {
                ctx.pop_op_stack(Type::I64)?;
                ctx.pop_op_stack(Type::I64)?;
                ctx.op_stack.push(Type::I32);
            }
            F32Eq | F32Ne | F32Lt | F32Gt | F32Le | F32Ge => {
                ctx.pop_op_stack(Type::F32)?;
                ctx.pop_op_stack(Type::F32)?;
                ctx.op_stack.push(Type::I32);
            }
            F64Eq | F64Ne | F64Lt | F64Gt | F64Le | F64Ge => {
                ctx.pop_op_stack(Type::F64)?;
                ctx.pop_op_stack(Type::F64)?;
                ctx.op_stack.push(Type::I32);
            }
            // https://webassembly.github.io/spec/core/valid/instructions.html#valid-cvtop
            // [t1] -> [t2]
            I32WrapI64 => ctx.validate_convert(ValType::I64, ValType::I32)?,
            I32TruncF32S => ctx.validate_convert(ValType::F32, ValType::I32)?,
            I32TruncF32U => ctx.validate_convert(ValType::F32, ValType::I32)?,
            I32TruncF64S => ctx.validate_convert(ValType::F64, ValType::I32)?,
            I32TruncF64U => ctx.validate_convert(ValType::F64, ValType::I32)?,
            I64ExtendI32S => ctx.validate_convert(ValType::I32, ValType::I64)?,
            I64ExtendI32U => ctx.validate_convert(ValType::I32, ValType::I64)?,
            I64TruncF32S => ctx.validate_convert(ValType::F32, ValType::I64)?,
            I64TruncF32U => ctx.validate_convert(ValType::F32, ValType::I64)?,
            I64TruncF64S => ctx.validate_convert(ValType::F64, ValType::I64)?,
            I64TruncF64U => ctx.validate_convert(ValType::F64, ValType::I64)?,
            F32ConvertI32S => ctx.validate_convert(ValType::I32, ValType::F32)?,
            F32ConvertI32U => ctx.validate_convert(ValType::I32, ValType::F32)?,
            F32ConvertI64S => ctx.validate_convert(ValType::I64, ValType::F32)?,
            F32ConvertI64U => ctx.validate_convert(ValType::I64, ValType::F32)?,
            F32DemoteF64 => ctx.validate_convert(ValType::F64, ValType::F32)?,
            F64ConvertI32S => ctx.validate_convert(ValType::I32, ValType::F64)?,
            F64ConvertI32U => ctx.validate_convert(ValType::I32, ValType::F64)?,
            F64ConvertI64S => ctx.validate_convert(ValType::I64, ValType::F64)?,
            F64ConvertI64U => ctx.validate_convert(ValType::I64, ValType::F64)?,
            F64PromoteF32 => ctx.validate_convert(ValType::F32, ValType::F64)?,
            I32ReinterpretF32 => ctx.validate_convert(ValType::F32, ValType::I32)?,
            I64ReinterpretF64 => ctx.validate_convert(ValType::F64, ValType::I64)?,
            F32ReinterpretI32 => ctx.validate_convert(ValType::I32, ValType::F32)?,
            F64ReinterpretI64 => ctx.validate_convert(ValType::I64, ValType::F64)?,
        }
        Ok(())
    }
}

// https://webassembly.github.io/spec/core/valid/instructions.html#constant-expressions
pub(crate) fn validate_constant<'m, 's, S: Source>(
    insns: &[Instruction],
    ctx: &OuterContext<'m, 's, S>,
    expr_ty: ValType,
    when: &'static str,
    start: usize,
) -> Result<(), S> {
    match insns.len() {
        0 => return ctx.error(ErrorKind::NoInstructionForConstant, when, start),
        1 => {}
        len => return ctx.error(ErrorKind::TooManyInstructionForConstant(len), when, start),
    }

    use InsnKind::*;
    let insn = &insns[0];
    let name = insn.kind.name();
    let ty = match &insn.kind {
        GlobalGet(globalidx) => {
            if *globalidx < ctx.module.import_globals_len {
                let global = ctx.module.globals.get(*globalidx as usize).unwrap();
                if global.mutable {
                    return ctx.error(ErrorKind::MutableForConstant(*globalidx), when, start);
                } else {
                    global.ty
                }
            } else {
                return ctx
                    .error(
                        ErrorKind::IndexOutOfBounds {
                            idx: *globalidx,
                            upper: ctx.module.import_globals_len as usize,
                            what: "global variable read",
                        },
                        "",
                        insn.start,
                    )
                    .map_err(|e| {
                        e.update_msg(format!("constant expression in {} at {}", name, when))
                    });
            }
        }
        I32Const(_) => ValType::I32,
        I64Const(_) => ValType::I64,
        F32Const(_) => ValType::F32,
        F64Const(_) => ValType::F64,
        _ => {
            return ctx
                .error(ErrorKind::NotConstantInstruction(name), "", insn.start)
                .map_err(|e| e.update_msg(format!("constant expression at {}", when)));
        }
    };
    if ty != expr_ty {
        ctx.error(
            ErrorKind::TypeMismatch {
                expected: Some(expr_ty),
                actual: Some(ty),
            },
            "",
            start,
        )
        .map_err(|e| e.update_msg(format!("type of constant expression at {}", when)))
    } else {
        Ok(())
    }
}
