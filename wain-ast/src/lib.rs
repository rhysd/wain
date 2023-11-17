#![forbid(unsafe_code)]
#![warn(clippy::dbg_macro)]

pub mod source;

use std::borrow::Cow;
use std::fmt;

// Root of the tree
#[derive(Clone, Debug, PartialEq)]
pub struct Root<'s, S: source::Source> {
    pub module: Module<'s>,
    pub source: S,
}

// Note: Since crate for syntax tree data structure is separated, all fields of AST node structs need
// to be public. Or we need a factory function like Module::new() for each struct.

// https://webassembly.github.io/spec/core/syntax/modules.html#indices
pub type FuncIdx = u32;
pub type TableIdx = u32;
pub type MemIdx = u32;
pub type GlobalIdx = u32;
pub type TypeIdx = u32;
pub type LocalIdx = u32;
pub type LabelIdx = u32;

// https://webassembly.github.io/spec/core/syntax/modules.html
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Module<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub types: Vec<FuncType>,
    pub exports: Vec<Export<'s>>,
    pub funcs: Vec<Func<'s>>,
    pub elems: Vec<ElemSegment>,
    pub tables: Vec<Table<'s>>,
    pub data: Vec<DataSegment<'s>>,
    pub memories: Vec<Memory<'s>>,
    pub globals: Vec<Global<'s>>,
    pub entrypoint: Option<StartFunction>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#syntax-module
#[derive(Debug, Clone, PartialEq)]
pub struct Import<'s> {
    pub mod_name: Name<'s>,
    pub name: Name<'s>,
}

// https://webassembly.github.io/spec/core/syntax/types.html#function-types
#[derive(Debug, PartialEq, Clone)]
pub struct FuncType {
    pub start: usize,
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

// https://webassembly.github.io/spec/core/syntax/types.html#value-types
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

impl ValType {
    #[must_use]
    pub fn bytes(self) -> usize {
        match self {
            Self::I32 | Self::F32 => 4,
            Self::I64 | Self::F64 => 8,
        }
    }
}

impl AsRef<str> for ValType {
    fn as_ref(&self) -> &str {
        match self {
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
        }
    }
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

pub trait AsValType {
    const VAL_TYPE: ValType;
}

impl AsValType for i32 {
    const VAL_TYPE: ValType = ValType::I32;
}

impl AsValType for i64 {
    const VAL_TYPE: ValType = ValType::I64;
}

impl AsValType for f32 {
    const VAL_TYPE: ValType = ValType::F32;
}

impl AsValType for f64 {
    const VAL_TYPE: ValType = ValType::F64;
}

// https://webassembly.github.io/spec/core/syntax/values.html#syntax-name
//
// Use Cow<'s, str> since it is String on text format and it is &str on binary format.
// In text format, special characters in string literal are escaped. Unescaped string must
// be allocated in heap. In binary format, it is directly encoded as bytes so borrowing the
// part of source is enough.
#[derive(Debug, Clone, PartialEq)]
pub struct Name<'s>(pub Cow<'s, str>);

// https://webassembly.github.io/spec/core/syntax/types.html#table-types
// Note: elemtype is currently fixed to 'funcref'
#[derive(Debug, Clone, PartialEq)]
pub struct TableType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/syntax/types.html#limits
#[derive(Debug, Clone, PartialEq)]
pub enum Limits {
    Range(u32, u32),
    From(u32),
}

// https://webassembly.github.io/spec/core/syntax/types.html#memory-types
#[derive(Debug, Clone, PartialEq)]
pub struct MemType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#exports
#[derive(Debug, Clone, PartialEq)]
pub enum ExportKind {
    Func(FuncIdx),
    Table(TableIdx),
    Memory(MemIdx),
    Global(GlobalIdx),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export<'s> {
    pub start: usize,
    pub name: Name<'s>,
    pub kind: ExportKind,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#syntax-func
#[derive(Debug, Clone, PartialEq)]
pub enum FuncKind<'s> {
    Import(Import<'s>),
    Body {
        locals: Vec<ValType>,
        expr: Vec<Instruction>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func<'s> {
    pub start: usize,
    pub idx: TypeIdx,
    pub kind: FuncKind<'s>,
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#instructions
#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub start: usize,
    pub kind: InsnKind,
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-memarg
#[derive(Debug, Clone, PartialEq)]
pub struct Mem {
    pub align: u32,
    pub offset: u32,
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#instructions
#[derive(Debug, Clone, PartialEq)]
pub enum InsnKind {
    // Control instructions
    // https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions
    Block {
        ty: Option<ValType>, // resulttype
        body: Vec<Instruction>,
    },
    Loop {
        ty: Option<ValType>, // resulttype
        body: Vec<Instruction>,
    },
    If {
        ty: Option<ValType>, // resulttype
        then_body: Vec<Instruction>,
        else_body: Vec<Instruction>,
    },
    Unreachable,
    Nop,
    Br(LabelIdx),
    BrIf(LabelIdx),
    BrTable {
        labels: Vec<LabelIdx>,
        default_label: LabelIdx,
    },
    Return,
    Call(FuncIdx),
    CallIndirect(TypeIdx),
    // Parametric instructions
    // https://webassembly.github.io/spec/core/syntax/instructions.html#parametric-instructions
    Drop,
    Select,
    // Variable instructions
    // https://webassembly.github.io/spec/core/syntax/instructions.html#variable-instructions
    LocalGet(LocalIdx),
    LocalSet(LocalIdx),
    LocalTee(LocalIdx),
    GlobalGet(GlobalIdx),
    GlobalSet(GlobalIdx),
    // Memory instructions
    // https://webassembly.github.io/spec/core/syntax/instructions.html#memory-instructions
    I32Load(Mem),
    I64Load(Mem),
    F32Load(Mem),
    F64Load(Mem),
    I32Load8S(Mem),
    I32Load8U(Mem),
    I32Load16S(Mem),
    I32Load16U(Mem),
    I64Load8S(Mem),
    I64Load8U(Mem),
    I64Load16S(Mem),
    I64Load16U(Mem),
    I64Load32S(Mem),
    I64Load32U(Mem),
    I32Store(Mem),
    I64Store(Mem),
    F32Store(Mem),
    F64Store(Mem),
    I32Store8(Mem),
    I32Store16(Mem),
    I64Store8(Mem),
    I64Store16(Mem),
    I64Store32(Mem),
    MemorySize,
    MemoryGrow,
    // Numeric instructions
    // https://webassembly.github.io/spec/core/syntax/instructions.html#numeric-instructions
    // Constants
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    // i32 operations
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,
    // i64 operations
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,
    // f32 operations
    F32Abs,
    F32Neg,
    F32Ceil,
    F32Floor,
    F32Trunc,
    F32Nearest,
    F32Sqrt,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Min,
    F32Max,
    F32Copysign,
    // f64 operations
    F64Abs,
    F64Neg,
    F64Ceil,
    F64Floor,
    F64Trunc,
    F64Nearest,
    F64Sqrt,
    F64Add,
    F64Sub,
    F64Mul,
    F64Div,
    F64Min,
    F64Max,
    F64Copysign,
    // i32 comparison
    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    // i64 comparison
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
    // f32 comparison
    F32Eq,
    F32Ne,
    F32Lt,
    F32Gt,
    F32Le,
    F32Ge,
    // f64 comparison
    F64Eq,
    F64Ne,
    F64Lt,
    F64Gt,
    F64Le,
    F64Ge,
    // Conversion
    I32WrapI64,
    I32TruncF32S,
    I32TruncF32U,
    I32TruncF64S,
    I32TruncF64U,
    I64ExtendI32S,
    I64ExtendI32U,
    I64TruncF32S,
    I64TruncF32U,
    I64TruncF64S,
    I64TruncF64U,
    F32ConvertI32S,
    F32ConvertI32U,
    F32ConvertI64S,
    F32ConvertI64U,
    F32DemoteF64,
    F64ConvertI32S,
    F64ConvertI32U,
    F64ConvertI64S,
    F64ConvertI64U,
    F64PromoteF32,
    I32ReinterpretF32,
    I64ReinterpretF64,
    F32ReinterpretI32,
    F64ReinterpretI64,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,
}

impl InsnKind {
    #[must_use]
    pub fn name(&self) -> &'static str {
        use InsnKind::*;
        match self {
            Block { .. } => "block",
            Loop { .. } => "loop",
            If { .. } => "if",
            Unreachable => "unreachable",
            Nop => "nop",
            Br(_) => "br",
            BrIf(_) => "br_if",
            BrTable { .. } => "br_table",
            Return => "return",
            Call(_) => "call",
            CallIndirect(_) => "call_indirect",
            Drop => "drop",
            Select => "select",
            LocalGet(_) => "local.get",
            LocalSet(_) => "local.set",
            LocalTee(_) => "local.tee",
            GlobalGet(_) => "global.get",
            GlobalSet(_) => "global.set",
            I32Load(_) => "i32.load",
            I64Load(_) => "i64.load",
            F32Load(_) => "f32.load",
            F64Load(_) => "f64.load",
            I32Load8S(_) => "i32.load8_s",
            I32Load8U(_) => "i32.load8_u",
            I32Load16S(_) => "i32.load16_s",
            I32Load16U(_) => "i32.load16_u",
            I64Load8S(_) => "i64.load8_s",
            I64Load8U(_) => "i64.load8_u",
            I64Load16S(_) => "i64.load16_s",
            I64Load16U(_) => "i64.load16_u",
            I64Load32S(_) => "i64.load32_s",
            I64Load32U(_) => "i64.load32_u",
            I32Store(_) => "i32.store",
            I64Store(_) => "i64.store",
            F32Store(_) => "f32.store",
            F64Store(_) => "f64.store",
            I32Store8(_) => "i32.store8",
            I32Store16(_) => "i32.store16",
            I64Store8(_) => "i64.store8",
            I64Store16(_) => "i64.store16",
            I64Store32(_) => "i64.store32",
            MemorySize => "memory.size",
            MemoryGrow => "memory.grow",
            I32Const(_) => "i32.const",
            I64Const(_) => "i64.const",
            F32Const(_) => "f32.const",
            F64Const(_) => "f64.const",
            I32Clz => "i32.clz",
            I32Ctz => "i32.ctz",
            I32Popcnt => "i32.popcnt",
            I32Add => "i32.add",
            I32Sub => "i32.sub",
            I32Mul => "i32.mul",
            I32DivS => "i32.div_s",
            I32DivU => "i32.div_u",
            I32RemS => "i32.rem_s",
            I32RemU => "i32.rem_u",
            I32And => "i32.and",
            I32Or => "i32.or",
            I32Xor => "i32.xor",
            I32Shl => "i32.shl",
            I32ShrS => "i32.shr_s",
            I32ShrU => "i32.shr_u",
            I32Rotl => "i32.rotl",
            I32Rotr => "i32.rotr",
            I64Clz => "i64.clz",
            I64Ctz => "i64.ctz",
            I64Popcnt => "i64.popcnd",
            I64Add => "i64.add",
            I64Sub => "i64.sub",
            I64Mul => "i64.mul",
            I64DivS => "i64.div_s",
            I64DivU => "i64.div_u",
            I64RemS => "i64.rem_s",
            I64RemU => "i64.rem_u",
            I64And => "i64.and",
            I64Or => "i64.or",
            I64Xor => "i64.xor",
            I64Shl => "i64.shl",
            I64ShrS => "i64.shr_s",
            I64ShrU => "i64.shr_u",
            I64Rotl => "i64.rotl",
            I64Rotr => "i64.rotr",
            F32Abs => "f32.abs",
            F32Neg => "f32.neg",
            F32Ceil => "f32.ceil",
            F32Floor => "f32.floor",
            F32Trunc => "f32.trunc",
            F32Nearest => "f32.nearest",
            F32Sqrt => "f32.sqrt",
            F32Add => "f32.add",
            F32Sub => "f32.sub",
            F32Mul => "f32.mul",
            F32Div => "f32.div",
            F32Min => "f32.min",
            F32Max => "f32.max",
            F32Copysign => "f32.copysign",
            F64Abs => "f64.abs",
            F64Neg => "f64.neg",
            F64Ceil => "f64.ceil",
            F64Floor => "f64.floor",
            F64Trunc => "f64.trunc",
            F64Nearest => "f64.nearest",
            F64Sqrt => "f64.sqrt",
            F64Add => "f64.add",
            F64Sub => "f64.sub",
            F64Mul => "f64.mul",
            F64Div => "f64.div",
            F64Min => "f64.min",
            F64Max => "f64.max",
            F64Copysign => "f64.copysign",
            // i32 comparison
            I32Eqz => "i32.eqz",
            I32Eq => "i32.eq",
            I32Ne => "i32.ne",
            I32LtS => "i32.lt_s",
            I32LtU => "i32.lt_u",
            I32GtS => "i32.gt_s",
            I32GtU => "i32.gt_u",
            I32LeS => "i32.le_s",
            I32LeU => "i32.le_u",
            I32GeS => "i32.ge_s",
            I32GeU => "i32.ge_u",
            // i64 comparison
            I64Eqz => "i64.eqz",
            I64Eq => "i64.eq",
            I64Ne => "i64.ne",
            I64LtS => "i64.lt_s",
            I64LtU => "i64.lt_u",
            I64GtS => "i64.gt_s",
            I64GtU => "i64.gt_u",
            I64LeS => "i64.le_s",
            I64LeU => "i64.le_u",
            I64GeS => "i64.ge_s",
            I64GeU => "i64.ge_u",
            // f32 comparison
            F32Eq => "f32.eq",
            F32Ne => "f32.ne",
            F32Lt => "f32.lt",
            F32Gt => "f32.gt",
            F32Le => "f32.le",
            F32Ge => "f32.ge",
            // f64 comparison
            F64Eq => "f64.eq",
            F64Ne => "f64.ne",
            F64Lt => "f64.lt",
            F64Gt => "f64.gt",
            F64Le => "f64.le",
            F64Ge => "f64.ge",
            // Conversion
            I32WrapI64 => "i32.wrap_i64",
            I32TruncF32S => "i32.trunc_f32_s",
            I32TruncF32U => "i32.trunc_f32_u",
            I32TruncF64S => "i32.trunc_f64_s",
            I32TruncF64U => "i32.trunc_f64_u",
            I64ExtendI32S => "i64.extend_i32_s",
            I64ExtendI32U => "i64.extend_i32_u",
            I64TruncF32S => "i64.trunc_f32_s",
            I64TruncF32U => "i64.trunc_f32_u",
            I64TruncF64S => "i64.trunc_f64_s",
            I64TruncF64U => "i64.trunc_f64_u",
            F32ConvertI32S => "f32.convert_i32_s",
            F32ConvertI32U => "f32.convert_i32_u",
            F32ConvertI64S => "f32.convert_i64_s",
            F32ConvertI64U => "f32.convert_i64_u",
            F32DemoteF64 => "f32.demote_f64",
            F64ConvertI32S => "f64.convert_i32_s",
            F64ConvertI32U => "f64.convert_i32_u",
            F64ConvertI64S => "f64.convert_i64_s",
            F64ConvertI64U => "f64.convert_i64_u",
            F64PromoteF32 => "f64.promote_f32",
            I32ReinterpretF32 => "i32.reinterpret_f32",
            I64ReinterpretF64 => "i64.reinterpret_f64",
            F32ReinterpretI32 => "f32.reinterpret_i32",
            F64ReinterpretI64 => "f64.reinterpret_i64",
            // sign extension
            I32Extend8S => "i32.extend8_s",
            I32Extend16S => "i32.extend16_s",
            I64Extend8S => "i64.extend8_s",
            I64Extend16S => "i64.extend16_s",
            I64Extend32S => "i64.extend32_s",
        }
    }
}

// https://webassembly.github.io/spec/core/syntax/modules.html#element-segments
#[derive(Debug, Clone, PartialEq)]
pub struct ElemSegment {
    pub start: usize,
    pub idx: TableIdx,
    pub offset: Vec<Instruction>, // expr
    pub init: Vec<FuncIdx>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#tables
#[derive(Debug, Clone, PartialEq)]
pub struct Table<'s> {
    pub start: usize,
    pub ty: TableType,
    pub import: Option<Import<'s>>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#data-segments
#[derive(Debug, Clone, PartialEq)]
pub struct DataSegment<'s> {
    pub start: usize,
    pub idx: MemIdx,
    pub offset: Vec<Instruction>, // expr
    pub data: Cow<'s, [u8]>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#memories
#[derive(Debug, Clone, PartialEq)]
pub struct Memory<'s> {
    pub start: usize,
    pub ty: MemType,
    pub import: Option<Import<'s>>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#globals
// https://webassembly.github.io/spec/core/syntax/types.html#global-types
#[derive(Debug, Clone, PartialEq)]
pub enum GlobalKind<'s> {
    Import(Import<'s>),
    Init(Vec<Instruction>), // expr
}

#[derive(Debug, Clone, PartialEq)]
pub struct Global<'s> {
    pub start: usize,
    pub mutable: bool,
    pub ty: ValType,
    pub kind: GlobalKind<'s>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#start-function
#[derive(Debug, Clone, PartialEq)]
pub struct StartFunction {
    pub start: usize,
    pub idx: FuncIdx,
}
