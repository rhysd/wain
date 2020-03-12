use std::borrow::Cow;

// Root of the tree
pub struct Root<'a> {
    pub module: Module<'a>,
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
pub struct Module<'a> {
    pub start: usize,
    pub types: Vec<FuncType>,
    pub exports: Vec<Export<'a>>,
    pub funcs: Vec<Func<'a>>,
    pub elems: Vec<ElemSegment>,
    pub tables: Vec<Table<'a>>,
    pub data: Vec<DataSegment<'a>>,
    pub memories: Vec<Memory<'a>>,
    pub globals: Vec<Global<'a>>,
    pub entrypoint: Option<StartFunction>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#syntax-module
pub struct Import<'a> {
    pub mod_name: Name<'a>,
    pub name: Name<'a>,
}

// https://webassembly.github.io/spec/core/syntax/types.html#function-types
pub struct FuncType {
    pub start: usize,
    pub params: Vec<ValType>,
    pub results: Vec<ValType>,
}

// https://webassembly.github.io/spec/core/syntax/types.html#value-types
#[derive(PartialEq, Clone, Copy)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

// https://webassembly.github.io/spec/core/syntax/values.html#syntax-name
//
// Use Cow<'a, str> since it is String on text format and it is &str on binary format.
// In text format, special characters in string literal are escaped. Unescaped string must
// be allocated in heap. In binary format, it is directly encoded as bytes so borrowing the
// part of source is enough.
pub struct Name<'a>(pub Cow<'a, str>);

// https://webassembly.github.io/spec/core/syntax/types.html#table-types
// Note: elemtype is currently fixed to 'funcref'
pub struct TableType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/syntax/types.html#limits
pub enum Limits {
    Range(u32, u32),
    From(u32),
}

// https://webassembly.github.io/spec/core/syntax/types.html#memory-types
pub struct MemType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/syntax/types.html#global-types
pub struct GlobalType {
    pub mutable: bool,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#exports
pub enum ExportKind {
    Func(FuncIdx),
    Table(TableIdx),
    Memory(MemIdx),
    Global(GlobalIdx),
}
pub struct Export<'a> {
    pub start: usize,
    pub name: Name<'a>,
    pub kind: ExportKind,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#syntax-func
pub enum FuncKind<'a> {
    Import(Import<'a>),
    Body {
        locals: Vec<ValType>,
        expr: Vec<Instruction>,
    },
}
pub struct Func<'a> {
    pub start: usize,
    pub idx: TypeIdx,
    pub kind: FuncKind<'a>,
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#instructions
pub struct Instruction {
    pub start: usize,
    pub kind: InsnKind,
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#syntax-memarg
pub struct Mem {
    pub align: Option<u32>,
    pub offset: Option<u32>,
}

// https://webassembly.github.io/spec/core/syntax/instructions.html#instructions
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
}

// https://webassembly.github.io/spec/core/syntax/modules.html#element-segments
pub struct ElemSegment {
    pub start: usize,
    pub idx: TableIdx,
    pub offset: Vec<Instruction>, // expr
    pub init: Vec<FuncIdx>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#tables
pub struct Table<'a> {
    pub start: usize,
    pub ty: TableType,
    pub import: Option<Import<'a>>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#data-segments
pub struct DataSegment<'a> {
    pub start: usize,
    pub idx: MemIdx,
    pub offset: Vec<Instruction>, // expr
    pub data: Cow<'a, [u8]>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#memories
pub struct Memory<'a> {
    pub start: usize,
    pub ty: MemType,
    pub import: Option<Import<'a>>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#globals
pub enum GlobalKind<'a> {
    Import(Import<'a>),
    Init(Vec<Instruction>), // expr
}
pub struct Global<'a> {
    pub start: usize,
    pub ty: GlobalType,
    pub kind: GlobalKind<'a>,
}

// https://webassembly.github.io/spec/core/syntax/modules.html#start-function
pub struct StartFunction {
    pub start: usize,
    pub idx: FuncIdx,
}
