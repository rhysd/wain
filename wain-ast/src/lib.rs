use std::borrow::Cow;
use std::fmt;

// Root of the tree
#[derive(Debug)]
pub struct SyntaxTree<'a> {
    pub module: Module<'a>,
}

// Note: Since crate for syntax tree data structure is separated, all fields of AST node structs need
// to be public. Or we need a factory function like Module::new() for each struct.

// https://webassembly.github.io/spec/core/text/modules.html#text-module
#[derive(Debug)]
pub struct Module<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub types: Vec<TypeDef<'a>>,
    pub exports: Vec<Export<'a>>,
    pub funcs: Vec<Func<'a>>,
    pub elems: Vec<Elem<'a>>,
    pub tables: Vec<Table<'a>>,
    pub data: Vec<Data<'a>>,
    pub memories: Vec<Memory<'a>>,
    pub globals: Vec<Global<'a>>,
    pub entrypoint: Option<Start<'a>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-typedef
#[derive(Debug)]
pub struct TypeDef<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: FuncType<'a>,
}

// https://webassembly.github.io/spec/core/text/types.html#text-functype
#[derive(Debug)]
pub struct FuncType<'a> {
    pub start: usize,
    pub params: Vec<Param<'a>>,
    pub results: Vec<FuncResult>,
}

// https://webassembly.github.io/spec/core/text/types.html#text-param
#[derive(Debug, Clone)]
pub struct Param<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/types.html#text-result
#[derive(Debug, Clone)]
pub struct FuncResult {
    pub start: usize,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/types.html#text-valtype
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-import
#[derive(Debug)]
pub struct Import<'a> {
    pub mod_name: Name<'a>,
    pub name: Name<'a>,
}

// https://webassembly.github.io/spec/core/text/values.html#text-name
//
// Use Cow<'a, str> since it is String on text format and it is &str on binary format.
// In text format, special characters in string literal are escaped. Unescaped string must
// be allocated in heap. In binary format, it is directly encoded as bytes so borrowing the
// part of source is enough.
#[derive(Debug)]
pub struct Name<'a>(pub Cow<'a, str>);

// https://webassembly.github.io/spec/core/text/modules.html#type-uses
#[derive(Debug)]
pub struct TypeUse<'a> {
    pub start: usize,
    pub idx: u32,
    pub params: Vec<Param<'a>>,
    pub results: Vec<FuncResult>,
}

// https://webassembly.github.io/spec/core/text/modules.html#indices
#[derive(Debug)]
pub enum Index<'a> {
    Num(u32),
    Ident(&'a str),
}
impl<'a> fmt::Display for Index<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Index::Num(i) => write!(f, "{}", i),
            Index::Ident(i) => write!(f, "{}", i),
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-tabletype
// Note: elemtype is currently fixed to 'funcref'
#[derive(Debug)]
pub struct TableType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/text/types.html#text-limits
#[derive(Debug)]
pub enum Limits {
    Range { min: u32, max: u32 },
    From { min: u32 },
}

// https://webassembly.github.io/spec/core/text/types.html#text-memtype
#[derive(Debug)]
pub struct MemType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/text/types.html#text-globaltype
#[derive(Debug)]
pub struct GlobalType {
    pub mutable: bool,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-export
#[derive(Debug)]
pub struct Export<'a> {
    pub start: usize,
    pub name: Name<'a>,
    pub kind: ExportKind,
    pub idx: Index<'a>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-exportdesc
#[derive(Debug)]
pub enum ExportKind {
    Func,
    Table,
    Memory,
    Global,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-func
#[derive(Debug)]
pub enum FuncKind<'a> {
    Import(Import<'a>),
    Body {
        locals: Vec<Local<'a>>,
        body: Vec<Instruction<'a>>,
    },
}
#[derive(Debug)]
pub struct Func<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: TypeUse<'a>,
    pub kind: FuncKind<'a>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-local
#[derive(Debug)]
pub struct Local<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/instructions.html#instructions
#[derive(Debug)]
pub struct Instruction<'a> {
    pub start: usize,
    pub kind: InsnKind<'a>,
}

// https://webassembly.github.io/spec/core/text/instructions.html#text-memarg
#[derive(Debug)]
pub struct Mem {
    pub align: Option<u32>,
    pub offset: Option<u32>,
}

#[derive(Debug)]
pub enum InsnKind<'a> {
    // Control instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
    Block {
        label: Option<&'a str>,
        ty: Option<ValType>,
        body: Vec<Instruction<'a>>,
        id: Option<&'a str>,
    },
    Loop {
        label: Option<&'a str>,
        ty: Option<ValType>,
        body: Vec<Instruction<'a>>,
        id: Option<&'a str>,
    },
    If {
        label: Option<&'a str>,
        ty: Option<ValType>,
        then_body: Vec<Instruction<'a>>,
        else_id: Option<&'a str>,
        else_body: Vec<Instruction<'a>>,
        end_id: Option<&'a str>,
    },
    Unreachable,
    Nop,
    Br(Index<'a>),
    BrIf(Index<'a>),
    BrTable {
        labels: Vec<Index<'a>>,
        default_label: Index<'a>,
    },
    Return,
    Call(Index<'a>),
    CallIndirect(TypeUse<'a>),
    // Parametric instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
    Drop,
    Select,
    // Variable instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
    LocalGet(Index<'a>),
    LocalSet(Index<'a>),
    LocalTee(Index<'a>),
    GlobalGet(Index<'a>),
    GlobalSet(Index<'a>),
    // Memory instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
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
    // https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
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

impl<'a> InsnKind<'a> {
    pub fn is_block(&self) -> bool {
        use InsnKind::*;
        match self {
            Block { .. } | Loop { .. } | If { .. } => true,
            _ => false,
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#element-segments
#[derive(Debug)]
pub struct Elem<'a> {
    pub start: usize,
    pub idx: Index<'a>,
    pub offset: Vec<Instruction<'a>>,
    pub init: Vec<Index<'a>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#tables
#[derive(Debug)]
pub struct Table<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: TableType,
    pub import: Option<Import<'a>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-data
#[derive(Debug)]
pub struct Data<'a> {
    pub start: usize,
    pub idx: Index<'a>,
    pub offset: Vec<Instruction<'a>>,
    pub data: Cow<'a, [u8]>,
}

// https://webassembly.github.io/spec/core/text/modules.html#memories
#[derive(Debug)]
pub struct Memory<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: MemType,
    pub import: Option<Import<'a>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#globals
#[derive(Debug)]
pub enum GlobalKind<'a> {
    Import(Import<'a>),
    Init(Vec<Instruction<'a>>),
}
#[derive(Debug)]
pub struct Global<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: GlobalType,
    pub kind: GlobalKind<'a>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-start
#[derive(Debug)]
pub struct Start<'a> {
    pub start: usize,
    pub idx: Index<'a>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insn_is_block() {
        let insn = InsnKind::Block {
            label: None,
            ty: None,
            body: vec![],
            id: None,
        };
        assert!(insn.is_block());
        assert!(!InsnKind::Nop.is_block());
    }
}
