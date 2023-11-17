use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;

// TODO: Remove duplication between text format AST and binary format AST.
// For example, wat::ValType and wasm::TableType has the same structure. So we can use
// wasm::TableType directly in parsing WAT.

type Indices<'s> = HashMap<&'s str, u32>;

// Root of the tree
#[cfg_attr(test, derive(Debug))]
pub struct Parsed<'s> {
    pub module: Module<'s>,
    pub type_indices: Indices<'s>,
    pub func_indices: Indices<'s>,
    pub table_indices: Indices<'s>,
    pub mem_indices: Indices<'s>,
    pub global_indices: Indices<'s>,
}

// Note: Since crate for syntax tree data structure is separated, all fields of AST node structs need
// to be public. Or we need a factory function like Module::new() for each struct.

// https://webassembly.github.io/spec/core/text/modules.html#text-module
#[cfg_attr(test, derive(Debug))]
pub struct Module<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub types: Vec<TypeDef<'s>>,
    pub exports: Vec<Export<'s>>,
    pub funcs: Vec<Func<'s>>,
    pub elems: Vec<Elem<'s>>,
    pub tables: Vec<Table<'s>>,
    pub data: Vec<Data<'s>>,
    pub memories: Vec<Memory<'s>>,
    pub globals: Vec<Global<'s>>,
    pub entrypoint: Option<Start<'s>>,
    pub implicit_type_uses: Vec<u32>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-typedef
#[cfg_attr(test, derive(Debug))]
pub struct TypeDef<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: FuncType<'s>,
}

// https://webassembly.github.io/spec/core/text/types.html#text-functype
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub struct FuncType<'s> {
    pub start: usize,
    pub params: Vec<Param<'s>>,
    pub results: Vec<FuncResult>,
}

// https://webassembly.github.io/spec/core/text/types.html#text-param
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub struct Param<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/types.html#text-result
#[derive(Clone)]
#[cfg_attr(test, derive(Debug))]
pub struct FuncResult {
    pub start: usize,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/types.html#text-valtype
#[derive(PartialEq, Clone, Copy)]
#[cfg_attr(test, derive(Debug))]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}

impl fmt::Display for ValType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ValType::I32 => "i32",
            ValType::I64 => "i64",
            ValType::F32 => "f32",
            ValType::F64 => "f64",
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-import
#[cfg_attr(test, derive(Debug))]
pub struct Import<'s> {
    pub mod_name: Name<'s>,
    pub name: Name<'s>,
}

// https://webassembly.github.io/spec/core/text/values.html#text-name
//
// Use Cow<'s, str> since it is String on text format and it is &str on binary format.
// In text format, special characters in string literal are escaped. Unescaped string must
// be allocated in heap. In binary format, it is directly encoded as bytes so borrowing the
// part of source is enough.
#[cfg_attr(test, derive(Debug))]
pub struct Name<'s>(pub Cow<'s, str>);

// Type index has two flavors. One is specified by user explicitly in u32 index or identifier name.
// Another is inserted implicitly and automatically by interpreter when typeuse is omitted.
// For the implicit typeuse, we use a separate index space and resolve the indices after.
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum TypeIndex<'s> {
    Explicit(Index<'s>),
    Implicit(u32),
}

// https://webassembly.github.io/spec/core/text/modules.html#type-uses
#[cfg_attr(test, derive(Debug))]
pub struct TypeUse<'s> {
    pub start: usize,
    pub idx: TypeIndex<'s>,
    pub params: Vec<Param<'s>>,
    pub results: Vec<FuncResult>,
}

// https://webassembly.github.io/spec/core/text/modules.html#type-uses
pub struct ImplicitTypeUse<'s> {
    pub start: usize,
    pub params: Vec<Param<'s>>,
    pub results: Vec<FuncResult>,
}

// https://webassembly.github.io/spec/core/text/modules.html#indices
#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Index<'s> {
    Num(u32),
    Ident(&'s str),
}
impl<'s> fmt::Display for Index<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Index::Num(i) => write!(f, "{}", i),
            Index::Ident(i) => write!(f, "{}", i),
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-tabletype
// Note: elemtype is currently fixed to 'funcref'
#[cfg_attr(test, derive(Debug))]
pub struct TableType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/text/types.html#text-limits
#[cfg_attr(test, derive(Debug))]
pub enum Limits {
    Range { min: u32, max: u32 },
    From { min: u32 },
}

// https://webassembly.github.io/spec/core/text/types.html#text-memtype
#[cfg_attr(test, derive(Debug))]
pub struct MemType {
    pub limit: Limits,
}

// https://webassembly.github.io/spec/core/text/types.html#text-globaltype
#[cfg_attr(test, derive(Debug))]
pub struct GlobalType {
    pub mutable: bool,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-export
#[cfg_attr(test, derive(Debug))]
pub struct Export<'s> {
    pub start: usize,
    pub name: Name<'s>,
    pub kind: ExportKind,
    pub idx: Index<'s>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-exportdesc
#[cfg_attr(test, derive(Debug))]
pub enum ExportKind {
    Func,
    Table,
    Memory,
    Global,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-func
#[cfg_attr(test, derive(Debug))]
pub enum FuncKind<'s> {
    Import(Import<'s>),
    Body {
        locals: Vec<Local<'s>>,
        body: Vec<Instruction<'s>>,
    },
}
#[cfg_attr(test, derive(Debug))]
pub struct Func<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: TypeUse<'s>,
    pub kind: FuncKind<'s>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-local
#[cfg_attr(test, derive(Debug))]
pub struct Local<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/instructions.html#instructions
#[cfg_attr(test, derive(Debug))]
pub struct Instruction<'s> {
    pub start: usize,
    pub kind: InsnKind<'s>,
}

// https://webassembly.github.io/spec/core/text/instructions.html#text-memarg
#[cfg_attr(test, derive(Debug))]
pub struct Mem {
    pub align: u32,
    pub offset: u32,
}

#[cfg_attr(test, derive(Debug))]
pub enum InsnKind<'s> {
    // Control instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
    Block {
        label: Option<&'s str>,
        ty: Option<ValType>,
        body: Vec<Instruction<'s>>,
        id: Option<&'s str>,
    },
    Loop {
        label: Option<&'s str>,
        ty: Option<ValType>,
        body: Vec<Instruction<'s>>,
        id: Option<&'s str>,
    },
    If {
        label: Option<&'s str>,
        ty: Option<ValType>,
        then_body: Vec<Instruction<'s>>,
        else_id: Option<&'s str>,
        else_body: Vec<Instruction<'s>>,
        end_id: Option<&'s str>,
    },
    Unreachable,
    Nop,
    Br(Index<'s>),
    BrIf(Index<'s>),
    BrTable {
        labels: Vec<Index<'s>>,
        default_label: Index<'s>,
    },
    Return,
    Call(Index<'s>),
    CallIndirect(TypeUse<'s>),
    // Parametric instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
    Drop,
    Select,
    // Variable instructions
    // https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
    LocalGet(Index<'s>),
    LocalSet(Index<'s>),
    LocalTee(Index<'s>),
    GlobalGet(Index<'s>),
    GlobalSet(Index<'s>),
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
    // Sign extension
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,
}

impl<'s> InsnKind<'s> {
    pub fn is_block(&self) -> bool {
        use InsnKind::*;
        matches!(self, Block { .. } | Loop { .. } | If { .. })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#element-segments
#[cfg_attr(test, derive(Debug))]
pub struct Elem<'s> {
    pub start: usize,
    pub idx: Index<'s>,
    pub offset: Vec<Instruction<'s>>,
    pub init: Vec<Index<'s>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#tables
#[cfg_attr(test, derive(Debug))]
pub struct Table<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: TableType,
    pub import: Option<Import<'s>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-data
#[cfg_attr(test, derive(Debug))]
pub struct Data<'s> {
    pub start: usize,
    pub idx: Index<'s>,
    pub offset: Vec<Instruction<'s>>,
    pub data: Cow<'s, [u8]>,
}

// https://webassembly.github.io/spec/core/text/modules.html#memories
#[cfg_attr(test, derive(Debug))]
pub struct Memory<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: MemType,
    pub import: Option<Import<'s>>,
}

// https://webassembly.github.io/spec/core/text/modules.html#globals
#[cfg_attr(test, derive(Debug))]
pub enum GlobalKind<'s> {
    Import(Import<'s>),
    Init(Vec<Instruction<'s>>),
}
#[cfg_attr(test, derive(Debug))]
pub struct Global<'s> {
    pub start: usize,
    pub id: Option<&'s str>,
    pub ty: GlobalType,
    pub kind: GlobalKind<'s>,
}

// https://webassembly.github.io/spec/core/text/modules.html#text-start
#[cfg_attr(test, derive(Debug))]
pub struct Start<'s> {
    pub start: usize,
    pub idx: Index<'s>,
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
