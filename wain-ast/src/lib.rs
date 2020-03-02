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
    pub ident: &'a str,
    pub types: Vec<TypeDef<'a>>,
    pub imports: Vec<Import<'a>>,
    pub exports: Vec<Export<'a>>,
    // TODO: funcs, table, memory, globals, start, elems, data
}

// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
#[derive(Debug)]
pub enum ModuleField<'a> {
    Type(TypeDef<'a>),
    Import(Import<'a>),
    Export(Export<'a>),
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
#[derive(Debug)]
pub struct Param<'a> {
    pub start: usize,
    pub id: Option<&'a str>,
    pub ty: ValType,
}

// https://webassembly.github.io/spec/core/text/types.html#text-result
#[derive(Debug)]
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
    pub start: usize,
    pub mod_name: Name,
    pub name: Name,
    pub desc: ImportDesc<'a>,
}

// https://webassembly.github.io/spec/core/text/values.html#text-name
#[derive(Debug)]
pub struct Name(pub String);

// https://webassembly.github.io/spec/core/text/modules.html#text-importdesc
#[derive(Debug)]
pub enum ImportDesc<'a> {
    Func {
        start: usize,
        id: Option<&'a str>,
        ty: TypeUse<'a>,
    },
    Table {
        start: usize,
        id: Option<&'a str>,
        ty: TableType,
    },
    Memory {
        start: usize,
        id: Option<&'a str>,
        ty: MemType,
    },
    Global {
        start: usize,
        id: Option<&'a str>,
        ty: GlobalType,
    },
}

// https://webassembly.github.io/spec/core/text/modules.html#type-uses
#[derive(Debug)]
pub struct TypeUse<'a> {
    pub start: usize,
    pub idx: Index<'a>,
    pub params: Vec<Param<'a>>,
    pub results: Vec<FuncResult>,
}

// https://webassembly.github.io/spec/core/text/modules.html#indices
#[derive(Debug)]
pub enum Index<'a> {
    Num(u32),
    Ident(&'a str),
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
    pub name: Name,
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
pub struct Func<'a> {
    pub start: usize,
    pub ty: TypeUse<'a>,
    pub locals: Vec<Local<'a>>,
    pub body: Vec<Instruction>,
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
pub struct Instruction {
    pub start: usize,
    pub kind: InsnKind,
}

#[derive(Debug)]
pub enum InsnKind {
    Unreachable,
    Nop,
    Return,
}
