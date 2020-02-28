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
    // TODO: imports, funcs, table, memory, globals, exports, start, elems, data
}

// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
#[derive(Debug)]
pub enum ModuleField<'a> {
    Type(TypeDef<'a>),
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
#[derive(Debug)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
}
