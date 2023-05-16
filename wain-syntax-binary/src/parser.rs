use crate::error::{Error, ErrorKind, Result};
use crate::leb128::Leb128;
use crate::source::BinarySource;
use std::borrow::Cow;
use std::convert::TryInto;
use std::marker::PhantomData;
use std::str;
use wain_ast::*;

mod section_id {
    pub const CUSTOM: u8 = 0;
    pub const TYPE: u8 = 1;
    pub const IMPORT: u8 = 2;
    pub const FUNCTION: u8 = 3;
    pub const TABLE: u8 = 4;
    pub const MEMORY: u8 = 5;
    pub const GLOBAL: u8 = 6;
    pub const EXPORT: u8 = 7;
    pub const START: u8 = 8;
    pub const ELEMENT: u8 = 9;
    pub const CODE: u8 = 10;
    pub const DATA: u8 = 11;

    pub fn to_name(id: u8) -> &'static str {
        // https://webassembly.github.io/spec/core/binary/modules.html#sections
        match id {
            self::CUSTOM => "custom section",
            self::TYPE => "type section",
            self::IMPORT => "import section",
            self::FUNCTION => "function section",
            self::TABLE => "table section",
            self::MEMORY => "memory section",
            self::GLOBAL => "global section",
            self::EXPORT => "export section",
            self::START => "start section",
            self::ELEMENT => "element section",
            self::CODE => "code section",
            self::DATA => "data section",
            _ => unreachable!(),
        }
    }
}

// An iterator to iterate vec(P)
// https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
struct VecItems<'p, 's, P: Parse<'s>> {
    parser: &'p mut Parser<'s>,
    count: usize,
    phantom: PhantomData<P>, // Without this, P is not constrainted in Iterator implementation
}

impl<'p, 's, P: Parse<'s>> VecItems<'p, 's, P> {
    fn new(parser: &'p mut Parser<'s>, count: usize) -> Self {
        Self {
            parser,
            count,
            phantom: PhantomData,
        }
    }

    // Helper method to collect as Vec<P>. Without this, type must be specified for ? operator as bellow
    //   parser.parse_vec()?.collect::<Result<'_, _>>()
    fn into_vec(self) -> Result<'s, Vec<P>> {
        self.collect()
    }
}

impl<'p, 's, P: Parse<'s>> Iterator for VecItems<'p, 's, P> {
    type Item = Result<'s, P>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.count == 0 {
            return None;
        }
        self.count -= 1;
        Some(self.parser.parse())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.count, Some(self.count))
    }
}

pub struct Parser<'source> {
    source: &'source [u8],
    input: &'source [u8],
    rest_len: usize,
    // What is being parsed for better error message
    parsing: &'static str,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s [u8]) -> Parser<'s> {
        Self {
            source: input,
            input,
            rest_len: 0,
            parsing: "module",
        }
    }

    fn eat(&mut self, bytes: usize) {
        self.input = &self.input[bytes..];
    }

    fn consume(&mut self, expected: &'static str) -> Result<'s, u8> {
        if self.input.is_empty() {
            Err(self.error(ErrorKind::UnexpectedEof { expected }))
        } else {
            let b = self.input[0];
            self.input = &self.input[1..];
            Ok(b)
        }
    }

    fn current_pos(&self) -> usize {
        self.source.len() - self.input.len() - self.rest_len
    }

    fn error(&self, kind: ErrorKind) -> Box<Error<'s>> {
        let pos = self.current_pos();
        Error::new(kind, pos, self.source, self.parsing)
    }

    fn unexpected_byte<T: AsRef<[u8]>>(
        &self,
        expected: T,
        got: u8,
        what: &'static str,
    ) -> Box<Error<'s>> {
        let pos = self.current_pos() - 1; // Unget one character for magic
        let kind = ErrorKind::UnexpectedByte {
            expected: expected.as_ref().to_vec(),
            got,
            what,
        };
        Error::new(kind, pos, self.source, self.parsing)
    }

    pub fn parse<P: Parse<'s>>(&mut self) -> Result<'s, P> {
        Parse::parse(self)
    }

    fn parse_int<I: Leb128>(&mut self) -> Result<'s, I> {
        match I::read_leb128(self.input) {
            Ok((i, len)) => {
                self.eat(len);
                Ok(i)
            }
            Err(kind) => Err(self.error(*kind)),
        }
    }

    fn check_len(&self, len: usize, what: &'static str) -> Result<'s, ()> {
        if self.input.len() < len {
            Err(self.error(ErrorKind::LengthOutOfInput {
                input: self.input.len(),
                specified: len,
                what,
            }))
        } else {
            Ok(())
        }
    }

    fn sub_parser(&self, sub_len: usize, what: &'static str) -> Result<'s, Parser<'s>> {
        self.check_len(sub_len, what)?;
        Ok(Parser {
            source: self.source,
            rest_len: self.input.len() - sub_len,
            input: &self.input[..sub_len],
            parsing: what,
        })
    }

    fn section_parser(&mut self) -> Result<'s, Parser<'s>> {
        let section = section_id::to_name(self.input[0]);
        self.eat(1); // Eat section ID
        let size = self.parse_int::<u32>()? as usize;
        let parser = self.sub_parser(size, section)?;
        self.eat(size);
        Ok(parser)
    }

    // Note: Custom section is a pair of name and bytes payload. Currently custom section is simply
    // ignored since it is not necessary to execute wasm binary.
    fn ignore_custom_sections(&mut self) -> Result<'s, ()> {
        while let [section_id::CUSTOM, ..] = self.input {
            let mut inner = self.section_parser()?;
            let _: Name = inner.parse()?;
        }
        Ok(())
    }

    fn parse_flag(&mut self, byte: u8, what: &'static str) -> Result<'s, ()> {
        let b = self.consume(what)?;
        if b == byte {
            Ok(())
        } else {
            Err(self.unexpected_byte([byte], b, what))
        }
    }

    // https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
    fn parse_vec<P: Parse<'s>>(&mut self) -> Result<'s, VecItems<'_, 's, P>> {
        let size = self.parse_int::<u32>()? as usize;
        // At least `size` bytes must be followed. This is necessary to check the size is correct
        // before actually allocating Vec capacity. Otherwise Vec::reserve crashes (#30)
        self.check_len(size, "size of vec elements")?;
        Ok(VecItems::new(self, size))
    }

    fn check_section_end(&self, section_id: u8) -> Result<'s, ()> {
        if self.input.is_empty() {
            Ok(())
        } else {
            Err(self.error(ErrorKind::MalformedSectionSize {
                name: section_id::to_name(section_id),
                remaining_bytes: self.input.len(),
            }))
        }
    }

    fn push_vec_items<P: Parse<'s>>(&mut self, section_id: u8, vec: &mut Vec<P>) -> Result<'s, ()> {
        if let [b, ..] = self.input {
            if *b == section_id {
                let mut inner = self.section_parser()?;
                let vec_items = inner.parse_vec()?;
                vec.reserve(vec_items.count);
                for elem in vec_items {
                    vec.push(elem?);
                }
                inner.check_section_end(section_id)?;
            }
        }
        Ok(())
    }
}

pub trait Parse<'source>: Sized {
    fn parse(parser: &mut Parser<'source>) -> Result<'source, Self>;
}

// Parse u32 for typeidx, funcidx, memidx, ...
impl<'s> Parse<'s> for u32 {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        parser.parse_int()
    }
}

impl<'s> Parse<'s> for Root<'s, BinarySource<'s>> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(Root {
            module: parser.parse()?,
            source: BinarySource(parser.source),
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-module
impl<'s> Parse<'s> for Module<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        fn parse_section_into_vec<'s, P: Parse<'s>>(
            parser: &mut Parser<'s>,
            id: u8,
        ) -> Result<'s, Vec<P>> {
            if parser.input.starts_with(&[id]) {
                let mut inner = parser.section_parser()?;
                let vec = inner.parse_vec()?.into_vec();
                inner.check_section_end(id)?;
                vec
            } else {
                Ok(vec![])
            }
        }

        let start = parser.current_pos();

        match parser.input {
            [0x00, 0x61, 0x73, 0x6d, ..] => parser.eat(4), // b"\0asm"
            _ => return Err(parser.error(ErrorKind::WasmMagicNotFound)),
        }

        parser.check_len(4, "wasm version")?;
        match parser.input {
            [0x01, 0x00, 0x00, 0x00, ..] => parser.eat(4),
            _ => {
                let bytes = parser.input[..4].try_into().unwrap();
                return Err(parser.error(ErrorKind::VersionMismatch(bytes)));
            }
        }

        parser.ignore_custom_sections()?;

        // Type section
        let types = parse_section_into_vec(parser, section_id::TYPE)?;

        parser.ignore_custom_sections()?;

        let mut funcs = vec![];
        let mut tables = vec![];
        let mut memories = vec![];
        let mut globals = vec![];

        // Import section
        if let [section_id::IMPORT, ..] = parser.input {
            let mut inner = parser.section_parser()?;
            for desc in inner.parse_vec()? {
                match desc? {
                    ImportDesc::Func(f) => funcs.push(f),
                    ImportDesc::Table(t) => tables.push(t),
                    ImportDesc::Memory(m) => memories.push(m),
                    ImportDesc::Global(g) => globals.push(g),
                }
            }
            inner.check_section_end(section_id::IMPORT)?;
        }

        parser.ignore_custom_sections()?;

        // Function section
        // https://webassembly.github.io/spec/core/binary/modules.html#binary-funcsec
        // Only type indices are stored in function section. Locals and bodies are stored in code section
        let func_indices: Vec<u32> = parse_section_into_vec(parser, section_id::FUNCTION)?;
        funcs.reserve(func_indices.len());

        parser.ignore_custom_sections()?;

        // Table section
        parser.push_vec_items(section_id::TABLE, &mut tables)?;

        parser.ignore_custom_sections()?;

        // Memory section
        parser.push_vec_items(section_id::MEMORY, &mut memories)?;

        parser.ignore_custom_sections()?;

        // Global section
        parser.push_vec_items(section_id::GLOBAL, &mut globals)?;

        parser.ignore_custom_sections()?;

        let exports = parse_section_into_vec(parser, section_id::EXPORT)?;

        parser.ignore_custom_sections()?;

        // Start function section
        let entrypoint = if let [section_id::START, ..] = parser.input {
            let mut inner = parser.section_parser()?;
            let start = inner.parse()?;
            inner.check_section_end(section_id::START)?;
            Some(start)
        } else {
            None
        };

        parser.ignore_custom_sections()?;

        // Element segments section
        let elems = parse_section_into_vec(parser, section_id::ELEMENT)?;

        parser.ignore_custom_sections()?;

        // Code section
        if let [section_id::CODE, ..] = parser.input {
            let mut inner = parser.section_parser()?;

            let codes = inner.parse_vec::<Code>()?;
            if codes.count != func_indices.len() {
                return Err(codes.parser.error(ErrorKind::FuncCodeLengthMismatch {
                    num_funcs: func_indices.len(),
                    num_codes: codes.count,
                }));
            }

            for (code, typeidx) in codes.zip(func_indices.into_iter()) {
                let code = code?;
                funcs.push(Func {
                    start: code.start,
                    idx: typeidx,
                    kind: FuncKind::Body {
                        locals: code.locals,
                        expr: code.expr,
                    },
                });
            }

            inner.check_section_end(section_id::CODE)?
        } else if !func_indices.is_empty() {
            return Err(parser.error(ErrorKind::FuncCodeLengthMismatch {
                num_funcs: func_indices.len(),
                num_codes: 0,
            }));
        }

        parser.ignore_custom_sections()?;

        let data = parse_section_into_vec(parser, section_id::DATA)?;

        parser.ignore_custom_sections()?;

        if !parser.input.is_empty() {
            return Err(parser.error(ErrorKind::ExpectedEof(parser.input[0])));
        }

        Ok(Module {
            start,
            id: None,
            types,
            exports,
            funcs,
            elems,
            tables,
            data,
            memories,
            globals,
            entrypoint,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/values.html#names
impl<'s> Parse<'s> for Name<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let size = parser.parse_int::<u32>()? as usize;
        parser.check_len(size, "name")?;
        match str::from_utf8(&parser.input[..size]) {
            Ok(name) => {
                parser.eat(size);
                Ok(Name(Cow::Borrowed(name)))
            }
            Err(error) => Err(parser.error(ErrorKind::InvalidUtf8 {
                what: "name",
                error,
            })),
        }
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#function-types
impl<'s> Parse<'s> for FuncType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        parser.parse_flag(0x60, "function type")?;
        let params = parser.parse_vec()?.into_vec()?;
        let results = parser.parse_vec()?.into_vec()?;
        Ok(FuncType {
            start,
            params,
            results,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#binary-valtype
impl<'s> Parse<'s> for ValType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        match parser.consume("value type")? {
            0x7f => Ok(ValType::I32),
            0x7e => Ok(ValType::I64),
            0x7d => Ok(ValType::F32),
            0x7c => Ok(ValType::F64),
            b => Err(parser.unexpected_byte([0x7f, 0x7e, 0x7d, 0x7c], b, "value type")),
        }
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-import
enum ImportDesc<'s> {
    Func(Func<'s>),
    Table(Table<'s>),
    Memory(Memory<'s>),
    Global(Global<'s>),
}
impl<'s> Parse<'s> for ImportDesc<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        let import = Import {
            mod_name: parser.parse()?,
            name: parser.parse()?,
        };
        match parser.consume("import description")? {
            0x00 => Ok(ImportDesc::Func(Func {
                start,
                idx: parser.parse()?,
                kind: FuncKind::Import(import),
            })),
            0x01 => Ok(ImportDesc::Table(Table {
                start,
                ty: parser.parse()?,
                import: Some(import),
            })),
            0x02 => Ok(ImportDesc::Memory(Memory {
                start,
                ty: parser.parse()?,
                import: Some(import),
            })),
            0x03 => {
                let GlobalType(mutable, ty) = parser.parse()?;
                Ok(ImportDesc::Global(Global {
                    start,
                    mutable,
                    ty,
                    kind: GlobalKind::Import(import),
                }))
            }
            b => Err(parser.unexpected_byte([0x00, 0x01, 0x02, 0x03], b, "import description")),
        }
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#binary-tabletype
impl<'s> Parse<'s> for TableType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        parser.parse_flag(0x70, "funcref of table type")?;
        Ok(TableType {
            limit: parser.parse()?,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#binary-limits
impl<'s> Parse<'s> for Limits {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        match parser.consume("limit")? {
            0x00 => Ok(Limits::From(parser.parse_int()?)),
            0x01 => Ok(Limits::Range(parser.parse_int()?, parser.parse_int()?)),
            b => Err(parser.unexpected_byte([0x00, 0x01], b, "limit")),
        }
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#memory-types
impl<'s> Parse<'s> for MemType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(MemType {
            limit: parser.parse()?,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#global-types
struct GlobalType(bool, ValType);
impl<'s> Parse<'s> for GlobalType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let ty = parser.parse()?;
        let mutable = match parser.consume("mutability of global type")? {
            0x00 => false,
            0x01 => true,
            b => return Err(parser.unexpected_byte([0x00, 0x01], b, "mutability of global type")),
        };
        Ok(GlobalType(mutable, ty))
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-tablesec
impl<'s> Parse<'s> for Table<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(Table {
            start: parser.current_pos(),
            ty: parser.parse()?,
            import: None,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-memsec
impl<'s> Parse<'s> for Memory<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(Memory {
            start: parser.current_pos(),
            ty: parser.parse()?,
            import: None,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-globalsec
impl<'s> Parse<'s> for Global<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        let GlobalType(mutable, ty) = parser.parse()?;
        let Expr(insns) = parser.parse()?;
        Ok(Global {
            start,
            mutable,
            ty,
            kind: GlobalKind::Init(insns),
        })
    }
}

// https://webassembly.github.io/spec/core/binary/instructions.html#expressions
struct Expr(Vec<Instruction>);
impl<'s> Parse<'s> for Expr {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mut insns = vec![];
        loop {
            if let [0x0b, ..] = parser.input {
                parser.eat(1);
                return Ok(Expr(insns));
            }
            insns.push(parser.parse()?);
        }
    }
}

// https://webassembly.github.io/spec/core/binary/types.html#binary-blocktype
struct BlockType(Option<ValType>);
impl<'s> Parse<'s> for BlockType {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        if let [0x40, ..] = parser.input {
            parser.eat(1);
            return Ok(BlockType(None));
        }
        Ok(BlockType(Some(parser.parse()?)))
    }
}

// https://webassembly.github.io/spec/core/binary/instructions.html#instructions
impl<'s> Parse<'s> for Instruction {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        use InsnKind::*;
        let start = parser.current_pos();
        let kind = match parser.consume("instruction")? {
            // Control instructions
            // https://webassembly.github.io/spec/core/binary/instructions.html#control-instructions
            0x00 => Unreachable,
            0x01 => Nop,
            0x02 => {
                let BlockType(ty) = parser.parse()?;
                let Expr(body) = parser.parse()?;
                Block { ty, body }
            }
            0x03 => {
                let BlockType(ty) = parser.parse()?;
                let Expr(body) = parser.parse()?;
                Loop { ty, body }
            }
            0x04 => {
                let BlockType(ty) = parser.parse()?;

                let mut then_body = vec![];
                let has_else = loop {
                    match parser.input {
                        [0x05, ..] => break true,
                        [0x0b, ..] => break false,
                        _ => then_body.push(parser.parse()?),
                    }
                };
                parser.eat(1);

                let else_body = if has_else {
                    let Expr(insns) = parser.parse()?;
                    insns
                } else {
                    vec![]
                };

                If {
                    ty,
                    then_body,
                    else_body,
                }
            }
            0x0c => Br(parser.parse()?),
            0x0d => BrIf(parser.parse()?),
            0x0e => BrTable {
                labels: parser.parse_vec()?.into_vec()?,
                default_label: parser.parse()?,
            },
            0x0f => Return,
            0x10 => Call(parser.parse()?),
            0x11 => {
                let insn = CallIndirect(parser.parse()?);
                parser.parse_flag(0x00, "reserved byte in call_indirect")?;
                insn
            }
            // Parametric instructions
            // https://webassembly.github.io/spec/core/binary/instructions.html#parametric-instructions
            0x1a => Drop,
            0x1b => Select,
            // Variable instructions
            // https://webassembly.github.io/spec/core/binary/instructions.html#variable-instructions
            0x20 => LocalGet(parser.parse()?),
            0x21 => LocalSet(parser.parse()?),
            0x22 => LocalTee(parser.parse()?),
            0x23 => GlobalGet(parser.parse()?),
            0x24 => GlobalSet(parser.parse()?),
            // Memory instructions
            // https://webassembly.github.io/spec/core/binary/instructions.html#memory-instructions
            0x28 => I32Load(parser.parse()?),
            0x29 => I64Load(parser.parse()?),
            0x2a => F32Load(parser.parse()?),
            0x2b => F64Load(parser.parse()?),
            0x2c => I32Load8S(parser.parse()?),
            0x2d => I32Load8U(parser.parse()?),
            0x2e => I32Load16S(parser.parse()?),
            0x2f => I32Load16U(parser.parse()?),
            0x30 => I64Load8S(parser.parse()?),
            0x31 => I64Load8U(parser.parse()?),
            0x32 => I64Load16S(parser.parse()?),
            0x33 => I64Load16U(parser.parse()?),
            0x34 => I64Load32S(parser.parse()?),
            0x35 => I64Load32U(parser.parse()?),
            0x36 => I32Store(parser.parse()?),
            0x37 => I64Store(parser.parse()?),
            0x38 => F32Store(parser.parse()?),
            0x39 => F64Store(parser.parse()?),
            0x3a => I32Store8(parser.parse()?),
            0x3b => I32Store16(parser.parse()?),
            0x3c => I64Store8(parser.parse()?),
            0x3d => I64Store16(parser.parse()?),
            0x3e => I64Store32(parser.parse()?),
            0x3f => {
                parser.parse_flag(0x00, "reserved byte in memory.size")?;
                MemorySize
            }
            0x40 => {
                parser.parse_flag(0x00, "reserved byte in memory.grow")?;
                MemoryGrow
            }
            // Numeric instructions
            // constants
            0x41 => I32Const(parser.parse_int()?),
            0x42 => I64Const(parser.parse_int()?),
            0x43 => F32Const(parser.parse()?),
            0x44 => F64Const(parser.parse()?),
            // test and relation operators
            0x45 => I32Eqz,
            0x46 => I32Eq,
            0x47 => I32Ne,
            0x48 => I32LtS,
            0x49 => I32LtU,
            0x4a => I32GtS,
            0x4b => I32GtU,
            0x4c => I32LeS,
            0x4d => I32LeU,
            0x4e => I32GeS,
            0x4f => I32GeU,
            0x50 => I64Eqz,
            0x51 => I64Eq,
            0x52 => I64Ne,
            0x53 => I64LtS,
            0x54 => I64LtU,
            0x55 => I64GtS,
            0x56 => I64GtU,
            0x57 => I64LeS,
            0x58 => I64LeU,
            0x59 => I64GeS,
            0x5a => I64GeU,
            0x5b => F32Eq,
            0x5c => F32Ne,
            0x5d => F32Lt,
            0x5e => F32Gt,
            0x5f => F32Le,
            0x60 => F32Ge,
            0x61 => F64Eq,
            0x62 => F64Ne,
            0x63 => F64Lt,
            0x64 => F64Gt,
            0x65 => F64Le,
            0x66 => F64Ge,
            // i32 operations
            0x67 => I32Clz,
            0x68 => I32Ctz,
            0x69 => I32Popcnt,
            0x6a => I32Add,
            0x6b => I32Sub,
            0x6c => I32Mul,
            0x6d => I32DivS,
            0x6e => I32DivU,
            0x6f => I32RemS,
            0x70 => I32RemU,
            0x71 => I32And,
            0x72 => I32Or,
            0x73 => I32Xor,
            0x74 => I32Shl,
            0x75 => I32ShrS,
            0x76 => I32ShrU,
            0x77 => I32Rotl,
            0x78 => I32Rotr,
            // i64 operations
            0x79 => I64Clz,
            0x7a => I64Ctz,
            0x7b => I64Popcnt,
            0x7c => I64Add,
            0x7d => I64Sub,
            0x7e => I64Mul,
            0x7f => I64DivS,
            0x80 => I64DivU,
            0x81 => I64RemS,
            0x82 => I64RemU,
            0x83 => I64And,
            0x84 => I64Or,
            0x85 => I64Xor,
            0x86 => I64Shl,
            0x87 => I64ShrS,
            0x88 => I64ShrU,
            0x89 => I64Rotl,
            0x8a => I64Rotr,
            // f32 operations
            0x8b => F32Abs,
            0x8c => F32Neg,
            0x8d => F32Ceil,
            0x8e => F32Floor,
            0x8f => F32Trunc,
            0x90 => F32Nearest,
            0x91 => F32Sqrt,
            0x92 => F32Add,
            0x93 => F32Sub,
            0x94 => F32Mul,
            0x95 => F32Div,
            0x96 => F32Min,
            0x97 => F32Max,
            0x98 => F32Copysign,
            // f64 operations
            0x99 => F64Abs,
            0x9a => F64Neg,
            0x9b => F64Ceil,
            0x9c => F64Floor,
            0x9d => F64Trunc,
            0x9e => F64Nearest,
            0x9f => F64Sqrt,
            0xa0 => F64Add,
            0xa1 => F64Sub,
            0xa2 => F64Mul,
            0xa3 => F64Div,
            0xa4 => F64Min,
            0xa5 => F64Max,
            0xa6 => F64Copysign,
            // conversions
            0xa7 => I32WrapI64,
            0xa8 => I32TruncF32S,
            0xa9 => I32TruncF32U,
            0xaa => I32TruncF64S,
            0xab => I32TruncF64U,
            0xac => I64ExtendI32S,
            0xad => I64ExtendI32U,
            0xae => I64TruncF32S,
            0xaf => I64TruncF32U,
            0xb0 => I64TruncF64S,
            0xb1 => I64TruncF64U,
            0xb2 => F32ConvertI32S,
            0xb3 => F32ConvertI32U,
            0xb4 => F32ConvertI64S,
            0xb5 => F32ConvertI64U,
            0xb6 => F32DemoteF64,
            0xb7 => F64ConvertI32S,
            0xb8 => F64ConvertI32U,
            0xb9 => F64ConvertI64S,
            0xba => F64ConvertI64U,
            0xbb => F64PromoteF32,
            0xbc => I32ReinterpretF32,
            0xbd => I64ReinterpretF64,
            0xbe => F32ReinterpretI32,
            0xbf => F64ReinterpretI64,
            // https://webassembly.github.io/spec/core/binary/instructions.html#numeric-instructions
            b => return Err(parser.unexpected_byte([], b, "instruction")),
        };
        Ok(Instruction { start, kind })
    }
}

// https://webassembly.github.io/spec/core/binary/instructions.html#binary-memarg
impl<'s> Parse<'s> for Mem {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let align = parser.parse_int()?;
        let offset = parser.parse_int()?;
        Ok(Mem { align, offset })
    }
}

// https://webassembly.github.io/spec/core/binary/values.html#floating-point
impl<'s> Parse<'s> for f32 {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        if parser.input.len() < 4 {
            return Err(parser.error(ErrorKind::UnexpectedEof {
                expected: "32bit floating point number",
            }));
        }
        let buf: [u8; 4] = parser.input[..4].try_into().unwrap();
        parser.eat(4);
        Ok(f32::from_le_bytes(buf))
    }
}

// https://webassembly.github.io/spec/core/binary/values.html#floating-point
impl<'s> Parse<'s> for f64 {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        if parser.input.len() < 8 {
            return Err(parser.error(ErrorKind::UnexpectedEof {
                expected: "32bit floating point number",
            }));
        }
        let buf: [u8; 8] = parser.input[..8].try_into().unwrap();
        parser.eat(8);
        Ok(f64::from_le_bytes(buf))
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-export
impl<'s> Parse<'s> for Export<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        let name = parser.parse()?;
        let kind = match parser.consume("export description")? {
            0x00 => ExportKind::Func(parser.parse()?),
            0x01 => ExportKind::Table(parser.parse()?),
            0x02 => ExportKind::Memory(parser.parse()?),
            0x03 => ExportKind::Global(parser.parse()?),
            b => {
                return Err(parser.unexpected_byte(
                    [0x00, 0x01, 0x02, 0x03],
                    b,
                    "export description",
                ));
            }
        };
        Ok(Export { start, name, kind })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#start-section
impl<'s> Parse<'s> for StartFunction {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(StartFunction {
            start: parser.current_pos(),
            idx: parser.parse()?,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-elem
impl<'s> Parse<'s> for ElemSegment {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        let idx = parser.parse()?;
        let Expr(offset) = parser.parse()?;
        let init = parser.parse_vec()?.into_vec()?;
        Ok(ElemSegment {
            start,
            idx,
            offset,
            init,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-code
struct Code {
    start: usize,
    locals: Vec<ValType>,
    expr: Vec<Instruction>,
}
impl<'s> Parse<'s> for Code {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        let size = parser.parse_int::<u32>()? as usize;
        // Parsing code section out of the size is malformed. To check it,
        // here we parse the subsequence with nested parser.
        let mut inner = parser.sub_parser(size, "code section")?;
        parser.eat(size);

        let mut locals = vec![];
        for loc in inner.parse_vec::<Locals>()? {
            let loc = loc?;
            if let Some(c) = loc.count.checked_add(locals.len() as u32) {
                locals.resize(c as usize, loc.ty);
            } else {
                return Err(parser.error(ErrorKind::TooManyLocalVariables(locals.len())));
            }
        }

        let Expr(expr) = inner.parse()?;
        if !inner.input.is_empty() {
            return Err(inner.error(ErrorKind::MalformedCodeSize {
                remaining_bytes: inner.input.len(),
            }));
        }
        Ok(Code {
            start,
            locals,
            expr,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-local
struct Locals {
    count: u32,
    ty: ValType,
}
impl<'s> Parse<'s> for Locals {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        Ok(Locals {
            count: parser.parse_int()?,
            ty: parser.parse()?,
        })
    }
}

// https://webassembly.github.io/spec/core/binary/modules.html#binary-data
impl<'s> Parse<'s> for DataSegment<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos();
        let idx = parser.parse()?;
        let Expr(offset) = parser.parse()?;

        // Parse vec(byte) with zero allocation
        let size = parser.parse_int::<u32>()? as usize;
        if size > parser.input.len() {
            return Err(parser.error(ErrorKind::LengthOutOfInput {
                input: parser.input.len(),
                specified: size,
                what: "byte buffer in data segment",
            }));
        }
        let data = &parser.input[..size];
        parser.eat(size);

        Ok(DataSegment {
            start,
            idx,
            offset,
            data: Cow::Borrowed(data),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;

    fn unwrap<T>(res: Result<'_, T>) -> T {
        match res {
            Ok(x) => x,
            Err(e) => panic!("unexpected error: {}", e),
        }
    }

    fn read_hello_file(name: &'static str) -> Vec<u8> {
        let mut dir = env::current_dir().unwrap();
        dir.pop();
        dir.push("examples");
        dir.push("hello");
        dir.push(name);
        fs::read(dir).unwrap()
    }

    // From examples
    #[test]
    fn hello_world() {
        let bin = read_hello_file("hello.wasm");

        let mut parser = Parser::new(&bin);
        let root: Root<'_, _> = unwrap(parser.parse());

        let t = &root.module.types;
        assert_eq!(t.len(), 3);
        assert_eq!(&t[0].params, &[ValType::I32]);
        assert_eq!(&t[0].results, &[ValType::I32]);
        assert_eq!(&t[1].params, &[ValType::I32]);
        assert_eq!(&t[1].results, &[]);
        assert_eq!(&t[2].params, &[]);
        assert_eq!(&t[2].results, &[]);

        let f = &root.module.funcs;
        assert_eq!(f.len(), 3);
        assert!(matches!(&f[0], Func {
            idx: 0,
            kind: FuncKind::Import(Import {
                mod_name: Name(n1),
                name: Name(n2),
            }),
            ..
        } if n1 == "env" && n2 == "putchar"));
        assert!(matches!(
            &f[1],
            Func {
                idx: 1,
                kind: FuncKind::Body { .. },
                ..
            }
        ));
        assert!(matches!(
            &f[2],
            Func {
                idx: 2,
                kind: FuncKind::Body { .. },
                ..
            }
        ));

        let e = &root.module.exports;
        assert_eq!(e.len(), 2);
        assert!(matches!(&e[0], Export {
            name: Name(n),
            kind: ExportKind::Memory(0),
            ..
        } if n == "memory"));
        assert!(matches!(&e[1], Export {
            name: Name(n),
            kind: ExportKind::Func(2),
            ..
        } if n == "_start"));

        assert!(root.module.elems.is_empty());

        let t = &root.module.tables;
        assert_eq!(t.len(), 1);
        assert!(matches!(
            &t[0],
            Table {
                ty: TableType {
                    limit: Limits::Range(1, 1),
                },
                import: None,
                ..
            }
        ));

        let m = &root.module.memories;
        assert_eq!(m.len(), 1);
        assert!(matches!(
            &m[0],
            Memory {
                ty: MemType {
                    limit: Limits::From(2),
                },
                import: None,
                ..
            }
        ));

        let d = &root.module.data;
        assert_eq!(d.len(), 1);
        assert_eq!(d[0].idx, 0);
        assert!(matches!(
            d[0].offset.as_slice(),
            [Instruction {
                kind: InsnKind::I32Const(1024),
                ..
            }]
        ));
        assert_eq!(d[0].data.as_ref(), b"Hello, world\n\0".as_ref());

        let g = &root.module.globals;
        assert_eq!(g.len(), 1);
        assert!(matches!(&g[0], Global {
            mutable: true,
            ty: ValType::I32,
            kind: GlobalKind::Init(expr),
            ..
        } if matches!(
            expr.as_slice(),
            [
                Instruction {
                    kind: InsnKind::I32Const(66576),
                    ..
                }
            ]
        )));

        assert!(root.module.entrypoint.is_none());

        let bin = read_hello_file("hello_global.wasm");
        let mut parser = Parser::new(&bin);
        let _: Root<'_, _> = unwrap(parser.parse());

        let bin = read_hello_file("hello_indirect_call.wasm");
        let mut parser = Parser::new(&bin);
        let _: Root<'_, _> = unwrap(parser.parse());

        let bin = read_hello_file("hello_struct.wasm");
        let mut parser = Parser::new(&bin);
        let _: Root<'_, _> = unwrap(parser.parse());
    }

    macro_rules! test_parse_error {
        ($name:ident, $expected:pat, $bin:expr) => {
            #[test]
            fn $name() {
                let bin: &[_] = $bin;
                let mut parser = Parser::new(bin);
                match parser.parse::<Module<'_>>() {
                    Ok(_) => panic!("unexpected success"),
                    Err(err) => assert!(matches!(err.kind, $expected)),
                }
            }
        };
    }

    test_parse_error!(
        regression_issue_29,
        ErrorKind::VersionMismatch(_),
        b"\x00\x61\x73\x6d\x01\x31\x39\x00\x61\x73\x6d\x01\x31\x39\x35\x01" // .asm.19.asm.195.
    );

    test_parse_error!(
        regression_issue_30,
        ErrorKind::LengthOutOfInput {
            what: "size of vec elements",
            ..
        },
        b"\x00\x61\x73\x6d\x01\x00\x00\x00\x05\x05\xff\xff\xff\x0d\xfb\x81\x05\x00\x00"
    );

    test_parse_error!(
        code_size,
        ErrorKind::MalformedCodeSize { remaining_bytes: 1 },
        b"\x00\x61\x73\x6d\
          \x01\x00\x00\x00\
          \x01\x04\x01\x60\x00\x00\
          \x03\x02\x01\x00\
          \x0a\x05\x01\x03\x00\x0b\x0b"
    );
}
