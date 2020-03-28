use crate::error::{Error, ErrorKind, Result};
use crate::leb128::Leb128;
use crate::source::BinarySource;
use std::borrow::Cow;
use std::convert::TryInto;
use std::str;
use wain_ast::*;

fn section_name(id: u8) -> &'static str {
    // https://webassembly.github.io/spec/core/binary/modules.html#sections
    match id {
        0 => "custom section",
        1 => "type section",
        2 => "import section",
        3 => "import section",
        4 => "table section",
        5 => "memory section",
        6 => "global section",
        7 => "export section",
        8 => "start section",
        9 => "element section",
        10 => "code section",
        11 => "data section",
        _ => unreachable!(),
    }
}

pub struct Parser<'source> {
    source: &'source [u8],
    input: &'source [u8],
    rest_len: usize,
}

impl<'s> Parser<'s> {
    pub fn new(input: &'s [u8]) -> Parser<'s> {
        Self {
            source: input,
            input,
            rest_len: 0,
        }
    }

    fn end(&self) -> bool {
        self.input.is_empty()
    }

    fn eat(&mut self, bytes: usize) {
        self.input = &self.input[bytes..];
    }

    fn peek(&self, expected: &'static str) -> Result<'s, u8> {
        if self.input.is_empty() {
            Err(self.error(ErrorKind::UnexpectedEof { expected }))
        } else {
            Ok(self.input[0])
        }
    }

    fn current_pos(&self) -> usize {
        self.source.len() - self.input.len() - self.rest_len
    }

    fn error(&self, kind: ErrorKind) -> Box<Error<'s>> {
        let pos = self.current_pos();
        Error::new(kind, pos, self.source)
    }

    pub fn parse<P: Parse<'s>>(&mut self) -> Result<'s, P> {
        Parse::parse(self)
    }

    fn parse_int<I: Leb128>(&mut self) -> Result<'s, I> {
        match I::read_leb128(&self.input) {
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
        })
    }

    fn section_parser(&mut self) -> Result<'s, Parser<'s>> {
        let section = section_name(self.input[0]);
        self.eat(1); // Eat section ID
        let size = self.parse_int::<u32>()? as usize;
        let parser = self.sub_parser(size as usize, section)?;
        self.eat(size);
        Ok(parser)
    }

    // Note: Custom section is a pair of name and bytes payload. Currently custom section is simply
    // ignored since it is not necessary to execute wasm binary.
    fn ignore_custom_sections(&mut self) -> Result<'s, ()> {
        while let [0x00, ..] = self.input {
            self.section_parser()?; // Generating section parser eats the entire section
        }
        Ok(())
    }

    fn parse_magic(&mut self, byte: u8, what: &'static str) -> Result<'s, ()> {
        let top = self.peek(what)?;
        if top == byte {
            self.eat(1);
            Ok(())
        } else {
            Err(self.error(ErrorKind::MagicMismatch {
                expected: byte,
                got: top,
                what,
            }))
        }
    }

    // https://webassembly.github.io/spec/core/binary/conventions.html#binary-vec
    fn parse_vec<P: Parse<'s>>(&mut self) -> Result<'s, Vec<P>> {
        let size: u32 = self.parse_int()?;
        let mut vec = Vec::with_capacity(size as usize);
        for _ in 0..size {
            vec.push(self.parse()?);
        }
        Ok(vec)
    }
}

pub trait Parse<'source>: Sized {
    fn parse(parser: &mut Parser<'source>) -> Result<'source, Self>;
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
        fn parse_section<'s, P: Parse<'s>>(parser: &mut Parser<'s>, id: u8) -> Result<'s, Vec<P>> {
            if parser.input.starts_with(&[id]) {
                let mut inner = parser.section_parser()?;
                inner.parse_vec()
            } else {
                Ok(vec![])
            }
        }

        let start = parser.current_pos();

        match parser.input {
            [0x00, 0x61, 0x73, 0x6d, ..] => parser.eat(4),
            _ => return Err(parser.error(ErrorKind::MagicNotFound)),
        }

        match parser.input {
            [0x01, 0x00, 0x00, 0x00, ..] => parser.eat(4),
            _ => {
                return Err(
                    parser.error(ErrorKind::VersionMismatch(parser.input.try_into().unwrap()))
                )
            }
        }

        parser.ignore_custom_sections()?;

        let types = parse_section(parser, 0x01)?;

        parser.ignore_custom_sections()?;

        let mut funcs = vec![];
        let mut tables = vec![];
        let mut memories = vec![];
        let mut globals = vec![];

        if let [0x02, ..] = parser.input {
            let mut inner = parser.section_parser()?;
            let size: u32 = inner.parse_int()?;
            for _ in 0..size {
                match inner.parse()? {
                    ImportDesc::Func(f) => funcs.push(f),
                    ImportDesc::Table(t) => tables.push(t),
                    ImportDesc::Memory(m) => memories.push(m),
                    ImportDesc::Global(g) => globals.push(g),
                }
            }
        }

        parser.ignore_custom_sections()?;

        // TODO

        Ok(Module {
            start,
            id: None,
            types,
            exports: unimplemented!(),
            funcs,
            elems: unimplemented!(),
            tables,
            data: unimplemented!(),
            memories,
            globals,
            entrypoint: unimplemented!(),
        })
    }
}

// https://webassembly.github.io/spec/core/binary/values.html#names
impl<'s> Parse<'s> for Name<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let size = parser.parse_int::<u32>()? as usize;
        parser.check_len(size, "name")?;
        match str::from_utf8(&parser.input[..size]) {
            Ok(name) => Ok(Name(Cow::Borrowed(name))),
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
        parser.parse_magic(0x60, "function type")?;
        let params = parser.parse_vec()?;
        let results = parser.parse_vec()?;
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
        let t = match parser.peek("value type")? {
            0x7f => ValType::I32,
            0x7e => ValType::I64,
            0x7d => ValType::F32,
            0x7c => ValType::F64,
            b => return Err(parser.error(ErrorKind::UnexpectedValType(b))),
        };
        parser.eat(1);
        Ok(t)
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
        match parser.peek("import description")? {
            0x00 => Ok(ImportDesc::Func(Func {
                start,
                idx: parser.parse_int()?,
                kind: FuncKind::Import(import),
            })),
            0x01 => Ok(ImportDesc::Table(Table {
                start,
                ty: unimplemented!(),
                import: Some(import),
            })),
            0x02 => Ok(ImportDesc::Memory(Memory {
                start,
                ty: unimplemented!(),
                import: Some(import),
            })),
            0x03 => unimplemented!(),
            b => Err(parser.error(ErrorKind::UnexpectedImportDesc(b))),
        }
    }
}
