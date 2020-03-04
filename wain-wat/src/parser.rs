use crate::lexer::{Float, LexError, Lexer, NumBase, Sign, Token};
use std::char;
use std::f32;
use std::f64;
use std::fmt;
use std::mem;
use wain_ast::*;

#[cfg_attr(test, derive(Debug))]
pub enum ParseErrorKind<'a> {
    LexError(LexError<'a>),
    UnexpectedToken {
        got: Token<'a>,
        expected: &'static str, // TODO: Make 'expected' better
    },
    UnexpectedEndOfFile {
        expected: &'static str, // TODO: Make 'expected' better
    },
    UnexpectedKeyword(&'a str),
    InvalidValType(&'a str),
    InvalidStringFormat(&'static str),
    MissingParen {
        paren: char,
        got: Option<Token<'a>>,
        what: &'a str,
    },
    InvalidOperand {
        insn: &'static str,
        msg: &'static str,
    },
    NumberMustBePositive(NumBase, &'a str),
    CannotParseNum {
        digits: &'a str,
        reason: &'static str,
        base: NumBase,
        sign: Sign,
    },
    InvalidAlignment(u32),
}

#[cfg_attr(test, derive(Debug))]
pub struct ParseError<'a> {
    kind: ParseErrorKind<'a>,
    offset: usize,
    source: &'a str,
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParseErrorKind::*;
        match &self.kind {
            LexError(err) => return err.fmt(f),
            UnexpectedToken { got, expected } => {
                write!(f, "expected {} but got {}", expected, got)?
            }
            UnexpectedEndOfFile { expected } => write!(f, "expected {} but reached EOF", expected)?,
            UnexpectedKeyword(kw) => write!(f, "unexpected keyword '{}'", kw)?,
            InvalidValType(ty) => write!(
                f,
                "value type must be one of 'i32', 'i64', 'f32', 'f64' but got '{}'",
                ty
            )?,
            InvalidStringFormat(reason) => {
                write!(f, "could not decode string literal: {}", reason)?
            }
            NumberMustBePositive(base, s) => {
                write!(f, "number must be positive but got -{}{}", base.prefix(), s)?
            }
            MissingParen {
                paren,
                got: Some(tok),
                what,
            } => write!(f, "expected paren '{}' for {} but got {}", paren, what, tok)?,
            MissingParen {
                paren,
                got: None,
                what,
            } => write!(f, "expected paren '{}' for {} but reached EOF", paren, what)?,
            InvalidOperand { insn, msg } => {
                write!(f, "invalid operand for '{}' instruction: {}", insn, msg)?
            }
            CannotParseNum {
                digits,
                reason,
                base,
                sign,
            } => {
                let base = if *base == NumBase::Hex { "0x" } else { "" };
                let sign = if *sign == Sign::Minus { "-" } else { "" };
                write!(
                    f,
                    "cannot parse integer {}{}{}: {}",
                    sign, base, digits, reason
                )?
            }
            InvalidAlignment(align) => {
                write!(f, "alignment value must be 2^N but got {:x}", align)?
            }
        };

        let start = self.offset;
        let end = self.source[start..]
            .find(['\n', '\r'].as_ref())
            .unwrap_or_else(|| self.source.len());
        write!(
            f,
            " at byte offset {}\n\n ... {}\n     ^\n     start from here",
            self.offset,
            &self.source[start..end],
        )
    }
}

impl<'a> From<Box<LexError<'a>>> for Box<ParseError<'a>> {
    fn from(err: Box<LexError<'a>>) -> Box<ParseError<'a>> {
        Box::new(ParseError {
            source: err.source(),
            offset: err.offset(),
            kind: ParseErrorKind::LexError(*err),
        })
    }
}

type Result<'a, T> = ::std::result::Result<T, Box<ParseError<'a>>>;

// iter::Peekable is not sufficient to parse WAT tokens
// WAT requires LL(1) parser to see a token after '('
struct LookAhead<I: Iterator> {
    it: I,
    current: Option<I::Item>,
    incoming: Option<I::Item>,
}

impl<I: Iterator> LookAhead<I> {
    fn new(mut it: I) -> Self {
        let current = it.next();
        let incoming = it.next();
        LookAhead {
            it,
            current,
            incoming,
        }
    }
    fn peek(&self) -> Option<&I::Item> {
        self.current.as_ref()
    }
    fn lookahead(&self) -> Option<&I::Item> {
        self.incoming.as_ref()
    }
}

impl<I: Iterator> Iterator for LookAhead<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        // This implementation can be better. We can delay calling self.it.next() until it is really
        // necessary. In the case, peek() and lookahead() will be `&mut self` method.
        mem::replace(
            &mut self.current,
            mem::replace(&mut self.incoming, self.it.next()),
        )
    }
}

// TODO: Add index-to-id tables for types, funcs, tables, mems, globals, locals and labels
// https://webassembly.github.io/spec/core/text/modules.html#indices
pub struct Parser<'a> {
    source: &'a str,
    tokens: LookAhead<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            source,
            tokens: LookAhead::new(Lexer::new(source)),
        }
    }

    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn reset(&mut self) {
        self.tokens = LookAhead::new(Lexer::new(self.source));
    }

    pub fn is_done(&self) -> bool {
        self.tokens.peek().is_none()
    }

    fn parse<P: Parse<'a>>(&mut self) -> Result<'a, P> {
        Parse::<'a>::parse(self)
    }

    fn error<T>(&self, kind: ParseErrorKind<'a>, offset: usize) -> Result<'a, T> {
        Err(Box::new(ParseError {
            source: self.source,
            offset,
            kind,
        }))
    }

    fn unexpected_token<T>(
        &self,
        got: Token<'a>,
        expected: &'static str,
        offset: usize,
    ) -> Result<'a, T> {
        self.error(ParseErrorKind::UnexpectedToken { got, expected }, offset)
    }

    fn unexpected_eof<T>(&self, expected: &'static str) -> Result<'a, T> {
        self.error(
            ParseErrorKind::UnexpectedEndOfFile { expected },
            self.source().len(),
        )
    }

    fn invalid_utf8_char<T>(&self, offset: usize) -> Result<'a, T> {
        self.error(
            ParseErrorKind::InvalidStringFormat(
                "UTF-8 character must be in format u{hexnum} like u{308f} in range of hexnum < 0xd800 or 0xe0000 <= hexnum < 0x110000",
            ),
            offset,
        )
    }

    fn cannot_parse_num<T>(
        &self,
        reason: &'static str,
        digits: &'a str,
        base: NumBase,
        sign: Sign,
        offset: usize,
    ) -> Result<'a, T> {
        self.error(
            ParseErrorKind::CannotParseNum {
                digits,
                reason,
                base,
                sign,
            },
            offset,
        )
    }

    fn maybe_eof<'p>(
        &'p self,
        lexed: Option<&'p <Lexer<'a> as Iterator>::Item>,
        expected: &'static str,
    ) -> Result<'a, (&Token<'a>, usize)> {
        match lexed {
            Some(Ok((tok, offset))) => Ok((tok, *offset)),
            Some(Err(err)) => Err(err.clone().into()),
            None => self.unexpected_eof(expected),
        }
    }

    fn lookahead(&self, expected: &'static str) -> Result<'a, (&Token<'a>, usize)> {
        self.maybe_eof(self.tokens.lookahead(), expected)
    }

    fn peek(&self, expected: &'static str) -> Result<'a, (&Token<'a>, usize)> {
        self.maybe_eof(self.tokens.peek(), expected)
    }

    fn next_token(&mut self, expected: &'static str) -> Result<'a, (Token<'a>, usize)> {
        match self.tokens.next() {
            Some(lexed) => Ok(lexed?),
            None => self.unexpected_eof(expected),
        }
    }

    fn eat_token(&mut self) -> bool {
        self.tokens.next().is_some()
    }

    fn lookahead_keyword(&self, expected: &'static str) -> Result<'a, (&'a str, usize)> {
        match self.lookahead(expected)? {
            (Token::Keyword(kw), offset) => Ok((*kw, offset)),
            (tok, offset) => self.unexpected_token(tok.clone(), expected, offset),
        }
    }

    fn maybe_ident(&mut self, expected: &'static str) -> Result<'a, Option<&'a str>> {
        Ok(match self.peek(expected)? {
            (Token::Ident(id), _) => {
                let id = *id;
                self.eat_token();
                Some(id)
            }
            _ => None,
        })
    }

    fn missing_paren<T>(
        &mut self,
        paren: char,
        got: Option<Token<'a>>,
        what: &'static str,
        offset: usize,
    ) -> Result<'a, T> {
        self.error(ParseErrorKind::MissingParen { paren, got, what }, offset)
    }

    fn opening_paren(&mut self, what: &'static str) -> Result<'a, usize> {
        if let Some(lexed) = self.tokens.next() {
            match lexed? {
                (Token::LParen, offset) => Ok(offset),
                (tok, offset) => self.missing_paren('(', Some(tok), what, offset),
            }
        } else {
            self.missing_paren('(', None, what, self.source.len())
        }
    }

    fn closing_paren(&mut self, what: &'static str) -> Result<'a, usize> {
        if let Some(lexed) = self.tokens.next() {
            match lexed? {
                (Token::RParen, offset) => Ok(offset),
                (tok, offset) => self.missing_paren(')', Some(tok), what, offset),
            }
        } else {
            self.missing_paren(')', None, what, self.source.len())
        }
    }

    fn parse_u32(&mut self, expected: &'static str) -> Result<'a, u32> {
        match self.next_token(expected)? {
            (Token::Int(sign, base, s), offset) if sign == Sign::Minus => {
                self.error(ParseErrorKind::NumberMustBePositive(base, s), offset)
            }
            (Token::Int(_, base, s), offset) => parse_u32_str(self, s, base, Sign::Plus, offset),
            (tok, offset) => self.unexpected_token(tok.clone(), expected, offset),
        }
    }

    fn parse_multiple<P: Parse<'a>>(&mut self, expected: &'static str) -> Result<'a, Vec<P>> {
        let mut parsed = vec![];
        loop {
            match self.tokens.peek() {
                Some(Ok((Token::LParen, _))) => parsed.push(self.parse()?),
                Some(Ok(_)) => break,
                Some(Err(err)) => return Err(err.clone().into()),
                None => return self.missing_paren('(', None, expected, self.source.len()),
            }
        }
        Ok(parsed)
    }

    fn fresh_id(&mut self) -> &'a str {
        // For some syntax sugars, text format requires fresh IDs. They are unique IDs not conflicting
        // with existing IDs.
        // https://webassembly.github.io/spec/core/text/values.html#text-id-fresh
        //
        // This method returns 'new' unowned string so there should be a trick. For example, having
        // &'a Vec<String> in Parser and generate a new ID in the vector and return it's slice.
        unimplemented!()
    }
}

// TODO: Use trait rather than macros to avoid duplication of implementations
macro_rules! parse_integer_function {
    ($name:ident, $int:ty) => {
        fn $name<'a>(
            parser: &mut Parser<'a>,
            input: &'a str,
            base: NumBase,
            sign: Sign,
            offset: usize,
        ) -> Result<'a, $int> {
            let radix = base.radix();
            let mut ret: $int = 0;
            for c in input.chars() {
                if c == '_' {
                    // Skip delimiter
                    continue;
                }
                if let Some(d) = c.to_digit(radix) {
                    if let Some(added) = ret
                        .checked_mul(radix as $int)
                        .and_then(|i| i.checked_add(d as $int))
                    {
                        ret = added;
                    } else {
                        return parser.cannot_parse_num(
                            concat!("too big or small integer for ", stringify!($int)),
                            input,
                            base,
                            sign,
                            offset,
                        );
                    }
                } else {
                    return parser.cannot_parse_num(
                        "invalid digit for integer",
                        input,
                        base,
                        sign,
                        offset,
                    );
                }
            }
            Ok(ret)
        }
    };
}

parse_integer_function!(parse_u32_str, u32);
parse_integer_function!(parse_u64_str, u64);

macro_rules! parse_float_number_function {
    ($name:ident, $float:ty) => {
        fn $name<'a>(
            parser: &mut Parser<'a>,
            input: &'a str,
            base: NumBase,
            sign: Sign,
            offset: usize,
        ) -> Result<'a, $float> {
            let radix_u = base.radix();
            let radix = radix_u as $float;
            let mut chars = input.chars();

            let mut ret: $float = 0.0;
            while let Some(c) = chars.next() {
                if c == '_' {
                    // delimiter
                    continue;
                } else if c == '.' {
                    break;
                }
                if let Some(d) = c.to_digit(radix_u) {
                    ret = ret * radix + (d as $float);
                } else {
                    return parser.cannot_parse_num(
                        "invalid digit for float number",
                        input,
                        base,
                        sign,
                        offset,
                    );
                }
            }

            // After '.' if exists
            let mut step = radix;
            for c in chars {
                if c == '_' {
                    // delimiter
                    continue;
                }
                if let Some(d) = c.to_digit(radix_u) {
                    ret += (d as $float) / step;
                    step *= radix;
                } else {
                    return parser.cannot_parse_num(
                        "invalid digit for float number",
                        input,
                        base,
                        sign,
                        offset,
                    );
                }
            }

            Ok(ret)
        }
    };
}

parse_float_number_function!(parse_f32_str, f32);
parse_float_number_function!(parse_f64_str, f64);

macro_rules! match_token {
    ($parser:expr, $expected:expr, $pattern:pat => $ret:expr) => {
        match $parser.next_token($expected)? {
            ($pattern, offset) => ($ret, offset),
            (tok, offset) => {
                return $parser.unexpected_token(tok, $expected, offset);
            }
        }
    };
    ($parser:expr, $expected:expr, $pattern:pat) => {
        match_token!($parser, $expected, $pattern => ())
    };
}

pub trait Parse<'a>: Sized {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self>;
}

// https://webassembly.github.io/spec/core/text/modules.html
impl<'a> Parse<'a> for SyntaxTree<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let mut module: Module<'a> = parser.parse()?;

        // The following restrictions are imposed on the composition of modules: `m1 ⊕ m2` is
        // defined if and only if
        //
        // - m1.start = ϵ ∨ m2.start = ϵ
        // - m1.funcs = m1.tables = m1.mems = m1.mems = ϵ ∨ m2.imports = ϵ
        while !parser.is_done() {
            let another: Module<'a> = parser.parse()?;
            // TODO: Check above restrictions
            for ty in another.types {
                module.types.push(ty);
                // TODO: Merge more fields
            }
        }

        Ok(SyntaxTree { module })
    }
}

// Helper enum to parse module fields
//
// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
#[cfg_attr(test, derive(Debug))]
enum ModuleField<'a> {
    Type(TypeDef<'a>),
    Import(Import<'a>),
    Export(Export<'a>),
    Func(FuncAbbrev<'a>),
}

// https://webassembly.github.io/spec/core/text/modules.html#text-module
impl<'a> Parse<'a> for Module<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        // https://webassembly.github.io/spec/core/text/modules.html#text-module
        let start = parser.opening_paren("module")?;
        match_token!(parser, "'module' keyword", Token::Keyword("module"));
        let (ident, _) = match_token!(parser, "identifier for module", Token::Ident(id) => id);

        let mut types = vec![];
        let mut imports = vec![];
        let mut exports = vec![];
        let mut funcs = vec![];

        while let (Token::LParen, _) = parser.peek("opening paren for starting module field")? {
            match parser.parse()? {
                ModuleField::Type(ty) => types.push(ty),
                ModuleField::Import(import) => imports.push(import),
                ModuleField::Export(export) => exports.push(export),
                ModuleField::Func(FuncAbbrev::Import(import)) => imports.push(import),
                ModuleField::Func(FuncAbbrev::Export(export, func)) => {
                    exports.push(export);
                    funcs.push(func);
                }
                ModuleField::Func(FuncAbbrev::NoAbbrev(func)) => funcs.push(func),
                // TODO: Add more fields
            }
        }

        parser.closing_paren("module")?;
        Ok(Module {
            start,
            ident,
            types,
            imports,
            exports,
            funcs,
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
impl<'a> Parse<'a> for ModuleField<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let (keyword, offset) = parser.lookahead_keyword("keyword for module field")?;
        match keyword {
            "type" => Ok(ModuleField::Type(parser.parse()?)),
            "import" => Ok(ModuleField::Import(parser.parse()?)),
            "export" => Ok(ModuleField::Export(parser.parse()?)),
            "func" => Ok(ModuleField::Func(parser.parse()?)),
            // TODO: Add more fields
            kw => parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-typedef
impl<'a> Parse<'a> for TypeDef<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("type")?;
        match_token!(parser, "'type' keyword", Token::Keyword("type"));
        let id = parser.maybe_ident("identifier for type")?;
        let ty = parser.parse()?;
        parser.closing_paren("type")?;
        Ok(TypeDef { start, id, ty })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-functype
impl<'a> Parse<'a> for FuncType<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("function type")?;
        match_token!(parser, "'func' keyword", Token::Keyword("func"));

        let mut params = vec![];
        loop {
            match parser.peek("opening paren for param in functype")? {
                (Token::LParen, _)
                    if parser.lookahead_keyword("param keyword in functype")?.0 == "param" =>
                {
                    params.push(parser.parse()?);
                }
                _ => break,
            }
        }

        let results = parser.parse_multiple("result in function type")?;
        parser.closing_paren("function type")?;
        Ok(FuncType {
            start,
            params,
            results,
        })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-param
impl<'a> Parse<'a> for Param<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("parameter")?;
        match_token!(parser, "'param' keyword", Token::Keyword("param"));
        let id = parser.maybe_ident("identifier for param")?;
        let ty = parser.parse()?;
        parser.closing_paren("parameter")?;
        Ok(Param { start, id, ty })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-valtype
impl<'a> Parse<'a> for ValType {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let expected = "keyword for value type";
        match parser.next_token(expected)? {
            (Token::Keyword("i32"), _) => Ok(ValType::I32),
            (Token::Keyword("i64"), _) => Ok(ValType::I64),
            (Token::Keyword("f32"), _) => Ok(ValType::F32),
            (Token::Keyword("f64"), _) => Ok(ValType::F64),
            (Token::Keyword(id), offset) => {
                parser.error(ParseErrorKind::InvalidValType(id), offset)
            }
            (tok, offset) => parser.unexpected_token(tok, expected, offset),
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-result
impl<'a> Parse<'a> for FuncResult {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("function result")?;
        match_token!(parser, "'result' keyword", Token::Keyword("result"));
        let ty = parser.parse()?;
        parser.closing_paren("function result")?;
        Ok(FuncResult { start, ty })
    }
}

// https://webassembly.github.io/spec/core/text/values.html#text-name
impl<'a> Parse<'a> for Name {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        fn invalid_char_escape<'a>(parser: &mut Parser<'a>, offset: usize) -> Result<'a, Name> {
            parser.error(
                ParseErrorKind::InvalidStringFormat(
                    r#"escape must be one of \t, \n, \r, \", \', \\, \u{hexnum}, \MN"#,
                ),
                offset,
            )
        }

        let (src, offset) = match_token!(parser, "string literal for name", Token::String(s) => s);

        // A name string must form a valid UTF-8 encoding as defined by Unicode (Section 2.5)
        let mut name = String::new();
        let mut chars = src.char_indices();
        while let Some((i, c)) = chars.next() {
            if c != '\\' {
                name.push(c);
            } else {
                // Note: Lexer guarantees that at least one char follows after '\'
                match chars.next().unwrap().1 {
                    't' => name.push('\t'),
                    'n' => name.push('\n'),
                    'r' => name.push('\r'),
                    '"' => name.push('"'),
                    '\'' => name.push('\''),
                    '\\' => name.push('\\'),
                    'u' => {
                        match chars.next() {
                            Some((i, '{')) => {
                                let start = i + 1; // next to '{'
                                let end = loop {
                                    match chars.next() {
                                        Some((i, '}')) => break i,
                                        Some(_) => continue,
                                        None => return parser.invalid_utf8_char(offset + i),
                                    }
                                };
                                if let Some(c) = u32::from_str_radix(&src[start..end], 16)
                                    .ok()
                                    .and_then(char::from_u32)
                                {
                                    name.push(c);
                                } else {
                                    return parser.invalid_utf8_char(offset + i);
                                }
                            }
                            _ => return parser.invalid_utf8_char(offset + i),
                        }
                    }
                    c => {
                        let hi = c.to_digit(16);
                        let lo = chars.next().and_then(|(_, c)| c.to_digit(16));
                        match (hi, lo) {
                            (Some(hi), Some(lo)) => match char::from_u32(hi * 16 + lo) {
                                Some(c) => name.push(c),
                                None => return invalid_char_escape(parser, offset + i),
                            },
                            _ => return invalid_char_escape(parser, offset + i),
                        }
                    }
                }
            }
        }
        Ok(Name(name))
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-import
impl<'a> Parse<'a> for Import<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("import")?;
        match_token!(parser, "'import' keyword", Token::Keyword("import"));
        let mod_name = parser.parse()?;
        let name = parser.parse()?;
        let desc = parser.parse()?;
        parser.closing_paren("import")?;
        Ok(Import {
            start,
            mod_name,
            name,
            desc,
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-importdesc
impl<'a> Parse<'a> for ImportDesc<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("import item")?;
        let (keyword, offset) = match_token!(parser, "one of 'func', 'table', 'memory', 'global'", Token::Keyword(kw) => kw);
        let id = parser.maybe_ident("identifier for import item")?;
        let desc = match keyword {
            "func" => ImportDesc::Func {
                start,
                id,
                ty: parser.parse()?,
            },
            "table" => ImportDesc::Table {
                start,
                id,
                ty: parser.parse()?,
            },
            "memory" => ImportDesc::Memory {
                start,
                id,
                ty: parser.parse()?,
            },
            "global" => ImportDesc::Global {
                start,
                id,
                ty: parser.parse()?,
            },
            kw => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
        };
        parser.closing_paren("import item")?;
        Ok(desc)
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#type-uses
impl<'a> Parse<'a> for TypeUse<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("type use")?;
        match_token!(parser, "'type' keyword for typeuse", Token::Keyword("type"));
        let idx = parser.parse()?;
        parser.closing_paren("type use")?;

        let mut params = vec![];
        loop {
            match parser.peek("opening paren for param in typeuse")? {
                (Token::LParen, _)
                    if parser.lookahead_keyword("param keyword in typeuse")?.0 == "param" =>
                {
                    params.push(parser.parse()?);
                }
                _ => break,
            }
        }

        let mut results = vec![];
        loop {
            match parser.peek("opening paren for result in typeuse")? {
                (Token::LParen, _)
                    if parser.lookahead_keyword("'result' keyword in typeuse")?.0 == "result" =>
                {
                    results.push(parser.parse()?);
                }
                _ => break,
            }
        }

        Ok(TypeUse {
            start,
            idx,
            params,
            results,
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#indices
impl<'a> Parse<'a> for Index<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let expected = "number or identifier for index";
        match parser.next_token(expected)? {
            (Token::Int(sign, base, s), offset) if sign == Sign::Minus => {
                parser.error(ParseErrorKind::NumberMustBePositive(base, s), offset)
            }
            (Token::Int(_, base, s), offset) => {
                parse_u32_str(parser, s, base, Sign::Plus, offset).map(Index::Num)
            }
            (Token::Ident(id), _) => Ok(Index::Ident(id)),
            (tok, offset) => parser.unexpected_token(tok.clone(), expected, offset),
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-tabletype
impl<'a> Parse<'a> for TableType {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let limit = parser.parse()?;
        match_token!(
            parser,
            "'funcref' keyword for table type",
            Token::Keyword("funcref")
        );
        Ok(TableType { limit })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-limits
impl<'a> Parse<'a> for Limits {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let min = parser.parse_u32("u32 for min table limit")?;
        Ok(match parser.peek("u32 for max table limit")? {
            (Token::Int(..), _) => {
                let max = parser.parse_u32("u32 for min table limit")?;
                Limits::Range { min, max }
            }
            _ => Limits::From { min },
        })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-memtype
impl<'a> Parse<'a> for MemType {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        parser.parse().map(|limit| MemType { limit })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-globaltype
impl<'a> Parse<'a> for GlobalType {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        match parser.peek("'(' for mut or value type of global type")? {
            (Token::LParen, _) => {
                parser.eat_token(); // eat '('
                match_token!(
                    parser,
                    "'mut' keyword for global type",
                    Token::Keyword("mut")
                );
                let ty = parser.parse()?;
                parser.closing_paren("mutable global type")?;
                Ok(GlobalType { mutable: true, ty })
            }
            _ => Ok(GlobalType {
                mutable: false,
                ty: parser.parse()?,
            }),
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-export
impl<'a> Parse<'a> for Export<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("export")?;
        match_token!(
            parser,
            "'export' keyword for export",
            Token::Keyword("export")
        );
        let name = parser.parse()?;

        parser.opening_paren("export item")?;
        let (keyword, offset) =
            match_token!(parser, "keyword for export item", Token::Keyword(kw) => kw);
        let kind = match keyword {
            "func" => ExportKind::Func,
            "table" => ExportKind::Table,
            "memory" => ExportKind::Memory,
            "global" => ExportKind::Global,
            _ => return parser.error(ParseErrorKind::UnexpectedKeyword(keyword), offset),
        };
        let idx = parser.parse()?;
        parser.closing_paren("export item")?;

        parser.closing_paren("export")?;

        Ok(Export {
            start,
            name,
            kind,
            idx,
        })
    }
}

// Helper enum to resolve import/export abbreviation in `func` syntax
// https://webassembly.github.io/spec/core/text/modules.html#text-func-abbrev
#[cfg_attr(test, derive(Debug))]
enum FuncAbbrev<'a> {
    Import(Import<'a>),
    Export(Export<'a>, Func<'a>),
    NoAbbrev(Func<'a>),
}

// https://webassembly.github.io/spec/core/text/modules.html#text-func
impl<'a> Parse<'a> for FuncAbbrev<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let start = parser.opening_paren("func")?;
        match_token!(
            parser,
            "'func' keyword for function",
            Token::Keyword("func")
        );

        let id = parser.maybe_ident("identifier for function")?;
        let (keyword, offset) =
            parser.lookahead_keyword("one of 'import', 'export', 'type' for function")?;
        let abbrev = match keyword {
            "import" => {
                // `(func $id (import "m" "n") {typeuse})` is a syntax sugar of
                // `(import "m" "n" (func $id {typeuse}))`
                parser.opening_paren("import in func")?;
                parser.eat_token(); // Eat 'import' keyword
                let mod_name = parser.parse()?;
                let name = parser.parse()?;
                parser.closing_paren("import in func")?;
                let ty = parser.parse()?;
                let desc = ImportDesc::Func { start, id, ty };
                FuncAbbrev::Import(Import {
                    start,
                    mod_name,
                    name,
                    desc,
                })
            }
            "export" => {
                // `(func $id (export "n") {typeuse})` is a syntax sugar of
                // `(export "n" (func $id)) (func $id {typeuse})`
                let export_start = parser.opening_paren("export in func")?;
                parser.eat_token(); // Eat 'export' keyword
                let name = parser.parse()?;
                parser.closing_paren("export in func")?;
                let ty = parser.parse()?;
                let locals = parser.parse()?;
                let body = parser.parse()?;
                let id = id.unwrap_or_else(|| parser.fresh_id());
                FuncAbbrev::Export(
                    Export {
                        start: export_start,
                        name,
                        kind: ExportKind::Func,
                        idx: Index::Ident(id),
                    },
                    Func {
                        start,
                        ty,
                        locals,
                        body,
                    },
                )
            }
            "type" => {
                let ty = parser.parse()?;
                let locals = parser.parse()?;
                let body = parser.parse()?;
                FuncAbbrev::NoAbbrev(Func {
                    start,
                    ty,
                    locals,
                    body,
                })
            }
            kw => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
        };

        parser.closing_paren("func")?;
        Ok(abbrev)
    }
}

// Implement Parse for Vec due to abbreviation
// https://webassembly.github.io/spec/core/text/modules.html#text-func-abbrev
//
// https://webassembly.github.io/spec/core/text/modules.html#text-local
impl<'a> Parse<'a> for Vec<Local<'a>> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let mut locals = vec![];
        while let (Token::LParen, start) = parser.peek("'(' for local")? {
            if parser.lookahead_keyword("'local' keyword for locals")?.0 != "local" {
                break;
            }

            parser.eat_token(); // Eat '(' keyword
            parser.eat_token(); // Eat 'local' keyword

            if let Some(id) = parser.maybe_ident("identifier for local")? {
                let ty = parser.parse()?;
                parser.closing_paren("local")?;
                locals.push(Local {
                    start,
                    id: Some(id),
                    ty,
                });
                continue;
            }

            // Only when ID is omitted, multiple types can be combined into one local statement
            // `(local ty*)` means `(local ty)*`. Here parse `i64 f32` of `(local i32 i64 f32)`
            loop {
                match parser.peek("')' or typeuse for local")? {
                    (Token::RParen, _) => break,
                    _ => locals.push(Local {
                        start,
                        id: None,
                        ty: parser.parse()?,
                    }),
                }
            }

            parser.closing_paren("local")?;
        }
        Ok(locals)
    }
}

impl<'a> Parse<'a> for Mem {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        fn parse_u32<'a>(s: &'a str, parser: &mut Parser<'a>, offset: usize) -> Result<'a, u32> {
            let (base, s) = if s.starts_with("0x") {
                (NumBase::Hex, &s[2..])
            } else {
                (NumBase::Dec, s)
            };
            parse_u32_str(parser, s, base, Sign::Plus, offset)
        }

        let offset = match parser.peek("'offset' keyword for memory instruction")? {
            (Token::Keyword(kw), offset) if kw.starts_with("offset=") => {
                let u = parse_u32(&kw[7..], parser, offset)?;
                parser.eat_token(); // Eat 'offset' keyword
                Some(u)
            }
            _ => None,
        };

        let align = match parser.peek("'align' keyword for memory instruction")? {
            (Token::Keyword(kw), offset) if kw.starts_with("align=") => {
                let u = parse_u32(&kw[6..], parser, offset)?;
                parser.eat_token(); // Eat 'align' keyword
                if u.count_ones() != 1 {
                    return parser.error(ParseErrorKind::InvalidAlignment(u), offset);
                }
                Some(u)
            }
            _ => None,
        };

        Ok(Mem { offset, align })
    }
}

// Instructions has special abbreviation. It can be folded and nested. This struct parses sequence
// of instructions with the abbreviation.
//
//   1: ({plaininstr} {foldedinstr}*) == {foldedinstr}* {plaininstr}
//   2: (block {label} {resulttype} {instr}*) == block {label} {resulttype} {instr}* end
//   3: (loop {label} {resulttype} {instr}*) == 'loop' {label} {resulttype} {instr}* end
//   4: (if {label} {resulttype} {foldedinstr}* (then {instr}*) (else {instr}*)?) ==
//          {foldedinstr}* if {label} {resulttype} {instr}* else {instr}* end
//
// e.g.
//   (local.get $x) (i32.const 2) i32.add (i32.const 3) i32.mul ==
//       (i32.mul (i32.add (local.get $x) (i32.const 2)) (i32.const 3))
//
// https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions
struct MaybeFoldedInsn<'a, 'p> {
    insns: Vec<Instruction<'a>>,
    parser: &'p mut Parser<'a>,
}

// Note: These are free functions not to modify `insns` field of MaybeFoldedInsn.

fn parse_maybe_result_type<'a>(parser: &mut Parser<'a>) -> Result<'a, Option<ValType>> {
    // Note: This requires that next token exists
    match parser.peek("'(' for result type")? {
        (Token::LParen, _)
            if parser.lookahead_keyword("'result' keyword in block")?.0 == "result" =>
        {
            parser.eat_token(); // Eat '('
            parser.eat_token(); // Eat 'result'
            let ty = parser.parse()?;
            parser.closing_paren("result in block")?;
            Ok(Some(ty))
        }
        _ => Ok(None),
    }
}

fn parse_one_insn<'a>(parser: &mut Parser<'a>, end: bool) -> Result<'a, Instruction<'a>> {
    match parser.next_token("instruction keyword")? {
        (Token::Keyword(kw), start) => {
            let kind = match kw {
                // Control instructions
                // https://webassembly.github.io/spec/core/text/instructions.html#control-instructions
                "block" | "loop" => {
                    let label = parser.maybe_ident("label for block or loop")?;
                    let ty = parse_maybe_result_type(parser)?;
                    let body = parser.parse()?;
                    let id = if end {
                        match_token!(
                            parser,
                            "'end' keyword for block or loop",
                            Token::Keyword("end")
                        );
                        parser.maybe_ident("ID for block or loop")?
                    } else {
                        None
                    };
                    if kw == "block" {
                        InsnKind::Block {
                            label,
                            ty,
                            body,
                            id,
                        }
                    } else {
                        InsnKind::Loop {
                            label,
                            ty,
                            body,
                            id,
                        }
                    }
                }
                "if" => {
                    // Note: 'else' can be omitted when else clause is empty
                    // https://webassembly.github.io/spec/core/text/instructions.html#abbreviations
                    let is_folded = !end;
                    let label = parser.maybe_ident("label for block or loop")?;
                    let ty = parse_maybe_result_type(parser)?;
                    if is_folded {
                        parser.opening_paren("'then' clause in folded 'if'")?;
                        match_token!(
                            parser,
                            "'then' keyword for folded 'if'",
                            Token::Keyword("then")
                        );
                    }
                    let then_body = parser.parse()?;
                    if is_folded {
                        parser.closing_paren("then clause in folded 'if'")?;
                    }
                    let (else_body, else_id) =
                        match parser.peek("'else', 'end', '(' or ')' in 'if'")? {
                            (Token::Keyword("end"), _) if end && !is_folded => (vec![], None),
                            (Token::RParen, _) if !end => (vec![], None),
                            (Token::LParen, _) if is_folded => {
                                parser.eat_token(); // Eat '('
                                match_token!(
                                    parser,
                                    "'else' keyword in else clause of folded 'if'",
                                    Token::Keyword("else")
                                );
                                let body = parser.parse()?;
                                parser.closing_paren("else clause in folded 'if'")?;
                                (body, None)
                            }
                            (Token::Keyword("else"), _) if !is_folded => {
                                parser.eat_token(); // Eat 'else'
                                let id = parser.maybe_ident("ID for 'else' in 'if'")?;
                                let body = parser.parse()?;
                                (body, id)
                            }
                            (tok, offset) => {
                                return parser.unexpected_token(
                                    tok.clone(),
                                    "'else' or 'end' in 'if'",
                                    offset,
                                );
                            }
                        };
                    let end_id = if end {
                        match_token!(parser, "'end' keyword for 'if'", Token::Keyword("end"));
                        parser.maybe_ident("ID for end of 'if'")?
                    } else {
                        None
                    };
                    InsnKind::If {
                        label,
                        ty,
                        then_body,
                        else_id,
                        else_body,
                        end_id,
                    }
                }
                "unreachable" => InsnKind::Unreachable,
                "nop" => InsnKind::Nop,
                "br" => InsnKind::Br(parser.parse()?),
                "br_if" => InsnKind::BrIf(parser.parse()?),
                "br_table" => {
                    let mut labels = vec![];
                    while let (Token::Int(..), _) | (Token::Ident(_), _) =
                        parser.peek("labels for 'br_table'")?
                    {
                        labels.push(parser.parse()?);
                    }
                    if let Some(default_label) = labels.pop() {
                        InsnKind::BrTable {
                            labels,
                            default_label,
                        }
                    } else {
                        return parser.error(
                            ParseErrorKind::InvalidOperand {
                                insn: "br_table",
                                msg: "at least one label is necessary",
                            },
                            start,
                        );
                    }
                }
                "return" => InsnKind::Return,
                "call" => InsnKind::Call(parser.parse()?),
                "call_indirect" => InsnKind::CallIndirect(parser.parse()?),
                // Parametric instructions
                // https://webassembly.github.io/spec/core/text/instructions.html#parametric-instructions
                "drop" => InsnKind::Drop,
                "select" => InsnKind::Select,
                // Variable instructions
                // https://webassembly.github.io/spec/core/text/instructions.html#variable-instructions
                "local.get" => InsnKind::LocalGet(parser.parse()?),
                "local.set" => InsnKind::LocalSet(parser.parse()?),
                "local.tee" => InsnKind::LocalTee(parser.parse()?),
                "global.get" => InsnKind::GlobalGet(parser.parse()?),
                "global.set" => InsnKind::GlobalSet(parser.parse()?),
                // Memory instructions
                // https://webassembly.github.io/spec/core/text/instructions.html#memory-instructions
                "i32.load" => InsnKind::I32Load(parser.parse()?),
                "i64.load" => InsnKind::I64Load(parser.parse()?),
                "f32.load" => InsnKind::F32Load(parser.parse()?),
                "f64.load" => InsnKind::F64Load(parser.parse()?),
                "i32.load8_s" => InsnKind::I32Load8S(parser.parse()?),
                "i32.load8_u" => InsnKind::I32Load8U(parser.parse()?),
                "i32.load16_s" => InsnKind::I32Load16S(parser.parse()?),
                "i32.load16_u" => InsnKind::I32Load16U(parser.parse()?),
                "i64.load8_s" => InsnKind::I64Load8S(parser.parse()?),
                "i64.load8_u" => InsnKind::I64Load8U(parser.parse()?),
                "i64.load16_s" => InsnKind::I64Load16S(parser.parse()?),
                "i64.load16_u" => InsnKind::I64Load16U(parser.parse()?),
                "i64.load32_s" => InsnKind::I64Load32S(parser.parse()?),
                "i64.load32_u" => InsnKind::I64Load32U(parser.parse()?),
                "i32.store" => InsnKind::I32Store(parser.parse()?),
                "i64.store" => InsnKind::I64Store(parser.parse()?),
                "f32.store" => InsnKind::F32Store(parser.parse()?),
                "f64.store" => InsnKind::F64Store(parser.parse()?),
                "i32.store8" => InsnKind::I32Store8(parser.parse()?),
                "i32.store16" => InsnKind::I32Store16(parser.parse()?),
                "i64.store8" => InsnKind::I64Store8(parser.parse()?),
                "i64.store16" => InsnKind::I64Store16(parser.parse()?),
                "i64.store32" => InsnKind::I64Store32(parser.parse()?),
                "memory.size" => InsnKind::MemorySize,
                "memory.grow" => InsnKind::MemoryGrow,
                // Numeric instructions
                // https://webassembly.github.io/spec/core/text/instructions.html#numeric-instructions
                // Constants
                "i32.const" => {
                    let ((sign, base, digits), offset) = match_token!(parser, "integer for i32.const operand", Token::Int(s, b, d) => (s, b, d));
                    let u = parse_u32_str(parser, digits, base, sign, offset)?;
                    if u == 0x8000_0000 && sign == Sign::Minus {
                        // In this case `u as i32` causes overflow
                        InsnKind::I32Const(i32::min_value())
                    } else if u < 0x8000_0000 {
                        InsnKind::I32Const(sign.apply(u as i32))
                    } else {
                        return parser.cannot_parse_num(
                            "too large or small integer for i32",
                            digits,
                            base,
                            sign,
                            offset,
                        );
                    }
                }
                "i64.const" => {
                    let ((sign, base, digits), offset) = match_token!(parser, "integer for i64.const operand", Token::Int(s, b, d) => (s, b, d));
                    let u = parse_u64_str(parser, digits, base, sign, offset)?;
                    if u == 0x8000_0000_0000_0000 && sign == Sign::Minus {
                        InsnKind::I64Const(i64::min_value())
                    } else if u < 0x8000_0000_0000_0000 {
                        InsnKind::I64Const(sign.apply(u as i64))
                    } else {
                        return parser.cannot_parse_num(
                            "too large or small integer for i64",
                            digits,
                            base,
                            sign,
                            offset,
                        );
                    }
                }
                "f32.const" => {
                    let ((sign, float), offset) = match_token!(parser, "float number for f32.const", Token::Float(s, f) => (s, f));
                    let val = match float {
                        Float::Inf => match sign {
                            Sign::Plus => f32::INFINITY,
                            Sign::Minus => f32::NEG_INFINITY,
                        },
                        Float::Nan(None) => sign.apply(f32::NAN),
                        Float::Nan(Some(payload)) => {
                            // Encode  f32 NaN value via u32 assuming IEEE-754 format for NaN boxing.
                            // Palyload must be
                            //   - within 23bits (fraction of f32 is 23bits)
                            //   - >= 2^(23-1) meant that most significant bit must be 1 (since frac cannot be zero for NaN value)
                            // https://webassembly.github.io/spec/core/syntax/values.html#floating-point
                            let payload_u =
                                parse_u32_str(parser, payload, NumBase::Hex, Sign::Plus, offset)?;
                            if payload_u < 0x40_0000 || 0x80_0000 <= payload_u {
                                return parser.cannot_parse_num(
                                    "payload of NaN for f32 must be in range of 2^22 <= payload < 2^23",
                                    payload,
                                    NumBase::Hex,
                                    Sign::Plus,
                                    offset,
                                );
                            }
                            // NaN boxing. 2^22 <= payload_u < 2^23 and floating point number is in IEEE754 format.
                            // This will encode the payload into fraction of NaN.
                            //   0x{sign}11111111{payload}
                            let sign = match sign {
                                Sign::Plus => 0,
                                Sign::Minus => 1u32 << 31, // most significant bit is 1 for negative number
                            };
                            let exp = 0b1111_1111u32 << (31 - 8);
                            f32::from_bits(sign | exp | payload_u)
                        }
                        Float::Val { base, frac, exp } => {
                            // Note: Better algorithm should be considered
                            // https://github.com/rust-lang/rust/blob/3982d3514efbb65b3efac6bb006b3fa496d16663/src/libcore/num/dec2flt/algorithm.rs
                            let mut frac =
                                sign.apply(parse_f32_str(parser, frac, base, sign, offset)?);
                            // In IEEE754, exp part is actually 8bits
                            if let Some((exp_sign, exp)) = exp {
                                let exp =
                                    parse_u32_str(parser, exp, NumBase::Dec, exp_sign, offset)?;
                                let step = match base {
                                    NumBase::Hex => 2.0,
                                    NumBase::Dec => 10.0,
                                };
                                // powi is not available because an error gets larger
                                match exp_sign {
                                    Sign::Plus => {
                                        for _ in 0..exp {
                                            frac *= step;
                                        }
                                    }
                                    Sign::Minus => {
                                        for _ in 0..exp {
                                            frac /= step;
                                        }
                                    }
                                }
                                frac
                            } else {
                                frac
                            }
                        }
                    };
                    InsnKind::F32Const(val)
                }
                "f64.const" => {
                    let ((sign, float), offset) = match_token!(parser, "float number for f64.const", Token::Float(s, f) => (s, f));
                    let val = match float {
                        Float::Inf => match sign {
                            Sign::Plus => f64::INFINITY,
                            Sign::Minus => f64::NEG_INFINITY,
                        },
                        Float::Nan(None) => sign.apply(f64::NAN),
                        Float::Nan(Some(payload)) => {
                            // Encode f64 NaN value via u64 assuming IEEE-754 format for NaN boxing.
                            // Palyload must be
                            //   - within 52bits (since fraction of f64 is 52bits)
                            //   - >= 2^(52-1) meant that most significant bit must be 1 (since frac cannot be zero for NaN value)
                            // https://webassembly.github.io/spec/core/syntax/values.html#floating-point
                            let payload_u =
                                parse_u64_str(parser, payload, NumBase::Hex, Sign::Plus, offset)?;
                            if payload_u < 0x8_0000_0000_0000 || 0x10_0000_0000_0000 <= payload_u {
                                return parser.cannot_parse_num(
                                    "payload of NaN for f64 must be in range of 2^51 <= payload < 2^52",
                                    payload,
                                    NumBase::Hex,
                                    Sign::Plus,
                                    offset,
                                );
                            }
                            // NaN boxing. 2^51 <= payload_u < 2^52 and floating point number is in IEEE754 format.
                            // This will encode the payload into fraction of NaN.
                            //   0x{sign}11111111111{payload}
                            let sign = match sign {
                                Sign::Plus => 0,
                                Sign::Minus => 1u64 << 63, // most significant bit is 1 for negative number
                            };
                            let exp = 0b111_1111_1111u64 << (63 - 11);
                            f64::from_bits(sign | exp | payload_u)
                        }
                        Float::Val { base, frac, exp } => {
                            let mut frac =
                                sign.apply(parse_f64_str(parser, frac, base, sign, offset)?);
                            // In IEEE754, exp part is actually 11bits
                            if let Some((exp_sign, exp)) = exp {
                                let exp = parse_u32_str(parser, exp, base, exp_sign, offset)?;
                                let step = match base {
                                    NumBase::Hex => 2.0,
                                    NumBase::Dec => 10.0,
                                };
                                // powi is not available because an error gets larger
                                match exp_sign {
                                    Sign::Plus => {
                                        for _ in 0..exp {
                                            frac *= step;
                                        }
                                    }
                                    Sign::Minus => {
                                        for _ in 0..exp {
                                            frac /= step;
                                        }
                                    }
                                }
                                frac
                            } else {
                                frac
                            }
                        }
                    };
                    InsnKind::F64Const(val)
                }
                "i32.clz" => InsnKind::I32Clz,
                "i32.ctz" => InsnKind::I32Ctz,
                "i32.popcnt" => InsnKind::I32Popcnt,
                "i32.add" => InsnKind::I32Add,
                "i32.sub" => InsnKind::I32Sub,
                "i32.mul" => InsnKind::I32Mul,
                "i32.div_s" => InsnKind::I32DivS,
                "i32.div_u" => InsnKind::I32DivU,
                "i32.rem_s" => InsnKind::I32RemS,
                "i32.rem_u" => InsnKind::I32RemU,
                "i32.and" => InsnKind::I32And,
                "i32.or" => InsnKind::I32Or,
                "i32.xor" => InsnKind::I32Xor,
                "i32.shl" => InsnKind::I32Shl,
                "i32.shr_s" => InsnKind::I32ShrS,
                "i32.shr_u" => InsnKind::I32ShrU,
                "i32.rotl" => InsnKind::I32Rotl,
                "i32.rotr" => InsnKind::I32Rotr,
                "i64.clz" => InsnKind::I64Clz,
                "i64.ctz" => InsnKind::I64Ctz,
                "i64.popcnt" => InsnKind::I64Popcnt,
                "i64.add" => InsnKind::I64Add,
                "i64.sub" => InsnKind::I64Sub,
                "i64.mul" => InsnKind::I64Mul,
                "i64.div_s" => InsnKind::I64DivS,
                "i64.div_u" => InsnKind::I64DivU,
                "i64.rem_s" => InsnKind::I64RemS,
                "i64.rem_u" => InsnKind::I64RemU,
                "i64.and" => InsnKind::I64And,
                "i64.or" => InsnKind::I64Or,
                "i64.xor" => InsnKind::I64Xor,
                "i64.shl" => InsnKind::I64Shl,
                "i64.shr_s" => InsnKind::I64ShrS,
                "i64.shr_u" => InsnKind::I64ShrU,
                "i64.rotl" => InsnKind::I64Rotl,
                "i64.rotr" => InsnKind::I64Rotr,
                "f32.abs" => InsnKind::F32Abs,
                "f32.neg" => InsnKind::F32Neg,
                "f32.ceil" => InsnKind::F32Ceil,
                "f32.floor" => InsnKind::F32Floor,
                "f32.trunc" => InsnKind::F32Trunc,
                "f32.nearest" => InsnKind::F32Nearest,
                "f32.sqrt" => InsnKind::F32Sqrt,
                "f32.add" => InsnKind::F32Add,
                "f32.sub" => InsnKind::F32Sub,
                "f32.mul" => InsnKind::F32Mul,
                "f32.div" => InsnKind::F32Div,
                "f32.min" => InsnKind::F32Min,
                "f32.max" => InsnKind::F32Max,
                "f32.copysign" => InsnKind::F32Copysign,
                "f64.abs" => InsnKind::F64Abs,
                "f64.neg" => InsnKind::F64Neg,
                "f64.ceil" => InsnKind::F64Ceil,
                "f64.floor" => InsnKind::F64Floor,
                "f64.trunc" => InsnKind::F64Trunc,
                "f64.nearest" => InsnKind::F64Nearest,
                "f64.sqrt" => InsnKind::F64Sqrt,
                "f64.add" => InsnKind::F64Add,
                "f64.sub" => InsnKind::F64Sub,
                "f64.mul" => InsnKind::F64Mul,
                "f64.div" => InsnKind::F64Div,
                "f64.min" => InsnKind::F64Min,
                "f64.max" => InsnKind::F64Max,
                "f64.copysign" => InsnKind::F64Copysign,
                "i32.eqz" => InsnKind::I32Eqz,
                "i32.eq" => InsnKind::I32Eq,
                "i32.ne" => InsnKind::I32Ne,
                "i32.lt_s" => InsnKind::I32LtS,
                "i32.lt_u" => InsnKind::I32LtU,
                "i32.gt_s" => InsnKind::I32GtS,
                "i32.gt_u" => InsnKind::I32GtU,
                "i32.le_s" => InsnKind::I32LeS,
                "i32.le_u" => InsnKind::I32LeU,
                "i32.ge_s" => InsnKind::I32GeS,
                "i32.ge_u" => InsnKind::I32GeU,
                "i64.eqz" => InsnKind::I64Eqz,
                "i64.eq" => InsnKind::I64Eq,
                "i64.ne" => InsnKind::I64Ne,
                "i64.lt_s" => InsnKind::I64LtS,
                "i64.lt_u" => InsnKind::I64LtU,
                "i64.gt_s" => InsnKind::I64GtS,
                "i64.gt_u" => InsnKind::I64GtU,
                "i64.le_s" => InsnKind::I64LeS,
                "i64.le_u" => InsnKind::I64LeU,
                "i64.ge_s" => InsnKind::I64GeS,
                "i64.ge_u" => InsnKind::I64GeU,
                "f32.eq" => InsnKind::F32Eq,
                "f32.ne" => InsnKind::F32Ne,
                "f32.lt" => InsnKind::F32Lt,
                "f32.gt" => InsnKind::F32Gt,
                "f32.le" => InsnKind::F32Le,
                "f32.ge" => InsnKind::F32Ge,
                "f64.eq" => InsnKind::F64Eq,
                "f64.ne" => InsnKind::F64Ne,
                "f64.lt" => InsnKind::F64Lt,
                "f64.gt" => InsnKind::F64Gt,
                "f64.le" => InsnKind::F64Le,
                "f64.ge" => InsnKind::F64Ge,
                "i32.wrap_i64" => InsnKind::I32WrapI64,
                "i32.trunc_f32_s" => InsnKind::I32TruncF32S,
                "i32.trunc_f32_u" => InsnKind::I32TruncF32U,
                "i32.trunc_f64_s" => InsnKind::I32TruncF64S,
                "i32.trunc_f64_u" => InsnKind::I32TruncF64U,
                "i64.extend_i32_s" => InsnKind::I64ExtendI32S,
                "i64.extend_i32_u" => InsnKind::I64ExtendI32U,
                "i64.trunc_f32_s" => InsnKind::I64TruncF32S,
                "i64.trunc_f32_u" => InsnKind::I64TruncF32U,
                "i64.trunc_f64_s" => InsnKind::I64TruncF64S,
                "i64.trunc_f64_u" => InsnKind::I64TruncF64U,
                "f32.convert_i32_s" => InsnKind::F32ConvertI32S,
                "f32.convert_i32_u" => InsnKind::F32ConvertI32U,
                "f32.convert_i64_s" => InsnKind::F32ConvertI64S,
                "f32.convert_i64_u" => InsnKind::F32ConvertI64U,
                "f32.demote_f64" => InsnKind::F32DemoteF64,
                "f64.convert_i32_s" => InsnKind::F64ConvertI32S,
                "f64.convert_i32_u" => InsnKind::F64ConvertI32U,
                "f64.convert_i64_s" => InsnKind::F64ConvertI64S,
                "f64.convert_i64_u" => InsnKind::F64ConvertI64U,
                "f64.promote_f32" => InsnKind::F64PromoteF32,
                "i32.reinterpret_f32" => InsnKind::I32ReinterpretF32,
                "i64.reinterpret_f64" => InsnKind::I64ReinterpretF64,
                "f32.reinterpret_i32" => InsnKind::F32ReinterpretI32,
                "f64.reinterpret_i64" => InsnKind::F64ReinterpretI64,
                _ => return parser.error(ParseErrorKind::UnexpectedKeyword(kw), start),
            };
            Ok(Instruction { start, kind })
        }
        (tok, offset) => parser.unexpected_token(tok, "keyword for instruction", offset),
    }
}

// https://webassembly.github.io/spec/core/text/instructions.html#folded-instructions
impl<'a, 'p> MaybeFoldedInsn<'a, 'p> {
    fn new(parser: &'p mut Parser<'a>) -> MaybeFoldedInsn<'a, 'p> {
        MaybeFoldedInsn {
            insns: vec![],
            parser,
        }
    }

    fn parse_folded(&mut self) -> Result<'a, ()> {
        let start = self.parser.opening_paren("folded instruction")?;
        let mut insn = parse_one_insn(self.parser, false)?;
        insn.start = start;

        if !insn.kind.is_block() {
            while let (Token::LParen, _) = self
                .parser
                .peek("')' for ending folded instruction or '(' for nested instruction")?
            {
                self.parse_folded()?;
            }
        }

        self.parser.closing_paren("folded instruction")?;
        self.insns.push(insn);
        Ok(())
    }

    fn parse(&mut self) -> Result<'a, ()> {
        // Parser {insn}*
        loop {
            // This assumes that instr* is always ending with ')', 'end' or 'else' but I did not assure.
            // If some other folded statement like (foo) is following instr*, this assumption does not
            // work. In the case, we need to peek current and next tokens without consuming the tokens.
            match self
                .parser
                .peek("instruction keyword or '(' for folded instruction")?
            {
                (Token::LParen, _) => self.parse_folded()?,
                (Token::RParen, _) | (Token::Keyword("end"), _) | (Token::Keyword("else"), _) => {
                    return Ok(())
                }
                _ => {
                    let insn = parse_one_insn(self.parser, true)?;
                    self.insns.push(insn);
                }
            };
        }
    }
}

impl<'a> Parse<'a> for Vec<Instruction<'a>> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let mut parser = MaybeFoldedInsn::new(parser);
        parser.parse()?;
        Ok(parser.insns)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lookahead() {
        let v = vec![1, 2, 3, 4];
        let mut i = LookAhead::new(v.into_iter());
        assert_eq!(i.peek(), Some(&1));
        assert_eq!(i.lookahead(), Some(&2));
        assert_eq!(i.next(), Some(1));

        assert_eq!(i.peek(), Some(&2));
        assert_eq!(i.lookahead(), Some(&3));
        assert_eq!(i.next(), Some(2));

        assert_eq!(i.peek(), Some(&3));
        assert_eq!(i.lookahead(), Some(&4));
        assert_eq!(i.next(), Some(3));

        assert_eq!(i.peek(), Some(&4));
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), Some(4));

        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), None);

        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);

        let v: Vec<i32> = vec![];
        let mut i = LookAhead::new(v.into_iter());
        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), None);
        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
    }

    macro_rules! assert_parse {
        ($input:expr, $node:ty, $expect:pat if $cond:expr) => {
            let input = $input;
            let mut parser = Parser::new(input);
            let node: $node = parser.parse().unwrap();
            match node {
                $expect if $cond => { /* OK */ }
                _ => panic!(
                    "assertion failed: {:?} did not match to {}",
                    node,
                    stringify!($expect if $cond)
                ),
            }
            assert!(parser.is_done(), "{:?}", parser.tokens.collect::<Vec<_>>());
        };
        ($input:expr, $node:ty, $expect:pat) => {
            assert_parse!($input, $node, $expect if true);
        };
    }

    macro_rules! assert_error {
        ($input:expr, $node:ty, $expect:pat if $cond:expr) => {{
            use ParseErrorKind::*;
            let input = $input;
            match Parser::new(input).parse::<$node>().unwrap_err().kind {
                $expect if $cond => { /* OK */ }
                err => panic!("assertion failed: {:?} did not match to pattern {}", err, stringify!($expect if $cond)),
            }
        }};
        ($input:expr, $node:ty, $expect:pat) => {
            assert_error!($input, $node, $expect if true);
        };
    }

    macro_rules! is_match {
        ($e:expr, $p:pat) => {
            match $e {
                $p => true,
                _ => false,
            }
        };
    }

    #[test]
    fn root() {
        assert_parse!(r#"
            (module $foo)
            (module $foo)
            (module $foo)
        "#, SyntaxTree<'_>, SyntaxTree{ module: Module { ident: "$foo", types, .. } } if types.is_empty());
        assert_parse!(r#"
            (module $foo (type $f1 (func)))
            (module $foo (type $f1 (func)))
            (module $foo (type $f1 (func)))
        "#, SyntaxTree<'_>, SyntaxTree{ module: Module { ident: "$foo", types, .. } } if types.len() == 3);
    }

    #[test]
    fn module() {
        assert_parse!(r#"(module $foo)"#, Module<'_>, Module { ident: "$foo", types, .. } if types.is_empty());
        assert_parse!(r#"(module $foo (type $f1 (func)))"#, Module<'_>, Module { ident: "$foo", types, .. } if types.len() == 1);
        assert_parse!(r#"
            (module $foo
                (type $f1 (func))
                (type $f1 (func)))
        "#, Module<'_>, Module { ident: "$foo", types, .. } if types.len() == 2);

        assert_error!(r#"module"#, Module<'_>, MissingParen{ paren: '(', ..});
        assert_error!(r#"(module )"#, Module<'_>, UnexpectedToken{ expected: "identifier for module", ..});
        assert_error!(r#"(module"#, Module<'_>, UnexpectedEndOfFile{..});
        assert_error!(r#"(module $foo (type $f (func))"#, Module<'_>, UnexpectedEndOfFile{..});
    }

    #[test]
    fn module_field() {
        assert_parse!(
            r#"(type $f1 (func))"#,
            ModuleField<'_>,
            ModuleField::Type(..)
        );
        assert_parse!(
            r#"(import "m" "n" (func (type 0)))"#,
            ModuleField<'_>,
            ModuleField::Import(..)
        );
        assert_parse!(
            r#"(export "n" (func $foo))"#,
            ModuleField<'_>,
            ModuleField::Export(..)
        );

        assert_error!(r#"((type $f1 (func)))"#, ModuleField<'_>, UnexpectedToken{ expected: "keyword for module field", ..});
        assert_error!(r#"(hello!)"#, ModuleField<'_>, UnexpectedKeyword("hello!"));
    }

    #[test]
    fn type_def() {
        assert_parse!(r#"(type $f1 (func))"#, TypeDef<'_>, TypeDef{ id: Some("$f1"), .. });
        assert_parse!(r#"(type (func))"#, TypeDef<'_>, TypeDef{ id: None, .. });

        assert_error!(r#"(type (func) (func))"#, TypeDef<'_>, MissingParen { paren: ')', .. });
        assert_error!(r#"(type)"#, TypeDef<'_>, MissingParen { paren: '(', .. });
        assert_error!(r#"(type"#, TypeDef<'_>, UnexpectedEndOfFile { .. });
    }

    #[test]
    fn func_type() {
        assert_parse!(r#"(func)"#, FuncType<'_>, FuncType{ params, results, .. } if params.is_empty() && results.is_empty());
        assert_parse!(r#"(func (param $a i32) (result i32))"#, FuncType<'_>, FuncType{ params, results, .. } if params.len() == 1 && results.len() == 1);
        assert_parse!(r#"(func (param $a i32))"#, FuncType<'_>, FuncType{ params, results, .. } if params.len() == 1 && results.is_empty());
        assert_parse!(r#"(func (result i32))"#, FuncType<'_>, FuncType{ params, results, .. } if params.is_empty() && results.len() == 1);
        assert_parse!(r#"
            (func
                (param $a i32)
                (param $a i32)
                (result i32)
                (result i32))
        "#, FuncType<'_>, FuncType{ params, results, .. } if params.len() == 2 && results.len() == 2);

        assert_error!(r#"func"#, FuncType<'_>, MissingParen{ paren: '(', .. });
        assert_error!(r#"(type"#, FuncType<'_>, UnexpectedToken{ expected: "'func' keyword", .. });
        assert_error!(r#"(func "#, FuncType<'_>, UnexpectedEndOfFile{ expected: "opening paren for param in functype", .. });
        assert_error!(r#"(func (result i32)"#, FuncType<'_>, MissingParen{ paren: '(', got: None, .. });
        assert_error!(r#"(func (result i32) foo"#, FuncType<'_>, MissingParen{ paren: ')', .. });
    }

    #[test]
    fn param() {
        assert_parse!(r#"(param $a i32)"#, Param<'_>, Param { id: Some("$a"), .. });
        assert_parse!(r#"(param i32)"#, Param<'_>, Param { id: None, .. });

        assert_error!(r#"param"#, Param<'_>, MissingParen{ paren: '(', .. });
        assert_error!(r#"(module i32)"#, Param<'_>, UnexpectedToken{ expected: "'param' keyword", .. });
        assert_error!(r#"(param)"#, Param<'_>, UnexpectedToken{ .. });
        assert_error!(r#"(param i32 i64)"#, Param<'_>, MissingParen { paren: ')', .. });
        assert_error!(r#"(param i32"#, Param<'_>, MissingParen{ paren: ')', .. });
    }

    #[test]
    fn value_type() {
        assert_parse!(r#"i32"#, ValType, ValType::I32);
        assert_parse!(r#"i64"#, ValType, ValType::I64);
        assert_parse!(r#"f32"#, ValType, ValType::F32);
        assert_parse!(r#"f64"#, ValType, ValType::F64);

        assert_error!(r#"string"#, ValType, InvalidValType("string"));
        assert_error!(r#"$hello"#, ValType, UnexpectedToken{ expected: "keyword for value type", .. });
    }

    #[test]
    fn func_result() {
        assert_parse!(r#"(result i32)"#, FuncResult, FuncResult { .. });

        assert_error!(r#"result"#, FuncResult, MissingParen { paren: '(', .. });
        assert_error!(r#"(hello"#, FuncResult, UnexpectedToken { expected: "'result' keyword", .. });
        assert_error!(r#"(result)"#, FuncResult, UnexpectedToken { .. });
        assert_error!(r#"(result i32"#, FuncResult, MissingParen { paren: ')', .. });
    }

    #[test]
    fn name() {
        assert_parse!(r#""n""#, Name, Name(n) if n == "n");
        assert_parse!(r#""name""#, Name, Name(n) if n == "name");
        assert_parse!(r#""a\tb\nc""#, Name, Name(n) if n == "a\tb\nc");
        assert_parse!(r#""""#, Name, Name(n) if n.is_empty());
        assert_parse!(r#""\t\n\r\"\'\\\u{3042}\41""#, Name, Name(n) if n == "\t\n\r\"'\\あA");

        assert_error!(r#""\x""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\0""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\0x""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\u""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\u{""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\u{41""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\u{}""#, Name, InvalidStringFormat(..));
        assert_error!(r#""\u{hello!}""#, Name, InvalidStringFormat(..));
    }

    #[test]
    fn import() {
        assert_parse!(
            r#"(import "mod" "name" (func (type 0)))"#,
            Import<'_>,
            Import {
                mod_name: Name(mn),
                name: Name(n),
                ..
            } if mn == "mod" && n == "name"
        );

        assert_error!(r#"import"#, Import<'_>, MissingParen{ paren: '(', .. });
        assert_error!(r#"(hello"#, Import<'_>, UnexpectedToken{ expected: "'import' keyword", .. });
        assert_error!(r#"(import "mod" "name" (func (type 0))"#, Import<'_>, MissingParen{ paren: ')', .. });
        assert_error!(r#"(import "mod" (func (type 0))"#, Import<'_>, UnexpectedToken{ .. });
        assert_error!(r#"(import (func (type 0))"#, Import<'_>, UnexpectedToken{ .. });
    }

    #[test]
    fn import_desc() {
        assert_parse!(r#"(func (type 0))"#, ImportDesc<'_>, ImportDesc::Func{ id: None, .. });
        assert_parse!(r#"(func $foo (type 0))"#, ImportDesc<'_>, ImportDesc::Func{ id: Some("$foo"), .. });
        assert_parse!(r#"(table 0 funcref)"#, ImportDesc<'_>, ImportDesc::Table{ id: None, .. });
        assert_parse!(r#"(table $foo 0 funcref)"#, ImportDesc<'_>, ImportDesc::Table{ id: Some("$foo"), .. });
        assert_parse!(r#"(memory 0)"#, ImportDesc<'_>, ImportDesc::Memory{ id: None, .. });
        assert_parse!(r#"(memory $foo 0)"#, ImportDesc<'_>, ImportDesc::Memory{ id: Some("$foo"), .. });
        assert_parse!(r#"(global i32)"#, ImportDesc<'_>, ImportDesc::Global{ id: None, .. });
        assert_parse!(r#"(global $foo i32)"#, ImportDesc<'_>, ImportDesc::Global{ id: Some("$foo"), .. });

        assert_error!(r#"func"#, ImportDesc<'_>, MissingParen{ paren: '(', .. });
        assert_error!(r#"(func (type 0)"#, ImportDesc<'_>, UnexpectedEndOfFile{ .. });
        assert_error!(r#"(hello $foo"#, ImportDesc<'_>, UnexpectedKeyword("hello"));
    }

    #[test]
    fn type_use() {
        // XXX: Parsing TypeUse<'a> directly does not work since parser tries to parse (param)* and
        // (result)* and fails with unexpected EOF when they are missing. This is not a real-world
        // problem because typeuse is always used within other statement.
        assert_parse!(
            r#"(func (type 0))"#,
            ImportDesc<'_>,
            ImportDesc::Func{
                ty: TypeUse { params, results, .. },
                ..
            } if params.is_empty() && results.is_empty()
        );
        assert_parse!(
            r#"(func (type 0) (param i32))"#,
            ImportDesc<'_>,
            ImportDesc::Func{
                ty: TypeUse { params, results, .. },
                ..
            } if params.len() == 1 && results.is_empty()
        );
        assert_parse!(
            r#"(func (type 0) (result i32))"#,
            ImportDesc<'_>,
            ImportDesc::Func{
                ty: TypeUse { params, results, .. },
                ..
            } if params.is_empty() && results.len() == 1
        );
        assert_parse!(
            r#"(func (type 0) (param i32) (result i32))"#,
            ImportDesc<'_>,
            ImportDesc::Func{
                ty: TypeUse { params, results, .. },
                ..
            } if params.len() == 1 && results.len() == 1
        );

        assert_error!(r#"type"#, TypeUse<'_>, MissingParen{ paren: '(', .. });
        assert_error!(r#"(type 0"#, TypeUse<'_>, MissingParen{ paren: ')', .. });
        assert_error!(r#"(hello"#, TypeUse<'_>, UnexpectedToken{ expected: "'type' keyword for typeuse", ..});
    }

    #[test]
    fn index() {
        assert_parse!(r#"0"#, Index<'_>, Index::Num(0));
        assert_parse!(r#"0x1f"#, Index<'_>, Index::Num(0x1f));
        assert_parse!(r#"$foo"#, Index<'_>, Index::Ident("$foo"));

        assert_error!(r#"hi"#, Index<'_>, UnexpectedToken{ expected: "number or identifier for index", .. });
        assert_error!(r#""#, Index<'_>, UnexpectedEndOfFile{ expected: "number or identifier for index", .. });
    }

    #[test]
    fn table_type_and_limits() {
        assert_parse!(
            r#"0 funcref"#,
            TableType,
            TableType {
                limit: Limits::From { min: 0 }
            }
        );
        assert_parse!(
            r#"0 1 funcref"#,
            TableType,
            TableType {
                limit: Limits::Range { min: 0, max: 1 }
            }
        );

        assert_error!(r#"0 1 hi"#, TableType, UnexpectedToken{ expected: "'funcref' keyword for table type", .. });
        assert_error!(r#"hi"#, TableType, UnexpectedToken{ expected: "u32 for min table limit", .. });
    }

    #[test]
    fn memtype() {
        assert_parse!(
            r#"0 10"#,
            MemType,
            MemType {
                limit: Limits::Range { min: 0, max: 10 }
            }
        );
    }

    #[test]
    fn global_type() {
        assert_parse!(
            r#"i32"#,
            GlobalType,
            GlobalType {
                mutable: false,
                ty: ValType::I32
            }
        );
        assert_parse!(
            r#"(mut i32)"#,
            GlobalType,
            GlobalType {
                mutable: true,
                ty: ValType::I32
            }
        );

        assert_error!(
            r#""#,
            GlobalType,
            UnexpectedEndOfFile { expected: "'(' for mut or value type of global type", .. }
        );
        assert_error!(
            r#"(hello"#,
            GlobalType,
            UnexpectedToken { expected: "'mut' keyword for global type", .. }
        );
        assert_error!(
            r#"(mut i32"#,
            GlobalType,
            MissingParen { paren: ')', .. }
        );
    }

    #[test]
    fn export() {
        assert_parse!(
            r#"(export "hi" (func 0))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Func, idx: Index::Num(0), .. } if n == "hi"
        );
        assert_parse!(
            r#"(export "hi" (table $foo))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Table, idx: Index::Ident("$foo"), .. } if n == "hi"
        );
        assert_parse!(
            r#"(export "hi" (memory 0))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Memory, idx: Index::Num(0), .. } if n == "hi"
        );
        assert_parse!(
            r#"(export "hi" (global 0))"#,
            Export<'_>,
            Export{ name: Name(n), kind: ExportKind::Global, idx: Index::Num(0), .. } if n == "hi"
        );

        assert_error!(r#"export"#, Export<'_>, MissingParen{ paren: '(', .. });
        assert_error!(r#"(export "hi" (func 0"#, Export<'_>, MissingParen{ paren: ')', .. });
        assert_error!(r#"(export "hi" (func 0)"#, Export<'_>, MissingParen{ paren: ')', .. });
        assert_error!(r#"(hello"#, Export<'_>, UnexpectedToken{ expected: "'export' keyword for export", .. });
        assert_error!(
            r#"(export "hi" (hello 0))"#,
            Export<'_>,
            UnexpectedKeyword("hello")
        );
    }

    #[test]
    fn func_field() {
        assert_parse!(
            r#"(func $f (import "m" "n") (type 0))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::Import(Import {
                mod_name: Name(m),
                name: Name(n),
                desc: ImportDesc::Func { id: Some("$f"), .. },
                ..
            }) if m == "m" && n == "n"
        );
        assert_parse!(
            r#"(func $f (export "n") (type 0))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::Export(
                Export {
                    name: Name(n),
                    kind: ExportKind::Func,
                    idx: Index::Ident("$f"),
                    ..
                },
                Func {
                    ty: TypeUse {
                        idx: Index::Num(0),
                        ..
                    },
                    locals,
                    body,
                    ..
                },
            ) if n == "n" && locals.is_empty() && body.is_empty()
        );
        assert_parse!(
            r#"(func $f (type 0))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                body,
                ..
            }) if locals.is_empty() && body.is_empty()
        );
        assert_parse!(
            r#"(func $f (type 0) (local))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                ..
            }) if locals.is_empty()
        );
        assert_parse!(
            r#"(func $f (type 0) (local i32))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                ..
            }) if locals[0].ty == ValType::I32 && locals[0].id == None
        );
        assert_parse!(
            r#"(func $f (type 0) (local $l i32))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                ..
            }) if locals[0].ty == ValType::I32 && locals[0].id == Some("$l")
        );
        assert_parse!(
            r#"(func $f (type 0) (local i32 f64))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                ..
            }) if locals.iter().map(|l| l.ty).collect::<Vec<_>>() == vec![ValType::I32, ValType::F64] &&
                locals.iter().map(|l| l.id).collect::<Vec<_>>() == vec![None, None]
        );
        assert_parse!(
            r#"(func $f (type 0) (local i32) (local f64))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                ..
            }) if locals.iter().map(|l| l.ty).collect::<Vec<_>>() == vec![ValType::I32, ValType::F64] &&
                locals.iter().map(|l| l.id).collect::<Vec<_>>() == vec![None, None]
        );
        assert_parse!(
            r#"(func $f (type 0) (local $l1 i32) (local $l2 f64))"#,
            FuncAbbrev<'_>,
            FuncAbbrev::NoAbbrev(Func {
                ty: TypeUse {
                    idx: Index::Num(0),
                    ..
                },
                locals,
                ..
            }) if locals.iter().map(|l| l.ty).collect::<Vec<_>>() == vec![ValType::I32, ValType::F64] &&
                locals.iter().map(|l| l.id).collect::<Vec<_>>() == vec![Some("$l1"), Some("$l2")]
        );

        assert_error!(
            r#"(func (local i32))"#,
            FuncAbbrev<'_>,
            UnexpectedKeyword("local")
        );
    }

    macro_rules! assert_insn {
        ($input:expr, $expect:pat if $cond:expr) => {
            let input = concat!($input, ')');
            let mut parser = Parser::new(input);
            let insns: Vec<Instruction> = parser.parse().unwrap();
            let kinds = insns.into_iter().map(|i| i.kind).collect::<Vec<_>>();
            match kinds.as_slice() {
                $expect if $cond => { /* OK */ }
                i => panic!(
                    "assertion failed: {:?} did not match to {}",
                    i,
                    stringify!($expect if $cond)
                ),
            }
            match parser.tokens.next() {
                Some(Ok((Token::RParen, _))) => (),
                tok => assert!(false, "Tokens still remain: {:?} and then {:?}", tok, parser.tokens.collect::<Vec<_>>()),
            }
        };
        ($input:expr, $expect:pat) => {
            assert_insn!($input, $expect if true);
        };
    }

    #[test]
    fn insn_abbrev() {
        use InsnKind::*;
        assert_insn!(r#"nop"#, [Nop]);
        assert_insn!(r#"(nop)"#, [Nop]);
        assert_insn!(r#"(unreachable (nop))"#, [Nop, Unreachable]);
        assert_insn!(
            r#"(return (nop) (unreachable))"#,
            [Nop, Unreachable, Return]
        );
        assert_insn!(
            r#"(return (unreachable (nop)))"#,
            [Nop, Unreachable, Return]
        );
        assert_insn!(
            r#"(nop (return (nop) (unreachable)))"#,
            [Nop, Unreachable, Return, Nop]
        );
        assert_insn!(r#"nop unreachable return"#, [Nop, Unreachable, Return]);
        assert_insn!(r#"nop (nop) (nop)"#, [Nop, Nop, Nop]);
        assert_insn!(r#"(nop) nop (nop)"#, [Nop, Nop, Nop]);
        assert_insn!(r#"(nop) (nop) nop"#, [Nop, Nop, Nop]);
        assert_insn!(r#"nop (nop (nop))"#, [Nop, Nop, Nop]);
        assert_insn!(r#"(nop (nop)) nop"#, [Nop, Nop, Nop]);
        assert_insn!(r#"block block block end end end"#, [Block { body, .. }]
            if match &body[0].kind {
                Block { body, ..} => match &body[0].kind {
                    Block { body, .. } => body.is_empty(),
                    n => panic!("nest 2: {:?}", n),
                }
                n => panic!("nest 1: {:?}", n),
            }
        );
        assert_insn!(r#"(block (block (block )))"#, [Block { body, .. }]
            if match &body[0].kind {
                Block { body, ..} => match &body[0].kind {
                    Block { body, .. } => body.is_empty(),
                    n => panic!("nest 2: {:?}", n),
                }
                n => panic!("nest 1: {:?}", n),
            }
        );
        assert_insn!(r#"block (block block end) end"#, [Block { body, .. }]
            if match &body[0].kind {
                Block { body, ..} => match &body[0].kind {
                    Block { body, .. } => body.is_empty(),
                    n => panic!("nest 2: {:?}", n),
                }
                n => panic!("nest 1: {:?}", n),
            }
        );

        assert_error!(
            r#"(nop nop)"#,
            Vec<Instruction>,
            MissingParen {
                paren: ')',
                what: "folded instruction",
                ..
            }
        );
        assert_error!(
            r#"not-exist"#,
            Vec<Instruction>,
            UnexpectedKeyword("not-exist")
        );
        assert_error!(
            r#"(not-exist)"#,
            Vec<Instruction>,
            UnexpectedKeyword("not-exist")
        );
    }

    #[test]
    fn control_instructions() {
        use InsnKind::*;
        assert_insn!(
            r#"block end"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block $blk end $id"#,
            [
                Block{ label: Some("$blk"), ty: None, body, id: Some("$id") }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block $blk (result i32) end"#,
            [
                Block{ label: Some("$blk"), ty: Some(ValType::I32), body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block (result i32) end"#,
            [
                Block{ label: None, ty: Some(ValType::I32), body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"block nop end"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if is_match!(body[0].kind, Nop)
        );
        assert_insn!(
            r#"block $blk nop end"#,
            [
                Block{ label: Some("$blk"), ty: None, body, id: None }
            ] if is_match!(body[0].kind, Nop)
        );
        assert_insn!(
            r#"block (result i32) nop end"#,
            [
                Block{ label: None, ty: Some(ValType::I32), body, id: None }
            ] if is_match!(body[0].kind, Nop)
        );
        assert_insn!(
            r#"(block)"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"(block $blk)"#,
            [
                Block{ label: Some("$blk"), ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"(block (result i32))"#,
            [
                Block{ label: None, ty: Some(ValType::I32), body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"(block nop)"#,
            [
                Block{ label: None, ty: None, body, id: None }
            ] if is_match!(body[0].kind, Nop)
        );
        // Note: 'loop' instruction is parsed with the same logic as 'block' instruction. Only one test case is sufficient
        assert_insn!(
            r#"loop end"#,
            [
                Loop{ label: None, ty: None, body, id: None }
            ] if body.is_empty()
        );
        assert_insn!(
            r#"if end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if else end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) else $a end $b"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: Some("$a"), else_body, end_id: Some("$b") }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) else $a end $b"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: Some("$a"), else_body, end_id: Some("$b") }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) end $b"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: Some("$b") }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l end"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if (result i32) end"#,
            [
                If{ label: None, ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"if nop end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l nop end"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"if $l (result i32) nop end"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"if nop else unreachable end"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && is_match!(else_body[0].kind, Unreachable)
        );
        assert_insn!(
            r#"(if (then))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (then))"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (result i32) (then))"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (then) (else))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (then) (else))"#,
            [
                If{ label: Some("$l"), ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if $l (result i32) (then) (else))"#,
            [
                If{ label: Some("$l"), ty: Some(ValType::I32), then_body, else_id: None, else_body, end_id: None }
            ] if then_body.is_empty() && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (then nop))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && else_body.is_empty()
        );
        assert_insn!(
            r#"(if (then nop) (else nop))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && is_match!(else_body[0].kind, Nop)
        );
        assert_insn!(
            r#"(if (then (nop)) (else (nop)))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if is_match!(then_body[0].kind, Nop) && is_match!(else_body[0].kind, Nop)
        );
        assert_insn!(
            r#"(if (then nop nop) (else nop nop))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.len() == 2 && else_body.len() == 2
        );
        assert_insn!(
            r#"(if (then (nop (nop))) (else (nop (nop))))"#,
            [
                If{ label: None, ty: None, then_body, else_id: None, else_body, end_id: None }
            ] if then_body.len() == 2 && else_body.len() == 2
        );
        assert_insn!(r#"unreachable"#, [Unreachable]);
        assert_insn!(r#"br 0"#, [Br(Index::Num(0))]);
        assert_insn!(r#"br $blk"#, [Br(Index::Ident("$blk"))]);
        assert_insn!(r#"br_if 0"#, [BrIf(Index::Num(0))]);
        assert_insn!(r#"br_if $blk"#, [BrIf(Index::Ident("$blk"))]);
        assert_insn!(
            r#"br_table 0"#,
            [BrTable {
                labels,
                default_label: Index::Num(0)
            }] if labels.is_empty()
        );
        assert_insn!(
            r#"br_table 1 2 3 0"#,
            [
                BrTable {
                    labels,
                    default_label: Index::Num(0)
                }
            ] if labels
                    .into_iter()
                    .map(|i| match i {
                        Index::Num(n) => *n,
                        Index::Ident(i) => panic!("unexpected index: {}", i),
                    })
                    .collect::<Vec<_>>() == vec![1, 2, 3]
        );
        assert_insn!(
            r#"br_table $a $b $c $x"#,
            [
                BrTable {
                    labels,
                    default_label: Index::Ident("$x")
                }
            ] if labels
                    .into_iter()
                    .map(|i| match i {
                        Index::Num(n) => panic!("unexpected index: {}", n),
                        Index::Ident(i) => *i,
                    })
                    .collect::<Vec<_>>() == vec!["$a", "$b", "$c"]
        );
        assert_insn!(r#"call 0"#, [Call(Index::Num(0))]);
        assert_insn!(r#"call $f"#, [Call(Index::Ident("$f"))]);
        assert_insn!(r#"call_indirect (type 0)"#, [CallIndirect(TypeUse{ idx: Index::Num(0), .. })]);

        assert_error!(r#"br_table)"#, Vec<Instruction<'_>>, InvalidOperand{ .. });
        assert_error!(
            r#"(if (then nop) else nop)"#,
            Vec<Instruction<'_>>,
            UnexpectedToken{ got: Token::Keyword("else"), .. }
        );
    }

    #[test]
    fn parametric_instructions() {
        use InsnKind::*;
        assert_insn!(r#"drop"#, [Drop]);
        assert_insn!(r#"select"#, [Select]);
    }

    #[test]
    fn variable_instructions() {
        use InsnKind::*;
        assert_insn!(r#"local.get 0"#, [LocalGet(Index::Num(0))]);
        assert_insn!(r#"local.get $x"#, [LocalGet(Index::Ident("$x"))]);
        assert_insn!(r#"local.set 0"#, [LocalSet(Index::Num(0))]);
        assert_insn!(r#"local.tee 0"#, [LocalTee(Index::Num(0))]);
        assert_insn!(r#"global.get 0"#, [GlobalGet(Index::Num(0))]);
        assert_insn!(r#"global.set 0"#, [GlobalSet(Index::Num(0))]);

        assert_error!(r#"local.get foo"#, Vec<Instruction<'_>>, UnexpectedToken{ .. });
    }

    #[test]
    fn memory_instructions() {
        use InsnKind::*;
        assert_insn!(
            r#"i32.load"#,
            [I32Load(Mem {
                align: None,
                offset: None
            })]
        );
        assert_insn!(
            r#"i32.load align=32"#,
            [I32Load(Mem {
                align: Some(32),
                offset: None,
            })]
        );
        assert_insn!(
            r#"i32.load offset=10"#,
            [I32Load(Mem {
                align: None,
                offset: Some(10),
            })]
        );
        assert_insn!(
            r#"i32.load offset=10 align=32"#,
            [I32Load(Mem {
                align: Some(32),
                offset: Some(10),
            })]
        );
        assert_insn!(
            r#"i32.load offset=0x1f align=0x80"#,
            [I32Load(Mem {
                align: Some(0x80),
                offset: Some(0x1f),
            })]
        );
        assert_insn!(r#"i64.load"#, [I64Load(..)]);
        assert_insn!(r#"f32.load"#, [F32Load(..)]);
        assert_insn!(r#"f64.load"#, [F64Load(..)]);
        assert_insn!(r#"i32.load8_s"#, [I32Load8S(..)]);
        assert_insn!(r#"i32.load8_u"#, [I32Load8U(..)]);
        assert_insn!(r#"i32.load16_s"#, [I32Load16S(..)]);
        assert_insn!(r#"i32.load16_u"#, [I32Load16U(..)]);
        assert_insn!(r#"i64.load8_s"#, [I64Load8S(..)]);
        assert_insn!(r#"i64.load8_u"#, [I64Load8U(..)]);
        assert_insn!(r#"i64.load16_s"#, [I64Load16S(..)]);
        assert_insn!(r#"i64.load16_u"#, [I64Load16U(..)]);
        assert_insn!(r#"i64.load32_s"#, [I64Load32S(..)]);
        assert_insn!(r#"i64.load32_u"#, [I64Load32U(..)]);
        assert_insn!(r#"i32.store"#, [I32Store(..)]);
        assert_insn!(r#"i64.store"#, [I64Store(..)]);
        assert_insn!(r#"f32.store"#, [F32Store(..)]);
        assert_insn!(r#"f64.store"#, [F64Store(..)]);
        assert_insn!(r#"i32.store8"#, [I32Store8(..)]);
        assert_insn!(r#"i32.store16"#, [I32Store16(..)]);
        assert_insn!(r#"i64.store8"#, [I64Store8(..)]);
        assert_insn!(r#"i64.store16"#, [I64Store16(..)]);
        assert_insn!(r#"i64.store32"#, [I64Store32(..)]);
        assert_insn!(r#"memory.size"#, [MemorySize]);
        assert_insn!(r#"memory.grow"#, [MemoryGrow]);

        assert_error!(
            r#"i32.load align=11"#,
            Vec<Instruction<'_>>,
            InvalidAlignment(11)
        );
        assert_error!(
            r#"i32.load align=32 offset=10"#,
            Vec<Instruction<'_>>,
            UnexpectedKeyword("offset=10")
        );
        assert_error!(
            r#"i32.load align=pqr"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ .. }
        );
    }

    #[test]
    fn const_instructions() {
        use InsnKind::*;
        assert_insn!(r#"i32.const 0"#, [I32Const(0)]);
        assert_insn!(r#"i32.const 42"#, [I32Const(42)]);
        assert_insn!(r#"i32.const 4_2"#, [I32Const(42)]);
        assert_insn!(r#"i32.const 0x1f"#, [I32Const(0x1f)]);
        assert_insn!(r#"i32.const -0"#, [I32Const(-0)]);
        assert_insn!(r#"i32.const -42"#, [I32Const(-42)]);
        assert_insn!(r#"i32.const -0x1f"#, [I32Const(-0x1f)]);
        assert_insn!(r#"i32.const 2147483647"#, [I32Const(2147483647)]); // INT32_MAX
        assert_insn!(r#"i32.const 0x7fffffff"#, [I32Const(0x7fffffff)]); // INT32_MAX
        assert_insn!(r#"i32.const -2147483648"#, [I32Const(-2147483648)]); // INT32_MIN
        assert_insn!(r#"i32.const -0x80000000"#, [I32Const(-0x80000000)]); // INT32_MAX

        assert_error!(
            r#"i32.const 0.123"#,
            Vec<Instruction<'_>>,
            UnexpectedToken{ expected: "integer for i32.const operand", .. }
        );
        assert_error!(
            r#"i32.const 0x80000000"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i32", .. }
        );
        assert_error!(
            r#"i32.const 0x99999999"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i32", .. }
        );
        assert_error!(
            r#"i32.const -0x80000001"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i32", .. }
        );
        assert_error!(
            r#"i32.const -0x99999999"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i32", .. }
        );

        assert_insn!(r#"i64.const 0"#, [I64Const(0)]);
        assert_insn!(r#"i64.const 42"#, [I64Const(42)]);
        assert_insn!(r#"i64.const 4_2"#, [I64Const(42)]);
        assert_insn!(r#"i64.const 0x1f"#, [I64Const(0x1f)]);
        assert_insn!(r#"i64.const 0x1_f"#, [I64Const(0x1f)]);
        assert_insn!(r#"i64.const -42"#, [I64Const(-42)]);
        assert_insn!(r#"i64.const -0x1f"#, [I64Const(-0x1f)]);
        assert_insn!(
            r#"i64.const 9223372036854775807"#, // INT64_MAX
            [I64Const(9223372036854775807)]
        );
        assert_insn!(
            r#"i64.const -9223372036854775808"#, // INT64_MIN
            [I64Const(-9223372036854775808)]
        );
        assert_insn!(
            r#"i64.const 0x7fffffffffffffff"#, // INT64_MAX
            [I64Const(0x7fffffffffffffff)]
        );
        assert_insn!(
            r#"i64.const -0x8000000000000000"#, // INT64_MIN
            [I64Const(-0x8000000000000000)]
        );

        assert_error!(
            r#"i64.const 0.123"#,
            Vec<Instruction<'_>>,
            UnexpectedToken{ expected: "integer for i64.const operand", .. }
        );
        assert_error!(
            r#"i64.const 0x8000000000000000"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i64", .. }
        );
        assert_error!(
            r#"i64.const 0x9999999999999999"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i64", .. }
        );
        assert_error!(
            r#"i64.const -0x8000000000000001"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i64", .. }
        );
        assert_error!(
            r#"i64.const -0x9999999999999999"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "too large or small integer for i64", .. }
        );

        assert_insn!(r#"f32.const 42."#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 42.0"#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 4_2."#, [F32Const(f)] if *f == 42.0);
        assert_insn!(r#"f32.const 0x1f."#, [F32Const(f)] if *f == 31.0);
        assert_insn!(r#"f32.const -42."#, [F32Const(f)] if *f == -42.0);
        assert_insn!(r#"f32.const -4_2."#, [F32Const(f)] if *f == -42.0);
        assert_insn!(r#"f32.const -0x1f."#, [F32Const(f)] if *f == -31.0);
        assert_insn!(r#"f32.const 1.2_3"#, [F32Const(f)] if *f == 1.23);
        assert_insn!(r#"f32.const 1.2_3E3"#, [F32Const(f)] if *f == 1.23e3);
        assert_insn!(r#"f32.const 120.4E-3"#, [F32Const(f)] if *f == 120.4e-3);
        assert_insn!(r#"f32.const 0xe."#, [F32Const(f)] if *f == 0xe as f32);
        assert_insn!(r#"f32.const 0xe_f."#, [F32Const(f)] if *f == 0xef as f32);
        assert_insn!(r#"f32.const 0x1_f.2_e"#, [F32Const(f)] if *f == 0x1f as f32 + 2.0 / 16.0 + 14.0 / (16.0 * 16.0));
        assert_insn!(r#"f32.const 0xe.f"#, [F32Const(f)] if *f == 14.0 + 15.0 / 16.0);
        assert_insn!(r#"f32.const 0xe.fP3"#, [F32Const(f)] if *f == (14.0 + 15.0 / 16.0) * 2.0 * 2.0 * 2.0);
        assert_insn!(r#"f32.const 0xe.fP-1"#, [F32Const(f)] if *f == (14.0 + 15.0 / 16.0) / 2.0);
        assert_insn!(r#"f32.const 3.4E38"#, [F32Const(f)] if *f == 3.4E38); // ≈ (2 - 2^(-23)) * 2^127
        assert_insn!(r#"f32.const 2.E-149"#, [F32Const(f)] if *f == 2.0e-149);
        assert_insn!(r#"f32.const 1.4E-45"#, [F32Const(f)] if *f == 1.4E-45); // ≈ 2^(-149)
        assert_insn!(r#"f32.const inf"#, [F32Const(f)] if *f == f32::INFINITY);
        assert_insn!(r#"f32.const nan"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const nan:0x401234"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const -inf"#, [F32Const(f)] if *f == f32::NEG_INFINITY);
        assert_insn!(r#"f32.const -nan"#, [F32Const(f)] if f.is_nan());
        assert_insn!(r#"f32.const -nan:0x401234"#, [F32Const(f)] if f.is_nan());

        assert_error!(
            r#"f32.const 12"#,
            Vec<Instruction<'_>>,
            UnexpectedToken{ expected: "float number for f32.const", .. }
        );
        assert_error!(
            r#"f32.const nan:0x1234"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "payload of NaN for f32 must be in range of 2^22 <= payload < 2^23", .. }
        );
        assert_error!(
            r#"f32.const nan:0x100_0000"#, // Larger than 0x80_0000
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "payload of NaN for f32 must be in range of 2^22 <= payload < 2^23", .. }
        );

        assert_insn!(r#"f64.const 42."#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 42.0"#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 4_2."#, [F64Const(f)] if *f == 42.0);
        assert_insn!(r#"f64.const 0x1f."#, [F64Const(f)] if *f == 31.0);
        assert_insn!(r#"f64.const -42."#, [F64Const(f)] if *f == -42.0);
        assert_insn!(r#"f64.const -4_2."#, [F64Const(f)] if *f == -42.0);
        assert_insn!(r#"f64.const -0x1f."#, [F64Const(f)] if *f == -31.0);
        assert_insn!(r#"f64.const 1.2_3"#, [F64Const(f)] if *f == 1.23);
        assert_insn!(r#"f64.const 1.2_3E3"#, [F64Const(f)] if *f == 1.23e3);
        assert_insn!(r#"f64.const 100.4E-2"#, [F64Const(f)] if *f == 100.4e-2);
        assert_insn!(r#"f64.const 0xe."#, [F64Const(f)] if *f == 0xe as f64);
        assert_insn!(r#"f64.const 0xe_f."#, [F64Const(f)] if *f == 0xef as f64);
        assert_insn!(r#"f64.const 0x1_f.2_e"#, [F64Const(f)] if *f == 0x1f as f64 + 2.0 / 16.0 + 14.0 / (16.0 * 16.0));
        assert_insn!(r#"f64.const 0xe.f"#, [F64Const(f)] if *f == 14.0 + 15.0 / 16.0);
        assert_insn!(r#"f64.const 0xe.fP3"#, [F64Const(f)] if *f == (14.0 + 15.0 / 16.0) * 2.0 * 2.0 * 2.0);
        assert_insn!(r#"f64.const 0xe.fP-1"#, [F64Const(f)] if *f == (14.0 + 15.0 / 16.0) / 2.0);
        assert_insn!(r#"f64.const inf"#, [F64Const(f)] if *f == f64::INFINITY);
        assert_insn!(r#"f64.const nan"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const nan:0x8_0000_0000_1245"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const -inf"#, [F64Const(f)] if *f == f64::NEG_INFINITY);
        assert_insn!(r#"f64.const -nan"#, [F64Const(f)] if f.is_nan());
        assert_insn!(r#"f64.const -nan:0x8_0000_0000_1245"#, [F64Const(f)] if f.is_nan());

        assert_error!(
            r#"f64.const 12"#,
            Vec<Instruction<'_>>,
            UnexpectedToken{ expected: "float number for f64.const", .. }
        );
        assert_error!(
            r#"f64.const nan:0x1234"#,
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "payload of NaN for f64 must be in range of 2^51 <= payload < 2^52", .. }
        );
        assert_error!(
            r#"f64.const nan:0x20_0000_0000_0000"#, // Larger than 0x10_0000_0000_0000
            Vec<Instruction<'_>>,
            CannotParseNum{ reason: "payload of NaN for f64 must be in range of 2^51 <= payload < 2^52", .. }
        );
    }

    #[test]
    fn numeric_instructions() {
        use InsnKind::*;
        assert_insn!(r#"i32.clz"#, [I32Clz]);
        assert_insn!(r#"i32.ctz"#, [I32Ctz]);
        assert_insn!(r#"i32.popcnt"#, [I32Popcnt]);
        assert_insn!(r#"i32.add"#, [I32Add]);
        assert_insn!(r#"i32.sub"#, [I32Sub]);
        assert_insn!(r#"i32.mul"#, [I32Mul]);
        assert_insn!(r#"i32.div_s"#, [I32DivS]);
        assert_insn!(r#"i32.div_u"#, [I32DivU]);
        assert_insn!(r#"i32.rem_s"#, [I32RemS]);
        assert_insn!(r#"i32.rem_u"#, [I32RemU]);
        assert_insn!(r#"i32.and"#, [I32And]);
        assert_insn!(r#"i32.or"#, [I32Or]);
        assert_insn!(r#"i32.xor"#, [I32Xor]);
        assert_insn!(r#"i32.shl"#, [I32Shl]);
        assert_insn!(r#"i32.shr_s"#, [I32ShrS]);
        assert_insn!(r#"i32.shr_u"#, [I32ShrU]);
        assert_insn!(r#"i32.rotl"#, [I32Rotl]);
        assert_insn!(r#"i32.rotr"#, [I32Rotr]);
        assert_insn!(r#"i64.clz"#, [I64Clz]);
        assert_insn!(r#"i64.ctz"#, [I64Ctz]);
        assert_insn!(r#"i64.popcnt"#, [I64Popcnt]);
        assert_insn!(r#"i64.add"#, [I64Add]);
        assert_insn!(r#"i64.sub"#, [I64Sub]);
        assert_insn!(r#"i64.mul"#, [I64Mul]);
        assert_insn!(r#"i64.div_s"#, [I64DivS]);
        assert_insn!(r#"i64.div_u"#, [I64DivU]);
        assert_insn!(r#"i64.rem_s"#, [I64RemS]);
        assert_insn!(r#"i64.rem_u"#, [I64RemU]);
        assert_insn!(r#"i64.and"#, [I64And]);
        assert_insn!(r#"i64.or"#, [I64Or]);
        assert_insn!(r#"i64.xor"#, [I64Xor]);
        assert_insn!(r#"i64.shl"#, [I64Shl]);
        assert_insn!(r#"i64.shr_s"#, [I64ShrS]);
        assert_insn!(r#"i64.shr_u"#, [I64ShrU]);
        assert_insn!(r#"i64.rotl"#, [I64Rotl]);
        assert_insn!(r#"i64.rotr"#, [I64Rotr]);
        assert_insn!(r#"f32.abs"#, [F32Abs]);
        assert_insn!(r#"f32.neg"#, [F32Neg]);
        assert_insn!(r#"f32.ceil"#, [F32Ceil]);
        assert_insn!(r#"f32.floor"#, [F32Floor]);
        assert_insn!(r#"f32.trunc"#, [F32Trunc]);
        assert_insn!(r#"f32.nearest"#, [F32Nearest]);
        assert_insn!(r#"f32.sqrt"#, [F32Sqrt]);
        assert_insn!(r#"f32.add"#, [F32Add]);
        assert_insn!(r#"f32.sub"#, [F32Sub]);
        assert_insn!(r#"f32.mul"#, [F32Mul]);
        assert_insn!(r#"f32.div"#, [F32Div]);
        assert_insn!(r#"f32.min"#, [F32Min]);
        assert_insn!(r#"f32.max"#, [F32Max]);
        assert_insn!(r#"f32.copysign"#, [F32Copysign]);
        assert_insn!(r#"f64.abs"#, [F64Abs]);
        assert_insn!(r#"f64.neg"#, [F64Neg]);
        assert_insn!(r#"f64.ceil"#, [F64Ceil]);
        assert_insn!(r#"f64.floor"#, [F64Floor]);
        assert_insn!(r#"f64.trunc"#, [F64Trunc]);
        assert_insn!(r#"f64.nearest"#, [F64Nearest]);
        assert_insn!(r#"f64.sqrt"#, [F64Sqrt]);
        assert_insn!(r#"f64.add"#, [F64Add]);
        assert_insn!(r#"f64.sub"#, [F64Sub]);
        assert_insn!(r#"f64.mul"#, [F64Mul]);
        assert_insn!(r#"f64.div"#, [F64Div]);
        assert_insn!(r#"f64.min"#, [F64Min]);
        assert_insn!(r#"f64.max"#, [F64Max]);
        assert_insn!(r#"f64.copysign"#, [F64Copysign]);
        assert_insn!(r#"i32.eqz"#, [I32Eqz]);
        assert_insn!(r#"i32.eq"#, [I32Eq]);
        assert_insn!(r#"i32.ne"#, [I32Ne]);
        assert_insn!(r#"i32.lt_s"#, [I32LtS]);
        assert_insn!(r#"i32.lt_u"#, [I32LtU]);
        assert_insn!(r#"i32.gt_s"#, [I32GtS]);
        assert_insn!(r#"i32.gt_u"#, [I32GtU]);
        assert_insn!(r#"i32.le_s"#, [I32LeS]);
        assert_insn!(r#"i32.le_u"#, [I32LeU]);
        assert_insn!(r#"i32.ge_s"#, [I32GeS]);
        assert_insn!(r#"i32.ge_u"#, [I32GeU]);
        assert_insn!(r#"i64.eqz"#, [I64Eqz]);
        assert_insn!(r#"i64.eq"#, [I64Eq]);
        assert_insn!(r#"i64.ne"#, [I64Ne]);
        assert_insn!(r#"i64.lt_s"#, [I64LtS]);
        assert_insn!(r#"i64.lt_u"#, [I64LtU]);
        assert_insn!(r#"i64.gt_s"#, [I64GtS]);
        assert_insn!(r#"i64.gt_u"#, [I64GtU]);
        assert_insn!(r#"i64.le_s"#, [I64LeS]);
        assert_insn!(r#"i64.le_u"#, [I64LeU]);
        assert_insn!(r#"i64.ge_s"#, [I64GeS]);
        assert_insn!(r#"i64.ge_u"#, [I64GeU]);
        assert_insn!(r#"f32.eq"#, [F32Eq]);
        assert_insn!(r#"f32.ne"#, [F32Ne]);
        assert_insn!(r#"f32.lt"#, [F32Lt]);
        assert_insn!(r#"f32.gt"#, [F32Gt]);
        assert_insn!(r#"f32.le"#, [F32Le]);
        assert_insn!(r#"f32.ge"#, [F32Ge]);
        assert_insn!(r#"f64.eq"#, [F64Eq]);
        assert_insn!(r#"f64.ne"#, [F64Ne]);
        assert_insn!(r#"f64.lt"#, [F64Lt]);
        assert_insn!(r#"f64.gt"#, [F64Gt]);
        assert_insn!(r#"f64.le"#, [F64Le]);
        assert_insn!(r#"f64.ge"#, [F64Ge]);
        assert_insn!(r#"i32.wrap_i64"#, [I32WrapI64]);
        assert_insn!(r#"i32.trunc_f32_s"#, [I32TruncF32S]);
        assert_insn!(r#"i32.trunc_f32_u"#, [I32TruncF32U]);
        assert_insn!(r#"i32.trunc_f64_s"#, [I32TruncF64S]);
        assert_insn!(r#"i32.trunc_f64_u"#, [I32TruncF64U]);
        assert_insn!(r#"i64.extend_i32_s"#, [I64ExtendI32S]);
        assert_insn!(r#"i64.extend_i32_u"#, [I64ExtendI32U]);
        assert_insn!(r#"i64.trunc_f32_s"#, [I64TruncF32S]);
        assert_insn!(r#"i64.trunc_f32_u"#, [I64TruncF32U]);
        assert_insn!(r#"i64.trunc_f64_s"#, [I64TruncF64S]);
        assert_insn!(r#"i64.trunc_f64_u"#, [I64TruncF64U]);
        assert_insn!(r#"f32.convert_i32_s"#, [F32ConvertI32S]);
        assert_insn!(r#"f32.convert_i32_u"#, [F32ConvertI32U]);
        assert_insn!(r#"f32.convert_i64_s"#, [F32ConvertI64S]);
        assert_insn!(r#"f32.convert_i64_u"#, [F32ConvertI64U]);
        assert_insn!(r#"f32.demote_f64"#, [F32DemoteF64]);
        assert_insn!(r#"f64.convert_i32_s"#, [F64ConvertI32S]);
        assert_insn!(r#"f64.convert_i32_u"#, [F64ConvertI32U]);
        assert_insn!(r#"f64.convert_i64_s"#, [F64ConvertI64S]);
        assert_insn!(r#"f64.convert_i64_u"#, [F64ConvertI64U]);
        assert_insn!(r#"f64.promote_f32"#, [F64PromoteF32]);
        assert_insn!(r#"i32.reinterpret_f32"#, [I32ReinterpretF32]);
        assert_insn!(r#"i64.reinterpret_f64"#, [I64ReinterpretF64]);
        assert_insn!(r#"f32.reinterpret_i32"#, [F32ReinterpretI32]);
        assert_insn!(r#"f64.reinterpret_i64"#, [F64ReinterpretI64]);
    }
}
