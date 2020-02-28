use crate::lexer::{LexError, Lexer, Token};
use std::fmt;
use std::mem;
use wain_ast::*;

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
}

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

    pub fn parse_all(&mut self) -> Result<'a, SyntaxTree<'a>> {
        let module = self.parse()?;
        Ok(SyntaxTree { module })
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

    fn maybe_eof<'p>(
        &'p self,
        lexed: Option<&'p <Lexer<'a> as Iterator>::Item>,
        expected: &'static str,
    ) -> Result<'a, (&Token<'a>, usize)> {
        match lexed {
            Some(Ok((tok, offset))) => Ok((tok, *offset)),
            Some(Err(err)) => Err(err.clone())?,
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
            (tok, offset) => return self.unexpected_token(tok.clone(), expected, offset),
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
}

macro_rules! match_token {
    ($parser:ident, $expected:expr, $pattern:pat => $ret:expr) => {
        match $parser.next_token($expected)? {
            ($pattern, offset) => ($ret, offset),
            (tok, offset) => {
                return $parser.unexpected_token(tok, $expected, offset);
            }
        }
    };
    ($parser:ident, $expected:expr, $pattern:pat) => {
        match_token!($parser, $expected, $pattern => ())
    };
}

pub trait Parse<'a>: Sized {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self>;
}

// https://webassembly.github.io/spec/core/text/modules.html#text-module
impl<'a> Parse<'a> for Module<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        // https://webassembly.github.io/spec/core/text/modules.html#text-module
        let (_, start) = match_token!(parser, "left paren for module", Token::LParen);
        match_token!(parser, "'module' keyword", Token::Keyword("module"));
        let (ident, _) = match_token!(parser, "identifier for module", Token::Ident(id) => id);

        let mut types = vec![];

        loop {
            match parser.peek("closing paren for module")? {
                (Token::RParen, _) => {
                    parser.eat_token();
                    break;
                }
                _ => match parser.parse()? {
                    ModuleField::Type(ty) => types.push(ty),
                },
            }
        }

        Ok(Module {
            start,
            ident,
            types,
        })
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-modulefield
impl<'a> Parse<'a> for ModuleField<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let (keyword, offset) = parser.lookahead_keyword("keyword for module field")?;
        match keyword {
            "type" => Ok(ModuleField::Type(parser.parse()?)),
            kw => parser.error(ParseErrorKind::UnexpectedKeyword(kw), offset),
        }
    }
}

// https://webassembly.github.io/spec/core/text/modules.html#text-typedef
impl<'a> Parse<'a> for TypeDef<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let (_, start) = match_token!(parser, "left paren for type", Token::LParen);
        let id = parser.maybe_ident("identifier for type")?;
        let ty = parser.parse()?;
        Ok(TypeDef { start, id, ty })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-functype
impl<'a> Parse<'a> for FuncType<'a> {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let (_, start) = match_token!(parser, "left paren for functype", Token::LParen);
        match_token!(parser, "'func' keyword", Token::Keyword("func"));

        let mut params = vec![];
        loop {
            if parser.lookahead_keyword("param keyword in functype")?.0 != "param" {
                break;
            }
            params.push(parser.parse()?);
        }

        let mut results = vec![];
        loop {
            if parser.lookahead_keyword("result keyword in functype")?.0 != "result" {
                break;
            }
            results.push(parser.parse()?);
        }

        match_token!(parser, "closing paren for functype", Token::RParen);
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
        let (_, start) = match_token!(parser, "left paren for param", Token::LParen);
        match_token!(parser, "'param' keyword", Token::Keyword("param"));
        let id = parser.maybe_ident("identifier for param")?;
        let ty = parser.parse()?;
        match_token!(parser, "closing paren for param", Token::RParen);
        Ok(Param { start, id, ty })
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-valtype
impl<'a> Parse<'a> for ValType {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let expected = "identifier for value type";
        match parser.peek(expected)? {
            (Token::Ident("i32"), _) => Ok(ValType::I32),
            (Token::Ident("i64"), _) => Ok(ValType::I64),
            (Token::Ident("f32"), _) => Ok(ValType::F32),
            (Token::Ident("f64"), _) => Ok(ValType::F64),
            (Token::Ident(id), offset) => parser.error(ParseErrorKind::InvalidValType(*id), offset),
            _ => {
                let (tok, offset) = parser.next_token(expected)?;
                parser.unexpected_token(tok, expected, offset)
            }
        }
    }
}

// https://webassembly.github.io/spec/core/text/types.html#text-result
impl<'a> Parse<'a> for FuncResult {
    fn parse(parser: &mut Parser<'a>) -> Result<'a, Self> {
        let (_, start) = match_token!(parser, "left paren for function result type", Token::LParen);
        match_token!(parser, "'result' keyword", Token::Keyword("result"));
        let ty = parser.parse()?;
        match_token!(
            parser,
            "closing paren for function result type",
            Token::RParen
        );
        Ok(FuncResult { start, ty })
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
        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
        assert_eq!(i.next(), None);
        assert_eq!(i.peek(), None);
        assert_eq!(i.lookahead(), None);
    }
}
