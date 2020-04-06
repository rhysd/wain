use crate::error::{Error, ErrorKind, Result};
use crate::wast::*;
use std::char;
use wain_syntax_text::lexer::{Lexer, Token};
use wain_syntax_text::parser::LookAhead;

pub struct Parser<'source> {
    source: &'source str,
    offset: usize,
    tokens: LookAhead<Lexer<'source>>,
    current_pos: usize,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str, base_offset: usize) -> Self {
        Parser {
            source,
            offset: base_offset,
            tokens: LookAhead::new(Lexer::new(source)),
            current_pos: 0,
        }
    }

    fn consume(&mut self) -> Result<'s, Option<Token<'s>>> {
        match self.tokens.next() {
            Some(Ok((tok, off))) => {
                self.current_pos = off;
                Ok(Some(tok))
            }
            Some(Err(err)) => Err(err.into()),
            None => {
                self.current_pos = self.source.len();
                Ok(None)
            }
        }
    }

    fn peek(&mut self) -> Result<'s, (Option<&Token<'s>>, Option<&Token<'s>>)> {
        let t1 = match self.tokens.peek() {
            Some(Ok((t, _))) => Some(t),
            Some(Err(e)) => return Err(e.clone().into()),
            None => None,
        };
        let t2 = match self.tokens.lookahead() {
            Some(Ok((t, _))) => Some(t),
            Some(Err(e)) => return Err(e.clone().into()),
            None => None,
        };
        Ok((t1, t2))
    }

    fn error(&self, kind: ErrorKind<'s>) -> Box<Error<'s>> {
        let pos = self.offset + self.current_pos;
        Error::new(kind, self.source, pos)
    }

    fn fail<T>(&self, kind: ErrorKind<'s>) -> Result<'s, T> {
        Err(self.error(kind))
    }

    fn unexpected<T>(&self, expected: &'static str) -> Result<'s, T> {
        self.fail(ErrorKind::Unexpected { expected })
    }

    pub fn parse<P: Parse<'s>>(&mut self) -> Result<'s, P> {
        Parse::parse(self)
    }

    fn parse_escaped(&self, s: &'s str) -> Result<'s, Vec<u8>> {
        let mut buf = vec![];
        let mut chars = s.char_indices();
        while let Some((i, c)) = chars.next() {
            if c != '\\' {
                let mut b = [0; 4];
                buf.extend_from_slice(c.encode_utf8(&mut b).as_bytes());
            } else {
                // Note: Lexer guarantees that at least one char follows after '\'
                match chars.next().unwrap().1 {
                    't' => buf.push(b'\t'),
                    'n' => buf.push(b'\n'),
                    'r' => buf.push(b'\r'),
                    '"' => buf.push(b'"'),
                    '\'' => buf.push(b'\''),
                    '\\' => buf.push(b'\\'),
                    'u' => {
                        match chars.next() {
                            Some((i, '{')) => {
                                let start = i + 1; // next to '{'
                                let end = loop {
                                    match chars.next() {
                                        Some((i, '}')) => break i,
                                        Some(_) => continue,
                                        None => {
                                            return self.fail(ErrorKind::InvalidStringLiteral {
                                                lit: s,
                                                reason: "invalid \\u{xxxx} format",
                                            });
                                        }
                                    }
                                };
                                if let Some(c) = u32::from_str_radix(&s[start..end], 16)
                                    .ok()
                                    .and_then(char::from_u32)
                                {
                                    let mut b = [0; 4];
                                    buf.extend_from_slice(c.encode_utf8(&mut b).as_bytes());
                                } else {
                                    return self.fail(ErrorKind::InvalidStringLiteral {
                                        lit: s,
                                        reason: "invalid code point in \\u{xxxx}",
                                    });
                                }
                            }
                            _ => {
                                return self.fail(ErrorKind::InvalidStringLiteral {
                                    lit: s,
                                    reason: "invalid \\u{xxxx} format",
                                })
                            }
                        }
                    }
                    c => {
                        let hi = c.to_digit(16);
                        let lo = chars.next().and_then(|(_, c)| c.to_digit(16));
                        match (hi, lo) {
                            (Some(hi), Some(lo)) => {
                                buf.push((hi * 16 + lo) as u8);
                            }
                            _ => {
                                return self.fail(ErrorKind::InvalidStringLiteral {
                                    lit: s,
                                    reason: "invalid \\XX format",
                                })
                            }
                        }
                    }
                }
            }
        }
        Ok(buf)
    }

    fn parse_escaped_text(&self, s: &'s str) -> Result<'s, String> {
        let bytes = self.parse_escaped(s)?;
        match String::from_utf8(bytes) {
            Ok(s) => Ok(s),
            Err(e) => self.fail(ErrorKind::Utf8Error(e)),
        }
    }
}

macro_rules! expect {
    ($parser:ident, $tok:pat if $cond:expr => $ret:expr ) => {
        match $parser.consume()? {
            Some($tok) if $cond => $ret,
            _ => return $parser.unexpected(stringify!($tok if $cond)),
        }
    };
    ($parser:ident, $tok:pat => $ret:expr) => {
        expect!($parser, $tok if true => $ret)
    };
    ($parser:ident, $tok:pat if $cond:expr) => {
        expect!($parser, $tok if $cond => ())
    };
    ($parser:ident, $tok:pat) => {
        expect!($parser, $tok => ())
    };
}

trait Parse<'source>: Sized {
    fn parse(parser: &mut Parser<'source>) -> Result<'source, Self>;
}

impl<'s> Parse<'s> for EmbeddedModule {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.current_pos;
        expect!(parser, Token::LParen);
        expect!(parser, Token::Keyword("module"));

        let kw = expect!(parser, Token::Keyword(kw) if kw == "quote" || kw == "binary" => kw);
        match kw {
            "quote" => {
                let mut text = String::new();
                loop {
                    match parser.consume()? {
                        Some(Token::String(s)) => {
                            text.push_str(&parser.parse_escaped_text(s)?);
                        }
                        Some(Token::RParen) => {
                            return Ok(EmbeddedModule {
                                start,
                                embedded: Embedded::Quote(text),
                            });
                        }
                        _ => return parser.unexpected("string for module body or ending ')'"),
                    }
                }
            }
            "binary" => {
                let mut bin = vec![];
                loop {
                    match parser.consume()? {
                        Some(Token::String(s)) => {
                            bin.append(&mut parser.parse_escaped(s)?);
                        }
                        Some(Token::RParen) => {
                            return Ok(EmbeddedModule {
                                start,
                                embedded: Embedded::Binary(bin),
                            });
                        }
                        _ => return parser.unexpected("string for module body or ending ')'"),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn embedded_module() {
        let s = r#"
          (module binary
            "\00asm" "\01\00\00\00"
            "\05\04\01"                          ;; Memory section with 1 entry
            "\00\82\00"                          ;; no max, minimum 2
          )
        "#;
        let m: EmbeddedModule = Parser::new(s, 0).parse().unwrap();
        let b: &[u8] = &[0, b'a', b's', b'm', 0x1, 0, 0, 0, 0x5, 0x4, 1, 0, 0x82, 0];
        match m.embedded {
            Embedded::Binary(bin) => assert_eq!(bin.as_slice(), b),
            Embedded::Quote(s) => panic!("not a binary: {}", s),
        }

        let s = r#"
          (module quote
            "(memory $foo 1)"
            "(memory $foo 1)")
        "#;
        let m: EmbeddedModule = Parser::new(s, 0).parse().unwrap();
        let expected = r#"(memory $foo 1)(memory $foo 1)"#;
        match m.embedded {
            Embedded::Quote(s) => assert_eq!(&s, expected),
            Embedded::Binary(b) => panic!("not a text: {:?}", b),
        }
    }
}
