use crate::error::{Error, ErrorKind, Result};
use crate::wast::*;
use std::char;
use std::f32;
use std::f64;
use wain_syntax_text::lexer::{Float, Lexer, NumBase, Sign, Token};
use wain_syntax_text::parser::LookAhead;

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

trait Parse<'source>: Sized {
    fn parse(parser: &mut Parser<'source>) -> Result<'source, Self>;
}

// Parse {string}
impl<'s> Parse<'s> for String {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let s = expect!(parser, Token::String(s) => s);
        parser.parse_escaped_text(s)
    }
}

// Parse (module quote ...) or (module binary ...)
impl<'s> Parse<'s> for EmbeddedModule {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::LParen);
        let start = parser.current_pos;
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

impl<'s> Parse<'s> for Const {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::LParen);
        let kw = expect!(parser, Token::Keyword(k) => k);

        macro_rules! parse_int_fn {
            ($name:ident, $ty:ty) => {
                fn $name<'s>(
                    parser: &mut Parser<'s>,
                    sign: Sign,
                    base: NumBase,
                    digits: &'s str,
                ) -> Result<'s, $ty> {
                    let parsed = if base == NumBase::Hex {
                        <$ty>::from_str_radix(&digits.replace('_', ""), 16)
                    } else {
                        digits.replace('_', "").parse()
                    };

                    match parsed {
                        Ok(i) => Ok(sign.apply(i)),
                        Err(e) => parser.fail(ErrorKind::InvalidInt {
                            ty: stringify!($ty),
                            err: e,
                        }),
                    }
                }
            };
        }
        parse_int_fn!(parse_i32, i32);
        parse_int_fn!(parse_i64, i64);

        macro_rules! parse_float_fn {
            ($name:ident, $ty:ty) => {
                fn $name<'s>(
                    parser: &mut Parser<'s>,
                    sign: Sign,
                    base: NumBase,
                    frac: &'s str,
                    exp: Option<(Sign, &'s str)>,
                ) -> Result<'s, $ty> {
                    if base == NumBase::Dec {
                        let mut s = frac.replace('_', "");
                        if let Some((sign, exp)) = exp {
                            s.push('E');
                            if sign == Sign::Minus {
                                s.push('-');
                            }
                            s.push_str(&exp.replace('_', ""));
                        }
                        return match s.parse::<$ty>() {
                            Ok(f) => Ok(sign.apply(f)),
                            Err(e) => parser.fail(ErrorKind::InvalidFloat {
                                ty: stringify!($ty),
                                err: e,
                            }),
                        };
                    }

                    // Parse hex float
                    let mut f = 0.0;
                    let mut chars = frac.chars();

                    while let Some(c) = chars.next() {
                        if let Some(u) = c.to_digit(16) {
                            f = f * 16.0 + u as $ty;
                        } else if c == '.' {
                            break;
                        } else if c == '_' {
                            continue;
                        } else {
                            return parser.fail(ErrorKind::InvalidHexFloat {
                                ty: stringify!($ty),
                            });
                        }
                    }

                    let mut step = 16.0;
                    while let Some(c) = chars.next() {
                        if let Some(u) = c.to_digit(16) {
                            f += u as $ty / step;
                            step *= 16.0;
                        } else if c == '_' {
                            continue;
                        } else {
                            return parser.fail(ErrorKind::InvalidHexFloat {
                                ty: stringify!($ty),
                            });
                        }
                    }

                    // digits are decimal number e.g. p+1234
                    if let Some((sign, digits)) = exp {
                        let i = match digits.replace('_', "").parse::<i32>() {
                            Ok(i) => sign.apply(i),
                            Err(_) => {
                                return parser.fail(ErrorKind::InvalidHexFloat {
                                    ty: stringify!($ty),
                                })
                            }
                        };
                        let exp = (2.0 as $ty).powi(i);
                        f *= exp;
                    }

                    Ok(sign.apply(f))
                }
            };
        }
        parse_float_fn!(parse_f32, f32);
        parse_float_fn!(parse_f64, f64);

        let c = match kw {
            "i32.const" => match parser.consume()? {
                Some(Token::Int(s, b, d)) => Const::I32(parse_i32(parser, s, b, d)?),
                _ => return parser.unexpected("i32 value"),
            },
            "i64.const" => match parser.consume()? {
                Some(Token::Int(s, b, d)) => Const::I64(parse_i64(parser, s, b, d)?),
                _ => return parser.unexpected("i64 value"),
            },
            "f32.const" => match parser.consume()? {
                Some(Token::Keyword("nan:canonical")) => Const::CanonicalNan,
                Some(Token::Keyword("nan:arithmetic")) => Const::ArithmeticNan,
                Some(Token::Int(s, b, d)) => Const::F32(parse_i64(parser, s, b, d)? as f32),
                Some(Token::Float(s, Float::Nan(_))) => Const::F32(s.apply(f32::NAN)),
                Some(Token::Float(Sign::Plus, Float::Inf)) => Const::F32(f32::INFINITY),
                Some(Token::Float(Sign::Minus, Float::Inf)) => Const::F32(f32::NEG_INFINITY),
                Some(Token::Float(sign, Float::Val { base, frac, exp })) => {
                    Const::F32(parse_f32(parser, sign, base, frac, exp)?)
                }
                _ => return parser.unexpected("f32 value"),
            },
            "f64.const" => match parser.consume()? {
                Some(Token::Keyword("nan:canonical")) => Const::CanonicalNan,
                Some(Token::Keyword("nan:arithmetic")) => Const::ArithmeticNan,
                Some(Token::Int(s, b, d)) => Const::F64(parse_i64(parser, s, b, d)? as f64),
                Some(Token::Float(s, Float::Nan(_))) => Const::F64(s.apply(f64::NAN)),
                Some(Token::Float(Sign::Plus, Float::Inf)) => Const::F64(f64::INFINITY),
                Some(Token::Float(Sign::Minus, Float::Inf)) => Const::F64(f64::NEG_INFINITY),
                Some(Token::Float(sign, Float::Val { base, frac, exp })) => {
                    Const::F64(parse_f64(parser, sign, base, frac, exp)?)
                }
                _ => return parser.unexpected("f64 value"),
            },
            _ => return parser.unexpected("t.const for constant"),
        };

        expect!(parser, Token::RParen);
        Ok(c)
    }
}

// (invoke {name} {constant}*)
impl<'s> Parse<'s> for Invoke {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::LParen);
        let start = parser.current_pos;

        expect!(parser, Token::Keyword("invoke"));
        let name = parser.parse()?;

        let mut args = vec![];
        while let (Some(Token::LParen), _) = parser.peek()? {
            args.push(parser.parse()?);
        }

        expect!(parser, Token::RParen);
        Ok(Invoke { start, name, args })
    }
}

// (register {string})
impl<'s> Parse<'s> for Register {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::LParen);
        let start = parser.current_pos;

        expect!(parser, Token::Keyword("register"));
        let name = parser.parse()?;
        expect!(parser, Token::RParen);

        Ok(Register { start, name })
    }
}

// (assert_return (invoke {name} {constant}*) {constant}?)
impl<'s> Parse<'s> for AssertReturn {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::LParen);
        let start = parser.current_pos;

        expect!(parser, Token::Keyword("assert_return"));
        let invoke = parser.parse()?;

        let expected = if let (Some(Token::LParen), _) = parser.peek()? {
            Some(parser.parse()?)
        } else {
            None
        };

        expect!(parser, Token::RParen);
        Ok(AssertReturn {
            start,
            invoke,
            expected,
        })
    }
}

// (assert_trap (invoke {name} {constant}*) {string})
impl<'s> Parse<'s> for AssertTrap {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::LParen);
        let start = parser.current_pos;

        expect!(parser, Token::Keyword("assert_trap"));
        let invoke = parser.parse()?;
        let expected = parser.parse()?;

        expect!(parser, Token::RParen);
        Ok(AssertTrap {
            start,
            invoke,
            expected,
        })
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

        let s = r#"
          (module
            (type (func))
            (func $f (type 0))
            (memory 0)
            (start 0)
          )
        "#;
        assert!(Parser::new(s, 0).parse::<EmbeddedModule>().is_err());
    }

    #[test]
    fn constants() {
        fn p<'a>(s: &'a str) -> Result<'a, Const> {
            Parser::new(s, 0).parse()
        }

        assert_eq!(p("(i32.const 0)").unwrap(), Const::I32(0));
        assert_eq!(p("(i32.const 123)").unwrap(), Const::I32(123));
        assert_eq!(p("(i32.const 0xedf)").unwrap(), Const::I32(0xedf));
        assert_eq!(p("(i32.const -123)").unwrap(), Const::I32(-123));

        assert_eq!(p("(i64.const 0)").unwrap(), Const::I64(0));
        assert_eq!(p("(i64.const 123)").unwrap(), Const::I64(123));
        assert_eq!(p("(i64.const 0xedf)").unwrap(), Const::I64(0xedf));
        assert_eq!(p("(i64.const -123)").unwrap(), Const::I64(-123));

        assert_eq!(p("(f32.const 0)").unwrap(), Const::F32(0.0));
        assert_eq!(p("(f32.const 123)").unwrap(), Const::F32(123.0));
        assert_eq!(p("(f32.const 0xedf)").unwrap(), Const::F32(0xedf as f32));
        assert_eq!(p("(f32.const -123)").unwrap(), Const::F32(-123.0));
        assert_eq!(p("(f32.const 0.0)").unwrap(), Const::F32(0.0));
        assert_eq!(p("(f32.const 123.456)").unwrap(), Const::F32(123.456));
        assert_eq!(p("(f32.const 1.23e10)").unwrap(), Const::F32(1.23e10));
        assert_eq!(p("(f32.const 0x12.34)").unwrap(), Const::F32(18.203125));
        assert_eq!(p("(f32.const 0x12.34p2)").unwrap(), Const::F32(72.8125));
        assert_eq!(p("(f32.const 0x12.34p-2)").unwrap(), Const::F32(4.55078125));
        assert_eq!(p("(f32.const -123.456)").unwrap(), Const::F32(-123.456));
        assert_eq!(p("(f32.const -1.23e10)").unwrap(), Const::F32(-1.23e10));
        assert_eq!(p("(f32.const -0x12.34)").unwrap(), Const::F32(-18.203125));
        assert_eq!(p("(f32.const -0x12.34p2)").unwrap(), Const::F32(-72.8125));
        assert_eq!(
            p("(f32.const -0x12.34p-2)").unwrap(),
            Const::F32(-4.55078125)
        );

        assert_eq!(p("(f64.const 0)").unwrap(), Const::F64(0.0));
        assert_eq!(p("(f64.const 123)").unwrap(), Const::F64(123.0));
        assert_eq!(p("(f64.const 0xedf)").unwrap(), Const::F64(0xedf as f64));
        assert_eq!(p("(f64.const -123)").unwrap(), Const::F64(-123.0));
        assert_eq!(p("(f64.const 0.0)").unwrap(), Const::F64(0.0));
        assert_eq!(p("(f64.const 123.456)").unwrap(), Const::F64(123.456));
        assert_eq!(p("(f64.const 1.23e10)").unwrap(), Const::F64(1.23e10));
        assert_eq!(p("(f64.const 0x12.34)").unwrap(), Const::F64(18.203125));
        assert_eq!(p("(f64.const 0x12.34p2)").unwrap(), Const::F64(72.8125));
        assert_eq!(p("(f64.const 0x12.34p-2)").unwrap(), Const::F64(4.55078125));
        assert_eq!(p("(f64.const -123.456)").unwrap(), Const::F64(-123.456));
        assert_eq!(p("(f64.const -1.23e10)").unwrap(), Const::F64(-1.23e10));
        assert_eq!(p("(f64.const -0x12.34)").unwrap(), Const::F64(-18.203125));
        assert_eq!(p("(f64.const -0x12.34p2)").unwrap(), Const::F64(-72.8125));
        assert_eq!(
            p("(f64.const -0x12.34p-2)").unwrap(),
            Const::F64(-4.55078125)
        );

        let f = p("(f32.const nan)").unwrap();
        assert!(matches!(f, Const::F32(f) if f.is_nan()));
        let f = p("(f32.const -nan)").unwrap();
        assert!(matches!(f, Const::F32(f) if f.is_nan()));
        let f = p("(f32.const nan:0x12)").unwrap();
        assert!(matches!(f, Const::F32(f) if f.is_nan()));
        let f = p("(f32.const nan:canonical)").unwrap();
        assert!(matches!(f, Const::CanonicalNan));
        let f = p("(f32.const nan:arithmetic)").unwrap();
        assert!(matches!(f, Const::ArithmeticNan));
        let f = p("(f32.const inf)").unwrap();
        assert!(matches!(f, Const::F32(f32::INFINITY)));
        let f = p("(f32.const -inf)").unwrap();
        assert!(matches!(f, Const::F32(f32::NEG_INFINITY)));

        let f = p("(f64.const nan)").unwrap();
        assert!(matches!(f, Const::F64(f) if f.is_nan()));
        let f = p("(f64.const -nan)").unwrap();
        assert!(matches!(f, Const::F64(f) if f.is_nan()));
        let f = p("(f64.const nan:0x12)").unwrap();
        assert!(matches!(f, Const::F64(f) if f.is_nan()));
        let f = p("(f64.const nan:canonical)").unwrap();
        assert!(matches!(f, Const::CanonicalNan));
        let f = p("(f64.const nan:arithmetic)").unwrap();
        assert!(matches!(f, Const::ArithmeticNan));
        let f = p("(f64.const inf)").unwrap();
        assert!(matches!(f, Const::F64(f64::INFINITY)));
        let f = p("(f64.const -inf)").unwrap();
        assert!(matches!(f, Const::F64(f64::NEG_INFINITY)));
    }

    #[test]
    fn invoke() {
        let i: Invoke = Parser::new(r#"(invoke "foo")"#, 0).parse().unwrap();
        assert_eq!(i.name, "foo");
        assert!(i.args.is_empty());

        let i: Invoke = Parser::new(r#"(invoke "foo" (i32.const 123) (f64.const 1.23))"#, 0)
            .parse()
            .unwrap();
        assert_eq!(i.name, "foo");
        assert_eq!(i.args.len(), 2);
        assert_eq!(i.args[0], Const::I32(123));
        assert_eq!(i.args[1], Const::F64(1.23));
    }

    #[test]
    fn register() {
        let r: Register = Parser::new(r#"(register "foo")"#, 0).parse().unwrap();
        assert_eq!(r.name, "foo");
    }

    #[test]
    fn assert_return() {
        let a: AssertReturn = Parser::new(
            r#"(assert_return
              (invoke "8u_good1" (i32.const 0))
              (i32.const 97)
            )"#,
            0,
        )
        .parse()
        .unwrap();

        assert_eq!(a.invoke.name, "8u_good1");
        assert_eq!(a.invoke.args.len(), 1);
        assert_eq!(a.invoke.args[0], Const::I32(0));
        assert_eq!(a.expected, Some(Const::I32(97)));

        let a: AssertReturn = Parser::new(r#"(assert_return (invoke "type-i32"))"#, 0)
            .parse()
            .unwrap();

        assert_eq!(a.invoke.name, "type-i32");
        assert!(a.invoke.args.is_empty());
        assert_eq!(a.expected, None);
    }

    #[test]
    fn assert_trap() {
        let a: AssertTrap = Parser::new(
            r#"(assert_trap (invoke "32_good5" (i32.const 65508)) "out of bounds memory access")"#,
            0,
        )
        .parse()
        .unwrap();

        assert_eq!(a.invoke.name, "32_good5");
        assert_eq!(a.invoke.args.len(), 1);
        assert_eq!(a.invoke.args[0], Const::I32(65508));
        assert_eq!(a.expected, "out of bounds memory access");
    }
}
