use crate::error::{Error, ParseKind, Result};
use crate::wast::*;
use std::borrow::Cow;
use std::f32;
use std::f64;
use std::mem;
use wain_ast as ast;
use wain_syntax_text::lexer::{Float, Lexer, NumBase, Sign, Token};
use wain_syntax_text::parser::{LookAhead, Parser as WatParser};
use wain_syntax_text::source::TextSource;
use wain_syntax_text::wat2wasm::wat2wasm;

// Empty lexer for substitute pattern
fn empty_lexer<'s>() -> LookAhead<Lexer<'s>> {
    LookAhead::new(Lexer::new(""))
}

macro_rules! expect {
    ($parser:ident, $tok:pat if $cond:expr => $ret:expr ) => {
        match $parser.consume()? {
            Some($tok) if $cond => $ret,
            x => return $parser.unexpected_token(x, stringify!($tok if $cond)),
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
    tokens: LookAhead<Lexer<'source>>,
    current_pos: usize,
    ignored_error: Option<Box<Error<'source>>>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Parser {
            source,
            tokens: LookAhead::new(Lexer::new(source)),
            current_pos: 0,
            ignored_error: None,
        }
    }

    fn clone_lexer(&self) -> LookAhead<Lexer<'s>> {
        self.tokens.clone()
    }

    fn replace_lexer(&mut self, new: LookAhead<Lexer<'s>>) -> LookAhead<Lexer<'s>> {
        mem::replace(&mut self.tokens, new)
    }

    fn with_lexer<T, F>(&mut self, pred: F) -> Result<'s, T>
    where
        F: FnOnce(LookAhead<Lexer<'s>>) -> Result<'s, (T, LookAhead<Lexer<'s>>)>,
    {
        // Substitute pattern to give rent lexer temporarily
        let lexer = self.replace_lexer(empty_lexer());
        let (ret, lexer) = pred(lexer)?;
        self.replace_lexer(lexer);
        Ok(ret)
    }

    fn is_done(&self) -> Result<'s, bool> {
        let (t, _) = self.peek()?;
        Ok(t.is_none())
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

    fn peek(&self) -> Result<'s, (Option<&Token<'s>>, Option<&Token<'s>>)> {
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

    fn error(&mut self, kind: ParseKind<'s>) -> Box<Error<'s>> {
        let mut err = Error::parse_error(kind, self.source, self.current_pos);
        if let Some(mut ignored) = self.ignored_error.take() {
            ignored.prev_error = None; // Do not chain all errors
            err.prev_error = Some(ignored);
        }
        err
    }

    fn fail<T>(&mut self, kind: ParseKind<'s>) -> Result<'s, T> {
        Err(self.error(kind))
    }

    fn unexpected<T, E: Into<Cow<'static, str>>>(&mut self, expected: E) -> Result<'s, T> {
        self.fail(ParseKind::Unexpected {
            expected: expected.into(),
            token: None,
        })
    }

    fn unexpected_token<T, E: Into<Cow<'static, str>>>(
        &mut self,
        token: Option<Token<'s>>,
        expected: E,
    ) -> Result<'s, T> {
        let expected = expected.into();
        self.fail(ParseKind::Unexpected { expected, token })
    }

    pub fn parse<P: Parse<'s>>(&mut self) -> Result<'s, P> {
        Parse::parse(self)
    }

    fn parse_escaped_text(&mut self, bytes: Vec<u8>) -> Result<'s, String> {
        match String::from_utf8(bytes) {
            Ok(s) => Ok(s),
            Err(e) => self.fail(ParseKind::Utf8Error(e)),
        }
    }

    fn parse_start(&mut self, directive: &'static str) -> Result<'s, usize> {
        match self.consume()? {
            Some(Token::LParen) => {
                let start = self.current_pos;
                match self.consume()? {
                    Some(Token::Keyword(k)) if k == directive => Ok(start),
                    x => self.unexpected_token(x, format!("keyword for '{}'", directive)),
                }
            }
            x => self.unexpected_token(x, format!("'(' for '{}'", directive)),
        }
    }

    fn parse_maybe_id(&mut self) -> Result<'s, Option<&'s str>> {
        if let (Some(Token::Ident(id)), _) = self.peek()? {
            let id = *id;
            self.tokens.next();
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }
}

pub trait Parse<'source>: Sized {
    fn parse(parser: &mut Parser<'source>) -> Result<'source, Self>;
}

// Parse {string}
impl<'s> Parse<'s> for String {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        expect!(parser, Token::String(s, _) => parser.parse_escaped_text(s.into_owned()))
    }
}

// Parse (module quote ...) or (module binary ...)
impl<'s> Parse<'s> for EmbeddedModule {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("module")?;

        // ID like (module $m quote ...). It seems unused so skipped here
        if let (Some(Token::Ident(_)), _) = parser.peek()? {
            parser.tokens.next();
        }

        let kw = expect!(parser, Token::Keyword(kw) if kw == "quote" || kw == "binary" => kw);
        match kw {
            "quote" => {
                let mut text = String::new();
                loop {
                    match parser.consume()? {
                        Some(Token::String(s, _)) => {
                            text.push_str(&parser.parse_escaped_text(s.into_owned())?);
                        }
                        Some(Token::RParen) => {
                            return Ok(EmbeddedModule {
                                start,
                                src: EmbeddedSrc::Quote(text),
                            });
                        }
                        x => return parser.unexpected_token(x, "string for module quote or ')'"),
                    }
                }
            }
            "binary" => {
                let mut bin = vec![];
                loop {
                    match parser.consume()? {
                        Some(Token::String(ref s, _)) => {
                            bin.extend_from_slice(s);
                        }
                        Some(Token::RParen) => {
                            return Ok(EmbeddedModule {
                                start,
                                src: EmbeddedSrc::Binary(bin),
                            });
                        }
                        x => return parser.unexpected_token(x, "string for module binary or ')'"),
                    }
                }
            }
            _ => unreachable!(),
        }
    }
}

impl<'s> Parse<'s> for Const {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        macro_rules! parse_int_fn {
            ($name:ident, $int:ty, $uint:ty) => {
                fn $name<'s>(parser: &mut Parser<'s>, sign: Sign, base: NumBase, digits: &'s str) -> Result<'s, $int> {
                    // Operand of iNN.const is in range of iNN::MIN <= i <= uNN::MAX.
                    // When the value is over iNN::MAX, it is parsed as uNN and bitcasted to iNN.
                    let parsed = if base == NumBase::Hex {
                        <$uint>::from_str_radix(&digits.replace('_', ""), 16)
                    } else {
                        digits.replace('_', "").parse()
                    };

                    match parsed {
                        Ok(u) if sign == Sign::Plus => Ok(u as $int),
                        Ok(u) if u <= <$int>::MAX as $uint + 1 => Ok(u.wrapping_neg() as $int),
                        Ok(u) => parser.fail(ParseKind::TooSmallInt {
                            ty: stringify!($int),
                            digits: u as u64,
                        }),
                        Err(e) => parser.fail(ParseKind::InvalidInt {
                            ty: stringify!($int),
                            err: e,
                        }),
                    }
                }
            };
        }
        parse_int_fn!(parse_i32, i32, u32);
        parse_int_fn!(parse_i64, i64, u64);

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
                            Err(e) => parser.fail(ParseKind::InvalidFloat {
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
                            return parser.fail(ParseKind::InvalidHexFloat {
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
                            return parser.fail(ParseKind::InvalidHexFloat {
                                ty: stringify!($ty),
                            });
                        }
                    }

                    // digits are decimal number e.g. p+1234
                    if let Some((sign, digits)) = exp {
                        let i = match digits.replace('_', "").parse::<i32>() {
                            Ok(i) => i,
                            Err(_) => {
                                return parser.fail(ParseKind::InvalidHexFloat {
                                    ty: stringify!($ty),
                                })
                            }
                        };
                        let step: $ty = if sign == Sign::Plus { 2.0 } else { 0.5 };
                        let exp = step.powi(i);
                        f *= exp;
                    }

                    Ok(sign.apply(f))
                }
            };
        }
        parse_float_fn!(parse_f32, f32);
        parse_float_fn!(parse_f64, f64);

        expect!(parser, Token::LParen);
        let kw = expect!(parser, Token::Keyword(k) => k);

        let c = match kw {
            "i32.const" => match parser.consume()? {
                Some(Token::Int(s, b, d)) => Const::I32(parse_i32(parser, s, b, d)?),
                x => return parser.unexpected_token(x, "i32 value"),
            },
            "i64.const" => match parser.consume()? {
                Some(Token::Int(s, b, d)) => Const::I64(parse_i64(parser, s, b, d)?),
                x => return parser.unexpected_token(x, "i64 value"),
            },
            "f32.const" => match parser.consume()? {
                Some(Token::Keyword("nan:canonical")) => Const::CanonicalNan,
                Some(Token::Keyword("nan:arithmetic")) => Const::ArithmeticNan,
                Some(Token::Int(s, b, d)) => Const::F32(parse_f32(parser, s, b, d, None)?),
                Some(Token::Float(s, Float::Nan(None))) => Const::F32(s.apply(f32::NAN)),
                Some(Token::Float(s, Float::Nan(Some(payload)))) => {
                    // Encode  f32 NaN value via u32 assuming IEEE-754 format for NaN boxing.
                    // Palyload must be
                    //   - within 23bits (fraction of f32 is 23bits)
                    //   - >= 2^(23-1) meant that most significant bit must be 1 (since frac cannot be zero for NaN value)
                    // https://webassembly.github.io/spec/core/syntax/values.html#floating-point
                    let payload_u = parse_i32(parser, Sign::Plus, NumBase::Hex, payload)? as u32;
                    if payload_u == 0 || 0x80_0000 <= payload_u {
                        return parser.fail(ParseKind::InvalidHexFloat { ty: "f32" });
                    }
                    // NaN boxing. 1 <= payload_u < 2^23 and floating point number is in IEEE754 format.
                    // This will encode the payload into fraction of NaN.
                    //   0x{sign}11111111{payload}
                    let exp = 0b1111_1111u32 << 23;
                    Const::F32(s.apply(f32::from_bits(exp | payload_u)))
                }
                Some(Token::Float(Sign::Plus, Float::Inf)) => Const::F32(f32::INFINITY),
                Some(Token::Float(Sign::Minus, Float::Inf)) => Const::F32(f32::NEG_INFINITY),
                Some(Token::Float(sign, Float::Val { base, frac, exp })) => {
                    Const::F32(parse_f32(parser, sign, base, frac, exp)?)
                }
                x => return parser.unexpected_token(x, "f32 value"),
            },
            "f64.const" => match parser.consume()? {
                Some(Token::Keyword("nan:canonical")) => Const::CanonicalNan,
                Some(Token::Keyword("nan:arithmetic")) => Const::ArithmeticNan,
                Some(Token::Int(s, b, d)) => Const::F64(parse_f64(parser, s, b, d, None)?),
                Some(Token::Float(s, Float::Nan(None))) => Const::F64(s.apply(f64::NAN)),
                Some(Token::Float(s, Float::Nan(Some(payload)))) => {
                    // Encode  f64 NaN value via u64 assuming IEEE-754 format for NaN boxing.
                    // Palyload must be
                    //   - within 52bits (fraction of f64 is 52bits)
                    //   - >= 2^(52-1) meant that most significant bit must be 1 (since frac cannot be zero for NaN value)
                    // https://webassembly.github.io/spec/core/syntax/values.html#floating-point
                    let payload_u = parse_i64(parser, Sign::Plus, NumBase::Hex, payload)? as u64;
                    if payload_u == 0 || 0x10_0000_0000_0000 <= payload_u {
                        return parser.fail(ParseKind::InvalidHexFloat { ty: "f64" });
                    }
                    // NaN boxing. 1 <= payload_u < 2^52 and floating point number is in IEEE754 format.
                    // This will encode the payload into fraction of NaN.
                    //   0x{sign}11111111111{payload}
                    let exp = 0b111_1111_1111u64 << 52;
                    Const::F64(s.apply(f64::from_bits(exp | payload_u)))
                }
                Some(Token::Float(Sign::Plus, Float::Inf)) => Const::F64(f64::INFINITY),
                Some(Token::Float(Sign::Minus, Float::Inf)) => Const::F64(f64::NEG_INFINITY),
                Some(Token::Float(sign, Float::Val { base, frac, exp })) => {
                    Const::F64(parse_f64(parser, sign, base, frac, exp)?)
                }
                x => return parser.unexpected_token(x, "f64 value"),
            },
            _ => return parser.unexpected("t.const for constant"),
        };

        expect!(parser, Token::RParen);
        Ok(c)
    }
}

// (invoke {id}? {name} {constant}*)
impl<'s> Parse<'s> for Invoke<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("invoke")?;
        let id = parser.parse_maybe_id()?;
        let name = parser.parse()?;

        let mut args = vec![];
        while let (Some(Token::LParen), _) = parser.peek()? {
            args.push(parser.parse()?);
        }

        expect!(parser, Token::RParen);
        Ok(Invoke { start, id, name, args })
    }
}

// (register {name})
impl<'s> Parse<'s> for Register<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("register")?;
        let name = parser.parse()?;
        let id = parser.parse_maybe_id()?;
        expect!(parser, Token::RParen);
        Ok(Register { start, name, id })
    }
}

// (get {id}? {name})
impl<'s> Parse<'s> for GetGlobal<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("get")?;
        let id = parser.parse_maybe_id()?;
        let name = parser.parse()?;
        expect!(parser, Token::RParen);
        Ok(GetGlobal { start, id, name })
    }
}

// (assert_return (invoke {name} {constant}*) {constant}?)
impl<'s> Parse<'s> for AssertReturn<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("assert_return")?;
        match parser.peek()? {
            (Some(Token::LParen), Some(Token::Keyword("invoke"))) => {
                let invoke = parser.parse()?;
                let expected = if let (Some(Token::LParen), _) = parser.peek()? {
                    Some(parser.parse()?)
                } else {
                    None
                };
                expect!(parser, Token::RParen);
                Ok(AssertReturn::Invoke {
                    start,
                    invoke,
                    expected,
                })
            }
            (Some(Token::LParen), Some(Token::Keyword("get"))) => {
                let get = parser.parse()?;
                let expected = parser.parse()?;
                expect!(parser, Token::RParen);
                Ok(AssertReturn::Global { start, get, expected })
            }
            (Some(Token::LParen), t) | (t, _) => {
                let t = t.cloned();
                parser.unexpected_token(t, "'(invoke' or '(get' for assert_return")
            }
        }
    }
}

// (assert_trap (invoke {name} {constant}*) {string})
impl<'s> Parse<'s> for AssertTrap<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("assert_trap")?;
        let pred = match parser.peek()? {
            (Some(Token::LParen), Some(Token::Keyword("invoke"))) => TrapPredicate::Invoke(parser.parse()?),
            (Some(Token::LParen), Some(Token::Keyword("module"))) => TrapPredicate::Module(parser.parse()?),
            (Some(Token::LParen), t) | (t, _) => {
                let t = t.cloned();
                return parser.unexpected_token(t, "'invoke' or 'module' for assert_trap");
            }
        };
        let expected = parser.parse()?;
        expect!(parser, Token::RParen);
        Ok(AssertTrap { start, pred, expected })
    }
}

// (assert_malformed (module ...) {string})
impl<'s> Parse<'s> for AssertMalformed {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("assert_malformed")?;
        let module = parser.parse()?;
        let expected = parser.parse()?;
        expect!(parser, Token::RParen);
        Ok(AssertMalformed {
            start,
            module,
            expected,
        })
    }
}

// inline module in assert_invalid, assert_trap and assert_unlinkable
impl<'s> Parse<'s> for ast::Root<'s, TextSource<'s>> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        // Check it starts with (module
        match parser.peek()? {
            (Some(Token::LParen), Some(Token::Keyword("module"))) => { /* ok */ }
            (Some(Token::LParen), t) | (t, _) => {
                let t = t.cloned();
                return parser.unexpected_token(t, "starting with '(module' for module argument");
            }
        }

        parser.with_lexer(|lexer| {
            let mut wat_parser = WatParser::with_lexer(lexer);
            let parsed = wat_parser.parse()?; // text -> wat
            let root = wat2wasm(parsed, wat_parser.source())?; // wat -> ast
            Ok((root, wat_parser.into_lexer()))
        })
    }
}

// (assert_invalid (module ...) {string})
impl<'s> Parse<'s> for AssertInvalid<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("assert_invalid")?;
        let wat = parser.parse()?;
        let expected = parser.parse()?;
        expect!(parser, Token::RParen);
        Ok(AssertInvalid { start, wat, expected })
    }
}

// (assert_unlinkable (module ...) {string})
impl<'s> Parse<'s> for AssertUnlinkable<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("assert_unlinkable")?;
        let wat = parser.parse()?;
        let expected = parser.parse()?;
        expect!(parser, Token::RParen);
        Ok(AssertUnlinkable { start, wat, expected })
    }
}

// (assert_unlinkable (module ...) {string})
impl<'s> Parse<'s> for AssertExhaustion<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let start = parser.parse_start("assert_exhaustion")?;
        let invoke = parser.parse()?;
        let expected = parser.parse()?;
        expect!(parser, Token::RParen);
        Ok(AssertExhaustion {
            start,
            invoke,
            expected,
        })
    }
}

impl<'s> Parse<'s> for Command<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let (t1, t2) = parser.peek()?;

        match t1 {
            Some(Token::LParen) => { /* ok */ }
            t => {
                let t = t.cloned();
                return parser.unexpected_token(t, "'(' for start of WAST directive");
            }
        }

        match t2 {
            Some(Token::Keyword("assert_return")) => Ok(Command::AssertReturn(parser.parse()?)),
            Some(Token::Keyword("assert_trap")) => Ok(Command::AssertTrap(parser.parse()?)),
            Some(Token::Keyword("assert_malformed")) => Ok(Command::AssertMalformed(parser.parse()?)),
            Some(Token::Keyword("assert_invalid")) => Ok(Command::AssertInvalid(parser.parse()?)),
            Some(Token::Keyword("assert_unlinkable")) => Ok(Command::AssertUnlinkable(parser.parse()?)),
            Some(Token::Keyword("assert_exhaustion")) => Ok(Command::AssertExhaustion(parser.parse()?)),
            Some(Token::Keyword("register")) => Ok(Command::Register(parser.parse()?)),
            Some(Token::Keyword("invoke")) => Ok(Command::Invoke(parser.parse()?)),
            Some(Token::Keyword("module")) => {
                // `parser.parse::<EmbeddedModule>()` eats tokens. When reaching 'Err(err) => { ... }'
                // clause, `parser`'s lexer is no longer available. To parse from start, remember the
                // lexer before calling `parser.parse::<EmbeddedModule>() by clone.
                // This is mandatory since Wasm parser is LL(1). To avoid the clone, LL(2) is necessary.
                let prev_lexer = parser.clone_lexer();

                match parser.parse::<EmbeddedModule>() {
                    Ok(module) => Ok(Command::EmbeddedModule(module)),
                    Err(err) => {
                        parser.ignored_error = Some(err);
                        // Here parser.lexer already ate some tokens. To parser from
                        let mut wat_parser = WatParser::with_lexer(prev_lexer);
                        let parsed = wat_parser.parse()?; // text -> wat
                        let root = wat2wasm(parsed, wat_parser.source())?; // wat -> ast
                        parser.replace_lexer(wat_parser.into_lexer());
                        Ok(Command::InlineModule(root))
                    }
                }
            }
            t => {
                let t = t.cloned();
                parser.unexpected_token(t, "keyword for WAST directive")
            }
        }
    }
}

impl<'s> Parse<'s> for Script<'s> {
    fn parse(parser: &mut Parser<'s>) -> Result<'s, Self> {
        let mut commands = vec![];
        while !parser.is_done()? {
            commands.push(parser.parse()?);
        }
        let start = commands.first().map(Command::start_pos).unwrap_or(0);
        Ok(Script { start, commands })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs;

    #[test]
    fn embedded_module() {
        let s = r#"
          (module binary
            "\00asm" "\01\00\00\00"
            "\05\04\01"                          ;; Memory section with 1 entry
            "\00\82\00"                          ;; no max, minimum 2
          )
        "#;
        let m: EmbeddedModule = Parser::new(s).parse().unwrap();
        let b: &[u8] = &[0, b'a', b's', b'm', 0x1, 0, 0, 0, 0x5, 0x4, 1, 0, 0x82, 0];
        match m.src {
            EmbeddedSrc::Binary(bin) => assert_eq!(bin.as_slice(), b),
            EmbeddedSrc::Quote(s) => panic!("not a binary: {}", s),
        }

        let s = r#"
          (module quote
            "(memory $foo 1)"
            "(memory $foo 1)")
        "#;
        let m: EmbeddedModule = Parser::new(s).parse().unwrap();
        let expected = r#"(memory $foo 1)(memory $foo 1)"#;
        match m.src {
            EmbeddedSrc::Quote(s) => assert_eq!(&s, expected),
            EmbeddedSrc::Binary(b) => panic!("not a text: {:?}", b),
        }

        // Identifier is ignored
        let s = r#"(module $M1 binary "\00asm\01\00\00\00")"#;
        let _: EmbeddedModule = Parser::new(s).parse().unwrap();

        let s = r#"
          (module
            (type (func))
            (func $f (type 0))
            (memory 0)
            (start 0)
          )
        "#;
        assert!(Parser::new(s).parse::<EmbeddedModule>().is_err());
    }

    #[test]
    fn constants() {
        fn p(s: &str) -> Result<'_, Const> {
            Parser::new(s).parse()
        }

        assert_eq!(p("(i32.const 0)").unwrap(), Const::I32(0));
        assert_eq!(p("(i32.const 123)").unwrap(), Const::I32(123));
        assert_eq!(p("(i32.const 0xedf)").unwrap(), Const::I32(0xedf));
        assert_eq!(p("(i32.const -123)").unwrap(), Const::I32(-123));
        assert_eq!(
            p(r#"(i32.const 2147483647)"#).unwrap(),
            Const::I32(2147483647), // INT32_MAX
        );
        assert_eq!(
            p(r#"(i32.const 0x7fffffff)"#).unwrap(),
            Const::I32(0x7fffffff), // INT32_MAX
        );
        assert_eq!(
            p(r#"(i32.const -2147483648)"#).unwrap(),
            Const::I32(-2147483648), // INT32_MIN
        );
        assert_eq!(
            p(r#"(i32.const -0x80000000)"#).unwrap(),
            Const::I32(-0x80000000), // INT32_MAX
        );
        assert_eq!(
            p(r#"(i32.const 0xfedc6543)"#).unwrap(),
            Const::I32(-19110589), // INT32_MAX < i < UINT32_MAX
        );
        assert_eq!(p(r#"(i32.const 4294967295)"#).unwrap(), Const::I32(-1)); // UINT32_MAX
        assert_eq!(p(r#"(i32.const 0xffffffff)"#).unwrap(), Const::I32(-1)); // UINT32_MAX

        assert_eq!(p("(i64.const 0)").unwrap(), Const::I64(0));
        assert_eq!(p("(i64.const 123)").unwrap(), Const::I64(123));
        assert_eq!(p("(i64.const 0xedf)").unwrap(), Const::I64(0xedf));
        assert_eq!(p("(i64.const -123)").unwrap(), Const::I64(-123));
        assert_eq!(
            p(r#"(i64.const 9223372036854775807)"#).unwrap(),
            Const::I64(9223372036854775807), // INT64_MAX
        );
        assert_eq!(
            p(r#"(i64.const -9223372036854775808)"#).unwrap(),
            Const::I64(-9223372036854775808), // INT64_MIN
        );
        assert_eq!(
            p(r#"(i64.const 0x7fffffffffffffff)"#).unwrap(),
            Const::I64(0x7fffffffffffffff), // INT64_MAX
        );
        assert_eq!(
            p(r#"(i64.const -0x8000000000000000)"#).unwrap(),
            Const::I64(-0x8000000000000000), // INT64_MIN
        );
        assert_eq!(
            p(r#"(i64.const 0x8000000000000000)"#).unwrap(),
            Const::I64(-9223372036854775808), // INT64_MAX + 1
        );
        assert_eq!(
            p(r#"(i64.const 0xffffffffffffffff)"#).unwrap(),
            Const::I64(-1), // UINT64_MAX
        );
        assert_eq!(
            p(r#"(i64.const 18446744073709551615)"#).unwrap(),
            Const::I64(-1), // UINT64_MAX
        );

        assert_eq!(p("(f32.const 0)").unwrap(), Const::F32(0.0));
        assert_eq!(p("(f32.const 123)").unwrap(), Const::F32(123.0));
        assert_eq!(p("(f32.const 0xedf)").unwrap(), Const::F32(0xedf as f32));
        assert_eq!(p("(f32.const -123)").unwrap(), Const::F32(-123.0));
        assert_eq!(p("(f32.const 0.0)").unwrap(), Const::F32(0.0));
        assert_eq!(p("(f32.const 123.456)").unwrap(), Const::F32(123.456));
        assert_eq!(p("(f32.const 1.23e10)").unwrap(), Const::F32(1.23e10));
        assert_eq!(p("(f32.const 0x12.34)").unwrap(), Const::F32(18.203125));
        assert_eq!(p("(f32.const 0x12.34p2)").unwrap(), Const::F32(72.8125));
        assert_eq!(p("(f32.const 0x12.34p-2)").unwrap(), Const::F32(4.550_781_3));
        assert_eq!(p("(f32.const -123.456)").unwrap(), Const::F32(-123.456));
        assert_eq!(p("(f32.const -1.23e10)").unwrap(), Const::F32(-1.23e10));
        assert_eq!(p("(f32.const -0x12.34)").unwrap(), Const::F32(-18.203125));
        assert_eq!(p("(f32.const -0x12.34p2)").unwrap(), Const::F32(-72.8125));
        assert_eq!(p("(f32.const -0x12.34p-2)").unwrap(), Const::F32(-4.550_781_3));

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
        assert_eq!(p("(f64.const -0x12.34p-2)").unwrap(), Const::F64(-4.55078125));

        let f = p("(f32.const nan)").unwrap();
        assert!(matches!(f, Const::F32(f) if f.is_nan()));
        let f = p("(f32.const -nan)").unwrap();
        assert!(matches!(f, Const::F32(f) if f.is_nan()));
        let f = p("(f32.const nan:0x12)").unwrap();
        assert!(matches!(f, Const::F32(f) if f.is_nan()));
        let f = p("(f32.const nan:canonical)").unwrap();
        assert_eq!(f, Const::CanonicalNan);
        let f = p("(f32.const nan:arithmetic)").unwrap();
        assert_eq!(f, Const::ArithmeticNan);
        let f = p("(f32.const inf)").unwrap();
        assert_eq!(f, Const::F32(f32::INFINITY));
        let f = p("(f32.const -inf)").unwrap();
        assert_eq!(f, Const::F32(f32::NEG_INFINITY));

        let f = p("(f64.const nan)").unwrap();
        assert!(matches!(f, Const::F64(f) if f.is_nan()));
        let f = p("(f64.const -nan)").unwrap();
        assert!(matches!(f, Const::F64(f) if f.is_nan()));
        let f = p("(f64.const nan:0x12)").unwrap();
        assert!(matches!(f, Const::F64(f) if f.is_nan()));
        let f = p("(f64.const nan:canonical)").unwrap();
        assert_eq!(f, Const::CanonicalNan);
        let f = p("(f64.const nan:arithmetic)").unwrap();
        assert_eq!(f, Const::ArithmeticNan);
        let f = p("(f64.const inf)").unwrap();
        assert_eq!(f, Const::F64(f64::INFINITY));
        let f = p("(f64.const -inf)").unwrap();
        assert_eq!(f, Const::F64(f64::NEG_INFINITY));
    }

    #[test]
    fn invoke() {
        let i: Invoke<'_> = Parser::new(r#"(invoke "foo")"#).parse().unwrap();
        assert_eq!(i.name, "foo");
        assert!(i.args.is_empty());

        let i: Invoke<'_> = Parser::new(r#"(invoke "foo" (i32.const 123) (f64.const 1.23))"#)
            .parse()
            .unwrap();
        assert_eq!(i.name, "foo");
        assert_eq!(i.args.len(), 2);
        assert_eq!(i.args[0], Const::I32(123));
        assert_eq!(i.args[1], Const::F64(1.23));

        let i: Invoke<'_> = Parser::new(r#"(invoke $Func "e" (i32.const 42))"#).parse().unwrap();
        assert_eq!(i.id, Some("$Func"));
        assert_eq!(i.name, "e");
        assert_eq!(i.args.len(), 1);
        assert_eq!(i.args[0], Const::I32(42));
    }

    #[test]
    fn register() {
        let r: Register<'_> = Parser::new(r#"(register "foo")"#).parse().unwrap();
        assert_eq!(r.name, "foo");
        assert_eq!(r.id, None);
        let r: Register<'_> = Parser::new(r#"(register "foo" $foo)"#).parse().unwrap();
        assert_eq!(r.name, "foo");
        assert_eq!(r.id, Some("$foo"));
    }

    #[test]
    fn assert_return() {
        let a: AssertReturn<'_> = Parser::new(
            r#"(assert_return
              (invoke "8u_good1" (i32.const 0))
              (i32.const 97)
            )"#,
        )
        .parse()
        .unwrap();

        match a {
            AssertReturn::Invoke { invoke, expected, .. } => {
                assert_eq!(invoke.name, "8u_good1");
                assert_eq!(invoke.args.len(), 1);
                assert_eq!(invoke.args[0], Const::I32(0));
                assert_eq!(expected, Some(Const::I32(97)));
            }
            _ => panic!("expected invoke"),
        }

        let a: AssertReturn<'_> = Parser::new(r#"(assert_return (invoke "type-i32"))"#).parse().unwrap();

        match a {
            AssertReturn::Invoke { invoke, expected, .. } => {
                assert_eq!(invoke.name, "type-i32");
                assert!(invoke.args.is_empty());
                assert_eq!(expected, None);
            }
            _ => panic!("expected invoke"),
        }

        let a: AssertReturn<'_> = Parser::new(r#"(assert_return (get "e") (i32.const 42))"#)
            .parse()
            .unwrap();

        match a {
            AssertReturn::Global { get, expected, .. } => {
                assert_eq!(get.id, None);
                assert_eq!(get.name, "e");
                assert_eq!(expected, Const::I32(42));
            }
            _ => panic!("expected global"),
        }

        let a: AssertReturn<'_> = Parser::new(r#"(assert_return (get $Global "e") (i32.const 42))"#)
            .parse()
            .unwrap();

        match a {
            AssertReturn::Global { get, expected, .. } => {
                assert_eq!(get.id, Some("$Global"));
                assert_eq!(get.name, "e");
                assert_eq!(expected, Const::I32(42));
            }
            _ => panic!("expected global"),
        }
    }

    #[test]
    fn assert_trap() {
        let a: AssertTrap<'_> =
            Parser::new(r#"(assert_trap (invoke "32_good5" (i32.const 65508)) "out of bounds memory access")"#)
                .parse()
                .unwrap();

        assert_eq!(a.expected, "out of bounds memory access");
        match a.pred {
            TrapPredicate::Invoke(invoke) => {
                assert_eq!(invoke.name, "32_good5");
                assert_eq!(invoke.args.len(), 1);
                assert_eq!(invoke.args[0], Const::I32(65508));
            }
            _ => panic!("expected invoke"),
        }

        let a: AssertTrap<'_> = Parser::new(
            r#"
            (assert_trap
              (module $hello
                (import "Ms" "memory" (memory 1))
                (func $main (unreachable))
                (start $main)
              )
              "unreachable"
            )
            "#,
        )
        .parse()
        .unwrap();

        assert_eq!(a.expected, "unreachable");
        match a.pred {
            TrapPredicate::Module(root) => {
                assert_eq!(root.module.id, Some("$hello"));
            }
            _ => panic!("expected invoke"),
        }
    }

    #[test]
    fn assert_malformed() {
        let a: AssertMalformed = Parser::new(
            r#"(assert_malformed
              (module quote
                "(module (memory 0) (func (drop (i32.load8_s align=7 (i32.const 0)))))"
              )
              "alignment"
            )"#,
        )
        .parse()
        .unwrap();

        assert!(matches!(a.module.src, EmbeddedSrc::Quote(_)));
        assert_eq!(a.expected, "alignment");

        let a: AssertMalformed = Parser::new(
            r#"(assert_malformed
              (module binary "\00asm" "\01\00\00\00")
              "integer too large"
            )"#,
        )
        .parse()
        .unwrap();

        assert!(matches!(a.module.src, EmbeddedSrc::Binary(_)));
        assert_eq!(a.expected, "integer too large");
    }

    #[test]
    fn assert_invalid() {
        let a: AssertInvalid<'_> = Parser::new(
            r#"(assert_invalid
              (module (memory 0) (func (drop (i32.load8_s align=2 (i32.const 0)))))
              "alignment must not be larger than natural"
            )"#,
        )
        .parse()
        .unwrap();

        assert_eq!(a.expected, "alignment must not be larger than natural");

        let m = a.wat.module;
        assert_eq!(m.memories.len(), 1);
        assert!(matches!(
            &m.memories[0],
            ast::Memory {
                ty: ast::MemType {
                    limit: ast::Limits::From(0)
                },
                import: None,
                ..
            }
        ));

        assert_eq!(m.funcs.len(), 1);
        assert!(matches!(&m.funcs[0], ast::Func {
            idx: 0,
            kind: ast::FuncKind::Body {
                locals,
                expr,
            },
            ..
        } if locals.is_empty() && expr.len() == 3));
    }

    #[test]
    fn assert_unlinkable() {
        let a: AssertUnlinkable<'_> = Parser::new(
            r#"(assert_unlinkable
              (module
                (memory 0)
                (data (i32.const 0) "a")
              )
              "data segment does not fit"
            )"#,
        )
        .parse()
        .unwrap();

        assert_eq!(a.expected, "data segment does not fit");

        let m = a.wat.module;
        assert_eq!(m.memories.len(), 1);
        assert!(matches!(
            &m.memories[0],
            ast::Memory {
                ty: ast::MemType {
                    limit: ast::Limits::From(0)
                },
                import: None,
                ..
            }
        ));

        assert_eq!(m.data.len(), 1);
        assert!(matches!(&m.data[0], ast::DataSegment {
            idx: 0,
            offset,
            data,
            ..
        } if data.as_ref() == &b"a"[..] && offset.len() == 1));
    }

    #[test]
    fn assert_exhaustion() {
        let a: AssertExhaustion<'_> = Parser::new(
            r#"(assert_exhaustion
              (invoke "fac-rec" (i64.const 1073741824))
              "call stack exhausted"
            )"#,
        )
        .parse()
        .unwrap();

        assert_eq!(a.expected, "call stack exhausted");
        assert_eq!(a.invoke.name, "fac-rec");
        assert_eq!(a.invoke.args.len(), 1);
        assert_eq!(a.invoke.args[0], Const::I64(1073741824));
    }

    #[test]
    fn command() {
        let d: Command<'_> = Parser::new(
            r#"
            (module
              (func (export "br") (block (br 0)))
              (func (export "br_if") (block (br_if 0 (i32.const 1))))
              (func (export "br_table") (block (br_table 0 (i32.const 0))))
            )
            "#,
        )
        .parse()
        .unwrap();
        assert!(matches!(d, Command::InlineModule(_)));

        let d: Command<'_> = Parser::new(r#"(assert_return (invoke "br"))"#).parse().unwrap();
        assert!(matches!(d, Command::AssertReturn(_)));

        let d: Command<'_> = Parser::new(
            r#"
            (assert_invalid
              (module (memory 0) (func (drop (i32.load8_s align=2 (i32.const 0)))))
              "alignment must not be larger than natural"
            )
            "#,
        )
        .parse()
        .unwrap();
        assert!(matches!(d, Command::AssertInvalid(_)));

        let d: Command<'_> = Parser::new(
            r#"
            (module binary "\00asm\01\00\00\00")
            (assert_return (invoke "br"))
            "#,
        )
        .parse()
        .unwrap();
        assert!(matches!(
            d,
            Command::EmbeddedModule(EmbeddedModule {
                src: EmbeddedSrc::Binary(_),
                ..
            })
        ));

        let d: Command<'_> = Parser::new(r#"(module quote "(memory $foo 1)" "(memory $foo 1)")"#)
            .parse()
            .unwrap();
        assert!(matches!(
            d,
            Command::EmbeddedModule(EmbeddedModule {
                src: EmbeddedSrc::Quote(_),
                ..
            })
        ));
    }

    #[test]
    fn script() {
        let script: Script<'_> = Parser::new(
            r#"
            (module
              (func (export "br") (block (br 0)))
              (func (export "br_if") (block (br_if 0 (i32.const 1))))
              (func (export "br_table") (block (br_table 0 (i32.const 0))))
            )
            (assert_return (invoke "br"))
            (assert_return (invoke "br_if"))
            (assert_return (invoke "br_table"))

            (module binary "\00asm\01\00\00\00")
            (module $M1 binary "\00asm\01\00\00\00")

            (module
              (func (export "add") (param $x i32) (param $y i32) (result i32) (i32.add (local.get $x) (local.get $y)))
            )

            (assert_return (invoke "add" (i32.const 1) (i32.const 1)) (i32.const 2))
            (assert_return (invoke "add" (i32.const 1) (i32.const 0)) (i32.const 1))
            (assert_return (invoke "add" (i32.const -1) (i32.const -1)) (i32.const -2))
            (assert_return (invoke "add" (i32.const -1) (i32.const 1)) (i32.const 0))
            "#,
        )
        .parse()
        .unwrap();

        assert_eq!(script.commands.len(), 11);

        let c = script.commands;
        assert!(matches!(c[0], Command::InlineModule(_)));
        assert!(matches!(c[1], Command::AssertReturn(_)));
        assert!(matches!(c[2], Command::AssertReturn(_)));
        assert!(matches!(c[3], Command::AssertReturn(_)));
        assert!(matches!(c[4], Command::EmbeddedModule(_)));
        assert!(matches!(c[5], Command::EmbeddedModule(_)));
        assert!(matches!(c[6], Command::InlineModule(_)));
        assert!(matches!(c[7], Command::AssertReturn(_)));
        assert!(matches!(c[8], Command::AssertReturn(_)));
        assert!(matches!(c[9], Command::AssertReturn(_)));
        assert!(matches!(c[10], Command::AssertReturn(_)));
    }

    #[test]
    fn official_test_suites() {
        let mut dir = env::current_dir().unwrap();
        dir.push("wasm-testsuite");

        // When submodule is not cloned, skip this test case
        if let Ok(dirs) = fs::read_dir(dir) {
            let mut count = 1;
            for entry in dirs {
                let path = entry.unwrap().path();

                let file = if let Some(file) = path.file_name() {
                    file
                } else {
                    continue;
                };

                let file = file.to_str().unwrap();
                if !file.ends_with(".wast") {
                    continue;
                }

                if file == "inline-module.wast" {
                    continue; // Special case
                }

                let content = fs::read_to_string(&path).unwrap();
                match Parser::new(&content).parse::<Script<'_>>() {
                    Err(err) => panic!("parse error at {:?} ({}): {}", path, count, err),
                    Ok(script) => assert!(!script.commands.is_empty()),
                }
                count += 1;
            }
        }
    }
}
