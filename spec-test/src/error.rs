use crate::wast;
use std::borrow::Cow;
use std::fmt;
use std::num::{ParseFloatError, ParseIntError};
use std::path::PathBuf;
use std::string::FromUtf8Error;
use wain_ast::source::Source;
use wain_exec::{trap, Value};
use wain_syntax_binary as binary;
use wain_syntax_binary::source::BinarySource;
use wain_syntax_text as wat;
use wain_syntax_text::lexer::{LexError, Token};
use wain_syntax_text::parser::ParseError;
use wain_syntax_text::source::{describe_position, TextSource};
use wain_syntax_text::wat2wasm::TransformError;
use wain_validate as validate;

pub enum ErrorKind<'source> {
    Parse(ParseKind<'source>),
    Run(RunKind<'source>),
}

pub enum ParseKind<'source> {
    Unexpected {
        expected: Cow<'static, str>,
        token: Option<Token<'source>>,
    },
    EndOfFile {
        expected: &'static str,
    },
    Utf8Error(FromUtf8Error),
    InvalidStringLiteral {
        lit: &'source str,
        reason: &'static str,
    },
    InvalidInt {
        ty: &'static str,
        err: ParseIntError,
    },
    TooSmallInt {
        ty: &'static str,
        digits: u64,
    },
    InvalidFloat {
        ty: &'static str,
        err: ParseFloatError,
    },
    InvalidHexFloat {
        ty: &'static str,
    },
    Lex(LexError<'source>),
    ParseWat(ParseError<'source>),
    Wat2Wasm(TransformError<'source>),
}

pub enum RunKind<'source> {
    NotImplementedYet,
    ParseQuoteFailure(wat::Error<'source>),
    ParseBinaryFailure(binary::Error<'source>),
    InvalidText(validate::Error<TextSource<'source>>),
    InvalidBinary(validate::Error<BinarySource<'source>>),
    ModuleNotFound(Option<&'source str>),
    Trapped(trap::Trap),
    InvokeUnexpectedReturn {
        actual: Value,
        expected: wast::Const,
    },
    InvokeTrapExpected {
        ret: Option<Value>,
        expected: String,
    },
    UnexpectedValid {
        expected: String,
    },
    ExpectedParseError {
        expected: String,
    },
    GlobalNotFound(String),
    DidNotCrash {
        bin: PathBuf,
        args: Box<[String]>,
        stdout: String,
    },
    UnexpectedCrash {
        stderr: String,
        expected: String,
    },
}

pub struct Error<'source> {
    pub pos: usize,
    source: &'source str,
    kind: ErrorKind<'source>,
    pub prev_error: Option<Box<Error<'source>>>,
    path: Option<PathBuf>,
}

impl<'s> Error<'s> {
    pub fn parse_error(kind: ParseKind<'s>, source: &'s str, pos: usize) -> Box<Error<'s>> {
        Box::new(Error {
            pos,
            source,
            kind: ErrorKind::Parse(kind),
            prev_error: None,
            path: None,
        })
    }

    pub fn run_error(kind: RunKind<'s>, source: &'s str, pos: usize) -> Box<Error<'s>> {
        Box::new(Error {
            pos,
            source,
            kind: ErrorKind::Run(kind),
            prev_error: None,
            path: None,
        })
    }

    pub fn set_path<P: Into<PathBuf>>(&mut self, p: P) {
        self.path = Some(p.into());
    }

    pub fn kind(&self) -> &ErrorKind<'s> {
        &self.kind
    }
}

macro_rules! parse_error_from {
    ($from:ty, $kind:ident) => {
        impl<'s> From<Box<$from>> for Box<Error<'s>> {
            fn from(err: Box<$from>) -> Box<Error<'s>> {
                let source = err.source();
                let offset = err.offset();
                Error::parse_error(ParseKind::$kind(*err), source, offset)
            }
        }
    };
}
parse_error_from!(LexError<'s>, Lex);
parse_error_from!(ParseError<'s>, ParseWat);
parse_error_from!(TransformError<'s>, Wat2Wasm);

impl<'s> From<wat::Error<'s>> for Box<Error<'s>> {
    fn from(err: wat::Error<'s>) -> Box<Error<'s>> {
        let (source, pos) = err.location();
        Error::run_error(RunKind::ParseQuoteFailure(err), source, pos)
    }
}
impl<'s> From<Box<validate::Error<TextSource<'s>>>> for Box<Error<'s>> {
    fn from(err: Box<validate::Error<TextSource<'s>>>) -> Box<Error<'s>> {
        let s = err.source().raw();
        let o = err.offset();
        Error::run_error(RunKind::InvalidText(*err), s, o)
    }
}

impl<'s> fmt::Display for Error<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let doing = match &self.kind {
            ErrorKind::Parse(kind) => {
                use ParseKind::*;
                match kind {
                    Lex(err) => write!(f, "lexer error: {}", err)?,
                    ParseWat(err) => write!(f, "parse error on parsing WAT module: {}", err)?,
                    Wat2Wasm(err) => write!(f, "could not transform from WAT to WASM: {}", err)?,
                    Unexpected { expected, token: None } => {
                        write!(f, "unexpected token while {} is expected", expected)?
                    }
                    Unexpected {
                        expected,
                        token: Some(token),
                    } => write!(f, "unexpected token {} while {} is expected", token, expected)?,
                    EndOfFile { expected } => write!(f, "unxpected EOF while {} is expected", expected)?,
                    Utf8Error(err) => write!(f, "cannot parse text as UTF-8: {}", err)?,
                    InvalidStringLiteral { lit, reason } => write!(f, "invalid string literal '{}': {}", lit, reason)?,
                    InvalidInt { ty, err } => write!(f, "invalid int literal for {}: {}", ty, err)?,
                    TooSmallInt { ty, digits } => write!(f, "-{} is too small value for {}", digits, ty)?,
                    InvalidFloat { ty, err } => write!(f, "invalid float number literal for {}: {}", ty, err)?,
                    InvalidHexFloat { ty } => write!(f, "invalid hex float number literal for {}", ty)?,
                }
                "parsing"
            }
            ErrorKind::Run(kind) => {
                use RunKind::*;
                match kind {
                    NotImplementedYet => write!(f, "this command is not implemented yet")?,
                    ParseQuoteFailure(err) => write!(f, "cannot parse quoted module: {}", err)?,
                    ParseBinaryFailure(err) => write!(f, "cannot parse binary module at offset {}: {}", err.pos, err,)?,
                    InvalidText(err) => write!(f, "invalid text module: {}", err)?,
                    InvalidBinary(err) => write!(f, "invalid binary module: {}", err)?,
                    ModuleNotFound(Some(id)) => write!(f, "module '{}' is not found", id)?,
                    ModuleNotFound(None) => write!(f, "no module is found")?,
                    Trapped(trap) => write!(f, "execution was unexpectedly trapped: {}", trap)?,
                    InvokeUnexpectedReturn { actual, expected } => {
                        write!(f, "assert_return expected '{:?}' but got '{}'", expected, actual)?
                    }
                    InvokeTrapExpected { ret: None, expected } => write!(
                        f,
                        "expected trap with message '{}' while invocation but it unexpectedly returned successfully",
                        expected
                    )?,
                    InvokeTrapExpected {
                        ret: Some(ret),
                        expected,
                    } => write!(
                        f,
                        "expected trap with message '{}' while invocation but it unexpectedly returned {} successfully",
                        expected, ret,
                    )?,
                    UnexpectedValid { expected } => {
                        write!(f, "expected invalid error with message '{}' but got no error", expected,)?
                    }
                    ExpectedParseError { expected } => write!(
                        f,
                        "expected parse error with message '{}' but it was successfully done",
                        expected,
                    )?,
                    GlobalNotFound(name) => write!(f, "exported global variable '{}' is not found", name,)?,
                    DidNotCrash { bin, args, stdout } => write!(
                        f,
                        "running crash-tester did not crash: bin {:?} with args {:?}. stdout: '{}'",
                        bin, args, stdout,
                    )?,
                    UnexpectedCrash { stderr, expected } => write!(
                        f,
                        "unexpected crash with stderr '{}' while expecting '{}'",
                        stderr, expected,
                    )?,
                }
                "running"
            }
        };

        write!(f, " while {}", doing)?;
        if let Some(name) = &self.path {
            write!(f, " {:?}", name)?;
        }

        describe_position(f, self.source, self.pos)?;

        if let Some(prev) = &self.prev_error {
            write!(f, "\n\nabove error may be caused by below previous error: {}", prev)?;
        }

        Ok(())
    }
}

impl<'s> fmt::Debug for Error<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

pub type Result<'s, T> = ::std::result::Result<T, Box<Error<'s>>>;
