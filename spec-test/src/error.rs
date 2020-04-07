use std::fmt;
use std::num::{ParseFloatError, ParseIntError};
use std::string::FromUtf8Error;
use wain_syntax_text::lexer::LexError;
use wain_syntax_text::source::describe_position;

pub enum ErrorKind<'source> {
    Unexpected {
        expected: &'static str,
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
    InvalidFloat {
        ty: &'static str,
        err: ParseFloatError,
    },
    InvalidHexFloat {
        ty: &'static str,
    },
    Lex(LexError<'source>),
}

pub struct Error<'source> {
    pos: usize,
    source: &'source str,
    kind: ErrorKind<'source>,
}

impl<'s> Error<'s> {
    pub fn new(kind: ErrorKind<'s>, source: &'s str, pos: usize) -> Box<Error<'s>> {
        Box::new(Error { pos, source, kind })
    }
}

impl<'s> From<Box<LexError<'s>>> for Box<Error<'s>> {
    fn from(err: Box<LexError<'s>>) -> Box<Error<'s>> {
        let source = err.source();
        let offset = err.offset();
        Error::new(ErrorKind::Lex(*err), source, offset)
    }
}

impl<'s> fmt::Display for Error<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            Lex(err) => write!(f, "lexer error: {}", err)?,
            Unexpected { expected } => {
                write!(f, "unexpected token while {} is expected", expected)?
            }
            EndOfFile { expected } => write!(f, "unxpected EOF while {} is expected", expected)?,
            Utf8Error(err) => write!(f, "cannot parse text as UTF-8: {}", err)?,
            InvalidStringLiteral { lit, reason } => {
                write!(f, "invalid string literal '{}': {}", lit, reason)?
            }
            InvalidInt { ty, err } => write!(f, "invalid int literal for {}: {}", ty, err)?,
            InvalidFloat { ty, err } => {
                write!(f, "invalid float number literal for {}: {}", ty, err)?
            }
            InvalidHexFloat { ty } => write!(f, "invalid hex float number literal for {}", ty)?,
        }
        describe_position(f, self.source, self.pos)
    }
}

impl<'s> fmt::Debug for Error<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

pub type Result<'s, T> = ::std::result::Result<T, Box<Error<'s>>>;
