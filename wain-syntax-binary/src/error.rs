use crate::source::describe_position;
use std::fmt;

#[cfg_attr(test, derive(Debug))]
pub enum ErrorKind<'s> {
    IntOverflow { ty: &'static str, got: Option<u64> },
    UnexpectedEof { expected: &'static str },
    Dummy(std::marker::PhantomData<&'s ()>),
}

#[cfg_attr(test, derive(Debug))]
pub struct Error<'source> {
    pub kind: ErrorKind<'source>,
    pub pos: usize,
    pub source: &'source [u8],
}

impl<'s> Error<'s> {
    pub(crate) fn new(kind: ErrorKind<'s>, pos: usize, source: &'s [u8]) -> Box<Error<'s>> {
        Box::new(Error { kind, pos, source })
    }

    pub(crate) fn err<T>(kind: ErrorKind<'s>, pos: usize, source: &'s [u8]) -> Result<'s, T> {
        Err(Self::new(kind, pos, source))
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self.kind {
            IntOverflow { ty, got: Some(got) } => write!(
                f,
                "LEB128-encoded integer '{:x}' is too large for {} value",
                got, ty
            )?,
            IntOverflow { ty, got: None } => {
                write!(f, "LEB128-encoded integer is too large for {} value", ty)?
            }
            UnexpectedEof { expected } => {
                write!(f, "expected {} but reached end of input", expected)?
            }
            Dummy(_) => {}
        }
        describe_position(f, self.source, self.pos)
    }
}

pub type Result<'s, T> = ::std::result::Result<T, Box<Error<'s>>>;
