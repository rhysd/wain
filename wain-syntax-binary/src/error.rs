use crate::source::describe_position;
use std::fmt;
use std::str::Utf8Error;

#[cfg_attr(test, derive(Debug))]
pub enum ErrorKind {
    IntOverflow {
        ty: &'static str,
        got: Option<u64>,
    },
    UnexpectedEof {
        expected: &'static str,
    },
    MagicNotFound,
    VersionMismatch([u8; 4]),
    LengthOutOfInput {
        input: usize,
        specified: usize,
        what: &'static str,
    },
    InvalidUtf8 {
        what: &'static str,
        error: Utf8Error,
    },
    MagicMismatch {
        expected: u8,
        got: u8,
        what: &'static str,
    },
    UnexpectedValType(u8),
    UnexpectedImportDesc(u8),
}

#[cfg_attr(test, derive(Debug))]
pub struct Error<'source> {
    pub kind: ErrorKind,
    pub pos: usize,
    pub source: &'source [u8],
}

impl<'s> Error<'s> {
    pub(crate) fn new(kind: ErrorKind, pos: usize, source: &'s [u8]) -> Box<Error<'s>> {
        Box::new(Error { kind, pos, source })
    }

    pub(crate) fn err<T>(kind: ErrorKind, pos: usize, source: &'s [u8]) -> Result<'s, T> {
        Err(Self::new(kind, pos, source))
    }
}

impl<'s> fmt::Display for Error<'s> {
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
            UnexpectedEof { expected } => write!(
                f,
                "expected {} but reached end of current section or input",
                expected
            )?,
            MagicNotFound => write!(
                f,
                "WebAssembly binary must start with magic 0x00 0x61 0x73 0x6d"
            )?,
            VersionMismatch(v) => write!(f, "expected version [1 0 0 0] but got {:?}", v)?,
            LengthOutOfInput {
                input,
                specified,
                what,
            } => write!(
                f,
                "{} ({} bytes) is larger than rest input ({} bytes)",
                what, specified, input
            )?,
            InvalidUtf8 { what, error } => write!(f, "{} must be UTF-8 sequence: {}", what, error)?,
            MagicMismatch {
                expected,
                got,
                what,
            } => write!(
                f,
                "magic byte 0x{:x} is expected for {} but got 0x{:x}",
                expected, what, got
            )?,
            UnexpectedValType(byte) => write!(
                f,
                "expected one of 0x7f, 0x7e, 0x7d, 0x7c for value type but got 0x{:x}",
                byte
            )?,
            UnexpectedImportDesc(byte) => write!(
                f,
                "expected one of 0x00, 0x01, 0x02, 0x03 for import description but got 0x{:x}",
                byte
            )?,
        }
        describe_position(f, self.source, self.pos)
    }
}

pub type Result<'s, T> = ::std::result::Result<T, Box<Error<'s>>>;
