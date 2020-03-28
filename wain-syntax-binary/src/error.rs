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
    WasmMagicNotFound,
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
    UnexpectedByte {
        expected: Vec<u8>,
        got: u8,
        what: &'static str,
    },
    FuncCodeLengthMismatch {
        num_funcs: usize,
        num_codes: usize,
    },
    ExpectedEof(u8),
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
}

impl<'s> fmt::Display for Error<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
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
            WasmMagicNotFound => write!(
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
            UnexpectedByte {
                expected,
                got,
                what,
            } if expected.is_empty() => write!(f, "unexpected byte 0x{:x} at {}", got, what,)?,
            UnexpectedByte {
                expected,
                got,
                what,
            } if expected.len() == 1 => write!(
                f,
                "expected byte 0x{:x} for {} but got 0x{:x}",
                expected[0], what, got
            )?,
            UnexpectedByte {
                expected,
                got,
                what,
            } => {
                f.write_str("expected one of ")?;
                let mut first = true;
                for b in expected.iter() {
                    if !first {
                        f.write_str(", ")?;
                    }
                    write!(f, "0x{:x}", b)?;
                    first = false;
                }
                write!(f, " for {} but got byte 0x{:x}", what, got)?;
            }
            FuncCodeLengthMismatch {
                num_funcs,
                num_codes,
            } => write!(
                f,
                "number of function sections '{}' does not match to number of code sections '{}'",
                num_funcs, num_codes,
            )?,
            ExpectedEof(b) => write!(
                f,
                "expected end of input but byte 0x{:x} is still following",
                b
            )?,
        }
        describe_position(f, self.source, self.pos)
    }
}

pub type Result<'s, T> = ::std::result::Result<T, Box<Error<'s>>>;
