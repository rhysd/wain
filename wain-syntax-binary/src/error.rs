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
    TooManyLocalVariables(usize),
    MalformedSectionSize {
        name: &'static str,
        remaining_bytes: usize,
    },
    MalformedCodeSize {
        remaining_bytes: usize,
    },
    ExpectedEof(u8),
}

#[cfg_attr(test, derive(Debug))]
pub struct Error<'source> {
    pub kind: ErrorKind,
    pub pos: usize,
    pub source: &'source [u8],
    pub when: &'static str,
}

impl<'s> Error<'s> {
    pub(crate) fn new(kind: ErrorKind, pos: usize, source: &'s [u8], when: &'static str) -> Box<Error<'s>> {
        Box::new(Error {
            kind,
            pos,
            source,
            when,
        })
    }
}

impl<'s> fmt::Display for Error<'s> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            IntOverflow { ty, got: Some(got) } => {
                write!(f, "LEB128-encoded integer '{:x}' is too large for {} value", got, ty)?
            }
            IntOverflow { ty, got: None } => write!(f, "LEB128-encoded integer is too large for {} value", ty)?,
            UnexpectedEof { expected } => {
                write!(f, "expected {} but reached end of current section or input", expected)?
            }
            WasmMagicNotFound => write!(f, "WebAssembly binary must start with magic 0x00 0x61 0x73 0x6d")?,
            VersionMismatch(v) => write!(f, "expected version [1, 0, 0, 0] but got {:?}", v)?,
            LengthOutOfInput { input, specified, what } => write!(
                f,
                "{} ({} bytes) is larger than the rest of input ({} bytes)",
                what, specified, input
            )?,
            InvalidUtf8 { what, error } => write!(f, "{} must be UTF-8 sequence: {}", what, error)?,
            UnexpectedByte { expected, got, what } if expected.is_empty() => {
                write!(f, "unexpected byte 0x{:02x} at {}", got, what,)?
            }
            UnexpectedByte { expected, got, what } if expected.len() == 1 => write!(
                f,
                "expected byte 0x{:02x} for {} but got 0x{:02x}",
                expected[0], what, got
            )?,
            UnexpectedByte { expected, got, what } => {
                f.write_str("expected one of ")?;
                let mut first = true;
                for b in expected.iter() {
                    if !first {
                        f.write_str(", ")?;
                    }
                    write!(f, "0x{:02x}", b)?;
                    first = false;
                }
                write!(f, " for {} but got byte 0x{:02x}", what, got)?;
            }
            FuncCodeLengthMismatch { num_funcs, num_codes } => write!(
                f,
                "number of function sections '{}' does not match to number of code sections '{}'",
                num_funcs, num_codes,
            )?,
            TooManyLocalVariables(num) => write!(f, "too many ({}) local variables", num)?,
            MalformedSectionSize { name, remaining_bytes } => write!(
                f,
                "expected end of {} but trailing {} bytes still remain",
                name, remaining_bytes
            )?,
            MalformedCodeSize { remaining_bytes } => write!(
                f,
                "expected end of code but trailing {} bytes still remain",
                remaining_bytes
            )?,
            ExpectedEof(b) => write!(f, "expected end of input but byte 0x{:02x} is still following", b)?,
        }
        write!(f, " while parsing {}.", self.when)?;
        describe_position(f, self.source, self.pos)
    }
}

pub type Result<'s, T> = ::std::result::Result<T, Box<Error<'s>>>;
