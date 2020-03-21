use std::fmt;

#[cfg_attr(test, derive(Debug))]
pub enum ErrorKind {
    UnknownImport {
        mod_name: String,
        name: String,
        kind: &'static str,
    },
}

#[cfg_attr(test, derive(Debug))]
pub struct Error {
    pub kind: ErrorKind,
    pub offset: usize,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind, offset: usize) -> Box<Error> {
        Box::new(Error { kind, offset })
    }

    pub(crate) fn err<T>(kind: ErrorKind, offset: usize) -> Result<T> {
        Err(Self::new(kind, offset))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            UnknownImport {
                mod_name,
                name,
                kind,
            } => write!(
                f,
                "unknown module '{}' or unknown {} value '{}' imported from the module",
                mod_name, kind, name
            )?,
        }
        write!(f, " caused at byte offset 0x{:x}", self.offset)
    }
}

pub type Result<T> = ::std::result::Result<T, Box<Error>>;
