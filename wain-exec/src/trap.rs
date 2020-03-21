use std::borrow::Cow;
use std::fmt;
use wain_ast::Import;

#[cfg_attr(test, derive(Debug))]
pub enum TrapReason {
    UnknownImport {
        mod_name: String,
        name: String,
        kind: &'static str,
    },
    StartFuncNotFound,
    OutOfLimit {
        max: usize,
        idx: usize,
        kind: &'static str,
    },
}

#[cfg_attr(test, derive(Debug))]
pub struct Trap {
    pub reason: TrapReason,
    pub offset: usize,
}

impl Trap {
    pub(crate) fn unknown_import<'a>(
        import: &Import<'a>,
        kind: &'static str,
        offset: usize,
    ) -> Box<Self> {
        Self::new(
            TrapReason::UnknownImport {
                mod_name: import.mod_name.0.to_string(),
                name: import.name.0.to_string(),
                kind,
            },
            offset,
        )
    }

    pub(crate) fn new(reason: TrapReason, offset: usize) -> Box<Trap> {
        Box::new(Trap { reason, offset })
    }
}

impl fmt::Display for Trap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TrapReason::*;
        match &self.reason {
            UnknownImport {
                mod_name,
                name,
                kind,
            } => write!(
                f,
                "unknown module '{}' or unknown {} value '{}' imported from the module",
                mod_name, kind, name
            )?,
            StartFuncNotFound => write!(
                f,
                "could not invoke start function because 'start' section nor '_start' exported function was found",
            )?,
            OutOfLimit{ max, idx, kind } => write!(f, "specified {} index {:x} is out of limit {:x}", kind, idx, max)?,
        }
        write!(
            f,
            ": execution was trapped at byte offset 0x{:x}",
            self.offset
        )
    }
}

pub type Result<T> = ::std::result::Result<T, Box<Trap>>;
