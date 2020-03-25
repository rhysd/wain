use std::fmt;
use std::io;
use wain_ast::{Import, ValType};

#[cfg_attr(test, derive(Debug))]
pub enum TrapReason {
    UnknownImport {
        mod_name: String,
        name: String,
        kind: &'static str,
    },
    OutOfLimit {
        max: usize,
        idx: usize,
        kind: &'static str,
    },
    DataSegmentOutOfBuffer {
        segment_end: usize,
        buffer_size: usize,
    },
    ElemSegmentLargerThanTable {
        segment_end: usize,
        table_size: usize,
    },
    ReachUnreachable,
    IdxOutOfTable {
        idx: usize,
        table_size: usize,
    },
    UninitializedElem(usize),
    FuncSignatureMismatch {
        expected_params: Vec<ValType>,
        expected_results: Vec<ValType>,
        actual_params: Vec<ValType>,
        actual_results: Vec<ValType>,
    },
    // 10. https://webassembly.github.io/spec/core/exec/instructions.html#and
    LoadMemoryOutOfRange {
        max: usize,
        addr: usize,
        operation: &'static str,
        ty: &'static str,
    },
    IoError(io::Error),
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

struct JoinWritable<'a, D: fmt::Display>(&'a [D], &'static str);

impl<'a, D: fmt::Display> fmt::Display for JoinWritable<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(d) = self.0.first() {
            d.fmt(f)?;
        }
        for d in self.0.iter().skip(1) {
            write!(f, "{}{}", self.1, d)?;
        }
        Ok(())
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
                mod_name, kind, name,
            )?,
            OutOfLimit { max, idx, kind } => write!(
                f,
                "specified {} index 0x{:x} is out of limit 0x{:x}",
                kind, idx, max,
            )?,
            DataSegmentOutOfBuffer {
                segment_end,
                buffer_size,
            } => write!(
                f,
                "'data' segment ends at address 0x{:x} but memory buffer size is 0x{:x}",
                segment_end, buffer_size,
            )?,
            ElemSegmentLargerThanTable {
                segment_end,
                table_size,
            } => write!(
                f,
                "'elem' segment ends at index {} but table length is {}",
                segment_end, table_size,
            )?,
            ReachUnreachable => f.write_str("reached unreachable code")?,
            IdxOutOfTable { idx, table_size } => write!(
                f,
                "cannot refer function because index {} is out of table size {}",
                idx, table_size
            )?,
            UninitializedElem(idx) => {
                write!(f, "element at index {} in table is uninitialized", idx,)?
            }
            FuncSignatureMismatch {
                expected_params,
                expected_results,
                actual_params,
                actual_results,
            } => write!(
                f,
                "cannot invoke function due to mismatch of function signatures. expected '[{}] -> [{}]' but got '[{}] -> [{}]'",
                JoinWritable(expected_params, " "),
                JoinWritable(expected_results, " "),
                JoinWritable(actual_params, " "),
                JoinWritable(actual_results, " "),
            )?,
            LoadMemoryOutOfRange { max, addr, operation, ty } => write!(
                f,
                "cannot {} {} value at 0x{:x} due to out of range of memory. memory size is 0x{:x}",
                operation, ty, addr, max,
            )?,
            IoError(e) => write!(f, "I/O error: {}", e)?,
        }
        write!(
            f,
            ": execution was trapped at byte offset 0x{:x}",
            self.offset
        )
    }
}

pub type Result<T> = ::std::result::Result<T, Box<Trap>>;
