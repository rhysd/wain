use std::fmt;
use wain_ast::*;

#[cfg_attr(test, derive(Debug))]
pub enum ErrorKind {
    IndexOutOfBounds {
        idx: u32,
        upper: usize,
        what: &'static str,
    },
    MultipleReturnTypes(Vec<ValType>),
    TypeMismatch {
        op: &'static str,
        expected: ValType,
        actual: ValType,
    },
    CtrlFrameEmpty {
        op: &'static str,
        frame_start: usize,
        idx_in_op_stack: usize,
    },
    LabelStackEmpty {
        op: &'static str,
    },
    SetImmutableGlobal {
        ty: ValType,
        idx: u32,
    },
    TooLargeAlign {
        align: u32,
        bits: u8,
    },
    LimitsOutOfRange {
        value: u32,
        min: u32,
        max: u32,
        what: &'static str,
    },
    NotConstantInstruction(&'static str),
    NoInstructionForConstant,
    StartFunctionSignature {
        idx: u32,
        params: Vec<ValType>,
        results: Vec<ValType>,
    },
    MultipleTables(usize),
    MultipleMemories(usize),
    AlreadyExported {
        name: String,
        prev_offset: usize,
    },
}

#[cfg_attr(test, derive(Debug))]
pub struct Error<'a> {
    kind: ErrorKind,
    source: &'a str,
    offset: usize,
}

struct Ordinal(usize);
impl fmt::Display for Ordinal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 % 10 {
            1 => write!(f, "{}st", self.0),
            2 => write!(f, "{}nd", self.0),
            3 => write!(f, "{}rd", self.0),
            _ => write!(f, "{}th", self.0),
        }
    }
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            IndexOutOfBounds { idx, upper, what } => write!(
                f,
                "{} index {} out of bounds 0 <= idx < {}",
                what, idx, upper
            )?,
            MultipleReturnTypes(tys) => {
                let ss = tys.iter().map(AsRef::as_ref).collect::<Vec<&str>>();
                write!(
                    f,
                    "multiple return types are not allowed for now but got [{}]",
                    ss.join(", ")
                )?
            }
            TypeMismatch {
                op,
                expected,
                actual,
            } => write!(
                f,
                "type does not match at '{}': expected {} but got {}",
                op, expected, actual
            )?,
            CtrlFrameEmpty {
                op,
                frame_start,
                idx_in_op_stack: 0,
            } => write!(f, "operand stack cannot be empty at '{}' instruction while validating instruction sequence starting at offset {}", op, frame_start)?,
            CtrlFrameEmpty {
                op,
                frame_start,
                idx_in_op_stack,
            } => write!(
                f,
                "empty control frame cannot be empty at '{}' instruction. the frame started at byte offset {} and top of \
                 control frame is op_stack[{}]", op, frame_start, idx_in_op_stack)?,
            LabelStackEmpty { op } => write!(f, "label stack for control instructions is unexpectedly empty at '{}' instruction", op)?,
            SetImmutableGlobal{ ty, idx } => write!(f, "{} value cannot be set to immutable global variable {}", ty, idx)?,
            TooLargeAlign { align, bits } => write!(f, "align {} must not be larger than {}bits / 8", align, bits)?,
            LimitsOutOfRange { value, min, max, what } => write!(f, "limit {} is out of range {}..{} at {}", value, min, max, what)?,
            NotConstantInstruction(op) => write!(f, "instruction '{}' is not valid for constant. only 'global.get' or '*.const' are valid in constant expressions", op)?,
            NoInstructionForConstant => write!(f, "at least one instruction is necessary for constant expressions")?,
            StartFunctionSignature{ idx, params, results } => write!(
                f,
                "start function should have no parameter and no result [] -> [] but found function {} [{}] -> [{}]",
                idx,
                params.iter().map(AsRef::<str>::as_ref).collect::<Vec<_>>().join(" "),
                results.iter().map(AsRef::<str>::as_ref).collect::<Vec<_>>().join(" "),
            )?,
            MultipleTables(size) => write!(f, "number of tables must not be larger than 1 but got {}", size)?,
            MultipleMemories(size) => write!(f, "number of memories must not be larger than 1 but got {}", size)?,
            AlreadyExported{ name, prev_offset } => write!(f, "'{}' was already exported at offset {}", name, prev_offset)?,
        }

        if self.offset == self.source.len() {
            write!(f, " caused at byte offset {} (end of input)", self.offset)
        } else {
            let source = &self.source[self.offset..];
            let end = source
                .find(['\n', '\r'].as_ref())
                .unwrap_or_else(|| source.len());
            write!(
                f,
                " caused at byte offset {}\n\n ... {}\n     ^\n     starts from here",
                self.offset,
                &source[..end],
            )
        }
    }
}

impl<'a> Error<'a> {
    pub(crate) fn new(kind: ErrorKind, offset: usize, source: &'a str) -> Box<Self> {
        Box::new(Self {
            kind,
            source,
            offset,
        })
    }
}

pub type Result<'a, T> = ::std::result::Result<T, Box<Error<'a>>>;
