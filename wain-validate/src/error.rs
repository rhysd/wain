use std::borrow::Cow;
use std::fmt;
use wain_ast::source::Source;
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
        expected: Option<ValType>,
        actual: Option<ValType>,
    },
    CtrlFrameEmpty {
        op: &'static str,
        frame_start: usize,
        idx_in_op_stack: usize,
    },
    SetImmutableGlobal {
        ty: ValType,
        idx: u32,
    },
    TooLargeAlign {
        align: u32,
        bits: u32,
    },
    InvalidLimitRange(u32, u32),
    LimitsOutOfRange {
        value: u32,
        min: u32,
        max: u32,
        what: &'static str,
    },
    NotConstantInstruction(&'static str),
    NoInstructionForConstant,
    TooManyInstructionForConstant(usize),
    MutableForConstant(u32),
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
    MemoryIsNotDefined,
    InvalidStackDepth {
        expected: usize,
        actual: usize,
        remaining: String,
    },
}

#[cfg_attr(test, derive(Debug))]
pub struct Error<S: Source> {
    kind: ErrorKind,
    source: S,
    offset: usize,
    pub(crate) when: Cow<'static, str>,
}

impl<S: Source> Error<S> {
    pub(crate) fn update_msg(mut self: Box<Self>, new_msg: String) -> Box<Self> {
        self.when = Cow::Owned(new_msg);
        self
    }
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
}

pub(crate) struct Ordinal(pub(crate) usize);
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

impl<S: Source> fmt::Display for Error<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match &self.kind {
            IndexOutOfBounds { idx, upper, what } => write!(f, "{} index {} out of bounds 0 <= idx < {}", what, idx, upper)?,
            MultipleReturnTypes(tys) => {
                let ss = tys.iter().map(AsRef::as_ref).collect::<Vec<&str>>();
                write!(f, "multiple return types are not allowed for now but got [{}]", ss.join(", "))?
            }
            TypeMismatch { expected, actual } => {
                assert_ne!(expected, actual);
                match expected {
                    Some(t) => write!(f, "expected type '{}'", t)?,
                    None => write!(f, "expected no type")?,
                }
                match actual {
                    Some(t) => write!(f, "but got type '{}'", t)?,
                    None => write!(f, "but got no type")?,
                }
            }
            CtrlFrameEmpty { op, frame_start, idx_in_op_stack: 0 } => write!(
                f,
                "operand stack cannot be empty at '{}' instruction while validating instruction sequence starting at offset {}",
                op, frame_start
            )?,
            CtrlFrameEmpty { op, frame_start, idx_in_op_stack } => write!(
                f,
                "empty control frame cannot be empty at '{}' instruction. the frame started at byte offset {} and top of \
                 control frame is op_stack[{}]",
                op, frame_start, idx_in_op_stack
            )?,
            SetImmutableGlobal { ty, idx } => write!(f, "{} value cannot be set to immutable global variable {}", ty, idx)?,
            TooLargeAlign { align, bits } => write!(f, "align {} must not be larger than {}bits / 8", align, bits)?,
            InvalidLimitRange(min, max) => write!(f, "range for limits {}..{} is invalid", min, max)?,
            LimitsOutOfRange { value, min, max, what } => write!(f, "limit {} is out of range {}..{} at {}", value, min, max, what)?,
            NotConstantInstruction(op) => write!(f, "instruction '{}' is not valid for constant. only 'global.get' or '*.const' are valid in constant expressions", op)?,
            NoInstructionForConstant => write!(f, "at least one instruction is necessary for constant expressions")?,
            TooManyInstructionForConstant(len) => write!(f, "exactly one instruction is allowed for constant expressions but {} instructions found", len)?,
            MutableForConstant(idx) => write!(f, "constant expressions cannot reference mutable global variable {}", idx)?,
            StartFunctionSignature { idx, params, results } => write!(
                f,
                "start function should have no parameter and no result [] -> [] but found function '{}' is [{}] -> [{}]",
                idx,
                params.iter().map(AsRef::<str>::as_ref).collect::<Vec<_>>().join(" "),
                results.iter().map(AsRef::<str>::as_ref).collect::<Vec<_>>().join(" "),
            )?,
            MultipleTables(size) => write!(f, "number of tables must not be larger than 1 but got {}", size)?,
            MultipleMemories(size) => write!(f, "number of memories must not be larger than 1 but got {}", size)?,
            AlreadyExported { name, prev_offset } => write!(f, "'{}' was already exported at offset {}", name, prev_offset)?,
            MemoryIsNotDefined => write!(f, "at least one memory section must be defined")?,
            InvalidStackDepth { expected, actual, remaining } => write!(f, "expected operand stack depth is {} but actually {} with {} remaining on the stack", expected, actual, remaining,)?,
        }

        write!(f, ". error while validating {}. ", self.when)?;

        self.source.describe(f, self.offset)
    }
}

impl<S: Source> Error<S> {
    pub(crate) fn new(kind: ErrorKind, when: Cow<'static, str>, offset: usize, source: &S) -> Box<Self> {
        Box::new(Self {
            kind,
            source: source.clone(),
            offset,
            when,
        })
    }

    pub fn source(&self) -> &S {
        &self.source
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

pub type Result<T, S> = ::std::result::Result<T, Box<Error<S>>>;
