// Module for WAST format (.wast files), which extends WebAssembly's text format (WAT).
// .wast is not a part of WebAssembly spec, but it's used by WebAssembly's spec tests to describe
// test cases.

use wain_ast as ast;
use wain_exec::Value;
use wain_syntax_text::source::TextSource;

// (module quote *{string})
// (module binary *{string})
pub enum EmbeddedSrc {
    Quote(String),
    Binary(Vec<u8>),
}
pub struct EmbeddedModule {
    pub start: usize,
    pub src: EmbeddedSrc,
}

// Argument of assertion and invoke
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    // nan:canonical
    CanonicalNan,
    // nan:arithmetic
    ArithmeticNan,
}

impl Const {
    pub fn matches(self, v: &Value) -> bool {
        use Const::*;
        match self {
            F32(l) if l.is_nan() => {
                matches!(v, Value::F32(r) if r.is_nan() && l.to_bits() == r.to_bits())
            }
            F64(l) if l.is_nan() => {
                matches!(v, Value::F64(r) if r.is_nan() && l.to_bits() == r.to_bits())
            }
            F32(l) if l == 0.0 => {
                // -0.0 == +0.0 in Rust but assertion should handle them as different value
                matches!(v, Value::F32(r) if l.to_bits() == r.to_bits())
            }
            F64(l) if l == 0.0 => {
                // -0.0 == +0.0 in Rust but assertion should handle them as different value
                matches!(v, Value::F64(r) if l.to_bits() == r.to_bits())
            }
            I32(_) | I64(_) | F32(_) | F64(_) => &self.to_value().unwrap() == v,
            // TODO: Check payload for arithmetic NaN
            CanonicalNan | ArithmeticNan => match v {
                Value::F32(f) => f.is_nan(),
                Value::F64(f) => f.is_nan(),
                _ => false,
            },
        }
    }

    pub fn to_value(self) -> Option<Value> {
        use Const::*;
        match self {
            I32(i) => Some(Value::I32(i)),
            I64(i) => Some(Value::I64(i)),
            F32(f) => Some(Value::F32(f)),
            F64(f) => Some(Value::F64(f)),
            _ => None,
        }
    }
}

pub struct Script<'source> {
    pub start: usize,
    pub commands: Vec<Command<'source>>,
}

// Allow AssertUnlinkable variant makes this enum large since this code is used only in tests
#[allow(clippy::large_enum_variant)]
pub enum Command<'source> {
    AssertReturn(AssertReturn<'source>),
    AssertTrap(AssertTrap<'source>),
    AssertMalformed(AssertMalformed),
    AssertInvalid(AssertInvalid<'source>),
    AssertUnlinkable(AssertUnlinkable<'source>),
    AssertExhaustion(AssertExhaustion<'source>),
    Register(Register<'source>),
    Invoke(Invoke<'source>),
    EmbeddedModule(EmbeddedModule),
    InlineModule(ast::Root<'source, TextSource<'source>>),
}
impl<'s> Command<'s> {
    pub fn start_pos(&self) -> usize {
        match self {
            Command::AssertReturn(AssertReturn::Invoke { start, .. }) => *start,
            Command::AssertReturn(AssertReturn::Global { start, .. }) => *start,
            Command::AssertTrap(a) => a.start,
            Command::AssertMalformed(a) => a.start,
            Command::AssertInvalid(a) => a.start,
            Command::AssertUnlinkable(a) => a.start,
            Command::AssertExhaustion(a) => a.start,
            Command::Register(r) => r.start,
            Command::Invoke(i) => i.start,
            Command::EmbeddedModule(m) => m.start,
            Command::InlineModule(r) => r.module.start,
        }
    }
}

// (invoke {id}? {name} {constant}*)
pub struct Invoke<'source> {
    pub start: usize,
    pub id: Option<&'source str>,
    pub name: String,
    pub args: Vec<Const>,
}

// (register {name} {id}?)
pub struct Register<'source> {
    pub start: usize,
    pub name: String,
    pub id: Option<&'source str>,
}

// (get {id}? {name})
pub struct GetGlobal<'source> {
    pub start: usize,
    pub id: Option<&'source str>,
    pub name: String,
}

// (assert_return (invoke {name} {constant}*) {constant}?)
// (assert_return (get {id}? {name}) {constant})
pub enum AssertReturn<'source> {
    Invoke {
        start: usize,
        invoke: Invoke<'source>,
        expected: Option<Const>,
    },
    Global {
        start: usize,
        get: GetGlobal<'source>,
        expected: Const,
    },
}

// (assert_trap (invoke {name} {constant}*) {string})
// (assert_trap (module ...) {string})
pub enum TrapPredicate<'source> {
    Invoke(Invoke<'source>),
    Module(ast::Root<'source, TextSource<'source>>),
}
pub struct AssertTrap<'source> {
    pub start: usize,
    pub pred: TrapPredicate<'source>,
    pub expected: String,
}

// (assert_malformed (module ...) {string})
// Module must be one of (module binary ...) or (module quote ...)
pub struct AssertMalformed {
    pub start: usize,
    pub module: EmbeddedModule,
    pub expected: String,
}

// (assert_invalid (module ...) {string})
pub struct AssertInvalid<'source> {
    pub start: usize,
    // module is put inline. The source is always text
    pub wat: ast::Root<'source, TextSource<'source>>,
    pub expected: String,
}

// (assert_unlinkable (module ...) {string})
pub struct AssertUnlinkable<'source> {
    pub start: usize,
    // module is put inline. The source is always text
    pub wat: ast::Root<'source, TextSource<'source>>,
    pub expected: String,
}

// (assert_exhaustion (invoke {name} {constant}*) {string})
pub struct AssertExhaustion<'source> {
    pub start: usize,
    pub invoke: Invoke<'source>,
    pub expected: String,
}
