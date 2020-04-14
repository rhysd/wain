// Module for WAST format (.wast files), which extends WebAssembly's text format (WAT).
// .wast is not a part of WebAssembly spec, but it's used by WebAssembly's spec tests to describe
// test cases.

use wain_ast as ast;
use wain_syntax_text::source::TextSource;

// (module quote *{string})
// (module binary *{string})
pub enum Embedded {
    Quote(String),
    Binary(Vec<u8>),
}
pub struct EmbeddedModule {
    pub start: usize,
    pub embedded: Embedded,
}

// Argument of assertion and invoke
#[cfg_attr(test, derive(Debug))]
#[derive(PartialEq)]
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

pub struct Root<'source> {
    pub start: usize,
    pub directives: Vec<Directive<'source>>,
}

// Allow AssertUnlinkable variant makes this enum large since this code is used only in tests
#[allow(clippy::large_enum_variant)]
pub enum Directive<'source> {
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
impl<'s> Directive<'s> {
    pub fn start_pos(&self) -> usize {
        match self {
            Directive::AssertReturn(AssertReturn::Invoke { start, .. }) => *start,
            Directive::AssertReturn(AssertReturn::Global { start, .. }) => *start,
            Directive::AssertTrap(a) => a.start,
            Directive::AssertMalformed(a) => a.start,
            Directive::AssertInvalid(a) => a.start,
            Directive::AssertUnlinkable(a) => a.start,
            Directive::AssertExhaustion(a) => a.start,
            Directive::Register(r) => r.start,
            Directive::Invoke(i) => i.start,
            Directive::EmbeddedModule(m) => m.start,
            Directive::InlineModule(r) => r.module.start,
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
