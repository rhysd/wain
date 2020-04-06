// Module for WAST format (.wast files), which extends WebAssembly's text format (WAT).
// .wast is not a part of WebAssembly spec, but it's used by WebAssembly's spec tests to describe
// test cases.

use wain_ast as wat;

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
pub enum Const {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    // nan:canonical
    NanCanonical,
    // nan:arithmetic
    NanArithmetic,
}

pub struct WastContext<'source> {
    pub module: wat::Module<'source>,
    pub directives: Vec<Directive<'source>>,
}

pub enum Directive<'source> {
    AssertReturn(AssertReturn),
    AssertTrap(AssertTrap<'source>),
    AssertMalformed(AssertMalformed<'source>),
    AssertInvalid(AssertInvalid<'source>),
    AssertUnlinkable(AssertUnlinkable<'source>),
    AssertExhaustion(AssertExhaustion<'source>),
    Register(String),
    Invoke(Invoke),
}

// (invoke {name} {constant}*)
pub struct Invoke {
    pub start: usize,
    pub name: String,
    pub args: Vec<Const>,
}

// (register {string})
pub struct Register {
    pub start: usize,
    pub name: String,
}

// (assert_return (invoke {name} {constant}*) {constant}?)
pub struct AssertReturn {
    pub start: usize,
    pub invoke: Invoke,
    pub expected: Option<Const>,
}

// (assert_trap (invoke {name} {constant}*) {string})
pub struct AssertTrap<'source> {
    pub start: usize,
    pub invoke: Invoke,
    pub expected: &'source str,
}

// (assert_malformed (module ...) {string})
// Module must be one of (module binary ...) or (module quote ...)
pub struct AssertMalformed<'source> {
    pub start: usize,
    pub module: EmbeddedModule,
    pub expected: &'source str,
}

// (assert_invalid (module ...) {string})
pub struct AssertInvalid<'source> {
    pub start: usize,
    pub module: wat::Module<'source>,
    pub expected: &'source str,
}

// (assert_unlinkable (module ...) {string})
pub struct AssertUnlinkable<'source> {
    pub start: usize,
    pub module: wat::Module<'source>,
    pub expected: &'source str,
}

// (assert_exhaustion (invoke {name} {constant}*) {string})
pub struct AssertExhaustion<'source> {
    pub start: usize,
    pub invoke: Invoke,
    pub expected: &'source str,
}
