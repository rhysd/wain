// Module for WAST format (.wast files), which extends WebAssembly's text format (WAT).
// .wast is not a part of WebAssembly spec, but it's used by WebAssembly's spec tests to describe
// test cases.

use wain_ast as ast;
use wain_syntax_binary::source::BinarySource;
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

pub struct TestSuite<'source> {
    test_cases: Vec<TestCase<'source>>,
}

pub enum TestCase<'source> {
    Text {
        module: ast::Root<'source, TextSource<'source>>,
        directives: Vec<Directive<'source>>,
    },
    Binary {
        module: ast::Root<'source, BinarySource<'source>>,
        directives: Vec<Directive<'source>>,
    },
}

pub enum Directive<'source> {
    AssertReturn(AssertReturn),
    AssertTrap(AssertTrap),
    AssertMalformed(AssertMalformed),
    AssertInvalid(AssertInvalid<'source>),
    AssertUnlinkable(AssertUnlinkable<'source>),
    AssertExhaustion(AssertExhaustion),
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
pub struct AssertTrap {
    pub start: usize,
    pub invoke: Invoke,
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
pub struct AssertExhaustion {
    pub start: usize,
    pub invoke: Invoke,
    pub expected: String,
}
