use std::fmt;

// Trait to handle source for better error message. Wasm has two format text and binary.
// This trait handles them with one generics.
pub trait Source: Clone {
    type Raw;
    fn describe(&self, f: &mut fmt::Formatter<'_>, offset: usize) -> fmt::Result;
    fn raw(&self) -> Self::Raw;
}
