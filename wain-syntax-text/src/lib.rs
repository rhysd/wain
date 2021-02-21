#![forbid(unsafe_code)]
#![warn(clippy::dbg_macro)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::manual_range_contains)]

extern crate wain_ast;

pub mod ast;
pub mod compose;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod wat2wasm;

use compose::{ComposeError, Composer};
use parser::{ParseError, Parser};
use source::TextSource;
use std::fmt;
use wat2wasm::{wat2wasm, TransformError};

// TODO: Unify all error types into one error type
pub enum Error<'source> {
    Parse(Box<ParseError<'source>>),
    Transform(Box<TransformError<'source>>),
    Compose(Box<ComposeError<'source>>),
}

impl<'s> Error<'s> {
    pub fn location(&self) -> (&'s str, usize) {
        match self {
            Error::Parse(e) => (e.source(), e.offset()),
            Error::Transform(e) => (e.source(), e.offset()),
            Error::Compose(e) => (e.source(), e.offset()),
        }
    }
}

macro_rules! from_errors {
    ($($ty:ty => $kind:ident,)+) => {
        $(
            impl<'s> From<Box<$ty>> for Error<'s> {
                fn from(err: Box<$ty>) -> Error<'s> {
                    Error::$kind(err)
                }
            }
        )+

        impl<'s> fmt::Display for Error<'s> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(
                        Error::$kind(err) => err.fmt(f),
                    )+
                }
            }
        }
    };
}
from_errors! {
    ParseError<'s> => Parse,
    TransformError<'s> => Transform,
    ComposeError<'s> => Compose,
}

pub fn parse(source: &'_ str) -> Result<wain_ast::Root<'_, TextSource>, Error<'_>> {
    let mut parser = Parser::new(source);
    let parsed = parser.parse()?;
    let mut tree = wat2wasm(parsed, source)?;

    // Compose multiple modules: https://webassembly.github.io/spec/core/text/modules.html#text-module
    while !parser.is_done() {
        let parsed = parser.parse()?;
        let module = wat2wasm(parsed, source)?.module;
        let composer = Composer::new(tree.module, source);
        tree.module = composer.compose(module)?;
    }

    Ok(tree)
}
