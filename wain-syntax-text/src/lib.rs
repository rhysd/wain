#![deny(unsafe_code)]
#![allow(clippy::cognitive_complexity)]

extern crate wain_ast;

mod compose;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod source;
pub mod wat2wasm;

use compose::{ComposeError, Composer};
use parser::{ParseError, Parser};
use source::TextSource;
use std::fmt;
use wat2wasm::{wat2wasm, TransformError};

pub enum Error<'source> {
    Parse(Box<ParseError<'source>>),
    Transform(Box<TransformError<'source>>),
    Compose(Box<ComposeError<'source>>),
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
