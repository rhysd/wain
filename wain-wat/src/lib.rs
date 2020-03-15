#![allow(clippy::cognitive_complexity)]

extern crate wain_ast;

mod compose;
mod util;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod wat2wasm;

use compose::{ComposeError, Composer};
use parser::{ParseError, Parser};
use std::fmt;
use wat2wasm::{wat2wasm, TransformError};

pub enum Error<'a> {
    Parse(Box<ParseError<'a>>),
    Transform(Box<TransformError<'a>>),
    Compose(Box<ComposeError<'a>>),
}

macro_rules! from_errors {
    ($($ty:ty => $kind:ident,)+) => {
        $(
            impl<'a> From<Box<$ty>> for Error<'a> {
                fn from(err: Box<$ty>) -> Error<'a> {
                    Error::$kind(err)
                }
            }
        )+

        impl<'a> fmt::Display for Error<'a> {
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
    ParseError<'a> => Parse,
    TransformError<'a> => Transform,
    ComposeError<'a> => Compose,
}

pub fn parse(source: &'_ str) -> Result<wain_ast::Root<'_>, Error<'_>> {
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
