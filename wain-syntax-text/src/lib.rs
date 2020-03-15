#![allow(clippy::cognitive_complexity)]

extern crate wain_ast;

pub mod ast;
pub mod lexer;
pub mod parser;
mod util;
pub mod wat2wasm;

use parser::{ParseError, Parser};
use std::fmt;
use wat2wasm::TransformError;

pub enum Error<'a> {
    Parse(Box<ParseError<'a>>),
    Transform(Box<TransformError<'a>>),
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
}

pub fn parse(source: &'_ str) -> Result<wain_ast::Root<'_>, Error<'_>> {
    let mut parser = Parser::new(source);
    let parsed = parser.parse()?;
    let mut tree = wat2wasm::from_wat(parsed, source)?;

    // Compose multiple modules: https://webassembly.github.io/spec/core/text/modules.html#text-module
    while !parser.is_done() {
        let parsed = parser.parse()?;
        wat2wasm::compose(&mut tree.module, parsed, source)?;
    }

    Ok(tree)
}
