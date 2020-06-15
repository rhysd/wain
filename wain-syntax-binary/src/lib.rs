#![forbid(unsafe_code)]
#![warn(clippy::dbg_macro)]

mod error;
mod leb128;
mod parser;

pub mod source;

pub use error::{Error, ErrorKind, Result};
pub use parser::Parser;
use source::BinarySource;
use wain_ast::Root;

pub fn parse(input: &[u8]) -> Result<'_, Root<'_, BinarySource<'_>>> {
    let mut parser = Parser::new(input);
    parser.parse()
}
