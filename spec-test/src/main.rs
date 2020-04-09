#![forbid(unsafe_code)]

mod error;
mod parser;
mod wast;

use parser::Parser;
use std::env;
use std::fs;
use wast::Root;

fn main() {
    let file = env::args().nth(1).unwrap();
    let content = fs::read_to_string(&file).unwrap();
    let root: Root = Parser::new(&content).parse().unwrap();
    println!("Parsed: tests: {}", root.directives.len());
}
