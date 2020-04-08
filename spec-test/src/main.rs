mod error;
mod parser;
mod wast;

use parser::Parser;
use std::env;
use std::fs;
use wast::TestSuite;

fn main() {
    let file = env::args().skip(1).next().unwrap();
    let content = fs::read_to_string(&file).unwrap();
    let test_suite: TestSuite = Parser::new(&content).parse().unwrap();
    println!("Parsed: tests: {}", test_suite.test_cases.len());
}
