extern crate wain_ast;

mod globals;
pub mod machine;
mod memory;
mod stack;
mod table;
mod trap;
mod value;

pub use machine::{Machine, Run};
use trap::Result;
use wain_ast::Module;

pub fn execute<'a>(module: Module<'a>) -> Result<Run> {
    Machine::instantiate(&module).and_then(|mut m| m.execute())
}
