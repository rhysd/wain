#![deny(unsafe_code)]

extern crate wain_ast;

mod globals;
pub mod machine;
mod memory;
mod stack;
mod table;
mod trap;
mod value;

pub use machine::{Machine, Run};
use std::io;
use std::io::Write;
use trap::Result;
use wain_ast::Module;

/// A convenient function to execute a WebAssembly module.
///
/// This function takes parsed and validated WebAssembly module and executes it until the end.
///
/// For standard I/O speed, this function buffers io::Stdin and io::Stdout objects because currently
/// getchar() and putchar() don't buffer its input/output. This behavior may change in the future.
///
/// If the behavior is not acceptable, please make an abstract machine instance with
/// Machine::instantiate and execute by Machine::execute method.
pub fn execute(module: Module<'_>) -> Result<Run> {
    let stdin = io::BufReader::new(io::stdin());
    let stdout = io::BufWriter::new(io::stdout());
    let mut machine = Machine::instantiate(&module, stdin, stdout)?;
    let run = machine.execute()?;
    machine.stdout().flush().unwrap();
    Ok(run)
}
