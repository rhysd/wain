#![deny(unsafe_code)]

extern crate wain_ast;

mod globals;
mod import;
mod machine;
mod memory;
mod stack;
mod table;
mod trap;
mod value;

pub use import::{DefaultImporter, ImportError, Importer};
pub use machine::{Machine, Run};
pub use memory::Memory;

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
///
/// You will need importer for initializing Machine struct. Please use DefaultImporter::with_stdio()
/// or make your own importer struct which implements Importer trait.
pub fn execute(module: Module<'_>) -> Result<Run> {
    let importer = DefaultImporter::default();
    let mut machine = Machine::instantiate(&module, importer)?;
    machine.execute()
}
