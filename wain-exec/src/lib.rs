extern crate wain_ast;

mod error;
mod globals;
mod memory;
mod stack;
mod value;

use error::Result;
use globals::Globals;
use memory::Memory;
use stack::{CallFrames, ValueStack};
use wain_ast::Module;

struct Executed {
    breaking: Option<u32>,
}

pub struct Machine<'a> {
    module: Module<'a>,
    values: ValueStack,
    frames: CallFrames, // Activations of function frames
    memory: Memory,
    globals: Globals,
}

impl<'a> Machine<'a> {
    pub fn new(module: Module<'a>) -> Result<Self> {
        Ok(Self {
            globals: Globals::new(&module.globals)?,
            module,
            values: ValueStack::new(),
            frames: CallFrames::new(),
            memory: Memory::new(),
        })
    }
}
