extern crate wain_ast;

mod memory;
mod stack;

use memory::Memory;
use stack::Stack;
use wain_ast::Module;

pub struct Machine<'a> {
    module: Module<'a>,
    stack: Stack,
    memory: Memory,
}

impl<'a> Machine<'a> {
    pub fn new(module: Module<'a>) -> Self {
        Self {
            module,
            stack: Stack::new(),
            memory: Memory::new(), // TODO
        }
    }
}
