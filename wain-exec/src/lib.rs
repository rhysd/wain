extern crate wain_ast;

mod globals;
mod memory;
mod stack;
mod table;
mod trap;
mod value;

use globals::Globals;
use memory::Memory;
use stack::Stack;
use table::Table;
use trap::{Result, Trap, TrapReason};
use wain_ast as ast;

// Note: This implementation currently ignores Wasm's thread model since MVP does not support multiple
// threads. https://webassembly.github.io/spec/core/exec/runtime.html#configurations

// TODO: Handle external values for imports and exports

enum ExecState {
    Breaking(u32), // Breaking
    Ret,           // Returning from current function call
    Continue,      // Continuing execution
}

type ExecResult = Result<ExecState>;

pub struct Machine<'module, 'source> {
    module: &'module ast::Module<'source>,
    table: Table,
    stack: Stack,
    memory: Memory,
    globals: Globals,
}

impl<'m, 'a> Machine<'m, 'a> {
    // https://webassembly.github.io/spec/core/exec/modules.html#instantiation
    pub fn instantiate(module: &'m ast::Module<'a>) -> Result<Self> {
        // TODO: 2., 3., 4. Validate external values before instantiate globals

        // 5. global initialization values determined by module and externval
        let globals = Globals::instantiate(&module.globals)?;

        // 6. a new module instance allocated from module in store S
        // https://webassembly.github.io/spec/core/exec/modules.html#alloc-module

        // 6.2 allocate functions
        for func in module.funcs.iter() {
            if let ast::FuncKind::Import(i) = &func.kind {
                if i.mod_name.0 != "env" || i.name.0 != "putchar" {
                    return Err(Trap::unknown_import(i, "function", func.start));
                }
            }
        }

        // 6.3 allocate table
        let mut table = Table::allocate(&module.tables)?;
        // 6.4 allocate memory
        let mut memory = Memory::allocate(&module.memories)?;

        // 7. and 8. push empty frame (unnecessary for now)
        let stack = Stack::new();

        // 9. add element segments to table
        for elem in module.elems.iter() {
            table.new_elem(elem, &globals)?;
        }

        // 10. add data segments to memory
        for data in module.data.iter() {
            memory.new_data(data, &globals)?;
        }

        // 11. and 12. pop frame (unnecessary for now)

        Ok(Self {
            module,
            table,
            stack,
            memory,
            globals,
        })
    }

    fn invoke(&mut self, funcidx: u32) -> ExecResult {
        self.module.funcs[funcidx as usize].execute(self)
    }

    // As the last step of instantiation, invoke start function
    pub fn execute(&mut self) -> Option<Box<Trap>> {
        // 15. If the start function is not empty, invoke it
        if let Some(start) = &self.module.entrypoint {
            // Execute entrypoint
            return self.invoke(start.idx).err();
        }

        // Note: This behavior is not described in spec. But current Clang does not emit 'start' section
        // even if a main function is included in the source. Instead, wasm-ld recognizes '_start' exported
        // function as entrypoint. Here the behavior is implemented
        for export in self.module.exports.iter() {
            if export.name.0 == "_start" {
                if let ast::ExportKind::Func(idx) = &export.kind {
                    return self.invoke(*idx).err();
                }
            }
        }

        Some(Trap::new(TrapReason::StartFuncNotFound, self.module.start))
    }
}

pub fn execute<'a>(module: ast::Module<'a>) -> Option<Box<Trap>> {
    match Machine::instantiate(&module) {
        Ok(mut machine) => machine.execute(),
        Err(e) => Some(e),
    }
}

trait Execute<'m, 'a> {
    fn execute(&self, machine: &mut Machine<'m, 'a>) -> ExecResult;
}

impl<'m, 'a> Execute<'m, 'a> for ast::Func<'a> {
    fn execute(&self, machine: &mut Machine<'m, 'a>) -> ExecResult {
        // TODO: Validate external values for imports
        unimplemented!()
    }
}
