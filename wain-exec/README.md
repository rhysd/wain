wain-exec
=========
[![crates.io][crates-io-badge]][crates-io]
[![CI][ci-badge]][ci]

[`wain-exec`][gh] is a crate to execute a WebAssembly abstract syntax tree.
Execution logic is defined in [spec][wasm-spec-exec]

This crate is part of larger [wain][proj] project.


## Installation

```toml
[dependencies]
wain-exec = "0"
```


## Usage

This crate assumes given Wasm syntax tree was [validated][wasm-spec-validation].

It takes `wain_ast::Module` value and validates it. The tree can be parsed by `wain-syntax-binary`
and `wain-syntax-text` parsers and validated by `wain-validate` validator:

- [wain-ast](https://crates.io/crates/wain-ast)
- [wain-syntax-text](https://crates.io/crates/wain-syntax-text)
- [wain-syntax-binary](https://crates.io/crates/wain-syntax-binary)
- [wain-validate](https://crates.io/crates/wain-validate)

Using `wain_exec::execute()` is the easiest way. It returns `wain_exec::Run` enum which represents
how the module was run.

```rust
extern crate wain_syntax_binary;
extern crate wain_validate;
extern crate wain_exec;

use std::fs;
use std::process::exit;
use wain_syntax_binary::parse;
use wain_validate::validate;
use wain_exec::{execute, Run};

let source = fs::read("foo.wasm").unwrap();
let tree = parse(&source).unwrap();

if let Err(err) = validate(&tree) {
    eprintln!("This .wasm file is invalid!: {}", err);
    exit(1);
}

match execute(tree.module) {
    Ok(run) => {
        if let Run::Warning(msg) = run {
            eprintln!("Warning: {}, msg);
        }
    }
    Err(trap) => eprintln!("Execution was trapped: {}", trap),
}
```

[Trap](https://webassembly.github.io/spec/core/exec/runtime.html#results) is returned as `Err` part
of `Result`.

`wain_exec::execute()` buffers stdin and stdout by default for now (this behavior may change in
the future). If this behavior is not acceptable, please specify your `io::Write`/`io::Read` values
for stdout/stdin at `wain_exec::Machine::new()`. Then run the module by `wain_exec::Machine::execute()`.

By default, only following C functions are supported in `env` module are supported as external functions

- `int putchar(int)` (in wasm `(func (param i32) (result i32))`)
- `int getchar(void)` (in wasm `(func (param) (result i32))`
- `void *memcpy(void *, void *, size_t)` (in wasm `(func (param i32 i32 i32) (result i32))`)

But you can implement your own struct which implements `wain_exec::Importer` for defining external
functions from Rust side.

```rust
extern crate wain_exec;
use wain_exec::{Machine, Stack, Memory, Importer}

struct YourOwnImporter {
    // ...
}

impl Importer for YourOwnImporter {
    fn call(&mut self, name: &str, stack: &mut Stack, memory: &mut Memory) -> Result<(), ImportError> {
        // Implement your own function call. `name` is a name of function and you have full access
        // to stack and linear memory. Pop values from stack for getting arguments and push value to
        // set return value.
        // Note: Consistency between imported function signature and implementation of this method
        // is your responsibility.
    };
}

let ast = ...; // Parse abstract syntax tree and validate it

let mut machine = Machine::instantiate(&ast.module, YourOwnImporter{ /* ... */ }).unwrap();
let run = machine.execute().unwrap();
```

Please read documentation (not yet) for details.


## Implementation

Thanks to validation, checks at runtime are minimal (e.g. function signature on indirect call).

1. Allocate memory, table, global variables. Initialize stack
2. Interpret syntax tree nodes pushing/popping values to/from stack

Currently wain interprets a Wasm syntax tree directly. I'm planning to define internal representation
which can be interpreted faster.

Entrypoint is 'start function' which is defined either

1. Function set in `start` section
2. Exported function named `_start` in export section

The 1. is a standard entrypoint but Clang does not emit `start` section. Instead it handles `_start`
function as entrypoint. wain implements both entrypoints (1. is prioritized).


## License

[the MIT license](./LICENSE.txt)

[ci-badge]: https://github.com/rhysd/wain/workflows/CI/badge.svg?branch=master&event=push
[ci]: https://github.com/rhysd/wain/actions?query=workflow%3ACI+branch%3Amaster+event%3Apush
[crates-io-badge]: https://img.shields.io/crates/v/wain-exec.svg
[crates-io]: https://crates.io/crates/wain-exec
[wasm-spec-exec]: https://webassembly.github.io/spec/core/exec/index.html
[gh]: https://github.com/rhysd/wain/tree/master/wain-exec
[proj]: https://github.com/rhysd/wain
[wasm-spec-validation]: https://webassembly.github.io/spec/core/valid/index.html
