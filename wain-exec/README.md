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

Using `wain_exec::execute()` is the easiest way. It invokes a start function in given module if presents.
Otherwise it invokes a function exported as `_start`.

```rust
extern crate wain_syntax_binary;
extern crate wain_validate;
extern crate wain_exec;

use std::fs;
use std::process::exit;
use wain_syntax_binary::parse;
use wain_validate::validate;
use wain_exec::execute;

// Read wasm binary
let source = fs::read("foo.wasm").unwrap();

// Parse binary into syntax tree
let tree = match parse(&source) {
    Ok(tree) => tree,
    Err(err) => {
        eprintln!("Could not parse: {}", err);
        exit(1);
    }
};

// Validate module
if let Err(err) = validate(&tree) {
    eprintln!("This .wasm file is invalid!: {}", err);
    exit(1);
}

// Execute module
if let Err(trap) = execute(&tree.module) {
    eprintln!("Execution was trapped: {}", trap);
    exit(1);
}
```

Or invoke specific exported function with arguments

```rust
// ...(snip)

use wain_exec::{Runtime, DefaultImporter, Value};
use std::io;

// Create default importer to call external function supported by default
let stdin = io::stdin();
let stdout = io::stdout();
let importer = DefaultImporter::with_stdio(stdin.lock(), stdout.lock());

// Make abstract machine runtime. It instantiates a module instance
let mut runtime = match Runtime::instantiate(&tree.module, importer) {
    Ok(m) => m,
    Err(err) => {
        eprintln!("could not instantiate module: {}", err);
        exit(1);
    }
};

// Let's say `int add(int, int)` is exported
match runtime.invoke("add", &[Value::I32(10), Value::I32(32)]) {
    Ok(ret) => {
        // `ret` is type of `Option<Value>` where it contains `Some` value when the invoked
        // function returned a value. Otherwise it's `None` value.
        if let Some(Value::I32(i)) = ret {
            println!("10 + 32 = {}", i);
        } else {
            unreachable!();
        }
    }
    Err(trap) => eprintln!("Execution was trapped: {}", trap),
}
```

[Trap](https://webassembly.github.io/spec/core/exec/runtime.html#results) is returned as `Err` part
of `Result`.

`wain_exec::execute()` buffers stdin and stdout by default for now (this behavior may change in
the future). If this behavior is not acceptable, please specify your `io::Write`/`io::Read` values
for stdout/stdin at `wain_exec::Runtime::new()`. Then run the module by `wain_exec::Runtime::invoke()`.

By default, only following C functions are supported in `env` module are supported as external functions

- `int putchar(int)` (in wasm `(func (param i32) (result i32))`)
- `int getchar(void)` (in wasm `(func (param) (result i32))`)
- `void *memcpy(void *, void *, size_t)` (in wasm `(func (param i32 i32 i32) (result i32))`)
- `void abort(void)` (in wasm `(func (param) (result))`)

But you can implement your own struct which implements `wain_exec::Importer` for defining external
functions from Rust side.

```rust
extern crate wain_exec;
extern crate wain_ast;
use wain_exec::{Runtime, Stack, Memory, Importer, ImportInvokeError, ImportInvalidError}
use wain_ast::ValType;

struct YourOwnImporter {
    // ...
}

impl Importer for YourOwnImporter {
    fn validate(&self, name: &str, params: &[ValType], ret: Option<ValType>) -> Option<ImportInvalidError> {
        // `name` is a name of function to validate. `params` and `ret` are the function's signature.
        // Return ImportInvalidError::NotFound when the name is unknown.
        // Return ImportInvalidError::SignatureMismatch when signature does not match.
        // wain_exec::check_func_signature() utility is would be useful for the check.
    }
    fn call(&mut self, name: &str, stack: &mut Stack, memory: &mut Memory) -> Result<(), ImportInvokeError> {
        // Implement your own function call. `name` is a name of function and you have full access
        // to stack and linear memory. Pop values from stack for getting arguments and push value to
        // set return value.
        // Note: Consistency between imported function signature and implementation of this method
        // is your responsibility.
        // On invocation failure, return ImportInvokeError::Fatal. It is trapped by interpreter and it
        // stops execution immediately.
    };
}

let ast = ...; // Parse abstract syntax tree and validate it

let mut runtime = Runtime::instantiate(&ast.module, YourOwnImporter{ /* ... */ }).unwrap();
let result = runtime.invoke("do_something", &[]);
```

Working examples can be seen at [examples/api/ directory][examples]

Please read documentation (not yet) for details.


## Implementation

Thanks to validation, checks at runtime are minimal (e.g. function signature on indirect call).

1. Allocate memory, table, global variables. Initialize stack
2. Interpret syntax tree nodes pushing/popping values to/from stack

Currently wain interprets a Wasm syntax tree directly. I'm planning to define an intermediate
representation which can be interpreted faster.

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
[examples]: https://github.com/rhysd/wain/tree/master/examples/api
