This directory shows some examples to know how to use [wain](https://github.com/rhysd/wain) interpreter
from Rust.

- [execute.rs](./execute.rs): Parse `.wasm` binary file into tree. Then validate and execute it.
- [wat.rs](./execute.rs): Parse `.wat` text file into tree. Then validate and execute it.
- [invoke.rs](./invoke.rs): Parse and validate Wasm module which only includes one `int add(int)`
  function. Then instantiate an abstract machine runtime and invoke the `add` function with
  arguments from Rust.

These examples can be run easily via `cargo run --example`.

```
$ git clone https://github.com/rhysd/wain.git && cd wain/
$ cargo run --example execute
$ cargo run --example wat
$ cargo run --example invoke
```
