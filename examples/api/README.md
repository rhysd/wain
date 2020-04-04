This directory shows some examples to know how to use [wain](https://github.com/rhysd/wain) interpreter
from Rust.

- [execute.rs](./execute.rs): Parse `.wasm` binary file into tree. Then validate and execute it.
- [wat.rs](./execute.rs): Parse `.wat` text file into tree. Then validate and execute it.
- [invoke.rs](./execute.rs): Parse and validate Wasm module which only includes one `int add(int)`
  function. Then instantiate an abstract machine and invoke the `add` function with arguments from
  Rust.
