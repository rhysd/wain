wain
====
[![crates.io][crates-io-badge]][crates-io]
[![CI][ci-badge]][ci]

[wain][proj] is a **W**eb**A**ssembly **IN**terpreter written in Rust from scratch with zero dependency.
An implementation of [WebAssembly][wasm-spec].

![screencast](https://github.com/rhysd/ss/blob/master/wain/main.gif?raw=true)

**Features:**

- No unsafe code. Memory safety and no undefined behavior are guaranteed
- No external crate dependencies
- Efficiency. Avoid unnecessary allocations and run instructions as much as possible
- Modular implementation. Binary format parser, text format parser, validator, executor are
  developed as separate libraries

Note that this project is in progress. Before v1.0.0 means experimental. Not all of the features
are implemented yet. Current status is that all the MVP implementations have been done and many
tasks are remaining.

**Roadmap to v1.0.0 (priority order):**

- Add sufficient tests to all libraries and fuzz tests for parsers
- Pass all [spec tests][wasm-test-suite]
- Add benchmarks to track performance
- Introduce internal representation to execute instructions more efficiently
- Add documentation for every public APIs

Please see [the task board](https://github.com/rhysd/wain/projects/1) for current progress.

This project started for fun and understanding Wasm deeply.


## Installation

`wain` crate is not published yet. Please clone this repository and build the project by `cargo build`.

Minimum supported Rust version is 1.42.0.

```
$ cargo install wain
$ wain --help
```

If you don't want to run text format code, it can be omitted:

```
# Only run binary format files
$ cargo install wain --no-default-features --features binary
```

## Usage

### `wain` command

Run a binary format Wasm source

```
$ wain examples/hello/hello.wasm
Hello, world
```

Run a text format Wasm source:

```
$ wain examples/hello/hello.wat
Hello, world
```

For examples wain can execute, please see [examples](./examples).

Current restrictions are as follows:

- Only `int putchar(int)` and `int getchar()` are implemented as external functions
- wain can run only one module at once. It means that importing things from other modules does not
  work yet
- Many extensions like threads, WASI support, SIMD support, ... are not implemented yet


## Structure

wain consists of multiple crates.

- **[wain](.):** Command line tool to execute given Wasm sources
- **[wain-ast](./wain-ast):** Abstract syntax tree definition. Implementation of [Wasm structure spec][wasm-spec-structure].
  This syntax tree is common for both binary format and text format
- **[wain-syntax-binary](./wain-syntax-binary):** Parser for Wasm binary format (`.wasm` files).
  Implementation for [Wasm binary format spec][wasm-spec-bin]. It parses `&[u8]` value into
  `wain_ast::Root` abstract syntax tree
- **[wain-syntax-text](./wain-syntax-text):** Parser for Wasm text format (`.wat` files).
  Implementation for [Wasm text format spec][wasm-spec-text]. It parses `&str` value into
  `wain_ast::Root` abstract syntax tree
- **[wain-validate](./wain-validate):** Validator of a Wasm abstract syntax tree. Implementation of
  [Wasm validation spec][wasm-spec-validation]
- **[wain-exec](./wain-exec):** Executor which interprets a Wasm abstract syntax tree. Implementation
  of [Wasm execution spec][wasm-spec-exec]. It directly interprets a syntax tree for now, but it
  would translate it into an internal representation and execute it for efficiency in the future


## How it works

Here I note some points on each phase of interpretation.

### Parsing

![Sequence to parse Wasm](https://github.com/rhysd/ss/blob/master/wain/parsing-diagram.png?raw=true)

[wain-syntax-binary](./wain-syntax-binary) parses `.wasm` binary file into `wain_ast::Root` abstract
syntax tree following [binary format spec][wasm-spec-bin]. Wasm binary format is designed to be
parsed by basic LL(1) parser. So parsing is very straightforward.
[Parser implementation](./wain-syntax-binary/src/parser.rs) is smaller than 1000 lines.

In contrast, implementation of parsing text format is more complicated. [wain-syntax-text](./wain-syntax-text)
parses `.wat` text file into `wain_ast::Root` abstract syntax tree following [text format spec][wasm-spec-text].

1. Lex and parse `.wat` file into WAT sytnax tree which is dedicated for text format resolving many
   syntax sugars. Since multiple modules can be put in `.wat` file, it can be parsed into multiple trees
2. Translate the WAT syntax trees into common Wasm syntax trees (`wain_ast::Root`) resolving identifiers.
   Identifiers may refer things not defined yet (foward references) so `.wat` file cannot be parsed
   into common Wasm syntax trees directly
3. Compose a single module from the multiple Wasm syntax trees following
   [spec](https://webassembly.github.io/spec/core/text/modules.html#text-module)

### Validation

Validation is done by traversing a given Wasm syntax tree in [wain-validate](./wain-validate) crate.
Conforming [spec][wasm-spec-validation], following things are validated:

- In Wasm, every reference is an index. It validates all indices are not out of bounds
- Wasm is designed to check stack operations statically. It validates instructions sequences with
  emulating stack state
- Type check is best-effort due to polymorphic instruction `select`. Since almost all instructions
  are not polymorphic, almost all type checks can be done in validation

Conforming the spec, wain validates instructions after `unreachable` instruction. For example,

```wat
(unreachable) (i64.const 0) (i32.add)
```

`i32.add` is invalid because it should take two `i32` values from stack but at least one `i64` value
is in the stack.

### Execution

[wain-exec](./wain-exec) crate interprets a Wasm syntax tree conforming [spec][wasm-spec-exec]. Thanks
to validation, checks at runtime are minimal (e.g. function signature on indirect call).

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
[crates-io-badge]: https://img.shields.io/crates/v/wain.svg
[crates-io]: https://crates.io/crates/wain
[proj]: https://github.com/rhysd/wain
[wasm-spec]: https://webassembly.github.io/spec/core/index.html
[wasm-test-suite]: https://github.com/WebAssembly/testsuite
[wasm-spec-structure]: https://webassembly.github.io/spec/core/syntax/index.html
[wasm-spec-bin]: https://webassembly.github.io/spec/core/binary/index.html
[wasm-spec-text]: https://webassembly.github.io/spec/core/text/index.html
[wasm-spec-validation]: https://webassembly.github.io/spec/core/valid/index.html
[wasm-spec-exec]: https://webassembly.github.io/spec/core/exec/index.html
