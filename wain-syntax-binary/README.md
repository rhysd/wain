wain-syntax-binary
==================
[![crates.io][crates-io-badge]][crates-io]
[![CI][ci-badge]][ci]

[`wain-syntax-binary`][gh] is a crate to parse [WebAssembly binary format][wasm-spec-bin] files.

This crate is part of larger [wain][proj] project.


## Installation

```toml
[dependencies]
wain-syntax-binary = "0"
```


## Usage

Using `wain_syntax_binary::parse()` is the easiest way.

```rust
extern crate wain_syntax_binary;

use std::fs;
use wain_syntax_binary::parse;

let source = fs::read("foo.wasm").unwrap();

match parse(&source) {
    Ok(tree) => { /* `tree` is `wain_ast::Root` value */ }
    Err(err) => eprintln!("Error! {}", err),
}
```

For the syntax tree structure parsed by this library, please see [wain-ast][ast] crate.

Using `Parser` struct, it can parse part of Wasm binary.

```rust
extern crate wain_syntax_binary;

use std::fs;
use wain_syntax_binary::Parser;
use wain_ast::DataSegment;

let source = fs::read("data_segment_only.bin").unwrap();

// Parse only data segment
let data: DataSegment<'_> = Parser.parse().unwrap();
```

Working examples can be seen at [examples/api/ directory][examples]

Please read documentation (not yet) for details.


## License

[the MIT license](./LICENSE.txt)

[ci-badge]: https://github.com/rhysd/wain/workflows/CI/badge.svg?branch=master&event=push
[ci]: https://github.com/rhysd/wain/actions?query=workflow%3ACI+branch%3Amaster+event%3Apush
[crates-io-badge]: https://img.shields.io/crates/v/wain-syntax-binary.svg
[crates-io]: https://crates.io/crates/wain-syntax-binary
[gh]: https://github.com/rhysd/wain/tree/master/wain-syntax-binary
[wasm-spec-bin]: https://webassembly.github.io/spec/core/binary/index.html
[proj]: https://github.com/rhysd/wain
[ast]: https://crates.io/crates/wain-syntax-binary
[examples]: https://github.com/rhysd/wain/tree/master/examples/api
