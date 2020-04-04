wain-syntax-text
================
[![crates.io][crates-io-badge]][crates-io]
[![CI][ci-badge]][ci]

[`wain-syntax-text`][gh] is a crate to parse [WebAssembly text format][wasm-spec-text] files.

This crate is part of larger [wain][proj] project.


## Installation

```toml
[dependencies]
wain-syntax-text = "0"
```


## Usage

Using `wain_syntax_text::parse()` is the easiest way.

```rust
extern crate wain_syntax_text;

use wain_syntax_text::parse;

let source = "(module
  (memory 0 2)
  (table 0 1 1 funcref)
  (export "_start" (func $_start))
  (func $_start)
)";

match parse(source) {
    Ok(tree) => { /* `tree` is a parsed syntax tree */ }
    Err(err) => eprintln!("Error! {}", err),
}
```

For the syntax tree structure parsed by this library, please see [wain-ast][ast] crate.

`wain_syntax_text::lexer::Lexer` lexes a text format source.

```rust
extern crate wain_syntax_text;

use wain_syntax_text::lexer::Lexer;

let source = "(module
  (memory 0 2)
  (table 0 1 1 funcref)
  (export "_start" (func $_start))
  (func $_start)
)";

// Lexer implements Iterator which traverses tokens in the given source
let mut lexer = Lexer::new(source);
for lexed in lexer {
    let (token, offset) = lexed.unwrap();
    // `token` is a lexed token
    // `offset` is a byte offset in the source
    if let Token::Symbol(sym) = token {
        println!("Symbol found: {}", sym);
    }
}
```

APIs are provided for each logic:

- Lexer and parser for WAT syntax tree
- WAT to WASM translation
- Composing multiple WASM modules into single WASM module

Working examples can be seen at [examples/api/ directory][examples]

Please read documentation (not yet) for details.


## Implementation

![Sequence to parse Wasm](https://github.com/rhysd/ss/blob/master/wain/parsing-diagram.png?raw=true)

1. Lex and parse `.wat` file into WAT sytnax tree which is dedicated for text format resolving many
   syntax sugars. Since multiple modules can be put in `.wat` file, it can be parsed into multiple trees
2. Translate the WAT syntax trees into common Wasm syntax trees (`wain_ast::Root`) resolving identifiers.
   Identifiers may refer things not defined yet (forward references) so `.wat` file cannot be parsed
   into common Wasm syntax trees directly
3. Compose a single module from the multiple Wasm syntax trees following
   [spec](https://webassembly.github.io/spec/core/text/modules.html#text-module)


## License

[the MIT license](./LICENSE.txt)

[ci-badge]: https://github.com/rhysd/wain/workflows/CI/badge.svg?branch=master&event=push
[ci]: https://github.com/rhysd/wain/actions?query=workflow%3ACI+branch%3Amaster+event%3Apush
[crates-io-badge]: https://img.shields.io/crates/v/wain-syntax-text.svg
[crates-io]: https://crates.io/crates/wain-syntax-text
[gh]: https://github.com/rhysd/wain/tree/master/wain-syntax-text
[wasm-spec-text]: https://webassembly.github.io/spec/core/text/index.html
[proj]: https://github.com/rhysd/wain
[ast]: https://crates.io/crates/wain-ast
[examples]: https://github.com/rhysd/wain/tree/master/examples/api
