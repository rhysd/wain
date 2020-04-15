WebAssembly spec test runner for wain project
=============================================

This crate is a test runner for running [official WebAssembly test suites][testsuite] for [wain][proj]
project. Not intended to be published so it may depend on some external crates to run tests easily.

This crate contains:

- `.wast` text format parser built on top of [wain-syntax-text](../wain-syntax-text).
- test runner to run wain interpreter with the test suites
- [WebAssembly/testsuite][testsuite] repository as a Git submodule

## Usage

Run this crate as command line tool:

```
cargo run
```

For running faster, release build would be useful:

```
cargo build --release
./target/release/spec-test
```

[proj]: https://github.com/rhysd/wain
[testsuite]: https://github.com/WebAssembly/testsuite
