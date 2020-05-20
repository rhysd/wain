WebAssembly spec test runner for wain project
=============================================

This crate is a test runner for running [official WebAssembly test suites][testsuite] for [wain][proj]
project. Not intended to be published so it may depend on some external crates to run tests easily.

This crate contains:

- `.wast` text format parser built on top of [wain-syntax-text](../wain-syntax-text)
- test runner to run wain interpreter with the test suites
- [WebAssembly/testsuite][testsuite] repository as a Git submodule

## Usage

Run this crate as command line tool:

Run all tests:

```
cargo run
```

Run specific tests:

```
cargo run -- ./pass/to/test.wast
```

Stop running tests on first failure:

```
cargo run -- -f
```

Comparing results between two commits (e.g. `1234567` v.s. `890abcd`):

Note: `colordiff -u` or `git diff --no-index` would be better than `diff -u`.

```
# Get results for commit 1234567
git reset --hard 1234567
cargo run -- -w before.txt

# Get results for commit 890abcd
git reset --hard 890abcd
cargo run -- -w after.txt

# Compare results as diff. Summary for one file per line
diff -u before.txt after.txt
```

For running faster, release build would be useful:

```
cargo build --release
./target/release/spec-test
```

[proj]: https://github.com/rhysd/wain
[testsuite]: https://github.com/WebAssembly/testsuite
