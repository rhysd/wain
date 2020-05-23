WebAssembly spec test runner for wain project
=============================================

This crate is a test runner for running [official WebAssembly test suites][testsuite] for [wain][proj]
project. Not intended to be published so it may depend on some external crates to run tests easily.

This crate contains:

- `.wast` text format parser built on top of [wain-syntax-text](../wain-syntax-text)
- test runner to run wain interpreter with the test suites
- [WebAssembly/testsuite][testsuite] repository as a Git submodule

## Usage

Run this crate as command line tool by `cargo run`.

### Run all tests

```
cargo run
```

For running faster, release build would be useful.

```
cargo run --release
```

### Run specific tests

```
cargo run -- ./pass/to/test.wast
```

### Stop running tests on the first failure

```
cargo run -- -f
```

### Compare results between two revisions

For example, let's say to compare `your-branch` branch with `master` branch.

```
# Get results for your branch
git checkout your-branch
cargo run -- -w after.txt

# Get results for master branch
git checkout master
cargo run -- -w before.txt

# Compare results as diff. Summary for one .wast file per line
diff -u before.txt after.txt
```

Note: `colordiff -u` or `git diff --no-index` would be better than `diff -u`.

[proj]: https://github.com/rhysd/wain
[testsuite]: https://github.com/WebAssembly/testsuite
