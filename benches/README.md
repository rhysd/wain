How to run benchmarks
=====================

Benchmarks are run via [`cargo bench`](https://doc.rust-lang.org/cargo/commands/cargo-bench.html).
It requires a nightly toolchain for now.

```sh
# Run all benchmarks
cargo +nightly bench

# Run specific benchmark suite
cargo +nightly bench --bench {suite}
```

Followings are the list of benchmark suite specified to `{suite}`:

- `examples`: Executing examples
- `parsers`: Parsing binary/text examples
- `validation`: Validating examples

To compare benchmark results between two revisions, I recommend to use
[cargo-benchcmp](https://github.com/BurntSushi/cargo-benchcmp).

For example, when you want to compare `your-branch` branch with `master` branch:

```sh
# Get benchmark results on master branch
git checkout master
cargo +nightly bench > master

# Get benchmark results on your-branch branch
git checkout your-branch
cargo +nightly bench > your-branch

# Compare the results
cargo benchcmp master your-branch
```
