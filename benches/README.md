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
