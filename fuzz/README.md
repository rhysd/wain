How to run fuzzing tests
========================

## Setup

Install [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz).

```
cargo install cargo-fuzz
```

## Run fuzzer

Start fuzzer:

```
cargo +nightly fuzz run {target}
```

After crash was found, the input which caused the crash is put in `fuzz/artifacts/{target}`.

Current targets are:

- `binary_parser`: Fuzzing `wain_syntax_binary::Parser`
- `text_parser`: Fuzzing `wain_syntax_text::Parser`
- `validation`: Fuzzing `wain_validate::Validate`
