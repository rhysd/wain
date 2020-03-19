#! /bin/bash

set -e

cargo clean -p wain -p wain-ast -p wain-syntax-text -p wain-validate
cargo clippy --all -- -D warnings
