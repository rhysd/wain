#! /bin/bash

set -e

cargo clean -p wain -p wain-ast -p wain-wat
cargo clippy --all -- -D warnings
