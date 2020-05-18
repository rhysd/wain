#!/bin/bash

set -e

export PATH=/usr/local/opt/llvm/bin:$PATH

SRC="$1"
BASE="${SRC%.*}"

set -x

clang -O2 -nostdlib --target=wasm32 -Wl,--allow-undefined "${SRC}" -o "${BASE}.wasm"
wasm2wat "${BASE}.wasm" > "${BASE}.wat"
