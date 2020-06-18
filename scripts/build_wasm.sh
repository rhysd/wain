#!/bin/bash

set -e

export PATH=/usr/local/opt/llvm/bin:$PATH

_build_c_file() {
    local src base opts clang flags
    src="$1"
    base="${src%.*}"
    opts=("${@:2}")

    if [[ "$src" == *.cpp ]]; then
        clang="clang++"
        flags="-std=c++17"
    else
        clang="clang"
        flags=""
    fi

    set -x

    "$clang" -nostdlib --target=wasm32 -Wl,--allow-undefined "${flags}" "${src}" -o "${base}.wasm" "${opts[@]}"
    wasm2wat "${base}.wasm" > "${base}.wat"

    { set +x; } 2>/dev/null
}

_build_c_files_in_dir() {
    local file files opts
    files=("${1%/}"/*.c)
    opts=("${@:2}")

    for file in "${files[@]}"; do
        _build_c_file "$file" "${opts[@]}"
    done
}

if [ -f "$1" ]; then
    _build_c_file "$1" "${@:2}"
elif [ -d "$1" ]; then
    _build_c_files_in_dir "$1" "${@:2}"
else
    echo "No such path: ${1}" 1>&2
    exit 1
fi

