## How to run Wasm examples

Since WebAssembly is a portable format, you don't need to rebuild `.wasm` or `.wat` files to run
them on your local machine.

Simply run:

```
$ wain hello/hello.wat
$ wain hello/hello.wasm
```

## How to rebuild Wasm examples

Please use [Clang](https://clang.llvm.org/) for compiling C source into Wasm and
[wabt](https://github.com/WebAssembly/wabt) to compile C sources into WebAssembly text-format.

If you're using macOS, please install Clang via Homebrew. Apple Clang does not seem to support
WebAssembly for now.

```
$ brew install llvm
```

[Makefile](./Makefile) is available to build files easily.

To rebuild all sources:

```
$ make
```

To rebuild a specific Wasm file in binary-format:

```
$ make hello/hello.wasm
```

To rebuild a specific Wasm file in text-format:

```
$ make hello/hello.wat
```

## How to run Rust API examples

Please see [api/README.md](./api/README.md).
