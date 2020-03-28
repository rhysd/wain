### How to run these examples

Please use [Clang](https://clang.llvm.org/) for compiling C source into Wasm and
[wabt](https://github.com/WebAssembly/wabt) to compile C sources into WebAssembly text-format.

Run binary format file:

```
$ clang -nostdlib --target=wasm32 -Wl,--allow-undefined hello/hello.c -o hello.wasm
$ wain hello.wasm
Hello, world
```

Run text format file:

```
$ clang -nostdlib --target=wasm32 -Wl,--allow-undefined hello/hello.c -o hello.wasm
$ wasm2wat hello.wasm > hello.wat
$ wain hello.wat
Hello, world
```

Or execute pre-built files.

```
$ wain hello/hello.wat
$ wain hello/hello.wasm
```
