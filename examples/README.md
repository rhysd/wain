### How to run these examples

Please use [Clang](https://clang.llvm.org/) and [wabt](https://github.com/WebAssembly/wabt) to
compile C sources into WebAssembly text-format.

```
$ clang -nostdlib --target=wasm32 -Wl,--allow-undefined hello/hello.c -o hello.wasm
$ wasm2wat hello.wasm > hello.wat
$ wain hello.wat
Hello, world
```

Or execute pre-built WebAssembly source.

```
$ wain hello/hello.wat
```
