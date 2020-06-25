#![feature(test)]

extern crate test;

#[cfg(test)]
mod example_benches {
    use std::env;
    use std::fmt;
    use std::fs;
    use std::io::{self, Read, Write};
    use test::Bencher;
    use wain_exec::{DefaultImporter, Runtime};
    use wain_syntax_binary::parse;
    use wain_validate::validate;

    struct Discard;

    impl Read for Discard {
        fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
            Ok(0)
        }
    }

    impl Write for Discard {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            Ok(buf.len())
        }
        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    fn unwrap<T, E: fmt::Display>(result: Result<T, E>) -> T {
        match result {
            Ok(r) => r,
            Err(e) => panic!("Unexpected error: {}", e),
        }
    }

    fn read_source(name: &'static str) -> Vec<u8> {
        let mut file = env::current_dir().unwrap();
        file.push("examples");
        file.push(name);
        fs::read(file).unwrap()
    }

    macro_rules! bench_example {
        ($name:ident, $file:expr) => {
            #[bench]
            pub fn $name(b: &mut Bencher) {
                let source = read_source($file);
                b.iter(|| {
                    let ast = unwrap(parse(&source));
                    unwrap(validate(&ast));
                    let importer = DefaultImporter::with_stdio(Discard, Discard);
                    let mut runtime = unwrap(Runtime::instantiate(&ast.module, importer));
                    unwrap(runtime.invoke("_start", &[]));
                });
            }
        };
    }

    bench_example!(hello, "hello/hello.wasm");
    bench_example!(hello_global, "hello/hello_global.wasm");
    bench_example!(hello_indirect_call, "hello/hello_indirect_call.wasm");
    bench_example!(hello_struct, "hello/hello_struct.wasm");
    bench_example!(sqrt, "sqrt.wasm");
    bench_example!(pi, "pi.wasm");
    bench_example!(mandelbrot, "mandelbrot.wasm");
    bench_example!(nbodies, "nbodies.wasm");
    bench_example!(brainfxxk, "brainfxxk.wasm");
    bench_example!(primes, "primes.wasm");
    bench_example!(quicksort, "quicksort.wasm");
    bench_example!(mt19937, "mt19937.wasm");
    bench_example!(n_queens, "n_queens.wasm");
    bench_example!(y_combinator, "y_combinator.wasm");
    bench_example!(fib, "fib.wasm");
    bench_example!(boyer_moore, "boyer_moore.wasm");
}
