#![feature(test)]

extern crate test;

#[cfg(test)]
mod parser_benches {
    use std::env;
    use std::fs;
    use std::path::PathBuf;
    use test::Bencher;

    fn example_path(name: &'static str) -> PathBuf {
        let mut file = env::current_dir().unwrap();
        file.push("examples");
        file.push(name);
        file
    }

    mod binary {
        use super::*;

        macro_rules! bench_binary_parser {
            ($name:ident, $file:expr) => {
                #[bench]
                pub fn $name(b: &mut Bencher) {
                    let file = example_path($file);
                    let source = fs::read(file).unwrap();
                    b.iter(|| {
                        if let Err(err) = wain_syntax_binary::parse(&source) {
                            panic!("binary parse failed at {}: {}", $file, err);
                        }
                    });
                }
            };
        }

        bench_binary_parser!(brainfxxk, "brainfxxk.wasm");
        bench_binary_parser!(guessing_game, "guessing_game.wasm");
        bench_binary_parser!(mandelbrot, "mandelbrot.wasm");
        bench_binary_parser!(mt19937, "mt19937.wasm");
        bench_binary_parser!(nbodies, "nbodies.wasm");
        bench_binary_parser!(n_queens, "n_queens.wasm");
        bench_binary_parser!(pi, "pi.wasm");
        bench_binary_parser!(primes, "primes.wasm");
        bench_binary_parser!(quicksort, "quicksort.wasm");
        bench_binary_parser!(sqrt, "sqrt.wasm");
    }

    mod text {
        use super::*;

        macro_rules! bench_text_parser {
            ($name:ident, $file:expr) => {
                #[bench]
                pub fn $name(b: &mut Bencher) {
                    let file = example_path($file);
                    let source = fs::read_to_string(file).unwrap();
                    b.iter(|| {
                        if let Err(err) = wain_syntax_text::parse(&source) {
                            panic!("text parse failed at {}: {}", $file, err);
                        }
                    });
                }
            };
        }

        bench_text_parser!(brainfxxk, "brainfxxk.wat");
        bench_text_parser!(guessing_game, "guessing_game.wat");
        bench_text_parser!(mandelbrot, "mandelbrot.wat");
        bench_text_parser!(mt19937, "mt19937.wat");
        bench_text_parser!(nbodies, "nbodies.wat");
        bench_text_parser!(n_queens, "n_queens.wat");
        bench_text_parser!(pi, "pi.wat");
        bench_text_parser!(primes, "primes.wat");
        bench_text_parser!(quicksort, "quicksort.wat");
        bench_text_parser!(sqrt, "sqrt.wat");
    }
}
