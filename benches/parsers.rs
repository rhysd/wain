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

    macro_rules! bench_suites {
        ($($name:ident,)+) => {
            mod binary {
                use super::*;
                $(
                    #[bench]
                    pub fn $name(b: &mut Bencher) {
                        let file = example_path(concat!(stringify!($name), ".wasm"));
                        let source = fs::read(file).unwrap();
                        b.iter(|| {
                            if let Err(err) = wain_syntax_binary::parse(&source) {
                                panic!("binary parse failed at {}.wasm: {}", stringify!($name), err);
                            }
                        });
                    }
                )+
            }

            mod text {
                use super::*;
                $(
                    #[bench]
                    pub fn $name(b: &mut Bencher) {
                        let file = example_path(concat!(stringify!($name), ".wat"));
                        let source = fs::read_to_string(file).unwrap();
                        b.iter(|| {
                            if let Err(err) = wain_syntax_text::parse(&source) {
                                panic!("text parse failed at {}.wat: {}", stringify!($name), err);
                            }
                        });
                    }
                )+
            }
        }
    }

    bench_suites!(
        brainfxxk,
        guessing_game,
        mandelbrot,
        mt19937,
        nbodies,
        n_queens,
        pi,
        primes,
        quicksort,
        sqrt,
        y_combinator,
        fib,
        boyer_moore,
    );
}
