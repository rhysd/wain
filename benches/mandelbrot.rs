#![feature(test)]

extern crate test;

#[cfg(test)]
mod benches {
    use std::env;
    use std::fs;
    use std::io::{self, Read, Write};
    use test::Bencher;
    use wain_exec::{DefaultImporter, Machine};
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

    fn unwrap<T, E: std::fmt::Display>(result: Result<T, E>) -> T {
        match result {
            Ok(r) => r,
            Err(e) => panic!("Unexpected error: {}", e),
        }
    }

    #[bench]
    pub fn mandelbrot_wasm(b: &mut Bencher) {
        let mut file = env::current_dir().unwrap();
        file.push("examples");
        file.push("mandelbrot.wasm");
        let source = fs::read(file).unwrap();

        b.iter(|| {
            let ast = unwrap(parse(&source));
            unwrap(validate(&ast));
            let importer = DefaultImporter::with_stdio(Discard, Discard);
            let mut machine = unwrap(Machine::instantiate(&ast.module, importer));
            unwrap(machine.execute());
        });
    }
}
