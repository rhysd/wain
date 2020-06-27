use std::env;
use std::io;
use std::io::BufRead;
use std::process;
use wain_exec::{DefaultImporter, Runtime, Value};
use wain_syntax_text::parser::Parser;
use wain_syntax_text::wat2wasm::wat2wasm;

struct Discard;

impl io::Read for Discard {
    fn read(&mut self, _buf: &mut [u8]) -> io::Result<usize> {
        Ok(0)
    }
}

impl io::Write for Discard {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(buf.len())
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn help() {
    eprintln!("Usage: crash-tester {{source}} {{byte-offset}} {{name}} [{{type}} {{value}}]...");
    process::exit(1);
}

fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    if args.len() < 2 {
        help();
    }
    let args = &args[..];

    let source = &args[0];
    let offset: usize = args[1].parse().unwrap();
    let source = &source[offset..];
    let name = &args[2];

    let invoke_args = {
        let mut vals = vec![];
        for line in io::stdin().lock().lines() {
            let line = line.unwrap();
            let mut it = line.split(' ');
            let (ty, val) = (it.next().unwrap(), it.next().unwrap());
            let val = match ty {
                "i32" => Value::I32(val.parse().unwrap()),
                "i64" => Value::I64(val.parse().unwrap()),
                "f32" => Value::F32(val.parse().unwrap()),
                "f64" => Value::F64(val.parse().unwrap()),
                unknown => panic!("unknown type {}", unknown),
            };
            vals.push(val);
        }
        vals
    };

    let wat = match Parser::new(source).parse_wat() {
        Ok(root) => root,
        Err(err) => panic!("cannot parse '{}' at offset {}: {}", source, offset, err),
    };

    let ast = match wat2wasm(wat, source) {
        Ok(ast) => ast,
        Err(err) => panic!(
            "cannot convert wat to ast in '{}' at offset {}: {}",
            source, offset, err
        ),
    };

    // Don't validate the tree since validation has been done in spec test

    let importer = DefaultImporter::with_stdio(Discard, Discard);
    let mut runtime = match Runtime::instantiate(&ast.module, importer) {
        Ok(rt) => rt,
        Err(err) => panic!(
            "cannot instantiate module '{}' at offset {}: {}",
            source, offset, err
        ),
    };

    match runtime.invoke(name, &invoke_args) {
        Ok(Some(ret)) => println!("returned: {}", ret),
        Ok(None) => println!("returned nothing"),
        Err(err) => {
            eprintln!("Trapped: {}", err);
            process::exit(1);
        }
    }
}
