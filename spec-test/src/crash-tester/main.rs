use std::env;
use std::io;
use std::io::{BufRead, Read};
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
    eprintln!("Usage: crash-tester {{byte-offset}} {{name}} with proper stdin for arguments and source");
    process::exit(1);
}

fn main() {
    let args: Vec<_> = env::args().skip(1).collect();
    if args.len() < 2 {
        help();
    }
    let args = args.as_slice();

    let offset: usize = args[0].parse().unwrap();
    let name = &args[1];

    let (invoke_args, source) = {
        let mut vals = vec![];
        let mut stdin = io::stdin();
        {
            for line in stdin.lock().lines() {
                let line = line.unwrap();
                if line.is_empty() {
                    // End of arguments part
                    break;
                }

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
        }

        let mut source = String::new();
        stdin.read_to_string(&mut source).unwrap();

        (vals, source)
    };
    let source = &source[offset..];

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
        Err(err) => panic!("cannot instantiate module '{}' at offset {}: {}", source, offset, err),
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
