extern crate wain_exec;
extern crate wain_syntax_text;
extern crate wain_validate;

use std::io;
use std::process::exit;
use wain_exec::{DefaultImporter, Runtime, Value};
use wain_syntax_text::parse;
use wain_validate::validate;

const MODULE_ADD: &str = r#"
(module
  (type (;0;) (func (param i32 i32) (result i32)))
  (func $add (type 0) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 16
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    local.get 0
    i32.store offset=12
    local.get 4
    local.get 1
    i32.store offset=8
    local.get 4
    i32.load offset=12
    local.set 5
    local.get 4
    i32.load offset=8
    local.set 6
    local.get 5
    local.get 6
    i32.add
    local.set 7
    local.get 7
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "add" (func $add)))
"#;

fn main() {
    // Parse WAT text into syntax tree
    let tree = match parse(MODULE_ADD) {
        Ok(tree) => tree,
        Err(err) => {
            eprintln!("Parse failed: {}", err);
            exit(1);
        }
    };

    // Validate module
    if let Err(err) = validate(&tree) {
        eprintln!("This .wat file is invalid: {}", err);
        exit(1);
    }

    // Create default importer to call external function supported by default
    let stdin = io::stdin();
    let stdout = io::stdout();
    let importer = DefaultImporter::with_stdio(stdin.lock(), stdout.lock());

    // Make abstract machine runtime. This instantiates Wasm module
    let mut runtime = match Runtime::instantiate(&tree.module, importer) {
        Ok(rt) => rt,
        Err(err) => {
            eprintln!("could not instantiate module: {}", err);
            exit(1);
        }
    };

    // `int add(int, int)` is exported as `(func (param i32) (result i32))`.
    // Let's invoke add(10, 32). `Value` is an enum to represent arbitrary value of Wasm. Wasm has
    // i32, i64, f32, f64 basic types.
    match runtime.invoke("add", &[Value::I32(10), Value::I32(32)]) {
        Ok(ret) => {
            // `ret` is type of `Option<Value>` where it contains `Some` value when the invoked
            // function returned a value. Otherwise it's `None` value.
            if let Some(Value::I32(i)) = ret {
                println!("10 + 32 = {}", i);
            } else {
                unreachable!();
            }
        }
        Err(trap) => eprintln!("Execution was trapped: {}", trap),
    }
}
