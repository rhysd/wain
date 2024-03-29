extern crate wain_exec;
extern crate wain_syntax_binary;
extern crate wain_validate;

use std::process::exit;
use wain_exec::execute;
use wain_syntax_binary::parse;
use wain_validate::validate;

// examples/hello/hello.wasm
const HELLO_WORLD: [u8; 381] = [
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0d, 0x03, 0x60, 0x01, 0x7f, 0x01, 0x7f, 0x60, 0x01, 0x7f,
    0x00, 0x60, 0x00, 0x00, 0x02, 0x0f, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x07, 0x70, 0x75, 0x74, 0x63, 0x68, 0x61, 0x72,
    0x00, 0x00, 0x03, 0x03, 0x02, 0x01, 0x02, 0x04, 0x05, 0x01, 0x70, 0x01, 0x01, 0x01, 0x05, 0x03, 0x01, 0x00, 0x02,
    0x06, 0x08, 0x01, 0x7f, 0x01, 0x41, 0x90, 0x88, 0x04, 0x0b, 0x07, 0x13, 0x02, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72,
    0x79, 0x02, 0x00, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72, 0x74, 0x00, 0x02, 0x0a, 0xc2, 0x01, 0x02, 0xa9, 0x01, 0x01,
    0x12, 0x7f, 0x23, 0x80, 0x80, 0x80, 0x80, 0x00, 0x21, 0x01, 0x41, 0x10, 0x21, 0x02, 0x20, 0x01, 0x20, 0x02, 0x6b,
    0x21, 0x03, 0x20, 0x03, 0x24, 0x80, 0x80, 0x80, 0x80, 0x00, 0x20, 0x03, 0x20, 0x00, 0x36, 0x02, 0x0c, 0x02, 0x40,
    0x03, 0x40, 0x20, 0x03, 0x28, 0x02, 0x0c, 0x21, 0x04, 0x20, 0x04, 0x2d, 0x00, 0x00, 0x21, 0x05, 0x41, 0x18, 0x21,
    0x06, 0x20, 0x05, 0x20, 0x06, 0x74, 0x21, 0x07, 0x20, 0x07, 0x20, 0x06, 0x75, 0x21, 0x08, 0x20, 0x08, 0x45, 0x0d,
    0x01, 0x20, 0x03, 0x28, 0x02, 0x0c, 0x21, 0x09, 0x20, 0x09, 0x2d, 0x00, 0x00, 0x21, 0x0a, 0x41, 0x18, 0x21, 0x0b,
    0x20, 0x0a, 0x20, 0x0b, 0x74, 0x21, 0x0c, 0x20, 0x0c, 0x20, 0x0b, 0x75, 0x21, 0x0d, 0x20, 0x0d, 0x10, 0x80, 0x80,
    0x80, 0x80, 0x00, 0x1a, 0x20, 0x03, 0x28, 0x02, 0x0c, 0x21, 0x0e, 0x41, 0x01, 0x21, 0x0f, 0x20, 0x0e, 0x20, 0x0f,
    0x6a, 0x21, 0x10, 0x20, 0x03, 0x20, 0x10, 0x36, 0x02, 0x0c, 0x0c, 0x00, 0x0b, 0x0b, 0x41, 0x10, 0x21, 0x11, 0x20,
    0x03, 0x20, 0x11, 0x6a, 0x21, 0x12, 0x20, 0x12, 0x24, 0x80, 0x80, 0x80, 0x80, 0x00, 0x0f, 0x0b, 0x15, 0x01, 0x01,
    0x7f, 0x41, 0x80, 0x88, 0x80, 0x80, 0x00, 0x21, 0x00, 0x20, 0x00, 0x10, 0x81, 0x80, 0x80, 0x80, 0x00, 0x0f, 0x0b,
    0x0b, 0x15, 0x01, 0x00, 0x41, 0x80, 0x08, 0x0b, 0x0e, 0x48, 0x65, 0x6c, 0x6c, 0x6f, 0x2c, 0x20, 0x77, 0x6f, 0x72,
    0x6c, 0x64, 0x0a, 0x00, 0x00, 0x20, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x01, 0x19, 0x03, 0x00, 0x07, 0x70, 0x75, 0x74,
    0x63, 0x68, 0x61, 0x72, 0x01, 0x05, 0x70, 0x72, 0x69, 0x6e, 0x74, 0x02, 0x06, 0x5f, 0x73, 0x74, 0x61, 0x72, 0x74,
    0x00, 0x25, 0x09, 0x70, 0x72, 0x6f, 0x64, 0x75, 0x63, 0x65, 0x72, 0x73, 0x01, 0x0c, 0x70, 0x72, 0x6f, 0x63, 0x65,
    0x73, 0x73, 0x65, 0x64, 0x2d, 0x62, 0x79, 0x01, 0x05, 0x63, 0x6c, 0x61, 0x6e, 0x67, 0x05, 0x39, 0x2e, 0x30, 0x2e,
    0x31,
];

fn main() {
    // Parse Wasm binary into syntax tree. Return value is wain_ast::Root
    let tree = match parse(&HELLO_WORLD) {
        Ok(tree) => tree,
        Err(err) => {
            eprintln!("Parse failed: {}", err);
            exit(1);
        }
    };

    // Validate module. Validation must be doen before execution
    if let Err(err) = validate(&tree) {
        eprintln!("This .wasm file is invalid: {}", err);
        exit(1);
    }

    // Execute module. It invokes 'start function'
    if let Err(trap) = execute(&tree.module) {
        eprintln!("Execution was trapped: {}", trap);
        exit(1);
    }
}
