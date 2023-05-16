extern crate wain_exec;
extern crate wain_syntax_text;
extern crate wain_validate;

use std::process::exit;
use wain_exec::execute;
use wain_syntax_text::parse;
use wain_validate::validate;

// examples/hello/hello.wat
const HELLO_WORLD: &str = r#"
(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $print (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    local.get 3
    local.get 0
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        local.get 3
        i32.load offset=12
        local.set 4
        local.get 4
        i32.load8_u
        local.set 5
        i32.const 24
        local.set 6
        local.get 5
        local.get 6
        i32.shl
        local.set 7
        local.get 7
        local.get 6
        i32.shr_s
        local.set 8
        local.get 8
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        i32.load offset=12
        local.set 9
        local.get 9
        i32.load8_u
        local.set 10
        i32.const 24
        local.set 11
        local.get 10
        local.get 11
        i32.shl
        local.set 12
        local.get 12
        local.get 11
        i32.shr_s
        local.set 13
        local.get 13
        call $putchar
        drop
        local.get 3
        i32.load offset=12
        local.set 14
        i32.const 1
        local.set 15
        local.get 14
        local.get 15
        i32.add
        local.set 16
        local.get 3
        local.get 16
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 16
    local.set 17
    local.get 3
    local.get 17
    i32.add
    local.set 18
    local.get 18
    global.set 0
    return)
  (func $_start (type 2)
    (local i32)
    i32.const 1024
    local.set 0
    local.get 0
    call $print
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66576))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "Hello, world\0a\00"))
"#;

fn main() {
    // Parse WAT text into syntax tree
    let tree = match parse(HELLO_WORLD) {
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

    // Execute module
    if let Err(trap) = execute(&tree.module) {
        eprintln!("Execution was trapped: {}", trap);
        exit(1);
    }
}
