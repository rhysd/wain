(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $_start (type 1)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 16
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 0
    local.set 3
    local.get 2
    local.get 3
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 13
        local.set 4
        local.get 2
        i32.load offset=12
        local.set 5
        local.get 5
        local.set 6
        local.get 4
        local.set 7
        local.get 6
        local.get 7
        i32.lt_u
        local.set 8
        i32.const 1
        local.set 9
        local.get 8
        local.get 9
        i32.and
        local.set 10
        local.get 10
        i32.eqz
        br_if 1 (;@1;)
        local.get 2
        i32.load offset=12
        local.set 11
        local.get 11
        i32.load8_u offset=1024
        local.set 12
        i32.const 24
        local.set 13
        local.get 12
        local.get 13
        i32.shl
        local.set 14
        local.get 14
        local.get 13
        i32.shr_s
        local.set 15
        local.get 15
        call $putchar
        drop
        local.get 2
        i32.load offset=12
        local.set 16
        i32.const 1
        local.set 17
        local.get 16
        local.get 17
        i32.add
        local.set 18
        local.get 2
        local.get 18
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 16
    local.set 19
    local.get 2
    local.get 19
    i32.add
    local.set 20
    local.get 20
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66576))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "Hello, world\0a\00"))
