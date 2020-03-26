(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32) (result i32)))
  (type (;2;) (func))
  (import "env" "putchar" (func $putchar (type 1)))
  (func $putc1 (type 0) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.store8 offset=15
    local.get 3
    i32.load8_u offset=15
    local.set 4
    i32.const 24
    local.set 5
    local.get 4
    local.get 5
    i32.shl
    local.set 6
    local.get 6
    local.get 5
    i32.shr_s
    local.set 7
    local.get 7
    call $putchar
    drop
    i32.const 16
    local.set 8
    local.get 3
    local.get 8
    i32.add
    local.set 9
    local.get 9
    global.set 0
    return)
  (func $putc2 (type 0) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.store8 offset=15
    local.get 3
    i32.load8_u offset=15
    local.set 4
    i32.const 24
    local.set 5
    local.get 4
    local.get 5
    i32.shl
    local.set 6
    local.get 6
    local.get 5
    i32.shr_s
    local.set 7
    local.get 7
    call $putchar
    drop
    i32.const 16
    local.set 8
    local.get 3
    local.get 8
    i32.add
    local.set 9
    local.get 9
    global.set 0
    return)
  (func $putc3 (type 0) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.store8 offset=15
    local.get 3
    i32.load8_u offset=15
    local.set 4
    i32.const 24
    local.set 5
    local.get 4
    local.get 5
    i32.shl
    local.set 6
    local.get 6
    local.get 5
    i32.shr_s
    local.set 7
    local.get 7
    call $putchar
    drop
    i32.const 16
    local.set 8
    local.get 3
    local.get 8
    i32.add
    local.set 9
    local.get 9
    global.set 0
    return)
  (func $putc4 (type 0) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.store8 offset=15
    local.get 3
    i32.load8_u offset=15
    local.set 4
    i32.const 24
    local.set 5
    local.get 4
    local.get 5
    i32.shl
    local.set 6
    local.get 6
    local.get 5
    i32.shr_s
    local.set 7
    local.get 7
    call $putchar
    drop
    i32.const 16
    local.set 8
    local.get 3
    local.get 8
    i32.add
    local.set 9
    local.get 9
    global.set 0
    return)
  (func $_start (type 2)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 i32 i32 i32 i64 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 48
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 0
    local.set 3
    i32.const 16
    local.set 4
    local.get 2
    local.get 4
    i32.add
    local.set 5
    local.get 5
    local.set 6
    i32.const 34
    local.set 7
    local.get 2
    local.get 7
    i32.add
    local.set 8
    local.get 8
    local.set 9
    i32.const 6
    local.set 10
    local.get 9
    local.get 10
    i32.add
    local.set 11
    i32.const 0
    local.set 12
    local.get 12
    i64.load offset=1030 align=1
    local.set 13
    local.get 11
    local.get 13
    i64.store align=1
    local.get 12
    i64.load offset=1024 align=1
    local.set 14
    local.get 9
    local.get 14
    i64.store align=1
    i32.const 8
    local.set 15
    local.get 6
    local.get 15
    i32.add
    local.set 16
    i32.const 0
    local.set 17
    local.get 17
    i64.load offset=1048
    local.set 18
    local.get 16
    local.get 18
    i64.store
    local.get 17
    i64.load offset=1040
    local.set 19
    local.get 6
    local.get 19
    i64.store
    local.get 2
    local.get 3
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 34
        local.set 20
        local.get 2
        local.get 20
        i32.add
        local.set 21
        local.get 21
        local.set 22
        local.get 2
        i32.load offset=12
        local.set 23
        local.get 22
        local.get 23
        i32.add
        local.set 24
        local.get 24
        i32.load8_u
        local.set 25
        i32.const 24
        local.set 26
        local.get 25
        local.get 26
        i32.shl
        local.set 27
        local.get 27
        local.get 26
        i32.shr_s
        local.set 28
        local.get 28
        i32.eqz
        br_if 1 (;@1;)
        i32.const 34
        local.set 29
        local.get 2
        local.get 29
        i32.add
        local.set 30
        local.get 30
        local.set 31
        i32.const 16
        local.set 32
        local.get 2
        local.get 32
        i32.add
        local.set 33
        local.get 33
        local.set 34
        local.get 2
        i32.load offset=12
        local.set 35
        i32.const 4
        local.set 36
        local.get 35
        local.get 36
        i32.rem_s
        local.set 37
        i32.const 2
        local.set 38
        local.get 37
        local.get 38
        i32.shl
        local.set 39
        local.get 34
        local.get 39
        i32.add
        local.set 40
        local.get 40
        i32.load
        local.set 41
        local.get 2
        i32.load offset=12
        local.set 42
        local.get 31
        local.get 42
        i32.add
        local.set 43
        local.get 43
        i32.load8_u
        local.set 44
        i32.const 24
        local.set 45
        local.get 44
        local.get 45
        i32.shl
        local.set 46
        local.get 46
        local.get 45
        i32.shr_s
        local.set 47
        local.get 47
        local.get 41
        call_indirect (type 0)
        local.get 2
        i32.load offset=12
        local.set 48
        i32.const 1
        local.set 49
        local.get 48
        local.get 49
        i32.add
        local.set 50
        local.get 2
        local.get 50
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 48
    local.set 51
    local.get 2
    local.get 51
    i32.add
    local.set 52
    local.get 52
    global.set 0
    return)
  (table (;0;) 5 5 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66592))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (elem (;0;) (i32.const 1) func $putc1 $putc2 $putc3 $putc4)
  (data (;0;) (i32.const 1024) "Hello, world\0a\00\00\00\01\00\00\00\02\00\00\00\03\00\00\00\04\00\00\00"))
