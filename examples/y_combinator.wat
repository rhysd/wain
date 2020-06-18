(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $print_uint_int_ (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    local.get 3
    i32.load offset=12
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        local.get 4
        br_if 0 (;@2;)
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 5
      i32.const 10
      local.set 6
      local.get 5
      local.get 6
      i32.div_s
      local.set 7
      local.get 7
      call $print_uint_int_
      local.get 3
      i32.load offset=12
      local.set 8
      i32.const 10
      local.set 9
      local.get 8
      local.get 9
      i32.rem_s
      local.set 10
      i32.const 48
      local.set 11
      local.get 10
      local.get 11
      i32.add
      local.set 12
      local.get 12
      call $putchar
      drop
    end
    i32.const 16
    local.set 13
    local.get 3
    local.get 13
    i32.add
    local.set 14
    local.get 14
    global.set 0
    return)
  (func $fib_int_ (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 32
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    global.set 0
    i32.const 16
    local.set 4
    local.get 3
    local.get 4
    i32.add
    local.set 5
    local.get 5
    local.set 6
    i32.const 24
    local.set 7
    local.get 3
    local.get 7
    i32.add
    local.set 8
    local.get 8
    local.set 9
    local.get 3
    local.get 0
    i32.store offset=28
    local.get 6
    local.get 9
    call $auto_fib_int_::$_0::operator__<fib_int_::$_1>_fib_int_::$_1__const
    local.get 3
    i32.load offset=28
    local.set 10
    local.get 6
    local.get 10
    call $auto_auto_fib_int_::$_0::operator__<fib_int_::$_1>_fib_int_::$_1__const::'lambda'_fib_int_::$_1_::operator__<int>_fib_int_::$_1__const
    local.set 11
    i32.const 32
    local.set 12
    local.get 3
    local.get 12
    i32.add
    local.set 13
    local.get 13
    global.set 0
    local.get 11
    return)
  (func $auto_fib_int_::$_0::operator__<fib_int_::$_1>_fib_int_::$_1__const (type 2) (param i32 i32)
    (local i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 16
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    local.get 1
    i32.store offset=4
    return)
  (func $auto_auto_fib_int_::$_0::operator__<fib_int_::$_1>_fib_int_::$_1__const::'lambda'_fib_int_::$_1_::operator__<int>_fib_int_::$_1__const (type 3) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 16
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    global.set 0
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
    call $int_fib_int_::$_1::operator__<$_1>_$_1__int__const
    local.set 7
    i32.const 16
    local.set 8
    local.get 4
    local.get 8
    i32.add
    local.set 9
    local.get 9
    global.set 0
    local.get 7
    return)
  (func $int_fib_int_::$_1::operator__<$_1>_$_1__int__const (type 3) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 32
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    global.set 0
    i32.const 1
    local.set 5
    local.get 4
    local.get 0
    i32.store offset=20
    local.get 4
    local.get 1
    i32.store offset=16
    local.get 4
    i32.load offset=16
    local.set 6
    local.get 6
    local.set 7
    local.get 5
    local.set 8
    local.get 7
    local.get 8
    i32.le_s
    local.set 9
    i32.const 1
    local.set 10
    local.get 9
    local.get 10
    i32.and
    local.set 11
    block  ;; label = @1
      block  ;; label = @2
        local.get 11
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1
        local.set 12
        local.get 12
        local.set 13
        br 1 (;@1;)
      end
      i32.const 24
      local.set 14
      local.get 4
      local.get 14
      i32.add
      local.set 15
      local.get 15
      local.set 16
      local.get 4
      i32.load offset=16
      local.set 17
      i32.const 1
      local.set 18
      local.get 17
      local.get 18
      i32.sub
      local.set 19
      local.get 16
      local.get 19
      call $int_fib_int_::$_1::operator__<$_1>_$_1__int__const
      local.set 20
      local.get 4
      i32.load offset=16
      local.set 21
      i32.const 2
      local.set 22
      local.get 21
      local.get 22
      i32.sub
      local.set 23
      local.get 16
      local.get 23
      call $int_fib_int_::$_1::operator__<$_1>_$_1__int__const
      local.set 24
      local.get 20
      local.get 24
      i32.add
      local.set 25
      local.get 25
      local.set 13
    end
    local.get 13
    local.set 26
    i32.const 32
    local.set 27
    local.get 4
    local.get 27
    i32.add
    local.set 28
    local.get 28
    global.set 0
    local.get 26
    return)
  (func $_start (type 4)
    (local i32 i32)
    i32.const 10
    local.set 0
    local.get 0
    call $fib_int_
    local.set 1
    local.get 1
    call $print_uint_int_
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
