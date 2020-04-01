(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param f64)))
  (type (;3;) (func (param f64) (result f64)))
  (type (;4;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $print_digits (type 1) (param i32)
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
      i32.div_u
      local.set 7
      local.get 7
      call $print_digits
      local.get 3
      i32.load offset=12
      local.set 8
      i32.const 10
      local.set 9
      local.get 8
      local.get 9
      i32.rem_u
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
  (func $print_uint (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
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
        i32.const 48
        local.set 5
        local.get 5
        call $putchar
        drop
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 6
      local.get 6
      call $print_digits
    end
    i32.const 16
    local.set 7
    local.get 3
    local.get 7
    i32.add
    local.set 8
    local.get 8
    global.set 0
    return)
  (func $print_float (type 2) (param f64)
    (local i32 i32 i32 i32 f64 f64 i32 i32 i32 i32 f64 f64 i32 f64 i32 f64 f64 i32 f64 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 f64 f64 f64 i32 i32 i32 i32 f64 f64 i32 i32 i32 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 f64 f64 i32 i32)
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
    i32.const 0
    local.set 4
    local.get 4
    f64.convert_i32_s
    local.set 5
    local.get 3
    local.get 0
    f64.store offset=8
    local.get 3
    f64.load offset=8
    local.set 6
    local.get 6
    local.get 5
    f64.lt
    local.set 7
    i32.const 1
    local.set 8
    local.get 7
    local.get 8
    i32.and
    local.set 9
    block  ;; label = @1
      local.get 9
      i32.eqz
      br_if 0 (;@1;)
      i32.const 45
      local.set 10
      local.get 10
      call $putchar
      drop
      local.get 3
      f64.load offset=8
      local.set 11
      local.get 11
      f64.neg
      local.set 12
      local.get 3
      local.get 12
      f64.store offset=8
    end
    i32.const 0
    local.set 13
    local.get 13
    f64.convert_i32_s
    local.set 14
    i32.const 46
    local.set 15
    local.get 3
    f64.load offset=8
    local.set 16
    f64.const 0x1p+32 (;=4.29497e+09;)
    local.set 17
    local.get 16
    local.get 17
    f64.lt
    local.set 18
    f64.const 0x0p+0 (;=0;)
    local.set 19
    local.get 16
    local.get 19
    f64.ge
    local.set 20
    local.get 18
    local.get 20
    i32.and
    local.set 21
    local.get 21
    i32.eqz
    local.set 22
    block  ;; label = @1
      block  ;; label = @2
        local.get 22
        br_if 0 (;@2;)
        local.get 16
        i32.trunc_f64_u
        local.set 23
        local.get 23
        local.set 24
        br 1 (;@1;)
      end
      i32.const 0
      local.set 25
      local.get 25
      local.set 24
    end
    local.get 24
    local.set 26
    local.get 26
    call $print_uint
    local.get 15
    call $putchar
    drop
    local.get 3
    f64.load offset=8
    local.set 27
    local.get 3
    f64.load offset=8
    local.set 28
    local.get 28
    f64.abs
    local.set 29
    f64.const 0x1p+31 (;=2.14748e+09;)
    local.set 30
    local.get 29
    local.get 30
    f64.lt
    local.set 31
    local.get 31
    i32.eqz
    local.set 32
    block  ;; label = @1
      block  ;; label = @2
        local.get 32
        br_if 0 (;@2;)
        local.get 28
        i32.trunc_f64_s
        local.set 33
        local.get 33
        local.set 34
        br 1 (;@1;)
      end
      i32.const -2147483648
      local.set 35
      local.get 35
      local.set 34
    end
    local.get 34
    local.set 36
    local.get 36
    f64.convert_i32_s
    local.set 37
    local.get 27
    local.get 37
    f64.sub
    local.set 38
    local.get 3
    local.get 38
    f64.store offset=8
    local.get 3
    f64.load offset=8
    local.set 39
    local.get 39
    local.get 14
    f64.eq
    local.set 40
    i32.const 1
    local.set 41
    local.get 40
    local.get 41
    i32.and
    local.set 42
    block  ;; label = @1
      block  ;; label = @2
        local.get 42
        i32.eqz
        br_if 0 (;@2;)
        i32.const 48
        local.set 43
        local.get 43
        call $putchar
        drop
        br 1 (;@1;)
      end
      loop  ;; label = @2
        f64.const 0x1.ad7f29abcaf48p-24 (;=1e-07;)
        local.set 44
        local.get 3
        f64.load offset=8
        local.set 45
        local.get 45
        local.get 44
        f64.gt
        local.set 46
        i32.const 1
        local.set 47
        local.get 46
        local.get 47
        i32.and
        local.set 48
        local.get 48
        i32.eqz
        br_if 1 (;@1;)
        f64.const 0x1.4p+3 (;=10;)
        local.set 49
        local.get 3
        f64.load offset=8
        local.set 50
        local.get 50
        local.get 49
        f64.mul
        local.set 51
        local.get 3
        local.get 51
        f64.store offset=8
        local.get 3
        f64.load offset=8
        local.set 52
        local.get 52
        f64.abs
        local.set 53
        f64.const 0x1p+31 (;=2.14748e+09;)
        local.set 54
        local.get 53
        local.get 54
        f64.lt
        local.set 55
        local.get 55
        i32.eqz
        local.set 56
        block  ;; label = @3
          block  ;; label = @4
            local.get 56
            br_if 0 (;@4;)
            local.get 52
            i32.trunc_f64_s
            local.set 57
            local.get 57
            local.set 58
            br 1 (;@3;)
          end
          i32.const -2147483648
          local.set 59
          local.get 59
          local.set 58
        end
        local.get 58
        local.set 60
        i32.const 48
        local.set 61
        local.get 60
        local.get 61
        i32.add
        local.set 62
        local.get 62
        call $putchar
        drop
        local.get 3
        f64.load offset=8
        local.set 63
        local.get 3
        f64.load offset=8
        local.set 64
        local.get 64
        f64.abs
        local.set 65
        f64.const 0x1p+31 (;=2.14748e+09;)
        local.set 66
        local.get 65
        local.get 66
        f64.lt
        local.set 67
        local.get 67
        i32.eqz
        local.set 68
        block  ;; label = @3
          block  ;; label = @4
            local.get 68
            br_if 0 (;@4;)
            local.get 64
            i32.trunc_f64_s
            local.set 69
            local.get 69
            local.set 70
            br 1 (;@3;)
          end
          i32.const -2147483648
          local.set 71
          local.get 71
          local.set 70
        end
        local.get 70
        local.set 72
        local.get 72
        f64.convert_i32_s
        local.set 73
        local.get 63
        local.get 73
        f64.sub
        local.set 74
        local.get 3
        local.get 74
        f64.store offset=8
        br 0 (;@2;)
      end
    end
    i32.const 16
    local.set 75
    local.get 3
    local.get 75
    i32.add
    local.set 76
    local.get 76
    global.set 0
    return)
  (func $print_str (type 1) (param i32)
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
  (func $my_sqrt (type 3) (param f64) (result f64)
    (local i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64)
    global.get 0
    local.set 1
    i32.const 32
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    f64.const 0x1p+0 (;=1;)
    local.set 4
    f64.const 0x1.12e0be826d695p-30 (;=1e-09;)
    local.set 5
    local.get 3
    local.get 0
    f64.store offset=24
    local.get 3
    local.get 5
    f64.store offset=16
    local.get 3
    local.get 4
    f64.store offset=8
    block  ;; label = @1
      loop  ;; label = @2
        f64.const 0x1.12e0be826d695p-30 (;=1e-09;)
        local.set 6
        local.get 3
        f64.load offset=8
        local.set 7
        local.get 3
        f64.load offset=8
        local.set 8
        local.get 7
        local.get 8
        f64.mul
        local.set 9
        local.get 3
        f64.load offset=24
        local.set 10
        local.get 9
        local.get 10
        f64.sub
        local.set 11
        local.get 11
        f64.abs
        local.set 12
        local.get 12
        local.get 6
        f64.ge
        local.set 13
        i32.const 1
        local.set 14
        local.get 13
        local.get 14
        i32.and
        local.set 15
        local.get 15
        i32.eqz
        br_if 1 (;@1;)
        f64.const 0x1p+1 (;=2;)
        local.set 16
        local.get 3
        f64.load offset=24
        local.set 17
        local.get 3
        f64.load offset=8
        local.set 18
        local.get 17
        local.get 18
        f64.div
        local.set 19
        local.get 3
        f64.load offset=8
        local.set 20
        local.get 19
        local.get 20
        f64.add
        local.set 21
        local.get 21
        local.get 16
        f64.div
        local.set 22
        local.get 3
        local.get 22
        f64.store offset=8
        br 0 (;@2;)
      end
    end
    local.get 3
    f64.load offset=8
    local.set 23
    local.get 23
    return)
  (func $_start (type 4)
    (local i32 f64 i32 f64 i32 f64 i32 f64 i32 f64 f64 f64 f64)
    i32.const 10
    local.set 0
    f64.const 0x1.9p+6 (;=100;)
    local.set 1
    i32.const 1052
    local.set 2
    f64.const 0x1.4p+3 (;=10;)
    local.set 3
    i32.const 1042
    local.set 4
    f64.const 0x1.8p+1 (;=3;)
    local.set 5
    i32.const 1033
    local.set 6
    f64.const 0x1p+1 (;=2;)
    local.set 7
    i32.const 1024
    local.set 8
    local.get 8
    call $print_str
    local.get 7
    call $my_sqrt
    local.set 9
    local.get 9
    call $print_float
    local.get 0
    call $putchar
    drop
    local.get 6
    call $print_str
    local.get 5
    call $my_sqrt
    local.set 10
    local.get 10
    call $print_float
    local.get 0
    call $putchar
    drop
    local.get 4
    call $print_str
    local.get 3
    call $my_sqrt
    local.set 11
    local.get 11
    call $print_float
    local.get 0
    call $putchar
    drop
    local.get 2
    call $print_str
    local.get 1
    call $my_sqrt
    local.set 12
    local.get 12
    call $print_float
    local.get 0
    call $putchar
    drop
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66608))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "sqrt(2)=\00sqrt(3)=\00sqrt(10)=\00sqrt(100)=\00"))
