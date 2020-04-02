(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func))
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
      i32.div_s
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
  (func $_start (type 2)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 448
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 0
    local.set 3
    i32.const 100
    local.set 4
    local.get 2
    local.get 4
    i32.store offset=28
    local.get 2
    local.get 3
    i32.store offset=24
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 100
        local.set 5
        local.get 2
        i32.load offset=24
        local.set 6
        local.get 6
        local.set 7
        local.get 5
        local.set 8
        local.get 7
        local.get 8
        i32.lt_s
        local.set 9
        i32.const 1
        local.set 10
        local.get 9
        local.get 10
        i32.and
        local.set 11
        local.get 11
        i32.eqz
        br_if 1 (;@1;)
        i32.const 1
        local.set 12
        i32.const 32
        local.set 13
        local.get 2
        local.get 13
        i32.add
        local.set 14
        local.get 14
        local.set 15
        local.get 2
        i32.load offset=24
        local.set 16
        i32.const 2
        local.set 17
        local.get 16
        local.get 17
        i32.shl
        local.set 18
        local.get 15
        local.get 18
        i32.add
        local.set 19
        local.get 19
        local.get 12
        i32.store
        local.get 2
        i32.load offset=24
        local.set 20
        i32.const 1
        local.set 21
        local.get 20
        local.get 21
        i32.add
        local.set 22
        local.get 2
        local.get 22
        i32.store offset=24
        br 0 (;@2;)
      end
    end
    i32.const 2
    local.set 23
    f64.const 0x1.9p+6 (;=100;)
    local.set 24
    local.get 24
    f64.sqrt
    local.set 25
    local.get 25
    f64.abs
    local.set 26
    f64.const 0x1p+31 (;=2.14748e+09;)
    local.set 27
    local.get 26
    local.get 27
    f64.lt
    local.set 28
    local.get 28
    i32.eqz
    local.set 29
    block  ;; label = @1
      block  ;; label = @2
        local.get 29
        br_if 0 (;@2;)
        local.get 25
        i32.trunc_f64_s
        local.set 30
        local.get 30
        local.set 31
        br 1 (;@1;)
      end
      i32.const -2147483648
      local.set 32
      local.get 32
      local.set 31
    end
    local.get 31
    local.set 33
    local.get 2
    local.get 33
    i32.store offset=20
    local.get 2
    local.get 23
    i32.store offset=16
    block  ;; label = @1
      loop  ;; label = @2
        local.get 2
        i32.load offset=16
        local.set 34
        local.get 2
        i32.load offset=20
        local.set 35
        local.get 34
        local.set 36
        local.get 35
        local.set 37
        local.get 36
        local.get 37
        i32.lt_s
        local.set 38
        i32.const 1
        local.set 39
        local.get 38
        local.get 39
        i32.and
        local.set 40
        local.get 40
        i32.eqz
        br_if 1 (;@1;)
        i32.const 32
        local.set 41
        local.get 2
        local.get 41
        i32.add
        local.set 42
        local.get 42
        local.set 43
        local.get 2
        i32.load offset=16
        local.set 44
        i32.const 2
        local.set 45
        local.get 44
        local.get 45
        i32.shl
        local.set 46
        local.get 43
        local.get 46
        i32.add
        local.set 47
        local.get 47
        i32.load
        local.set 48
        block  ;; label = @3
          block  ;; label = @4
            local.get 48
            br_if 0 (;@4;)
            br 1 (;@3;)
          end
          local.get 2
          i32.load offset=16
          local.set 49
          i32.const 1
          local.set 50
          local.get 49
          local.get 50
          i32.shl
          local.set 51
          local.get 2
          local.get 51
          i32.store offset=12
          block  ;; label = @4
            loop  ;; label = @5
              i32.const 100
              local.set 52
              local.get 2
              i32.load offset=12
              local.set 53
              local.get 53
              local.set 54
              local.get 52
              local.set 55
              local.get 54
              local.get 55
              i32.lt_s
              local.set 56
              i32.const 1
              local.set 57
              local.get 56
              local.get 57
              i32.and
              local.set 58
              local.get 58
              i32.eqz
              br_if 1 (;@4;)
              i32.const 0
              local.set 59
              i32.const 32
              local.set 60
              local.get 2
              local.get 60
              i32.add
              local.set 61
              local.get 61
              local.set 62
              local.get 2
              i32.load offset=12
              local.set 63
              i32.const 2
              local.set 64
              local.get 63
              local.get 64
              i32.shl
              local.set 65
              local.get 62
              local.get 65
              i32.add
              local.set 66
              local.get 66
              local.get 59
              i32.store
              local.get 2
              i32.load offset=16
              local.set 67
              local.get 2
              i32.load offset=12
              local.set 68
              local.get 68
              local.get 67
              i32.add
              local.set 69
              local.get 2
              local.get 69
              i32.store offset=12
              br 0 (;@5;)
            end
          end
        end
        local.get 2
        i32.load offset=16
        local.set 70
        i32.const 1
        local.set 71
        local.get 70
        local.get 71
        i32.add
        local.set 72
        local.get 2
        local.get 72
        i32.store offset=16
        br 0 (;@2;)
      end
    end
    i32.const 2
    local.set 73
    local.get 2
    local.get 73
    i32.store offset=8
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 100
        local.set 74
        local.get 2
        i32.load offset=8
        local.set 75
        local.get 75
        local.set 76
        local.get 74
        local.set 77
        local.get 76
        local.get 77
        i32.le_s
        local.set 78
        i32.const 1
        local.set 79
        local.get 78
        local.get 79
        i32.and
        local.set 80
        local.get 80
        i32.eqz
        br_if 1 (;@1;)
        i32.const 1
        local.set 81
        i32.const 32
        local.set 82
        local.get 2
        local.get 82
        i32.add
        local.set 83
        local.get 83
        local.set 84
        local.get 2
        i32.load offset=8
        local.set 85
        i32.const 2
        local.set 86
        local.get 85
        local.get 86
        i32.shl
        local.set 87
        local.get 84
        local.get 87
        i32.add
        local.set 88
        local.get 88
        i32.load
        local.set 89
        local.get 89
        local.set 90
        local.get 81
        local.set 91
        local.get 90
        local.get 91
        i32.eq
        local.set 92
        i32.const 1
        local.set 93
        local.get 92
        local.get 93
        i32.and
        local.set 94
        block  ;; label = @3
          local.get 94
          i32.eqz
          br_if 0 (;@3;)
          i32.const 10
          local.set 95
          local.get 2
          i32.load offset=8
          local.set 96
          local.get 96
          call $print_digits
          local.get 95
          call $putchar
          drop
        end
        local.get 2
        i32.load offset=8
        local.set 97
        i32.const 1
        local.set 98
        local.get 97
        local.get 98
        i32.add
        local.set 99
        local.get 2
        local.get 99
        i32.store offset=8
        br 0 (;@2;)
      end
    end
    i32.const 448
    local.set 100
    local.get 2
    local.get 100
    i32.add
    local.set 101
    local.get 101
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
