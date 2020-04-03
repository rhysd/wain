(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param f64)))
  (type (;3;) (func (param i32 f64 f64 f64 f64 f64 f64 f64)))
  (type (;4;) (func (param i32 i32 i32 f64 i32)))
  (type (;5;) (func (param i32 i32) (result f64)))
  (type (;6;) (func (param i32 i32)))
  (type (;7;) (func))
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
  (func $init_planet (type 3) (param i32 f64 f64 f64 f64 f64 f64 f64)
    (local i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64)
    global.get 0
    local.set 8
    i32.const 64
    local.set 9
    local.get 8
    local.get 9
    i32.sub
    local.set 10
    f64.const 0x1.3bd3cc9be45dep+5 (;=39.4784;)
    local.set 11
    f64.const 0x1.6d3d70a3d70a4p+8 (;=365.24;)
    local.set 12
    local.get 10
    local.get 1
    f64.store offset=56
    local.get 10
    local.get 2
    f64.store offset=48
    local.get 10
    local.get 3
    f64.store offset=40
    local.get 10
    local.get 4
    f64.store offset=32
    local.get 10
    local.get 5
    f64.store offset=24
    local.get 10
    local.get 6
    f64.store offset=16
    local.get 10
    local.get 7
    f64.store offset=8
    local.get 10
    f64.load offset=56
    local.set 13
    local.get 0
    local.get 13
    f64.store
    local.get 10
    f64.load offset=48
    local.set 14
    local.get 0
    local.get 14
    f64.store offset=8
    local.get 10
    f64.load offset=40
    local.set 15
    local.get 0
    local.get 15
    f64.store offset=16
    local.get 10
    f64.load offset=32
    local.set 16
    local.get 16
    local.get 12
    f64.mul
    local.set 17
    local.get 0
    local.get 17
    f64.store offset=24
    local.get 10
    f64.load offset=24
    local.set 18
    local.get 18
    local.get 12
    f64.mul
    local.set 19
    local.get 0
    local.get 19
    f64.store offset=32
    local.get 10
    f64.load offset=16
    local.set 20
    local.get 20
    local.get 12
    f64.mul
    local.set 21
    local.get 0
    local.get 21
    f64.store offset=40
    local.get 10
    f64.load offset=8
    local.set 22
    local.get 22
    local.get 11
    f64.mul
    local.set 23
    local.get 0
    local.get 23
    f64.store offset=48
    return)
  (func $planet_move_from_i (type 4) (param i32 i32 i32 f64 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 f64 f64 i32 f64 i32 f64 f64 i32 f64 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 i32 f64 f64 f64 i32 f64 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 i32 i32 i32 f64 i32 f64 f64 i32 f64 f64 f64 i32 f64 f64 i32 f64 f64 f64 i32 f64 f64 i32 f64 f64)
    global.get 0
    local.set 5
    i32.const 96
    local.set 6
    local.get 5
    local.get 6
    i32.sub
    local.set 7
    local.get 7
    local.get 0
    i32.store offset=92
    local.get 7
    local.get 1
    i32.store offset=88
    local.get 7
    local.get 2
    i32.store offset=84
    local.get 7
    local.get 3
    f64.store offset=72
    local.get 7
    local.get 4
    i32.store offset=68
    block  ;; label = @1
      loop  ;; label = @2
        local.get 7
        i32.load offset=68
        local.set 8
        local.get 7
        i32.load offset=84
        local.set 9
        local.get 8
        local.set 10
        local.get 9
        local.set 11
        local.get 10
        local.get 11
        i32.lt_u
        local.set 12
        i32.const 1
        local.set 13
        local.get 12
        local.get 13
        i32.and
        local.set 14
        local.get 14
        i32.eqz
        br_if 1 (;@1;)
        local.get 7
        i32.load offset=88
        local.set 15
        local.get 7
        i32.load offset=68
        local.set 16
        i32.const 56
        local.set 17
        local.get 16
        local.get 17
        i32.mul
        local.set 18
        local.get 15
        local.get 18
        i32.add
        local.set 19
        local.get 7
        local.get 19
        i32.store offset=64
        local.get 7
        i32.load offset=92
        local.set 20
        local.get 20
        f64.load
        local.set 21
        local.get 7
        i32.load offset=64
        local.set 22
        local.get 22
        f64.load
        local.set 23
        local.get 21
        local.get 23
        f64.sub
        local.set 24
        local.get 7
        local.get 24
        f64.store offset=56
        local.get 7
        i32.load offset=92
        local.set 25
        local.get 25
        f64.load offset=8
        local.set 26
        local.get 7
        i32.load offset=64
        local.set 27
        local.get 27
        f64.load offset=8
        local.set 28
        local.get 26
        local.get 28
        f64.sub
        local.set 29
        local.get 7
        local.get 29
        f64.store offset=48
        local.get 7
        i32.load offset=92
        local.set 30
        local.get 30
        f64.load offset=16
        local.set 31
        local.get 7
        i32.load offset=64
        local.set 32
        local.get 32
        f64.load offset=16
        local.set 33
        local.get 31
        local.get 33
        f64.sub
        local.set 34
        local.get 7
        local.get 34
        f64.store offset=40
        local.get 7
        f64.load offset=56
        local.set 35
        local.get 7
        f64.load offset=56
        local.set 36
        local.get 35
        local.get 36
        f64.mul
        local.set 37
        local.get 7
        f64.load offset=48
        local.set 38
        local.get 7
        f64.load offset=48
        local.set 39
        local.get 38
        local.get 39
        f64.mul
        local.set 40
        local.get 37
        local.get 40
        f64.add
        local.set 41
        local.get 7
        f64.load offset=40
        local.set 42
        local.get 7
        f64.load offset=40
        local.set 43
        local.get 42
        local.get 43
        f64.mul
        local.set 44
        local.get 41
        local.get 44
        f64.add
        local.set 45
        local.get 45
        f64.sqrt
        local.set 46
        local.get 7
        local.get 46
        f64.store offset=32
        local.get 7
        f64.load offset=72
        local.set 47
        local.get 7
        f64.load offset=32
        local.set 48
        local.get 7
        f64.load offset=32
        local.set 49
        local.get 48
        local.get 49
        f64.mul
        local.set 50
        local.get 7
        f64.load offset=32
        local.set 51
        local.get 50
        local.get 51
        f64.mul
        local.set 52
        local.get 47
        local.get 52
        f64.div
        local.set 53
        local.get 7
        local.get 53
        f64.store offset=24
        local.get 7
        i32.load offset=92
        local.set 54
        local.get 54
        f64.load offset=48
        local.set 55
        local.get 7
        f64.load offset=24
        local.set 56
        local.get 55
        local.get 56
        f64.mul
        local.set 57
        local.get 7
        local.get 57
        f64.store offset=16
        local.get 7
        i32.load offset=64
        local.set 58
        local.get 58
        f64.load offset=48
        local.set 59
        local.get 7
        f64.load offset=24
        local.set 60
        local.get 59
        local.get 60
        f64.mul
        local.set 61
        local.get 7
        local.get 61
        f64.store offset=8
        local.get 7
        f64.load offset=56
        local.set 62
        local.get 7
        f64.load offset=8
        local.set 63
        local.get 62
        local.get 63
        f64.mul
        local.set 64
        local.get 7
        i32.load offset=92
        local.set 65
        local.get 65
        f64.load offset=24
        local.set 66
        local.get 66
        local.get 64
        f64.sub
        local.set 67
        local.get 65
        local.get 67
        f64.store offset=24
        local.get 7
        f64.load offset=48
        local.set 68
        local.get 7
        f64.load offset=8
        local.set 69
        local.get 68
        local.get 69
        f64.mul
        local.set 70
        local.get 7
        i32.load offset=92
        local.set 71
        local.get 71
        f64.load offset=32
        local.set 72
        local.get 72
        local.get 70
        f64.sub
        local.set 73
        local.get 71
        local.get 73
        f64.store offset=32
        local.get 7
        f64.load offset=40
        local.set 74
        local.get 7
        f64.load offset=8
        local.set 75
        local.get 74
        local.get 75
        f64.mul
        local.set 76
        local.get 7
        i32.load offset=92
        local.set 77
        local.get 77
        f64.load offset=40
        local.set 78
        local.get 78
        local.get 76
        f64.sub
        local.set 79
        local.get 77
        local.get 79
        f64.store offset=40
        local.get 7
        f64.load offset=56
        local.set 80
        local.get 7
        f64.load offset=16
        local.set 81
        local.get 80
        local.get 81
        f64.mul
        local.set 82
        local.get 7
        i32.load offset=64
        local.set 83
        local.get 83
        f64.load offset=24
        local.set 84
        local.get 84
        local.get 82
        f64.add
        local.set 85
        local.get 83
        local.get 85
        f64.store offset=24
        local.get 7
        f64.load offset=48
        local.set 86
        local.get 7
        f64.load offset=16
        local.set 87
        local.get 86
        local.get 87
        f64.mul
        local.set 88
        local.get 7
        i32.load offset=64
        local.set 89
        local.get 89
        f64.load offset=32
        local.set 90
        local.get 90
        local.get 88
        f64.add
        local.set 91
        local.get 89
        local.get 91
        f64.store offset=32
        local.get 7
        f64.load offset=40
        local.set 92
        local.get 7
        f64.load offset=16
        local.set 93
        local.get 92
        local.get 93
        f64.mul
        local.set 94
        local.get 7
        i32.load offset=64
        local.set 95
        local.get 95
        f64.load offset=40
        local.set 96
        local.get 96
        local.get 94
        f64.add
        local.set 97
        local.get 95
        local.get 97
        f64.store offset=40
        local.get 7
        i32.load offset=68
        local.set 98
        i32.const 1
        local.set 99
        local.get 98
        local.get 99
        i32.add
        local.set 100
        local.get 7
        local.get 100
        i32.store offset=68
        br 0 (;@2;)
      end
    end
    local.get 7
    f64.load offset=72
    local.set 101
    local.get 7
    i32.load offset=92
    local.set 102
    local.get 102
    f64.load offset=24
    local.set 103
    local.get 101
    local.get 103
    f64.mul
    local.set 104
    local.get 7
    i32.load offset=92
    local.set 105
    local.get 105
    f64.load
    local.set 106
    local.get 106
    local.get 104
    f64.add
    local.set 107
    local.get 105
    local.get 107
    f64.store
    local.get 7
    f64.load offset=72
    local.set 108
    local.get 7
    i32.load offset=92
    local.set 109
    local.get 109
    f64.load offset=32
    local.set 110
    local.get 108
    local.get 110
    f64.mul
    local.set 111
    local.get 7
    i32.load offset=92
    local.set 112
    local.get 112
    f64.load offset=8
    local.set 113
    local.get 113
    local.get 111
    f64.add
    local.set 114
    local.get 112
    local.get 114
    f64.store offset=8
    local.get 7
    f64.load offset=72
    local.set 115
    local.get 7
    i32.load offset=92
    local.set 116
    local.get 116
    f64.load offset=40
    local.set 117
    local.get 115
    local.get 117
    f64.mul
    local.set 118
    local.get 7
    i32.load offset=92
    local.set 119
    local.get 119
    f64.load offset=16
    local.set 120
    local.get 120
    local.get 118
    f64.add
    local.set 121
    local.get 119
    local.get 121
    f64.store offset=16
    return)
  (func $energy (type 5) (param i32 i32) (result f64)
    (local i32 i32 i32 i32 f64 i32 i32 i32 i32 i32 i32 i32 f64 i32 i32 i32 i32 i32 i32 f64 f64 i32 f64 i32 f64 f64 i32 f64 i32 f64 f64 f64 i32 f64 i32 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 f64 f64 i32 f64 i32 f64 f64 i32 f64 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 i32 f64 i32 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 f64)
    global.get 0
    local.set 2
    i32.const 64
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    i32.const 0
    local.set 5
    local.get 5
    f64.convert_i32_s
    local.set 6
    local.get 4
    local.get 0
    i32.store offset=60
    local.get 4
    local.get 1
    i32.store offset=56
    local.get 4
    local.get 6
    f64.store offset=48
    local.get 4
    local.get 5
    i32.store offset=44
    block  ;; label = @1
      loop  ;; label = @2
        local.get 4
        i32.load offset=44
        local.set 7
        local.get 4
        i32.load offset=56
        local.set 8
        local.get 7
        local.set 9
        local.get 8
        local.set 10
        local.get 9
        local.get 10
        i32.lt_u
        local.set 11
        i32.const 1
        local.set 12
        local.get 11
        local.get 12
        i32.and
        local.set 13
        local.get 13
        i32.eqz
        br_if 1 (;@1;)
        f64.const 0x1p-1 (;=0.5;)
        local.set 14
        local.get 4
        i32.load offset=60
        local.set 15
        local.get 4
        i32.load offset=44
        local.set 16
        i32.const 56
        local.set 17
        local.get 16
        local.get 17
        i32.mul
        local.set 18
        local.get 15
        local.get 18
        i32.add
        local.set 19
        local.get 4
        local.get 19
        i32.store offset=40
        local.get 4
        i32.load offset=40
        local.set 20
        local.get 20
        f64.load offset=48
        local.set 21
        local.get 14
        local.get 21
        f64.mul
        local.set 22
        local.get 4
        i32.load offset=40
        local.set 23
        local.get 23
        f64.load offset=24
        local.set 24
        local.get 4
        i32.load offset=40
        local.set 25
        local.get 25
        f64.load offset=24
        local.set 26
        local.get 24
        local.get 26
        f64.mul
        local.set 27
        local.get 4
        i32.load offset=40
        local.set 28
        local.get 28
        f64.load offset=32
        local.set 29
        local.get 4
        i32.load offset=40
        local.set 30
        local.get 30
        f64.load offset=32
        local.set 31
        local.get 29
        local.get 31
        f64.mul
        local.set 32
        local.get 27
        local.get 32
        f64.add
        local.set 33
        local.get 4
        i32.load offset=40
        local.set 34
        local.get 34
        f64.load offset=40
        local.set 35
        local.get 4
        i32.load offset=40
        local.set 36
        local.get 36
        f64.load offset=40
        local.set 37
        local.get 35
        local.get 37
        f64.mul
        local.set 38
        local.get 33
        local.get 38
        f64.add
        local.set 39
        local.get 22
        local.get 39
        f64.mul
        local.set 40
        local.get 4
        f64.load offset=48
        local.set 41
        local.get 41
        local.get 40
        f64.add
        local.set 42
        local.get 4
        local.get 42
        f64.store offset=48
        local.get 4
        i32.load offset=44
        local.set 43
        i32.const 1
        local.set 44
        local.get 43
        local.get 44
        i32.add
        local.set 45
        local.get 4
        local.get 45
        i32.store offset=36
        block  ;; label = @3
          loop  ;; label = @4
            local.get 4
            i32.load offset=36
            local.set 46
            local.get 4
            i32.load offset=56
            local.set 47
            local.get 46
            local.set 48
            local.get 47
            local.set 49
            local.get 48
            local.get 49
            i32.lt_u
            local.set 50
            i32.const 1
            local.set 51
            local.get 50
            local.get 51
            i32.and
            local.set 52
            local.get 52
            i32.eqz
            br_if 1 (;@3;)
            local.get 4
            i32.load offset=60
            local.set 53
            local.get 4
            i32.load offset=36
            local.set 54
            i32.const 56
            local.set 55
            local.get 54
            local.get 55
            i32.mul
            local.set 56
            local.get 53
            local.get 56
            i32.add
            local.set 57
            local.get 4
            local.get 57
            i32.store offset=32
            local.get 4
            i32.load offset=40
            local.set 58
            local.get 58
            f64.load
            local.set 59
            local.get 4
            i32.load offset=32
            local.set 60
            local.get 60
            f64.load
            local.set 61
            local.get 59
            local.get 61
            f64.sub
            local.set 62
            local.get 4
            local.get 62
            f64.store offset=24
            local.get 4
            i32.load offset=40
            local.set 63
            local.get 63
            f64.load offset=8
            local.set 64
            local.get 4
            i32.load offset=32
            local.set 65
            local.get 65
            f64.load offset=8
            local.set 66
            local.get 64
            local.get 66
            f64.sub
            local.set 67
            local.get 4
            local.get 67
            f64.store offset=16
            local.get 4
            i32.load offset=40
            local.set 68
            local.get 68
            f64.load offset=16
            local.set 69
            local.get 4
            i32.load offset=32
            local.set 70
            local.get 70
            f64.load offset=16
            local.set 71
            local.get 69
            local.get 71
            f64.sub
            local.set 72
            local.get 4
            local.get 72
            f64.store offset=8
            local.get 4
            f64.load offset=24
            local.set 73
            local.get 4
            f64.load offset=24
            local.set 74
            local.get 73
            local.get 74
            f64.mul
            local.set 75
            local.get 4
            f64.load offset=16
            local.set 76
            local.get 4
            f64.load offset=16
            local.set 77
            local.get 76
            local.get 77
            f64.mul
            local.set 78
            local.get 75
            local.get 78
            f64.add
            local.set 79
            local.get 4
            f64.load offset=8
            local.set 80
            local.get 4
            f64.load offset=8
            local.set 81
            local.get 80
            local.get 81
            f64.mul
            local.set 82
            local.get 79
            local.get 82
            f64.add
            local.set 83
            local.get 83
            f64.sqrt
            local.set 84
            local.get 4
            local.get 84
            f64.store
            local.get 4
            i32.load offset=40
            local.set 85
            local.get 85
            f64.load offset=48
            local.set 86
            local.get 4
            i32.load offset=32
            local.set 87
            local.get 87
            f64.load offset=48
            local.set 88
            local.get 86
            local.get 88
            f64.mul
            local.set 89
            local.get 4
            f64.load
            local.set 90
            local.get 89
            local.get 90
            f64.div
            local.set 91
            local.get 4
            f64.load offset=48
            local.set 92
            local.get 92
            local.get 91
            f64.sub
            local.set 93
            local.get 4
            local.get 93
            f64.store offset=48
            local.get 4
            i32.load offset=36
            local.set 94
            i32.const 1
            local.set 95
            local.get 94
            local.get 95
            i32.add
            local.set 96
            local.get 4
            local.get 96
            i32.store offset=36
            br 0 (;@4;)
          end
        end
        local.get 4
        i32.load offset=44
        local.set 97
        i32.const 1
        local.set 98
        local.get 97
        local.get 98
        i32.add
        local.set 99
        local.get 4
        local.get 99
        i32.store offset=44
        br 0 (;@2;)
      end
    end
    local.get 4
    f64.load offset=48
    local.set 100
    local.get 100
    return)
  (func $offset_momentum (type 6) (param i32 i32)
    (local i32 i32 i32 i32 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 f64 f64 f64 f64 f64 i32 i32 i32 f64 i32 f64 f64 f64 i32 f64 f64 f64 i32 f64 f64 f64 i32)
    global.get 0
    local.set 2
    i32.const 64
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    i32.const 0
    local.set 5
    local.get 5
    f64.convert_i32_s
    local.set 6
    local.get 4
    local.get 0
    i32.store offset=60
    local.get 4
    local.get 1
    i32.store offset=56
    local.get 4
    local.get 6
    f64.store offset=48
    local.get 4
    local.get 6
    f64.store offset=40
    local.get 4
    local.get 6
    f64.store offset=32
    local.get 4
    local.get 5
    i32.store offset=28
    block  ;; label = @1
      loop  ;; label = @2
        local.get 4
        i32.load offset=28
        local.set 7
        local.get 4
        i32.load offset=56
        local.set 8
        local.get 7
        local.set 9
        local.get 8
        local.set 10
        local.get 9
        local.get 10
        i32.lt_u
        local.set 11
        i32.const 1
        local.set 12
        local.get 11
        local.get 12
        i32.and
        local.set 13
        local.get 13
        i32.eqz
        br_if 1 (;@1;)
        local.get 4
        i32.load offset=60
        local.set 14
        local.get 4
        i32.load offset=28
        local.set 15
        i32.const 56
        local.set 16
        local.get 15
        local.get 16
        i32.mul
        local.set 17
        local.get 14
        local.get 17
        i32.add
        local.set 18
        local.get 4
        local.get 18
        i32.store offset=24
        local.get 4
        i32.load offset=24
        local.set 19
        local.get 19
        f64.load offset=48
        local.set 20
        local.get 4
        local.get 20
        f64.store offset=16
        local.get 4
        i32.load offset=24
        local.set 21
        local.get 21
        f64.load offset=24
        local.set 22
        local.get 4
        f64.load offset=16
        local.set 23
        local.get 22
        local.get 23
        f64.mul
        local.set 24
        local.get 4
        f64.load offset=48
        local.set 25
        local.get 25
        local.get 24
        f64.add
        local.set 26
        local.get 4
        local.get 26
        f64.store offset=48
        local.get 4
        i32.load offset=24
        local.set 27
        local.get 27
        f64.load offset=32
        local.set 28
        local.get 4
        f64.load offset=16
        local.set 29
        local.get 28
        local.get 29
        f64.mul
        local.set 30
        local.get 4
        f64.load offset=40
        local.set 31
        local.get 31
        local.get 30
        f64.add
        local.set 32
        local.get 4
        local.get 32
        f64.store offset=40
        local.get 4
        i32.load offset=24
        local.set 33
        local.get 33
        f64.load offset=40
        local.set 34
        local.get 4
        f64.load offset=16
        local.set 35
        local.get 34
        local.get 35
        f64.mul
        local.set 36
        local.get 4
        f64.load offset=32
        local.set 37
        local.get 37
        local.get 36
        f64.add
        local.set 38
        local.get 4
        local.get 38
        f64.store offset=32
        local.get 4
        i32.load offset=28
        local.set 39
        i32.const 1
        local.set 40
        local.get 39
        local.get 40
        i32.add
        local.set 41
        local.get 4
        local.get 41
        i32.store offset=28
        br 0 (;@2;)
      end
    end
    f64.const 0x1.3bd3cc9be45dep+5 (;=39.4784;)
    local.set 42
    local.get 4
    i32.load offset=60
    local.set 43
    local.get 4
    local.get 43
    i32.store offset=12
    local.get 4
    f64.load offset=48
    local.set 44
    local.get 44
    f64.neg
    local.set 45
    local.get 45
    local.get 42
    f64.div
    local.set 46
    local.get 4
    i32.load offset=12
    local.set 47
    local.get 47
    local.get 46
    f64.store offset=24
    local.get 4
    f64.load offset=40
    local.set 48
    local.get 48
    f64.neg
    local.set 49
    local.get 49
    local.get 42
    f64.div
    local.set 50
    local.get 4
    i32.load offset=12
    local.set 51
    local.get 51
    local.get 50
    f64.store offset=32
    local.get 4
    f64.load offset=32
    local.set 52
    local.get 52
    f64.neg
    local.set 53
    local.get 53
    local.get 42
    f64.div
    local.set 54
    local.get 4
    i32.load offset=12
    local.set 55
    local.get 55
    local.get 54
    f64.store offset=40
    return)
  (func $_start (type 7)
    (local i32 i32 i32 i32 f64 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 i32)
    global.get 0
    local.set 0
    i32.const 320
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 0
    local.set 3
    f64.const 0x1.47ae147ae147bp-7 (;=0.01;)
    local.set 4
    i32.const 10
    local.set 5
    i32.const 5
    local.set 6
    i32.const 32
    local.set 7
    local.get 2
    local.get 7
    i32.add
    local.set 8
    local.get 8
    local.set 9
    i32.const 2000
    local.set 10
    f64.const 0x1.ec267a905572ap+3 (;=15.3797;)
    local.set 11
    f64.const -0x1.9eb5833c8a22p+4 (;=-25.9193;)
    local.set 12
    f64.const 0x1.6f1f393abe54p-3 (;=0.179259;)
    local.set 13
    f64.const 0x1.5f5c9e51b432p-9 (;=0.00268068;)
    local.set 14
    f64.const 0x1.aad5736999d88p-10 (;=0.00162824;)
    local.set 15
    f64.const -0x1.8f2070b7f975p-14 (;=-9.51592e-05;)
    local.set 16
    f64.const 0x1.b0213ca2d0eecp-15 (;=5.15139e-05;)
    local.set 17
    f64.const 0x1.9c9eacea7d9cfp+3 (;=12.8944;)
    local.set 18
    f64.const -0x1.e38e8d626667ep+3 (;=-15.1112;)
    local.set 19
    f64.const -0x1.c9557be257dap-3 (;=-0.223308;)
    local.set 20
    f64.const 0x1.849383e87d954p-9 (;=0.0029646;)
    local.set 21
    f64.const 0x1.37c044ac0ace1p-9 (;=0.00237847;)
    local.set 22
    f64.const -0x1.f1983fedbfaap-16 (;=-2.9659e-05;)
    local.set 23
    f64.const 0x1.6e44607a13bd6p-15 (;=4.36624e-05;)
    local.set 24
    f64.const 0x1.0afcdc332ca67p+3 (;=8.34337;)
    local.set 25
    f64.const 0x1.07fcb31de01bp+2 (;=4.1248;)
    local.set 26
    f64.const -0x1.9d353e1eb467cp-2 (;=-0.403523;)
    local.set 27
    f64.const -0x1.6abb60a8e1d76p-9 (;=-0.00276743;)
    local.set 28
    f64.const 0x1.47956257578b8p-8 (;=0.00499853;)
    local.set 29
    f64.const 0x1.829379cad4acp-16 (;=2.30417e-05;)
    local.set 30
    f64.const 0x1.2bc5eeff5e6f8p-12 (;=0.000285886;)
    local.set 31
    f64.const 0x1.35da0343cd92cp+2 (;=4.84143;)
    local.set 32
    f64.const -0x1.290abc01fdb7cp+0 (;=-1.16032;)
    local.set 33
    f64.const -0x1.a86f96c25ebfp-4 (;=-0.103622;)
    local.set 34
    f64.const 0x1.b32ddb8ec9209p-10 (;=0.00166008;)
    local.set 35
    f64.const 0x1.f88ff93f670b6p-8 (;=0.00769901;)
    local.set 36
    f64.const -0x1.2199946debd8p-14 (;=-6.9046e-05;)
    local.set 37
    f64.const 0x1.f49601333c135p-11 (;=0.000954792;)
    local.set 38
    local.get 3
    f64.convert_i32_s
    local.set 39
    f64.const 0x1p+0 (;=1;)
    local.set 40
    local.get 9
    local.get 39
    local.get 39
    local.get 39
    local.get 39
    local.get 39
    local.get 39
    local.get 40
    call $init_planet
    i32.const 56
    local.set 41
    local.get 9
    local.get 41
    i32.add
    local.set 42
    local.get 42
    local.get 32
    local.get 33
    local.get 34
    local.get 35
    local.get 36
    local.get 37
    local.get 38
    call $init_planet
    i32.const 56
    local.set 43
    local.get 42
    local.get 43
    i32.add
    local.set 44
    local.get 44
    local.get 25
    local.get 26
    local.get 27
    local.get 28
    local.get 29
    local.get 30
    local.get 31
    call $init_planet
    i32.const 56
    local.set 45
    local.get 44
    local.get 45
    i32.add
    local.set 46
    local.get 46
    local.get 18
    local.get 19
    local.get 20
    local.get 21
    local.get 22
    local.get 23
    local.get 24
    call $init_planet
    i32.const 56
    local.set 47
    local.get 46
    local.get 47
    i32.add
    local.set 48
    local.get 48
    local.get 11
    local.get 12
    local.get 13
    local.get 14
    local.get 15
    local.get 16
    local.get 17
    call $init_planet
    local.get 2
    local.get 6
    i32.store offset=28
    local.get 2
    local.get 10
    i32.store offset=24
    local.get 9
    local.get 6
    call $offset_momentum
    local.get 9
    local.get 6
    call $energy
    local.set 49
    local.get 49
    call $print_float
    local.get 5
    call $putchar
    drop
    local.get 2
    local.get 4
    f64.store offset=16
    local.get 2
    local.get 3
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 2000
        local.set 50
        local.get 2
        i32.load offset=12
        local.set 51
        local.get 51
        local.set 52
        local.get 50
        local.set 53
        local.get 52
        local.get 53
        i32.lt_u
        local.set 54
        i32.const 1
        local.set 55
        local.get 54
        local.get 55
        i32.and
        local.set 56
        local.get 56
        i32.eqz
        br_if 1 (;@1;)
        i32.const 0
        local.set 57
        local.get 2
        local.get 57
        i32.store offset=8
        block  ;; label = @3
          loop  ;; label = @4
            i32.const 5
            local.set 58
            local.get 2
            i32.load offset=8
            local.set 59
            local.get 59
            local.set 60
            local.get 58
            local.set 61
            local.get 60
            local.get 61
            i32.lt_u
            local.set 62
            i32.const 1
            local.set 63
            local.get 62
            local.get 63
            i32.and
            local.set 64
            local.get 64
            i32.eqz
            br_if 1 (;@3;)
            i32.const 5
            local.set 65
            f64.const 0x1.47ae147ae147bp-7 (;=0.01;)
            local.set 66
            i32.const 32
            local.set 67
            local.get 2
            local.get 67
            i32.add
            local.set 68
            local.get 68
            local.set 69
            local.get 2
            i32.load offset=8
            local.set 70
            i32.const 56
            local.set 71
            local.get 70
            local.get 71
            i32.mul
            local.set 72
            local.get 69
            local.get 72
            i32.add
            local.set 73
            local.get 2
            local.get 73
            i32.store offset=4
            local.get 2
            i32.load offset=4
            local.set 74
            local.get 2
            i32.load offset=8
            local.set 75
            i32.const 1
            local.set 76
            local.get 75
            local.get 76
            i32.add
            local.set 77
            local.get 74
            local.get 69
            local.get 65
            local.get 66
            local.get 77
            call $planet_move_from_i
            local.get 2
            i32.load offset=8
            local.set 78
            i32.const 1
            local.set 79
            local.get 78
            local.get 79
            i32.add
            local.set 80
            local.get 2
            local.get 80
            i32.store offset=8
            br 0 (;@4;)
          end
        end
        local.get 2
        i32.load offset=12
        local.set 81
        i32.const 1
        local.set 82
        local.get 81
        local.get 82
        i32.add
        local.set 83
        local.get 2
        local.get 83
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 10
    local.set 84
    i32.const 5
    local.set 85
    i32.const 32
    local.set 86
    local.get 2
    local.get 86
    i32.add
    local.set 87
    local.get 87
    local.set 88
    local.get 88
    local.get 85
    call $energy
    local.set 89
    local.get 89
    call $print_float
    local.get 84
    call $putchar
    drop
    i32.const 320
    local.set 90
    local.get 2
    local.get 90
    i32.add
    local.set 91
    local.get 91
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
