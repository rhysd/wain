(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32 i32)))
  (type (;2;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;3;) (func (param i32 i32) (result i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $print_board (type 1) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 0
    local.set 5
    local.get 4
    local.get 0
    i32.store offset=12
    local.get 4
    local.get 1
    i32.store offset=8
    local.get 4
    local.get 5
    i32.store offset=4
    block  ;; label = @1
      loop  ;; label = @2
        local.get 4
        i32.load offset=4
        local.set 6
        local.get 4
        i32.load offset=8
        local.set 7
        local.get 6
        local.set 8
        local.get 7
        local.set 9
        local.get 8
        local.get 9
        i32.lt_u
        local.set 10
        i32.const 1
        local.set 11
        local.get 10
        local.get 11
        i32.and
        local.set 12
        local.get 12
        i32.eqz
        br_if 1 (;@1;)
        i32.const 0
        local.set 13
        local.get 4
        local.get 13
        i32.store
        block  ;; label = @3
          loop  ;; label = @4
            local.get 4
            i32.load
            local.set 14
            local.get 4
            i32.load offset=8
            local.set 15
            local.get 14
            local.set 16
            local.get 15
            local.set 17
            local.get 16
            local.get 17
            i32.lt_u
            local.set 18
            i32.const 1
            local.set 19
            local.get 18
            local.get 19
            i32.and
            local.set 20
            local.get 20
            i32.eqz
            br_if 1 (;@3;)
            i32.const 81
            local.set 21
            i32.const 46
            local.set 22
            i32.const 32
            local.set 23
            local.get 23
            call $putchar
            drop
            local.get 4
            i32.load offset=12
            local.set 24
            local.get 4
            i32.load offset=4
            local.set 25
            i32.const 5
            local.set 26
            local.get 25
            local.get 26
            i32.shl
            local.set 27
            local.get 24
            local.get 27
            i32.add
            local.set 28
            local.get 4
            i32.load
            local.set 29
            i32.const 2
            local.set 30
            local.get 29
            local.get 30
            i32.shl
            local.set 31
            local.get 28
            local.get 31
            i32.add
            local.set 32
            local.get 32
            i32.load
            local.set 33
            local.get 21
            local.get 22
            local.get 33
            select
            local.set 34
            local.get 34
            call $putchar
            drop
            local.get 4
            i32.load
            local.set 35
            i32.const 1
            local.set 36
            local.get 35
            local.get 36
            i32.add
            local.set 37
            local.get 4
            local.get 37
            i32.store
            br 0 (;@4;)
          end
        end
        i32.const 10
        local.set 38
        local.get 38
        call $putchar
        drop
        local.get 4
        i32.load offset=4
        local.set 39
        i32.const 1
        local.set 40
        local.get 39
        local.get 40
        i32.add
        local.set 41
        local.get 4
        local.get 41
        i32.store offset=4
        br 0 (;@2;)
      end
    end
    i32.const 16
    local.set 42
    local.get 4
    local.get 42
    i32.add
    local.set 43
    local.get 43
    global.set 0
    return)
  (func $is_safe (type 2) (param i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 4
    i32.const 32
    local.set 5
    local.get 4
    local.get 5
    i32.sub
    local.set 6
    i32.const 0
    local.set 7
    local.get 6
    local.get 0
    i32.store offset=24
    local.get 6
    local.get 1
    i32.store offset=20
    local.get 6
    local.get 2
    i32.store offset=16
    local.get 6
    local.get 3
    i32.store offset=12
    local.get 6
    local.get 7
    i32.store offset=8
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          local.get 6
          i32.load offset=8
          local.set 8
          local.get 6
          i32.load offset=16
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
          br_if 1 (;@2;)
          local.get 6
          i32.load offset=24
          local.set 15
          local.get 6
          i32.load offset=8
          local.set 16
          i32.const 5
          local.set 17
          local.get 16
          local.get 17
          i32.shl
          local.set 18
          local.get 15
          local.get 18
          i32.add
          local.set 19
          local.get 6
          i32.load offset=12
          local.set 20
          i32.const 2
          local.set 21
          local.get 20
          local.get 21
          i32.shl
          local.set 22
          local.get 19
          local.get 22
          i32.add
          local.set 23
          local.get 23
          i32.load
          local.set 24
          block  ;; label = @4
            local.get 24
            i32.eqz
            br_if 0 (;@4;)
            i32.const 0
            local.set 25
            local.get 6
            local.get 25
            i32.store offset=28
            br 3 (;@1;)
          end
          local.get 6
          i32.load offset=12
          local.set 26
          local.get 6
          i32.load offset=8
          local.set 27
          local.get 26
          local.get 27
          i32.add
          local.set 28
          local.get 6
          i32.load offset=16
          local.set 29
          local.get 28
          local.set 30
          local.get 29
          local.set 31
          local.get 30
          local.get 31
          i32.ge_u
          local.set 32
          i32.const 1
          local.set 33
          local.get 32
          local.get 33
          i32.and
          local.set 34
          block  ;; label = @4
            local.get 34
            i32.eqz
            br_if 0 (;@4;)
            local.get 6
            i32.load offset=24
            local.set 35
            local.get 6
            i32.load offset=8
            local.set 36
            i32.const 5
            local.set 37
            local.get 36
            local.get 37
            i32.shl
            local.set 38
            local.get 35
            local.get 38
            i32.add
            local.set 39
            local.get 6
            i32.load offset=12
            local.set 40
            local.get 6
            i32.load offset=16
            local.set 41
            local.get 6
            i32.load offset=8
            local.set 42
            local.get 41
            local.get 42
            i32.sub
            local.set 43
            local.get 40
            local.get 43
            i32.sub
            local.set 44
            i32.const 2
            local.set 45
            local.get 44
            local.get 45
            i32.shl
            local.set 46
            local.get 39
            local.get 46
            i32.add
            local.set 47
            local.get 47
            i32.load
            local.set 48
            local.get 48
            i32.eqz
            br_if 0 (;@4;)
            i32.const 0
            local.set 49
            local.get 6
            local.get 49
            i32.store offset=28
            br 3 (;@1;)
          end
          local.get 6
          i32.load offset=12
          local.set 50
          local.get 6
          i32.load offset=16
          local.set 51
          local.get 50
          local.get 51
          i32.add
          local.set 52
          local.get 6
          i32.load offset=8
          local.set 53
          local.get 52
          local.get 53
          i32.sub
          local.set 54
          local.get 6
          i32.load offset=20
          local.set 55
          local.get 54
          local.set 56
          local.get 55
          local.set 57
          local.get 56
          local.get 57
          i32.lt_u
          local.set 58
          i32.const 1
          local.set 59
          local.get 58
          local.get 59
          i32.and
          local.set 60
          block  ;; label = @4
            local.get 60
            i32.eqz
            br_if 0 (;@4;)
            local.get 6
            i32.load offset=24
            local.set 61
            local.get 6
            i32.load offset=8
            local.set 62
            i32.const 5
            local.set 63
            local.get 62
            local.get 63
            i32.shl
            local.set 64
            local.get 61
            local.get 64
            i32.add
            local.set 65
            local.get 6
            i32.load offset=12
            local.set 66
            local.get 6
            i32.load offset=16
            local.set 67
            local.get 66
            local.get 67
            i32.add
            local.set 68
            local.get 6
            i32.load offset=8
            local.set 69
            local.get 68
            local.get 69
            i32.sub
            local.set 70
            i32.const 2
            local.set 71
            local.get 70
            local.get 71
            i32.shl
            local.set 72
            local.get 65
            local.get 72
            i32.add
            local.set 73
            local.get 73
            i32.load
            local.set 74
            local.get 74
            i32.eqz
            br_if 0 (;@4;)
            i32.const 0
            local.set 75
            local.get 6
            local.get 75
            i32.store offset=28
            br 3 (;@1;)
          end
          local.get 6
          i32.load offset=8
          local.set 76
          i32.const 1
          local.set 77
          local.get 76
          local.get 77
          i32.add
          local.set 78
          local.get 6
          local.get 78
          i32.store offset=8
          br 0 (;@3;)
        end
      end
      i32.const 1
      local.set 79
      local.get 6
      local.get 79
      i32.store offset=28
    end
    local.get 6
    i32.load offset=28
    local.set 80
    local.get 80
    return)
  (func $solve_row (type 2) (param i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 4
    i32.const 32
    local.set 5
    local.get 4
    local.get 5
    i32.sub
    local.set 6
    local.get 6
    global.set 0
    local.get 6
    local.get 0
    i32.store offset=24
    local.get 6
    local.get 1
    i32.store offset=20
    local.get 6
    local.get 2
    i32.store offset=16
    local.get 6
    local.get 3
    i32.store offset=12
    local.get 6
    i32.load offset=12
    local.set 7
    local.get 6
    i32.load offset=20
    local.set 8
    local.get 7
    local.set 9
    local.get 8
    local.set 10
    local.get 9
    local.get 10
    i32.ge_u
    local.set 11
    i32.const 1
    local.set 12
    local.get 11
    local.get 12
    i32.and
    local.set 13
    block  ;; label = @1
      block  ;; label = @2
        local.get 13
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1
        local.set 14
        local.get 6
        local.get 14
        i32.store offset=28
        br 1 (;@1;)
      end
      local.get 6
      i32.load offset=16
      local.set 15
      local.get 6
      i32.load offset=20
      local.set 16
      local.get 15
      local.set 17
      local.get 16
      local.set 18
      local.get 17
      local.get 18
      i32.ge_u
      local.set 19
      i32.const 1
      local.set 20
      local.get 19
      local.get 20
      i32.and
      local.set 21
      block  ;; label = @2
        local.get 21
        i32.eqz
        br_if 0 (;@2;)
        i32.const 0
        local.set 22
        local.get 6
        local.get 22
        i32.store offset=28
        br 1 (;@1;)
      end
      i32.const 0
      local.set 23
      local.get 6
      local.get 23
      i32.store offset=8
      block  ;; label = @2
        loop  ;; label = @3
          local.get 6
          i32.load offset=8
          local.set 24
          local.get 6
          i32.load offset=20
          local.set 25
          local.get 24
          local.set 26
          local.get 25
          local.set 27
          local.get 26
          local.get 27
          i32.lt_u
          local.set 28
          i32.const 1
          local.set 29
          local.get 28
          local.get 29
          i32.and
          local.set 30
          local.get 30
          i32.eqz
          br_if 1 (;@2;)
          local.get 6
          i32.load offset=24
          local.set 31
          local.get 6
          i32.load offset=20
          local.set 32
          local.get 6
          i32.load offset=16
          local.set 33
          local.get 6
          i32.load offset=8
          local.set 34
          local.get 31
          local.get 32
          local.get 33
          local.get 34
          call $is_safe
          local.set 35
          block  ;; label = @4
            block  ;; label = @5
              local.get 35
              br_if 0 (;@5;)
              br 1 (;@4;)
            end
            i32.const 1
            local.set 36
            local.get 6
            i32.load offset=24
            local.set 37
            local.get 6
            i32.load offset=16
            local.set 38
            i32.const 5
            local.set 39
            local.get 38
            local.get 39
            i32.shl
            local.set 40
            local.get 37
            local.get 40
            i32.add
            local.set 41
            local.get 6
            i32.load offset=8
            local.set 42
            i32.const 2
            local.set 43
            local.get 42
            local.get 43
            i32.shl
            local.set 44
            local.get 41
            local.get 44
            i32.add
            local.set 45
            local.get 45
            local.get 36
            i32.store
            local.get 6
            i32.load offset=24
            local.set 46
            local.get 6
            i32.load offset=20
            local.set 47
            local.get 6
            i32.load offset=16
            local.set 48
            i32.const 1
            local.set 49
            local.get 48
            local.get 49
            i32.add
            local.set 50
            local.get 6
            i32.load offset=12
            local.set 51
            i32.const 1
            local.set 52
            local.get 51
            local.get 52
            i32.add
            local.set 53
            local.get 46
            local.get 47
            local.get 50
            local.get 53
            call $solve_row
            local.set 54
            block  ;; label = @5
              local.get 54
              i32.eqz
              br_if 0 (;@5;)
              i32.const 1
              local.set 55
              local.get 6
              local.get 55
              i32.store offset=28
              br 4 (;@1;)
            end
            i32.const 0
            local.set 56
            local.get 6
            i32.load offset=24
            local.set 57
            local.get 6
            i32.load offset=16
            local.set 58
            i32.const 5
            local.set 59
            local.get 58
            local.get 59
            i32.shl
            local.set 60
            local.get 57
            local.get 60
            i32.add
            local.set 61
            local.get 6
            i32.load offset=8
            local.set 62
            i32.const 2
            local.set 63
            local.get 62
            local.get 63
            i32.shl
            local.set 64
            local.get 61
            local.get 64
            i32.add
            local.set 65
            local.get 65
            local.get 56
            i32.store
          end
          local.get 6
          i32.load offset=8
          local.set 66
          i32.const 1
          local.set 67
          local.get 66
          local.get 67
          i32.add
          local.set 68
          local.get 6
          local.get 68
          i32.store offset=8
          br 0 (;@3;)
        end
      end
      i32.const 0
      local.set 69
      local.get 6
      local.get 69
      i32.store offset=28
    end
    local.get 6
    i32.load offset=28
    local.set 70
    i32.const 32
    local.set 71
    local.get 6
    local.get 71
    i32.add
    local.set 72
    local.get 72
    global.set 0
    local.get 70
    return)
  (func $solve (type 3) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 0
    local.set 5
    local.get 4
    local.get 0
    i32.store offset=12
    local.get 4
    local.get 1
    i32.store offset=8
    local.get 4
    i32.load offset=12
    local.set 6
    local.get 4
    i32.load offset=8
    local.set 7
    local.get 6
    local.get 7
    local.get 5
    local.get 5
    call $solve_row
    local.set 8
    i32.const 16
    local.set 9
    local.get 4
    local.get 9
    i32.add
    local.set 10
    local.get 10
    global.set 0
    local.get 8
    return)
  (func $put_str (type 4) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 10
    local.set 17
    local.get 17
    call $putchar
    drop
    i32.const 16
    local.set 18
    local.get 3
    local.get 18
    i32.add
    local.set 19
    local.get 19
    global.set 0
    return)
  (func $_start (type 5)
    (local i32 i32 i32 i32 f64 f64 f64 i32 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 272
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 0
    local.set 3
    f64.const 0x1p+6 (;=64;)
    local.set 4
    local.get 4
    f64.sqrt
    local.set 5
    f64.const 0x1p+32 (;=4.29497e+09;)
    local.set 6
    local.get 5
    local.get 6
    f64.lt
    local.set 7
    f64.const 0x0p+0 (;=0;)
    local.set 8
    local.get 5
    local.get 8
    f64.ge
    local.set 9
    local.get 7
    local.get 9
    i32.and
    local.set 10
    local.get 10
    i32.eqz
    local.set 11
    block  ;; label = @1
      block  ;; label = @2
        local.get 11
        br_if 0 (;@2;)
        local.get 5
        i32.trunc_f64_u
        local.set 12
        local.get 12
        local.set 13
        br 1 (;@1;)
      end
      i32.const 0
      local.set 14
      local.get 14
      local.set 13
    end
    local.get 13
    local.set 15
    local.get 2
    local.get 15
    i32.store offset=12
    local.get 2
    local.get 3
    i32.store offset=8
    block  ;; label = @1
      loop  ;; label = @2
        local.get 2
        i32.load offset=8
        local.set 16
        local.get 2
        i32.load offset=12
        local.set 17
        local.get 16
        local.set 18
        local.get 17
        local.set 19
        local.get 18
        local.get 19
        i32.lt_u
        local.set 20
        i32.const 1
        local.set 21
        local.get 20
        local.get 21
        i32.and
        local.set 22
        local.get 22
        i32.eqz
        br_if 1 (;@1;)
        i32.const 0
        local.set 23
        local.get 2
        local.get 23
        i32.store offset=4
        block  ;; label = @3
          loop  ;; label = @4
            local.get 2
            i32.load offset=4
            local.set 24
            local.get 2
            i32.load offset=12
            local.set 25
            local.get 24
            local.set 26
            local.get 25
            local.set 27
            local.get 26
            local.get 27
            i32.lt_u
            local.set 28
            i32.const 1
            local.set 29
            local.get 28
            local.get 29
            i32.and
            local.set 30
            local.get 30
            i32.eqz
            br_if 1 (;@3;)
            i32.const 0
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
            i32.load offset=8
            local.set 35
            i32.const 5
            local.set 36
            local.get 35
            local.get 36
            i32.shl
            local.set 37
            local.get 34
            local.get 37
            i32.add
            local.set 38
            local.get 2
            i32.load offset=4
            local.set 39
            i32.const 2
            local.set 40
            local.get 39
            local.get 40
            i32.shl
            local.set 41
            local.get 38
            local.get 41
            i32.add
            local.set 42
            local.get 42
            local.get 31
            i32.store
            local.get 2
            i32.load offset=4
            local.set 43
            i32.const 1
            local.set 44
            local.get 43
            local.get 44
            i32.add
            local.set 45
            local.get 2
            local.get 45
            i32.store offset=4
            br 0 (;@4;)
          end
        end
        local.get 2
        i32.load offset=8
        local.set 46
        i32.const 1
        local.set 47
        local.get 46
        local.get 47
        i32.add
        local.set 48
        local.get 2
        local.get 48
        i32.store offset=8
        br 0 (;@2;)
      end
    end
    i32.const 16
    local.set 49
    local.get 2
    local.get 49
    i32.add
    local.set 50
    local.get 50
    local.set 51
    local.get 2
    i32.load offset=12
    local.set 52
    local.get 51
    local.get 52
    call $solve
    local.set 53
    block  ;; label = @1
      block  ;; label = @2
        local.get 53
        br_if 0 (;@2;)
        i32.const 1024
        local.set 54
        local.get 54
        call $put_str
        br 1 (;@1;)
      end
      i32.const 16
      local.set 55
      local.get 2
      local.get 55
      i32.add
      local.set 56
      local.get 56
      local.set 57
      local.get 2
      i32.load offset=12
      local.set 58
      local.get 57
      local.get 58
      call $print_board
    end
    i32.const 272
    local.set 59
    local.get 2
    local.get 59
    i32.add
    local.set 60
    local.get 60
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66576))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "No answer\00"))
