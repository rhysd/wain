(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32)))
  (type (;3;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (import "env" "getchar" (func $getchar (type 1)))
  (func $print_uint (type 2) (param i32)
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
      call $print_uint
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
  (func $print (type 2) (param i32)
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
  (func $get_input (type 1) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 1024
    local.set 3
    i32.const 0
    local.set 4
    local.get 2
    local.get 4
    i32.store offset=8
    local.get 3
    call $print
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 0
        local.set 5
        call $getchar
        local.set 6
        local.get 2
        local.get 6
        i32.store offset=4
        local.get 2
        i32.load offset=4
        local.set 7
        local.get 7
        local.set 8
        local.get 5
        local.set 9
        local.get 8
        local.get 9
        i32.lt_s
        local.set 10
        i32.const 1
        local.set 11
        local.get 10
        local.get 11
        i32.and
        local.set 12
        block  ;; label = @3
          block  ;; label = @4
            local.get 12
            br_if 0 (;@4;)
            i32.const 10
            local.set 13
            local.get 2
            i32.load offset=4
            local.set 14
            local.get 14
            local.set 15
            local.get 13
            local.set 16
            local.get 15
            local.get 16
            i32.eq
            local.set 17
            i32.const 1
            local.set 18
            local.get 17
            local.get 18
            i32.and
            local.set 19
            local.get 19
            i32.eqz
            br_if 1 (;@3;)
          end
          i32.const 1
          local.set 20
          local.get 2
          i32.load offset=8
          local.set 21
          local.get 21
          local.set 22
          local.get 20
          local.set 23
          local.get 22
          local.get 23
          i32.lt_u
          local.set 24
          i32.const 1
          local.set 25
          local.get 24
          local.get 25
          i32.and
          local.set 26
          block  ;; label = @4
            block  ;; label = @5
              local.get 26
              br_if 0 (;@5;)
              i32.const 100
              local.set 27
              local.get 2
              i32.load offset=8
              local.set 28
              local.get 27
              local.set 29
              local.get 28
              local.set 30
              local.get 29
              local.get 30
              i32.lt_u
              local.set 31
              i32.const 1
              local.set 32
              local.get 31
              local.get 32
              i32.and
              local.set 33
              local.get 33
              i32.eqz
              br_if 1 (;@4;)
            end
            i32.const 1059
            local.set 34
            local.get 34
            call $print
            call $get_input
            local.set 35
            local.get 2
            local.get 35
            i32.store offset=12
            br 3 (;@1;)
          end
          local.get 2
          i32.load offset=8
          local.set 36
          local.get 2
          local.get 36
          i32.store offset=12
          br 2 (;@1;)
        end
        i32.const 48
        local.set 37
        local.get 2
        i32.load offset=4
        local.set 38
        local.get 38
        local.set 39
        local.get 37
        local.set 40
        local.get 39
        local.get 40
        i32.lt_s
        local.set 41
        i32.const 1
        local.set 42
        local.get 41
        local.get 42
        i32.and
        local.set 43
        block  ;; label = @3
          block  ;; label = @4
            local.get 43
            br_if 0 (;@4;)
            i32.const 57
            local.set 44
            local.get 2
            i32.load offset=4
            local.set 45
            local.get 44
            local.set 46
            local.get 45
            local.set 47
            local.get 46
            local.get 47
            i32.lt_s
            local.set 48
            i32.const 1
            local.set 49
            local.get 48
            local.get 49
            i32.and
            local.set 50
            local.get 50
            i32.eqz
            br_if 1 (;@3;)
          end
          i32.const 1059
          local.set 51
          local.get 51
          call $print
          call $get_input
          local.set 52
          local.get 2
          local.get 52
          i32.store offset=12
          br 2 (;@1;)
        end
        local.get 2
        i32.load offset=8
        local.set 53
        i32.const 10
        local.set 54
        local.get 53
        local.get 54
        i32.mul
        local.set 55
        local.get 2
        i32.load offset=4
        local.set 56
        i32.const 48
        local.set 57
        local.get 56
        local.get 57
        i32.sub
        local.set 58
        local.get 55
        local.get 58
        i32.add
        local.set 59
        local.get 2
        local.get 59
        i32.store offset=8
        br 0 (;@2;)
      end
    end
    local.get 2
    i32.load offset=12
    local.set 60
    i32.const 16
    local.set 61
    local.get 2
    local.get 61
    i32.add
    local.set 62
    local.get 62
    global.set 0
    local.get 60
    return)
  (func $_start (type 3)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 1
    local.set 3
    i32.const 1078
    local.set 4
    i32.const 75
    local.set 5
    local.get 2
    local.get 5
    i32.store offset=12
    local.get 4
    call $print
    local.get 2
    local.get 3
    i32.store offset=8
    loop  ;; label = @1
      i32.const 75
      local.set 6
      call $get_input
      local.set 7
      local.get 2
      local.get 7
      i32.store offset=4
      local.get 2
      i32.load offset=4
      local.set 8
      local.get 8
      local.set 9
      local.get 6
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
      block  ;; label = @2
        block  ;; label = @3
          local.get 13
          i32.eqz
          br_if 0 (;@3;)
          i32.const 1098
          local.set 14
          local.get 14
          call $print
          br 1 (;@2;)
        end
        i32.const 75
        local.set 15
        local.get 2
        i32.load offset=4
        local.set 16
        local.get 16
        local.set 17
        local.get 15
        local.set 18
        local.get 17
        local.get 18
        i32.gt_u
        local.set 19
        i32.const 1
        local.set 20
        local.get 19
        local.get 20
        i32.and
        local.set 21
        block  ;; label = @3
          block  ;; label = @4
            local.get 21
            i32.eqz
            br_if 0 (;@4;)
            i32.const 1111
            local.set 22
            local.get 22
            call $print
            br 1 (;@3;)
          end
          i32.const 10
          local.set 23
          i32.const 1122
          local.set 24
          local.get 24
          call $print
          local.get 2
          i32.load offset=8
          local.set 25
          local.get 25
          call $print_uint
          local.get 23
          call $putchar
          drop
          i32.const 16
          local.set 26
          local.get 2
          local.get 26
          i32.add
          local.set 27
          local.get 27
          global.set 0
          return
        end
      end
      local.get 2
      i32.load offset=8
      local.set 28
      i32.const 1
      local.set 29
      local.get 28
      local.get 29
      i32.add
      local.set 30
      local.get 2
      local.get 30
      i32.store offset=8
      br 0 (;@1;)
    end)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66688))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "Please input your guess in 1..100\0a\00\0aInput is invalid\0a\00Guess the number!\0a\0a\00Too small!\0a\0a\00Too big!\0a\0a\00\0aYou win! Tries: \00"))
