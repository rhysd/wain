(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func (param i32 i32 i32)))
  (type (;4;) (func (param i32 i32 i32 i32)))
  (type (;5;) (func (param i32 i32 i32) (result i32)))
  (type (;6;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $strlen_ (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    i32.const 0
    local.set 4
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    local.get 4
    i32.store offset=8
    block  ;; label = @1
      loop  ;; label = @2
        local.get 3
        i32.load offset=12
        local.set 5
        i32.const 1
        local.set 6
        local.get 5
        local.get 6
        i32.add
        local.set 7
        local.get 3
        local.get 7
        i32.store offset=12
        local.get 5
        i32.load8_u
        local.set 8
        i32.const 24
        local.set 9
        local.get 8
        local.get 9
        i32.shl
        local.set 10
        local.get 10
        local.get 9
        i32.shr_s
        local.set 11
        local.get 11
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        i32.load offset=8
        local.set 12
        i32.const 1
        local.set 13
        local.get 12
        local.get 13
        i32.add
        local.set 14
        local.get 3
        local.get 14
        i32.store offset=8
        br 0 (;@2;)
      end
    end
    local.get 3
    i32.load offset=8
    local.set 15
    local.get 15
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
  (func $print_int (type 1) (param i32)
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
    i32.const 0
    local.set 4
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 5
    local.get 5
    local.set 6
    local.get 4
    local.set 7
    local.get 6
    local.get 7
    i32.lt_s
    local.set 8
    i32.const 1
    local.set 9
    local.get 8
    local.get 9
    i32.and
    local.set 10
    block  ;; label = @1
      local.get 10
      i32.eqz
      br_if 0 (;@1;)
      i32.const 0
      local.set 11
      i32.const 45
      local.set 12
      local.get 12
      call $putchar
      drop
      local.get 3
      i32.load offset=12
      local.set 13
      local.get 11
      local.get 13
      i32.sub
      local.set 14
      local.get 3
      local.get 14
      i32.store offset=12
    end
    local.get 3
    i32.load offset=12
    local.set 15
    block  ;; label = @1
      block  ;; label = @2
        local.get 15
        br_if 0 (;@2;)
        i32.const 48
        local.set 16
        local.get 16
        call $putchar
        drop
        br 1 (;@1;)
      end
      local.get 3
      i32.load offset=12
      local.set 17
      local.get 17
      call $print_digits
    end
    i32.const 16
    local.set 18
    local.get 3
    local.get 18
    i32.add
    local.set 19
    local.get 19
    global.set 0
    return)
  (func $bm_search (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 2
    i32.const 1056
    local.set 3
    local.get 2
    local.get 3
    i32.sub
    local.set 4
    local.get 4
    global.set 0
    i32.const 16
    local.set 5
    local.get 4
    local.get 5
    i32.add
    local.set 6
    local.get 6
    local.set 7
    i32.const 0
    local.set 8
    local.get 4
    local.get 0
    i32.store offset=1048
    local.get 4
    local.get 1
    i32.store offset=1044
    local.get 4
    local.get 8
    i32.store offset=12
    local.get 4
    local.get 8
    i32.store offset=8
    local.get 4
    local.get 8
    i32.store offset=4
    local.get 4
    local.get 8
    i32.store
    local.get 4
    i32.load offset=1044
    local.set 9
    local.get 9
    call $strlen_
    local.set 10
    local.get 4
    local.get 10
    i32.store offset=8
    local.get 4
    i32.load offset=1048
    local.set 11
    local.get 11
    call $strlen_
    local.set 12
    local.get 4
    local.get 12
    i32.store offset=12
    local.get 4
    i32.load offset=1044
    local.set 13
    local.get 4
    i32.load offset=8
    local.set 14
    local.get 7
    local.get 13
    local.get 14
    call $bm_table_init
    local.get 4
    i32.load offset=8
    local.set 15
    i32.const 1
    local.set 16
    local.get 15
    local.get 16
    i32.sub
    local.set 17
    local.get 4
    local.get 17
    i32.store
    local.get 4
    local.get 17
    i32.store offset=4
    loop  ;; label = @1
      i32.const 0
      local.set 18
      local.get 4
      i32.load offset=4
      local.set 19
      local.get 4
      i32.load offset=12
      local.set 20
      local.get 19
      local.set 21
      local.get 20
      local.set 22
      local.get 21
      local.get 22
      i32.lt_s
      local.set 23
      i32.const 1
      local.set 24
      local.get 23
      local.get 24
      i32.and
      local.set 25
      local.get 18
      local.set 26
      block  ;; label = @2
        local.get 25
        i32.eqz
        br_if 0 (;@2;)
        i32.const 0
        local.set 27
        local.get 4
        i32.load
        local.set 28
        local.get 28
        local.set 29
        local.get 27
        local.set 30
        local.get 29
        local.get 30
        i32.ge_s
        local.set 31
        local.get 31
        local.set 26
      end
      local.get 26
      local.set 32
      i32.const 1
      local.set 33
      local.get 32
      local.get 33
      i32.and
      local.set 34
      block  ;; label = @2
        local.get 34
        i32.eqz
        br_if 0 (;@2;)
        local.get 4
        i32.load offset=1048
        local.set 35
        local.get 4
        i32.load offset=1044
        local.set 36
        local.get 4
        i32.load offset=4
        local.set 37
        local.get 4
        i32.load
        local.set 38
        local.get 35
        local.get 36
        local.get 37
        local.get 38
        call $print_compare_process
        local.get 4
        i32.load offset=1048
        local.set 39
        local.get 4
        i32.load offset=4
        local.set 40
        local.get 39
        local.get 40
        i32.add
        local.set 41
        local.get 41
        i32.load8_u
        local.set 42
        i32.const 24
        local.set 43
        local.get 42
        local.get 43
        i32.shl
        local.set 44
        local.get 44
        local.get 43
        i32.shr_s
        local.set 45
        local.get 4
        i32.load offset=1044
        local.set 46
        local.get 4
        i32.load
        local.set 47
        local.get 46
        local.get 47
        i32.add
        local.set 48
        local.get 48
        i32.load8_u
        local.set 49
        i32.const 24
        local.set 50
        local.get 49
        local.get 50
        i32.shl
        local.set 51
        local.get 51
        local.get 50
        i32.shr_s
        local.set 52
        local.get 45
        local.set 53
        local.get 52
        local.set 54
        local.get 53
        local.get 54
        i32.ne
        local.set 55
        i32.const 1
        local.set 56
        local.get 55
        local.get 56
        i32.and
        local.set 57
        block  ;; label = @3
          block  ;; label = @4
            local.get 57
            i32.eqz
            br_if 0 (;@4;)
            i32.const 16
            local.set 58
            local.get 4
            local.get 58
            i32.add
            local.set 59
            local.get 59
            local.set 60
            local.get 4
            i32.load offset=1048
            local.set 61
            local.get 4
            i32.load offset=4
            local.set 62
            local.get 61
            local.get 62
            i32.add
            local.set 63
            local.get 63
            i32.load8_u
            local.set 64
            local.get 4
            i32.load offset=8
            local.set 65
            local.get 4
            i32.load
            local.set 66
            local.get 65
            local.get 66
            i32.sub
            local.set 67
            i32.const 24
            local.set 68
            local.get 64
            local.get 68
            i32.shl
            local.set 69
            local.get 69
            local.get 68
            i32.shr_s
            local.set 70
            local.get 60
            local.get 70
            local.get 67
            call $next_step
            local.set 71
            local.get 4
            i32.load offset=4
            local.set 72
            local.get 72
            local.get 71
            i32.add
            local.set 73
            local.get 4
            local.get 73
            i32.store offset=4
            local.get 4
            i32.load offset=8
            local.set 74
            i32.const 1
            local.set 75
            local.get 74
            local.get 75
            i32.sub
            local.set 76
            local.get 4
            local.get 76
            i32.store
            br 1 (;@3;)
          end
          local.get 4
          i32.load
          local.set 77
          i32.const -1
          local.set 78
          local.get 77
          local.get 78
          i32.add
          local.set 79
          local.get 4
          local.get 79
          i32.store
          local.get 4
          i32.load offset=4
          local.set 80
          i32.const -1
          local.set 81
          local.get 80
          local.get 81
          i32.add
          local.set 82
          local.get 4
          local.get 82
          i32.store offset=4
        end
        br 1 (;@1;)
      end
    end
    i32.const 0
    local.set 83
    local.get 4
    i32.load
    local.set 84
    local.get 84
    local.set 85
    local.get 83
    local.set 86
    local.get 85
    local.get 86
    i32.lt_s
    local.set 87
    i32.const 1
    local.set 88
    local.get 87
    local.get 88
    i32.and
    local.set 89
    block  ;; label = @1
      block  ;; label = @2
        local.get 89
        i32.eqz
        br_if 0 (;@2;)
        local.get 4
        i32.load offset=1048
        local.set 90
        local.get 4
        i32.load offset=4
        local.set 91
        i32.const 1
        local.set 92
        local.get 91
        local.get 92
        i32.add
        local.set 93
        local.get 90
        local.get 93
        i32.add
        local.set 94
        local.get 4
        local.get 94
        i32.store offset=1052
        br 1 (;@1;)
      end
      i32.const 0
      local.set 95
      local.get 4
      local.get 95
      i32.store offset=1052
    end
    local.get 4
    i32.load offset=1052
    local.set 96
    i32.const 1056
    local.set 97
    local.get 4
    local.get 97
    i32.add
    local.set 98
    local.get 98
    global.set 0
    local.get 96
    return)
  (func $bm_table_init (type 3) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 3
    i32.const 16
    local.set 4
    local.get 3
    local.get 4
    i32.sub
    local.set 5
    local.get 5
    global.set 0
    i32.const 0
    local.set 6
    local.get 5
    local.get 0
    i32.store offset=12
    local.get 5
    local.get 1
    i32.store offset=8
    local.get 5
    local.get 2
    i32.store offset=4
    local.get 5
    local.get 6
    i32.store
    local.get 5
    local.get 6
    i32.store
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 256
        local.set 7
        local.get 5
        i32.load
        local.set 8
        local.get 8
        local.set 9
        local.get 7
        local.set 10
        local.get 9
        local.get 10
        i32.lt_s
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
        local.get 5
        i32.load offset=4
        local.set 14
        local.get 5
        i32.load offset=12
        local.set 15
        local.get 5
        i32.load
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
        local.get 14
        i32.store
        local.get 5
        i32.load
        local.set 20
        i32.const 1
        local.set 21
        local.get 20
        local.get 21
        i32.add
        local.set 22
        local.get 5
        local.get 22
        i32.store
        br 0 (;@2;)
      end
    end
    i32.const 0
    local.set 23
    local.get 5
    local.get 23
    i32.store
    block  ;; label = @1
      loop  ;; label = @2
        local.get 5
        i32.load
        local.set 24
        local.get 5
        i32.load offset=4
        local.set 25
        local.get 24
        local.set 26
        local.get 25
        local.set 27
        local.get 26
        local.get 27
        i32.lt_s
        local.set 28
        i32.const 1
        local.set 29
        local.get 28
        local.get 29
        i32.and
        local.set 30
        local.get 30
        i32.eqz
        br_if 1 (;@1;)
        local.get 5
        i32.load offset=4
        local.set 31
        local.get 5
        i32.load
        local.set 32
        local.get 31
        local.get 32
        i32.sub
        local.set 33
        i32.const 1
        local.set 34
        local.get 33
        local.get 34
        i32.sub
        local.set 35
        local.get 5
        i32.load offset=12
        local.set 36
        local.get 5
        i32.load offset=8
        local.set 37
        local.get 5
        i32.load
        local.set 38
        local.get 37
        local.get 38
        i32.add
        local.set 39
        local.get 39
        i32.load8_u
        local.set 40
        i32.const 24
        local.set 41
        local.get 40
        local.get 41
        i32.shl
        local.set 42
        local.get 42
        local.get 41
        i32.shr_s
        local.set 43
        i32.const 2
        local.set 44
        local.get 43
        local.get 44
        i32.shl
        local.set 45
        local.get 36
        local.get 45
        i32.add
        local.set 46
        local.get 46
        local.get 35
        i32.store
        local.get 5
        i32.load
        local.set 47
        i32.const 1
        local.set 48
        local.get 47
        local.get 48
        i32.add
        local.set 49
        local.get 5
        local.get 49
        i32.store
        br 0 (;@2;)
      end
    end
    i32.const 0
    local.set 50
    i32.const 10
    local.set 51
    i32.const 1127
    local.set 52
    local.get 52
    call $print_str
    local.get 5
    i32.load offset=4
    local.set 53
    local.get 53
    call $print_int
    local.get 51
    call $putchar
    drop
    local.get 5
    local.get 50
    i32.store
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 256
        local.set 54
        local.get 5
        i32.load
        local.set 55
        local.get 55
        local.set 56
        local.get 54
        local.set 57
        local.get 56
        local.get 57
        i32.lt_s
        local.set 58
        i32.const 1
        local.set 59
        local.get 58
        local.get 59
        i32.and
        local.set 60
        local.get 60
        i32.eqz
        br_if 1 (;@1;)
        local.get 5
        i32.load offset=12
        local.set 61
        local.get 5
        i32.load
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
        i32.load
        local.set 66
        local.get 5
        i32.load offset=4
        local.set 67
        local.get 66
        local.set 68
        local.get 67
        local.set 69
        local.get 68
        local.get 69
        i32.ne
        local.set 70
        i32.const 1
        local.set 71
        local.get 70
        local.get 71
        i32.and
        local.set 72
        block  ;; label = @3
          local.get 72
          i32.eqz
          br_if 0 (;@3;)
          i32.const 10
          local.set 73
          i32.const 1179
          local.set 74
          i32.const 1170
          local.set 75
          i32.const 1153
          local.set 76
          local.get 76
          call $print_str
          local.get 5
          i32.load
          local.set 77
          local.get 77
          call $putchar
          drop
          local.get 75
          call $print_str
          local.get 5
          i32.load
          local.set 78
          local.get 78
          call $print_int
          local.get 74
          call $print_str
          local.get 5
          i32.load offset=12
          local.set 79
          local.get 5
          i32.load
          local.set 80
          i32.const 2
          local.set 81
          local.get 80
          local.get 81
          i32.shl
          local.set 82
          local.get 79
          local.get 82
          i32.add
          local.set 83
          local.get 83
          i32.load
          local.set 84
          local.get 84
          call $print_int
          local.get 73
          call $putchar
          drop
        end
        local.get 5
        i32.load
        local.set 85
        i32.const 1
        local.set 86
        local.get 85
        local.get 86
        i32.add
        local.set 87
        local.get 5
        local.get 87
        i32.store
        br 0 (;@2;)
      end
    end
    i32.const 16
    local.set 88
    local.get 5
    local.get 88
    i32.add
    local.set 89
    local.get 89
    global.set 0
    return)
  (func $print_compare_process (type 4) (param i32 i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 0
    local.set 7
    i32.const 1271
    local.set 8
    i32.const 10
    local.set 9
    i32.const 1260
    local.set 10
    i32.const 1257
    local.set 11
    i32.const 1244
    local.set 12
    i32.const 1225
    local.set 13
    i32.const 1188
    local.set 14
    local.get 6
    local.get 0
    i32.store offset=28
    local.get 6
    local.get 1
    i32.store offset=24
    local.get 6
    local.get 2
    i32.store offset=20
    local.get 6
    local.get 3
    i32.store offset=16
    local.get 6
    local.get 7
    i32.store offset=12
    local.get 14
    call $print_str
    local.get 13
    call $print_str
    local.get 6
    i32.load offset=20
    local.set 15
    local.get 15
    call $print_int
    local.get 12
    call $print_str
    local.get 6
    i32.load offset=16
    local.set 16
    local.get 16
    call $print_int
    local.get 11
    call $print_str
    local.get 10
    call $print_str
    local.get 6
    i32.load offset=28
    local.set 17
    local.get 17
    call $print_str
    local.get 9
    call $putchar
    drop
    local.get 8
    call $print_str
    local.get 6
    local.get 7
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        local.get 6
        i32.load offset=12
        local.set 18
        local.get 6
        i32.load offset=20
        local.set 19
        local.get 6
        i32.load offset=16
        local.set 20
        local.get 19
        local.get 20
        i32.sub
        local.set 21
        local.get 18
        local.set 22
        local.get 21
        local.set 23
        local.get 22
        local.get 23
        i32.lt_s
        local.set 24
        i32.const 1
        local.set 25
        local.get 24
        local.get 25
        i32.and
        local.set 26
        local.get 26
        i32.eqz
        br_if 1 (;@1;)
        i32.const 32
        local.set 27
        local.get 27
        call $putchar
        drop
        local.get 6
        i32.load offset=12
        local.set 28
        i32.const 1
        local.set 29
        local.get 28
        local.get 29
        i32.add
        local.set 30
        local.get 6
        local.get 30
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 0
    local.set 31
    i32.const 1282
    local.set 32
    i32.const 10
    local.set 33
    local.get 6
    i32.load offset=24
    local.set 34
    local.get 34
    call $print_str
    local.get 33
    call $putchar
    drop
    local.get 32
    call $print_str
    local.get 6
    local.get 31
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        local.get 6
        i32.load offset=12
        local.set 35
        local.get 6
        i32.load offset=20
        local.set 36
        local.get 35
        local.set 37
        local.get 36
        local.set 38
        local.get 37
        local.get 38
        i32.lt_s
        local.set 39
        i32.const 1
        local.set 40
        local.get 39
        local.get 40
        i32.and
        local.set 41
        local.get 41
        i32.eqz
        br_if 1 (;@1;)
        i32.const 32
        local.set 42
        local.get 42
        call $putchar
        drop
        local.get 6
        i32.load offset=12
        local.set 43
        i32.const 1
        local.set 44
        local.get 43
        local.get 44
        i32.add
        local.set 45
        local.get 6
        local.get 45
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 1293
    local.set 46
    local.get 46
    call $print_str
    i32.const 32
    local.set 47
    local.get 6
    local.get 47
    i32.add
    local.set 48
    local.get 48
    global.set 0
    return)
  (func $next_step (type 5) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 3
    i32.const 16
    local.set 4
    local.get 3
    local.get 4
    i32.sub
    local.set 5
    local.get 5
    local.get 0
    i32.store offset=8
    local.get 5
    local.get 1
    i32.store8 offset=7
    local.get 5
    local.get 2
    i32.store
    local.get 5
    i32.load offset=8
    local.set 6
    local.get 5
    i32.load8_u offset=7
    local.set 7
    i32.const 24
    local.set 8
    local.get 7
    local.get 8
    i32.shl
    local.set 9
    local.get 9
    local.get 8
    i32.shr_s
    local.set 10
    i32.const 2
    local.set 11
    local.get 10
    local.get 11
    i32.shl
    local.set 12
    local.get 6
    local.get 12
    i32.add
    local.set 13
    local.get 13
    i32.load
    local.set 14
    local.get 5
    i32.load
    local.set 15
    local.get 14
    local.set 16
    local.get 15
    local.set 17
    local.get 16
    local.get 17
    i32.gt_s
    local.set 18
    i32.const 1
    local.set 19
    local.get 18
    local.get 19
    i32.and
    local.set 20
    block  ;; label = @1
      block  ;; label = @2
        local.get 20
        i32.eqz
        br_if 0 (;@2;)
        local.get 5
        i32.load offset=8
        local.set 21
        local.get 5
        i32.load8_u offset=7
        local.set 22
        i32.const 24
        local.set 23
        local.get 22
        local.get 23
        i32.shl
        local.set 24
        local.get 24
        local.get 23
        i32.shr_s
        local.set 25
        i32.const 2
        local.set 26
        local.get 25
        local.get 26
        i32.shl
        local.set 27
        local.get 21
        local.get 27
        i32.add
        local.set 28
        local.get 28
        i32.load
        local.set 29
        local.get 5
        local.get 29
        i32.store offset=12
        br 1 (;@1;)
      end
      local.get 5
      i32.load
      local.set 30
      local.get 5
      local.get 30
      i32.store offset=12
    end
    local.get 5
    i32.load offset=12
    local.set 31
    local.get 31
    return)
  (func $_start (type 6)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 10
    local.set 4
    i32.const 1059
    local.set 5
    i32.const 1048
    local.set 6
    i32.const 1042
    local.set 7
    i32.const 1024
    local.set 8
    local.get 2
    local.get 8
    i32.store offset=12
    local.get 2
    local.get 7
    i32.store offset=8
    local.get 2
    local.get 3
    i32.store offset=4
    local.get 6
    call $print_str
    local.get 2
    i32.load offset=12
    local.set 9
    local.get 9
    call $print_str
    local.get 4
    call $putchar
    drop
    local.get 5
    call $print_str
    local.get 2
    i32.load offset=8
    local.set 10
    local.get 10
    call $print_str
    local.get 4
    call $putchar
    drop
    local.get 2
    i32.load offset=12
    local.set 11
    local.get 2
    i32.load offset=8
    local.set 12
    local.get 11
    local.get 12
    call $bm_search
    local.set 13
    local.get 2
    local.get 13
    i32.store offset=4
    local.get 2
    i32.load offset=4
    local.set 14
    local.get 14
    local.set 15
    local.get 3
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
    block  ;; label = @1
      block  ;; label = @2
        local.get 19
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1070
        local.set 20
        local.get 20
        call $print_str
        br 1 (;@1;)
      end
      i32.const 10
      local.set 21
      i32.const 1110
      local.set 22
      i32.const 1092
      local.set 23
      local.get 23
      call $print_str
      local.get 22
      call $print_str
      local.get 2
      i32.load offset=4
      local.set 24
      local.get 24
      call $print_str
      local.get 21
      call $putchar
      drop
    end
    i32.const 16
    local.set 25
    local.get 2
    local.get 25
    i32.add
    local.set 26
    local.get 26
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66832))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "GCTCACTGAGCGCTCGT\00GCTCG\00[text]   :\00[pattern]:\00[result] : not found\0a\00[result] : found\0a\00         : text=\00[table]  : default: step=\00         : char=\00: table[\00]: step=\00-----------------------------------\0a\00[compare]:(text i=\00)(pattern j=\00)\0a\00 text    :\00 pattern :\00         :\00^\0a\00"))
