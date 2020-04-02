(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (result i32)))
  (type (;2;) (func (param i32 i32 i32) (result i32)))
  (type (;3;) (func (param i32 i32 i32)))
  (type (;4;) (func (param i32)))
  (type (;5;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (import "env" "getchar" (func $getchar (type 1)))
  (import "env" "memcpy" (func $memcpy (type 2)))
  (func $init_vm (type 3) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 3
    i32.const 16
    local.set 4
    local.get 3
    local.get 4
    i32.sub
    local.set 5
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
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 30000
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
        i32.const 0
        local.set 14
        local.get 5
        i32.load offset=12
        local.set 15
        local.get 5
        i32.load
        local.set 16
        local.get 15
        local.get 16
        i32.add
        local.set 17
        local.get 17
        local.get 14
        i32.store8
        local.get 5
        i32.load
        local.set 18
        i32.const 1
        local.set 19
        local.get 18
        local.get 19
        i32.add
        local.set 20
        local.get 5
        local.get 20
        i32.store
        br 0 (;@2;)
      end
    end
    i32.const 0
    local.set 21
    local.get 5
    i32.load offset=12
    local.set 22
    local.get 5
    i32.load offset=12
    local.set 23
    local.get 23
    local.get 22
    i32.store offset=30000
    local.get 5
    i32.load offset=8
    local.set 24
    local.get 5
    i32.load offset=12
    local.set 25
    local.get 25
    local.get 24
    i32.store offset=30004
    local.get 5
    i32.load offset=4
    local.set 26
    local.get 5
    i32.load offset=12
    local.set 27
    local.get 27
    local.get 26
    i32.store offset=30008
    local.get 5
    i32.load offset=12
    local.set 28
    local.get 28
    local.get 21
    i32.store offset=30012
    return)
  (func $step (type 4) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    local.get 3
    local.get 0
    i32.store offset=28
    local.get 3
    i32.load offset=28
    local.set 4
    local.get 4
    i32.load offset=30004
    local.set 5
    local.get 4
    i32.load offset=30012
    local.set 6
    local.get 5
    local.get 6
    i32.add
    local.set 7
    local.get 7
    i32.load8_s
    local.set 8
    i32.const -43
    local.set 9
    local.get 8
    local.get 9
    i32.add
    local.set 10
    i32.const 50
    local.set 11
    local.get 10
    local.get 11
    i32.gt_u
    local.set 12
    block  ;; label = @1
      block  ;; label = @2
        local.get 12
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        local.get 10
                        br_table 2 (;@8;) 5 (;@5;) 3 (;@7;) 4 (;@6;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 1 (;@9;) 8 (;@2;) 0 (;@10;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 8 (;@2;) 6 (;@4;) 8 (;@2;) 7 (;@3;) 2 (;@8;)
                      end
                      local.get 3
                      i32.load offset=28
                      local.set 13
                      local.get 13
                      i32.load offset=30000
                      local.set 14
                      i32.const 1
                      local.set 15
                      local.get 14
                      local.get 15
                      i32.add
                      local.set 16
                      local.get 13
                      local.get 16
                      i32.store offset=30000
                      br 8 (;@1;)
                    end
                    local.get 3
                    i32.load offset=28
                    local.set 17
                    local.get 17
                    i32.load offset=30000
                    local.set 18
                    i32.const -1
                    local.set 19
                    local.get 18
                    local.get 19
                    i32.add
                    local.set 20
                    local.get 17
                    local.get 20
                    i32.store offset=30000
                    br 7 (;@1;)
                  end
                  local.get 3
                  i32.load offset=28
                  local.set 21
                  local.get 21
                  i32.load offset=30000
                  local.set 22
                  local.get 22
                  i32.load8_u
                  local.set 23
                  i32.const 1
                  local.set 24
                  local.get 23
                  local.get 24
                  i32.add
                  local.set 25
                  local.get 22
                  local.get 25
                  i32.store8
                  br 6 (;@1;)
                end
                local.get 3
                i32.load offset=28
                local.set 26
                local.get 26
                i32.load offset=30000
                local.set 27
                local.get 27
                i32.load8_u
                local.set 28
                i32.const -1
                local.set 29
                local.get 28
                local.get 29
                i32.add
                local.set 30
                local.get 27
                local.get 30
                i32.store8
                br 5 (;@1;)
              end
              local.get 3
              i32.load offset=28
              local.set 31
              local.get 31
              i32.load offset=30000
              local.set 32
              local.get 32
              i32.load8_u
              local.set 33
              i32.const 255
              local.set 34
              local.get 33
              local.get 34
              i32.and
              local.set 35
              local.get 35
              call $putchar
              drop
              br 4 (;@1;)
            end
            call $getchar
            local.set 36
            local.get 3
            i32.load offset=28
            local.set 37
            local.get 37
            i32.load offset=30000
            local.set 38
            local.get 38
            local.get 36
            i32.store8
            br 3 (;@1;)
          end
          local.get 3
          i32.load offset=28
          local.set 39
          local.get 39
          i32.load offset=30000
          local.set 40
          local.get 40
          i32.load8_u
          local.set 41
          i32.const 255
          local.set 42
          local.get 41
          local.get 42
          i32.and
          local.set 43
          block  ;; label = @4
            local.get 43
            br_if 0 (;@4;)
            i32.const 0
            local.set 44
            local.get 3
            local.get 44
            i32.store offset=24
            block  ;; label = @5
              loop  ;; label = @6
                i32.const 91
                local.set 45
                local.get 3
                i32.load offset=28
                local.set 46
                local.get 46
                i32.load offset=30012
                local.set 47
                i32.const 1
                local.set 48
                local.get 47
                local.get 48
                i32.add
                local.set 49
                local.get 46
                local.get 49
                i32.store offset=30012
                local.get 3
                i32.load offset=28
                local.set 50
                local.get 50
                i32.load offset=30004
                local.set 51
                local.get 3
                i32.load offset=28
                local.set 52
                local.get 52
                i32.load offset=30012
                local.set 53
                local.get 51
                local.get 53
                i32.add
                local.set 54
                local.get 54
                i32.load8_u
                local.set 55
                i32.const 24
                local.set 56
                local.get 55
                local.get 56
                i32.shl
                local.set 57
                local.get 57
                local.get 56
                i32.shr_s
                local.set 58
                local.get 3
                local.get 58
                i32.store offset=20
                local.get 3
                i32.load offset=20
                local.set 59
                local.get 59
                local.set 60
                local.get 45
                local.set 61
                local.get 60
                local.get 61
                i32.eq
                local.set 62
                i32.const 1
                local.set 63
                local.get 62
                local.get 63
                i32.and
                local.set 64
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 64
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 3
                    i32.load offset=24
                    local.set 65
                    i32.const 1
                    local.set 66
                    local.get 65
                    local.get 66
                    i32.add
                    local.set 67
                    local.get 3
                    local.get 67
                    i32.store offset=24
                    br 1 (;@7;)
                  end
                  i32.const 93
                  local.set 68
                  local.get 3
                  i32.load offset=20
                  local.set 69
                  local.get 69
                  local.set 70
                  local.get 68
                  local.set 71
                  local.get 70
                  local.get 71
                  i32.eq
                  local.set 72
                  i32.const 1
                  local.set 73
                  local.get 72
                  local.get 73
                  i32.and
                  local.set 74
                  block  ;; label = @8
                    local.get 74
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 3
                    i32.load offset=24
                    local.set 75
                    block  ;; label = @9
                      local.get 75
                      br_if 0 (;@9;)
                      br 4 (;@5;)
                    end
                    local.get 3
                    i32.load offset=24
                    local.set 76
                    i32.const -1
                    local.set 77
                    local.get 76
                    local.get 77
                    i32.add
                    local.set 78
                    local.get 3
                    local.get 78
                    i32.store offset=24
                  end
                end
                br 0 (;@6;)
              end
            end
          end
          br 2 (;@1;)
        end
        local.get 3
        i32.load offset=28
        local.set 79
        local.get 79
        i32.load offset=30000
        local.set 80
        local.get 80
        i32.load8_u
        local.set 81
        i32.const 255
        local.set 82
        local.get 81
        local.get 82
        i32.and
        local.set 83
        block  ;; label = @3
          local.get 83
          i32.eqz
          br_if 0 (;@3;)
          i32.const 0
          local.set 84
          local.get 3
          local.get 84
          i32.store offset=16
          block  ;; label = @4
            loop  ;; label = @5
              i32.const 93
              local.set 85
              local.get 3
              i32.load offset=28
              local.set 86
              local.get 86
              i32.load offset=30012
              local.set 87
              i32.const -1
              local.set 88
              local.get 87
              local.get 88
              i32.add
              local.set 89
              local.get 86
              local.get 89
              i32.store offset=30012
              local.get 3
              i32.load offset=28
              local.set 90
              local.get 90
              i32.load offset=30004
              local.set 91
              local.get 3
              i32.load offset=28
              local.set 92
              local.get 92
              i32.load offset=30012
              local.set 93
              local.get 91
              local.get 93
              i32.add
              local.set 94
              local.get 94
              i32.load8_u
              local.set 95
              i32.const 24
              local.set 96
              local.get 95
              local.get 96
              i32.shl
              local.set 97
              local.get 97
              local.get 96
              i32.shr_s
              local.set 98
              local.get 3
              local.get 98
              i32.store offset=12
              local.get 3
              i32.load offset=12
              local.set 99
              local.get 99
              local.set 100
              local.get 85
              local.set 101
              local.get 100
              local.get 101
              i32.eq
              local.set 102
              i32.const 1
              local.set 103
              local.get 102
              local.get 103
              i32.and
              local.set 104
              block  ;; label = @6
                block  ;; label = @7
                  local.get 104
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 3
                  i32.load offset=16
                  local.set 105
                  i32.const 1
                  local.set 106
                  local.get 105
                  local.get 106
                  i32.add
                  local.set 107
                  local.get 3
                  local.get 107
                  i32.store offset=16
                  br 1 (;@6;)
                end
                i32.const 91
                local.set 108
                local.get 3
                i32.load offset=12
                local.set 109
                local.get 109
                local.set 110
                local.get 108
                local.set 111
                local.get 110
                local.get 111
                i32.eq
                local.set 112
                i32.const 1
                local.set 113
                local.get 112
                local.get 113
                i32.and
                local.set 114
                block  ;; label = @7
                  local.get 114
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 3
                  i32.load offset=16
                  local.set 115
                  block  ;; label = @8
                    local.get 115
                    br_if 0 (;@8;)
                    br 4 (;@4;)
                  end
                  local.get 3
                  i32.load offset=16
                  local.set 116
                  i32.const -1
                  local.set 117
                  local.get 116
                  local.get 117
                  i32.add
                  local.set 118
                  local.get 3
                  local.get 118
                  i32.store offset=16
                end
              end
              br 0 (;@5;)
            end
          end
        end
        br 1 (;@1;)
      end
    end
    local.get 3
    i32.load offset=28
    local.set 119
    local.get 119
    i32.load offset=30012
    local.set 120
    i32.const 1
    local.set 121
    local.get 120
    local.get 121
    i32.add
    local.set 122
    local.get 119
    local.get 122
    i32.store offset=30012
    i32.const 32
    local.set 123
    local.get 3
    local.get 123
    i32.add
    local.set 124
    local.get 124
    global.set 0
    return)
  (func $done (type 0) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 4
    local.get 4
    i32.load offset=30012
    local.set 5
    local.get 3
    i32.load offset=12
    local.set 6
    local.get 6
    i32.load offset=30008
    local.set 7
    local.get 5
    local.set 8
    local.get 7
    local.set 9
    local.get 8
    local.get 9
    i32.ge_u
    local.set 10
    i32.const 1
    local.set 11
    local.get 10
    local.get 11
    i32.and
    local.set 12
    local.get 12
    return)
  (func $_start (type 5)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 30144
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    local.get 2
    global.set 0
    i32.const 128
    local.set 3
    local.get 2
    local.get 3
    i32.add
    local.set 4
    local.get 4
    local.set 5
    i32.const 107
    local.set 6
    i32.const 16
    local.set 7
    local.get 2
    local.get 7
    i32.add
    local.set 8
    local.get 8
    local.set 9
    i32.const 1024
    local.set 10
    i32.const 107
    local.set 11
    local.get 9
    local.get 10
    local.get 11
    call $memcpy
    drop
    local.get 2
    local.get 6
    i32.store offset=12
    local.get 5
    local.get 9
    local.get 6
    call $init_vm
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 0
        local.set 12
        i32.const 128
        local.set 13
        local.get 2
        local.get 13
        i32.add
        local.set 14
        local.get 14
        local.set 15
        local.get 15
        call $done
        local.set 16
        local.get 16
        local.set 17
        local.get 12
        local.set 18
        local.get 17
        local.get 18
        i32.ne
        local.set 19
        i32.const -1
        local.set 20
        local.get 19
        local.get 20
        i32.xor
        local.set 21
        i32.const 1
        local.set 22
        local.get 21
        local.get 22
        i32.and
        local.set 23
        local.get 23
        i32.eqz
        br_if 1 (;@1;)
        i32.const 128
        local.set 24
        local.get 2
        local.get 24
        i32.add
        local.set 25
        local.get 25
        local.set 26
        local.get 26
        call $step
        br 0 (;@2;)
      end
    end
    i32.const 30144
    local.set 27
    local.get 2
    local.get 27
    i32.add
    local.set 28
    local.get 28
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66672))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.\00"))
