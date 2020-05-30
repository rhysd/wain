(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32 i32)))
  (type (;3;) (func (result i32)))
  (type (;4;) (func (result f64)))
  (type (;5;) (func (param f64)))
  (type (;6;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $init_genrand (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    i32.const 1
    local.set 4
    local.get 3
    local.get 0
    i32.store offset=12
    local.get 3
    i32.load offset=12
    local.set 5
    i32.const 0
    local.set 6
    local.get 6
    local.get 5
    i32.store offset=1120
    i32.const 0
    local.set 7
    local.get 7
    local.get 4
    i32.store offset=1108
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 624
        local.set 8
        i32.const 0
        local.set 9
        local.get 9
        i32.load offset=1108
        local.set 10
        local.get 10
        local.set 11
        local.get 8
        local.set 12
        local.get 11
        local.get 12
        i32.lt_s
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
        i32.const 1120
        local.set 16
        i32.const 0
        local.set 17
        local.get 17
        i32.load offset=1108
        local.set 18
        i32.const 1
        local.set 19
        local.get 18
        local.get 19
        i32.sub
        local.set 20
        i32.const 2
        local.set 21
        local.get 20
        local.get 21
        i32.shl
        local.set 22
        local.get 16
        local.get 22
        i32.add
        local.set 23
        local.get 23
        i32.load
        local.set 24
        i32.const 0
        local.set 25
        local.get 25
        i32.load offset=1108
        local.set 26
        i32.const 1
        local.set 27
        local.get 26
        local.get 27
        i32.sub
        local.set 28
        i32.const 2
        local.set 29
        local.get 28
        local.get 29
        i32.shl
        local.set 30
        local.get 16
        local.get 30
        i32.add
        local.set 31
        local.get 31
        i32.load
        local.set 32
        i32.const 30
        local.set 33
        local.get 32
        local.get 33
        i32.shr_u
        local.set 34
        local.get 24
        local.get 34
        i32.xor
        local.set 35
        i32.const 1812433253
        local.set 36
        local.get 35
        local.get 36
        i32.mul
        local.set 37
        i32.const 0
        local.set 38
        local.get 38
        i32.load offset=1108
        local.set 39
        local.get 37
        local.get 39
        i32.add
        local.set 40
        i32.const 0
        local.set 41
        local.get 41
        i32.load offset=1108
        local.set 42
        i32.const 2
        local.set 43
        local.get 42
        local.get 43
        i32.shl
        local.set 44
        local.get 16
        local.get 44
        i32.add
        local.set 45
        local.get 45
        local.get 40
        i32.store
        i32.const 0
        local.set 46
        local.get 46
        i32.load offset=1108
        local.set 47
        i32.const 2
        local.set 48
        local.get 47
        local.get 48
        i32.shl
        local.set 49
        local.get 16
        local.get 49
        i32.add
        local.set 50
        local.get 50
        i32.load
        local.set 51
        local.get 50
        local.get 51
        i32.store
        i32.const 0
        local.set 52
        local.get 52
        i32.load offset=1108
        local.set 53
        i32.const 1
        local.set 54
        local.get 53
        local.get 54
        i32.add
        local.set 55
        i32.const 0
        local.set 56
        local.get 56
        local.get 55
        i32.store offset=1108
        br 0 (;@2;)
      end
    end
    return)
  (func $init_by_array (type 2) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 624
    local.set 5
    i32.const 0
    local.set 6
    i32.const 1
    local.set 7
    i32.const 19650218
    local.set 8
    local.get 4
    local.get 0
    i32.store offset=28
    local.get 4
    local.get 1
    i32.store offset=24
    local.get 8
    call $init_genrand
    local.get 4
    local.get 7
    i32.store offset=20
    local.get 4
    local.get 6
    i32.store offset=16
    local.get 4
    i32.load offset=24
    local.set 9
    local.get 5
    local.set 10
    local.get 9
    local.set 11
    local.get 10
    local.get 11
    i32.gt_s
    local.set 12
    i32.const 1
    local.set 13
    local.get 12
    local.get 13
    i32.and
    local.set 14
    block  ;; label = @1
      block  ;; label = @2
        local.get 14
        i32.eqz
        br_if 0 (;@2;)
        i32.const 624
        local.set 15
        local.get 15
        local.set 16
        br 1 (;@1;)
      end
      local.get 4
      i32.load offset=24
      local.set 17
      local.get 17
      local.set 16
    end
    local.get 16
    local.set 18
    local.get 4
    local.get 18
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        local.get 4
        i32.load offset=12
        local.set 19
        local.get 19
        i32.eqz
        br_if 1 (;@1;)
        i32.const 624
        local.set 20
        i32.const 1120
        local.set 21
        local.get 4
        i32.load offset=20
        local.set 22
        i32.const 2
        local.set 23
        local.get 22
        local.get 23
        i32.shl
        local.set 24
        local.get 21
        local.get 24
        i32.add
        local.set 25
        local.get 25
        i32.load
        local.set 26
        local.get 4
        i32.load offset=20
        local.set 27
        i32.const 1
        local.set 28
        local.get 27
        local.get 28
        i32.sub
        local.set 29
        i32.const 2
        local.set 30
        local.get 29
        local.get 30
        i32.shl
        local.set 31
        local.get 21
        local.get 31
        i32.add
        local.set 32
        local.get 32
        i32.load
        local.set 33
        local.get 4
        i32.load offset=20
        local.set 34
        i32.const 1
        local.set 35
        local.get 34
        local.get 35
        i32.sub
        local.set 36
        i32.const 2
        local.set 37
        local.get 36
        local.get 37
        i32.shl
        local.set 38
        local.get 21
        local.get 38
        i32.add
        local.set 39
        local.get 39
        i32.load
        local.set 40
        i32.const 30
        local.set 41
        local.get 40
        local.get 41
        i32.shr_u
        local.set 42
        local.get 33
        local.get 42
        i32.xor
        local.set 43
        i32.const 1664525
        local.set 44
        local.get 43
        local.get 44
        i32.mul
        local.set 45
        local.get 26
        local.get 45
        i32.xor
        local.set 46
        local.get 4
        i32.load offset=28
        local.set 47
        local.get 4
        i32.load offset=16
        local.set 48
        i32.const 2
        local.set 49
        local.get 48
        local.get 49
        i32.shl
        local.set 50
        local.get 47
        local.get 50
        i32.add
        local.set 51
        local.get 51
        i32.load
        local.set 52
        local.get 46
        local.get 52
        i32.add
        local.set 53
        local.get 4
        i32.load offset=16
        local.set 54
        local.get 53
        local.get 54
        i32.add
        local.set 55
        local.get 4
        i32.load offset=20
        local.set 56
        i32.const 2
        local.set 57
        local.get 56
        local.get 57
        i32.shl
        local.set 58
        local.get 21
        local.get 58
        i32.add
        local.set 59
        local.get 59
        local.get 55
        i32.store
        local.get 4
        i32.load offset=20
        local.set 60
        i32.const 2
        local.set 61
        local.get 60
        local.get 61
        i32.shl
        local.set 62
        local.get 21
        local.get 62
        i32.add
        local.set 63
        local.get 63
        i32.load
        local.set 64
        local.get 63
        local.get 64
        i32.store
        local.get 4
        i32.load offset=20
        local.set 65
        i32.const 1
        local.set 66
        local.get 65
        local.get 66
        i32.add
        local.set 67
        local.get 4
        local.get 67
        i32.store offset=20
        local.get 4
        i32.load offset=16
        local.set 68
        i32.const 1
        local.set 69
        local.get 68
        local.get 69
        i32.add
        local.set 70
        local.get 4
        local.get 70
        i32.store offset=16
        local.get 4
        i32.load offset=20
        local.set 71
        local.get 71
        local.set 72
        local.get 20
        local.set 73
        local.get 72
        local.get 73
        i32.ge_s
        local.set 74
        i32.const 1
        local.set 75
        local.get 74
        local.get 75
        i32.and
        local.set 76
        block  ;; label = @3
          local.get 76
          i32.eqz
          br_if 0 (;@3;)
          i32.const 1
          local.set 77
          i32.const 0
          local.set 78
          local.get 78
          i32.load offset=3612
          local.set 79
          i32.const 0
          local.set 80
          local.get 80
          local.get 79
          i32.store offset=1120
          local.get 4
          local.get 77
          i32.store offset=20
        end
        local.get 4
        i32.load offset=16
        local.set 81
        local.get 4
        i32.load offset=24
        local.set 82
        local.get 81
        local.set 83
        local.get 82
        local.set 84
        local.get 83
        local.get 84
        i32.ge_s
        local.set 85
        i32.const 1
        local.set 86
        local.get 85
        local.get 86
        i32.and
        local.set 87
        block  ;; label = @3
          local.get 87
          i32.eqz
          br_if 0 (;@3;)
          i32.const 0
          local.set 88
          local.get 4
          local.get 88
          i32.store offset=16
        end
        local.get 4
        i32.load offset=12
        local.set 89
        i32.const -1
        local.set 90
        local.get 89
        local.get 90
        i32.add
        local.set 91
        local.get 4
        local.get 91
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const 623
    local.set 92
    local.get 4
    local.get 92
    i32.store offset=12
    block  ;; label = @1
      loop  ;; label = @2
        local.get 4
        i32.load offset=12
        local.set 93
        local.get 93
        i32.eqz
        br_if 1 (;@1;)
        i32.const 624
        local.set 94
        i32.const 1120
        local.set 95
        local.get 4
        i32.load offset=20
        local.set 96
        i32.const 2
        local.set 97
        local.get 96
        local.get 97
        i32.shl
        local.set 98
        local.get 95
        local.get 98
        i32.add
        local.set 99
        local.get 99
        i32.load
        local.set 100
        local.get 4
        i32.load offset=20
        local.set 101
        i32.const 1
        local.set 102
        local.get 101
        local.get 102
        i32.sub
        local.set 103
        i32.const 2
        local.set 104
        local.get 103
        local.get 104
        i32.shl
        local.set 105
        local.get 95
        local.get 105
        i32.add
        local.set 106
        local.get 106
        i32.load
        local.set 107
        local.get 4
        i32.load offset=20
        local.set 108
        i32.const 1
        local.set 109
        local.get 108
        local.get 109
        i32.sub
        local.set 110
        i32.const 2
        local.set 111
        local.get 110
        local.get 111
        i32.shl
        local.set 112
        local.get 95
        local.get 112
        i32.add
        local.set 113
        local.get 113
        i32.load
        local.set 114
        i32.const 30
        local.set 115
        local.get 114
        local.get 115
        i32.shr_u
        local.set 116
        local.get 107
        local.get 116
        i32.xor
        local.set 117
        i32.const 1566083941
        local.set 118
        local.get 117
        local.get 118
        i32.mul
        local.set 119
        local.get 100
        local.get 119
        i32.xor
        local.set 120
        local.get 4
        i32.load offset=20
        local.set 121
        local.get 120
        local.get 121
        i32.sub
        local.set 122
        local.get 4
        i32.load offset=20
        local.set 123
        i32.const 2
        local.set 124
        local.get 123
        local.get 124
        i32.shl
        local.set 125
        local.get 95
        local.get 125
        i32.add
        local.set 126
        local.get 126
        local.get 122
        i32.store
        local.get 4
        i32.load offset=20
        local.set 127
        i32.const 2
        local.set 128
        local.get 127
        local.get 128
        i32.shl
        local.set 129
        local.get 95
        local.get 129
        i32.add
        local.set 130
        local.get 130
        i32.load
        local.set 131
        local.get 130
        local.get 131
        i32.store
        local.get 4
        i32.load offset=20
        local.set 132
        i32.const 1
        local.set 133
        local.get 132
        local.get 133
        i32.add
        local.set 134
        local.get 4
        local.get 134
        i32.store offset=20
        local.get 4
        i32.load offset=20
        local.set 135
        local.get 135
        local.set 136
        local.get 94
        local.set 137
        local.get 136
        local.get 137
        i32.ge_s
        local.set 138
        i32.const 1
        local.set 139
        local.get 138
        local.get 139
        i32.and
        local.set 140
        block  ;; label = @3
          local.get 140
          i32.eqz
          br_if 0 (;@3;)
          i32.const 1
          local.set 141
          i32.const 0
          local.set 142
          local.get 142
          i32.load offset=3612
          local.set 143
          i32.const 0
          local.set 144
          local.get 144
          local.get 143
          i32.store offset=1120
          local.get 4
          local.get 141
          i32.store offset=20
        end
        local.get 4
        i32.load offset=12
        local.set 145
        i32.const -1
        local.set 146
        local.get 145
        local.get 146
        i32.add
        local.set 147
        local.get 4
        local.get 147
        i32.store offset=12
        br 0 (;@2;)
      end
    end
    i32.const -2147483648
    local.set 148
    i32.const 0
    local.set 149
    local.get 149
    local.get 148
    i32.store offset=1120
    i32.const 32
    local.set 150
    local.get 4
    local.get 150
    i32.add
    local.set 151
    local.get 151
    global.set 0
    return)
  (func $genrand_int32 (type 3) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
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
    i32.const 624
    local.set 3
    i32.const 0
    local.set 4
    local.get 4
    i32.load offset=1108
    local.set 5
    local.get 5
    local.set 6
    local.get 3
    local.set 7
    local.get 6
    local.get 7
    i32.ge_s
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
      i32.const 625
      local.set 11
      i32.const 0
      local.set 12
      local.get 12
      i32.load offset=1108
      local.set 13
      local.get 13
      local.set 14
      local.get 11
      local.set 15
      local.get 14
      local.get 15
      i32.eq
      local.set 16
      i32.const 1
      local.set 17
      local.get 16
      local.get 17
      i32.and
      local.set 18
      block  ;; label = @2
        local.get 18
        i32.eqz
        br_if 0 (;@2;)
        i32.const 5489
        local.set 19
        local.get 19
        call $init_genrand
      end
      i32.const 0
      local.set 20
      local.get 2
      local.get 20
      i32.store offset=8
      block  ;; label = @2
        loop  ;; label = @3
          i32.const 227
          local.set 21
          local.get 2
          i32.load offset=8
          local.set 22
          local.get 22
          local.set 23
          local.get 21
          local.set 24
          local.get 23
          local.get 24
          i32.lt_s
          local.set 25
          i32.const 1
          local.set 26
          local.get 25
          local.get 26
          i32.and
          local.set 27
          local.get 27
          i32.eqz
          br_if 1 (;@2;)
          i32.const 1120
          local.set 28
          i32.const 1112
          local.set 29
          local.get 2
          i32.load offset=8
          local.set 30
          i32.const 2
          local.set 31
          local.get 30
          local.get 31
          i32.shl
          local.set 32
          local.get 28
          local.get 32
          i32.add
          local.set 33
          local.get 33
          i32.load
          local.set 34
          i32.const -2147483648
          local.set 35
          local.get 34
          local.get 35
          i32.and
          local.set 36
          local.get 2
          i32.load offset=8
          local.set 37
          i32.const 1
          local.set 38
          local.get 37
          local.get 38
          i32.add
          local.set 39
          i32.const 2
          local.set 40
          local.get 39
          local.get 40
          i32.shl
          local.set 41
          local.get 28
          local.get 41
          i32.add
          local.set 42
          local.get 42
          i32.load
          local.set 43
          i32.const 2147483647
          local.set 44
          local.get 43
          local.get 44
          i32.and
          local.set 45
          local.get 36
          local.get 45
          i32.or
          local.set 46
          local.get 2
          local.get 46
          i32.store offset=12
          local.get 2
          i32.load offset=8
          local.set 47
          i32.const 397
          local.set 48
          local.get 47
          local.get 48
          i32.add
          local.set 49
          i32.const 2
          local.set 50
          local.get 49
          local.get 50
          i32.shl
          local.set 51
          local.get 28
          local.get 51
          i32.add
          local.set 52
          local.get 52
          i32.load
          local.set 53
          local.get 2
          i32.load offset=12
          local.set 54
          i32.const 1
          local.set 55
          local.get 54
          local.get 55
          i32.shr_u
          local.set 56
          local.get 53
          local.get 56
          i32.xor
          local.set 57
          local.get 2
          i32.load offset=12
          local.set 58
          i32.const 1
          local.set 59
          local.get 58
          local.get 59
          i32.and
          local.set 60
          i32.const 2
          local.set 61
          local.get 60
          local.get 61
          i32.shl
          local.set 62
          local.get 29
          local.get 62
          i32.add
          local.set 63
          local.get 63
          i32.load
          local.set 64
          local.get 57
          local.get 64
          i32.xor
          local.set 65
          local.get 2
          i32.load offset=8
          local.set 66
          i32.const 2
          local.set 67
          local.get 66
          local.get 67
          i32.shl
          local.set 68
          local.get 28
          local.get 68
          i32.add
          local.set 69
          local.get 69
          local.get 65
          i32.store
          local.get 2
          i32.load offset=8
          local.set 70
          i32.const 1
          local.set 71
          local.get 70
          local.get 71
          i32.add
          local.set 72
          local.get 2
          local.get 72
          i32.store offset=8
          br 0 (;@3;)
        end
      end
      block  ;; label = @2
        loop  ;; label = @3
          i32.const 623
          local.set 73
          local.get 2
          i32.load offset=8
          local.set 74
          local.get 74
          local.set 75
          local.get 73
          local.set 76
          local.get 75
          local.get 76
          i32.lt_s
          local.set 77
          i32.const 1
          local.set 78
          local.get 77
          local.get 78
          i32.and
          local.set 79
          local.get 79
          i32.eqz
          br_if 1 (;@2;)
          i32.const 1120
          local.set 80
          i32.const 1112
          local.set 81
          local.get 2
          i32.load offset=8
          local.set 82
          i32.const 2
          local.set 83
          local.get 82
          local.get 83
          i32.shl
          local.set 84
          local.get 80
          local.get 84
          i32.add
          local.set 85
          local.get 85
          i32.load
          local.set 86
          i32.const -2147483648
          local.set 87
          local.get 86
          local.get 87
          i32.and
          local.set 88
          local.get 2
          i32.load offset=8
          local.set 89
          i32.const 1
          local.set 90
          local.get 89
          local.get 90
          i32.add
          local.set 91
          i32.const 2
          local.set 92
          local.get 91
          local.get 92
          i32.shl
          local.set 93
          local.get 80
          local.get 93
          i32.add
          local.set 94
          local.get 94
          i32.load
          local.set 95
          i32.const 2147483647
          local.set 96
          local.get 95
          local.get 96
          i32.and
          local.set 97
          local.get 88
          local.get 97
          i32.or
          local.set 98
          local.get 2
          local.get 98
          i32.store offset=12
          local.get 2
          i32.load offset=8
          local.set 99
          i32.const -227
          local.set 100
          local.get 99
          local.get 100
          i32.add
          local.set 101
          i32.const 2
          local.set 102
          local.get 101
          local.get 102
          i32.shl
          local.set 103
          local.get 80
          local.get 103
          i32.add
          local.set 104
          local.get 104
          i32.load
          local.set 105
          local.get 2
          i32.load offset=12
          local.set 106
          i32.const 1
          local.set 107
          local.get 106
          local.get 107
          i32.shr_u
          local.set 108
          local.get 105
          local.get 108
          i32.xor
          local.set 109
          local.get 2
          i32.load offset=12
          local.set 110
          i32.const 1
          local.set 111
          local.get 110
          local.get 111
          i32.and
          local.set 112
          i32.const 2
          local.set 113
          local.get 112
          local.get 113
          i32.shl
          local.set 114
          local.get 81
          local.get 114
          i32.add
          local.set 115
          local.get 115
          i32.load
          local.set 116
          local.get 109
          local.get 116
          i32.xor
          local.set 117
          local.get 2
          i32.load offset=8
          local.set 118
          i32.const 2
          local.set 119
          local.get 118
          local.get 119
          i32.shl
          local.set 120
          local.get 80
          local.get 120
          i32.add
          local.set 121
          local.get 121
          local.get 117
          i32.store
          local.get 2
          i32.load offset=8
          local.set 122
          i32.const 1
          local.set 123
          local.get 122
          local.get 123
          i32.add
          local.set 124
          local.get 2
          local.get 124
          i32.store offset=8
          br 0 (;@3;)
        end
      end
      i32.const 0
      local.set 125
      i32.const 1112
      local.set 126
      i32.const 0
      local.set 127
      local.get 127
      i32.load offset=3612
      local.set 128
      i32.const -2147483648
      local.set 129
      local.get 128
      local.get 129
      i32.and
      local.set 130
      i32.const 0
      local.set 131
      local.get 131
      i32.load offset=1120
      local.set 132
      i32.const 2147483647
      local.set 133
      local.get 132
      local.get 133
      i32.and
      local.set 134
      local.get 130
      local.get 134
      i32.or
      local.set 135
      local.get 2
      local.get 135
      i32.store offset=12
      i32.const 0
      local.set 136
      local.get 136
      i32.load offset=2704
      local.set 137
      local.get 2
      i32.load offset=12
      local.set 138
      i32.const 1
      local.set 139
      local.get 138
      local.get 139
      i32.shr_u
      local.set 140
      local.get 137
      local.get 140
      i32.xor
      local.set 141
      local.get 2
      i32.load offset=12
      local.set 142
      i32.const 1
      local.set 143
      local.get 142
      local.get 143
      i32.and
      local.set 144
      i32.const 2
      local.set 145
      local.get 144
      local.get 145
      i32.shl
      local.set 146
      local.get 126
      local.get 146
      i32.add
      local.set 147
      local.get 147
      i32.load
      local.set 148
      local.get 141
      local.get 148
      i32.xor
      local.set 149
      i32.const 0
      local.set 150
      local.get 150
      local.get 149
      i32.store offset=3612
      i32.const 0
      local.set 151
      local.get 151
      local.get 125
      i32.store offset=1108
    end
    i32.const 1120
    local.set 152
    i32.const 0
    local.set 153
    local.get 153
    i32.load offset=1108
    local.set 154
    i32.const 1
    local.set 155
    local.get 154
    local.get 155
    i32.add
    local.set 156
    i32.const 0
    local.set 157
    local.get 157
    local.get 156
    i32.store offset=1108
    i32.const 2
    local.set 158
    local.get 154
    local.get 158
    i32.shl
    local.set 159
    local.get 152
    local.get 159
    i32.add
    local.set 160
    local.get 160
    i32.load
    local.set 161
    local.get 2
    local.get 161
    i32.store offset=12
    local.get 2
    i32.load offset=12
    local.set 162
    i32.const 11
    local.set 163
    local.get 162
    local.get 163
    i32.shr_u
    local.set 164
    local.get 2
    i32.load offset=12
    local.set 165
    local.get 165
    local.get 164
    i32.xor
    local.set 166
    local.get 2
    local.get 166
    i32.store offset=12
    local.get 2
    i32.load offset=12
    local.set 167
    i32.const 7
    local.set 168
    local.get 167
    local.get 168
    i32.shl
    local.set 169
    i32.const -1658038656
    local.set 170
    local.get 169
    local.get 170
    i32.and
    local.set 171
    local.get 2
    i32.load offset=12
    local.set 172
    local.get 172
    local.get 171
    i32.xor
    local.set 173
    local.get 2
    local.get 173
    i32.store offset=12
    local.get 2
    i32.load offset=12
    local.set 174
    i32.const 15
    local.set 175
    local.get 174
    local.get 175
    i32.shl
    local.set 176
    i32.const -272236544
    local.set 177
    local.get 176
    local.get 177
    i32.and
    local.set 178
    local.get 2
    i32.load offset=12
    local.set 179
    local.get 179
    local.get 178
    i32.xor
    local.set 180
    local.get 2
    local.get 180
    i32.store offset=12
    local.get 2
    i32.load offset=12
    local.set 181
    i32.const 18
    local.set 182
    local.get 181
    local.get 182
    i32.shr_u
    local.set 183
    local.get 2
    i32.load offset=12
    local.set 184
    local.get 184
    local.get 183
    i32.xor
    local.set 185
    local.get 2
    local.get 185
    i32.store offset=12
    local.get 2
    i32.load offset=12
    local.set 186
    i32.const 16
    local.set 187
    local.get 2
    local.get 187
    i32.add
    local.set 188
    local.get 188
    global.set 0
    local.get 186
    return)
  (func $genrand_real2 (type 4) (result f64)
    (local f64 i32 f64 f64)
    f64.const 0x1p-32 (;=2.32831e-10;)
    local.set 0
    call $genrand_int32
    local.set 1
    local.get 1
    f64.convert_i32_u
    local.set 2
    local.get 2
    local.get 0
    f64.mul
    local.set 3
    local.get 3
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
  (func $put_float (type 5) (param f64)
    (local i32 i32 i32 i32 f64 f64 i32 i32 i32 i32 f64 f64 i32 f64 i32 f64 f64 i32 f64 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 f64 f64 f64 i32 i32 i32 i32 i32 f64 f64 i32 i32 i32 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 i32 i32 i32 i32 i32 i32 f64 f64 i32 i32 i32)
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
        i32.const 10
        local.set 43
        i32.const 48
        local.set 44
        local.get 44
        call $putchar
        drop
        local.get 43
        call $putchar
        drop
        br 1 (;@1;)
      end
      block  ;; label = @2
        loop  ;; label = @3
          f64.const 0x1.ad7f29abcaf48p-24 (;=1e-07;)
          local.set 45
          local.get 3
          f64.load offset=8
          local.set 46
          local.get 46
          local.get 45
          f64.gt
          local.set 47
          i32.const 1
          local.set 48
          local.get 47
          local.get 48
          i32.and
          local.set 49
          local.get 49
          i32.eqz
          br_if 1 (;@2;)
          f64.const 0x1.4p+3 (;=10;)
          local.set 50
          local.get 3
          f64.load offset=8
          local.set 51
          local.get 51
          local.get 50
          f64.mul
          local.set 52
          local.get 3
          local.get 52
          f64.store offset=8
          local.get 3
          f64.load offset=8
          local.set 53
          local.get 53
          f64.abs
          local.set 54
          f64.const 0x1p+31 (;=2.14748e+09;)
          local.set 55
          local.get 54
          local.get 55
          f64.lt
          local.set 56
          local.get 56
          i32.eqz
          local.set 57
          block  ;; label = @4
            block  ;; label = @5
              local.get 57
              br_if 0 (;@5;)
              local.get 53
              i32.trunc_f64_s
              local.set 58
              local.get 58
              local.set 59
              br 1 (;@4;)
            end
            i32.const -2147483648
            local.set 60
            local.get 60
            local.set 59
          end
          local.get 59
          local.set 61
          i32.const 48
          local.set 62
          local.get 61
          local.get 62
          i32.add
          local.set 63
          local.get 63
          call $putchar
          drop
          local.get 3
          f64.load offset=8
          local.set 64
          local.get 3
          f64.load offset=8
          local.set 65
          local.get 65
          f64.abs
          local.set 66
          f64.const 0x1p+31 (;=2.14748e+09;)
          local.set 67
          local.get 66
          local.get 67
          f64.lt
          local.set 68
          local.get 68
          i32.eqz
          local.set 69
          block  ;; label = @4
            block  ;; label = @5
              local.get 69
              br_if 0 (;@5;)
              local.get 65
              i32.trunc_f64_s
              local.set 70
              local.get 70
              local.set 71
              br 1 (;@4;)
            end
            i32.const -2147483648
            local.set 72
            local.get 72
            local.set 71
          end
          local.get 71
          local.set 73
          local.get 73
          f64.convert_i32_s
          local.set 74
          local.get 64
          local.get 74
          f64.sub
          local.set 75
          local.get 3
          local.get 75
          f64.store offset=8
          br 0 (;@3;)
        end
      end
      i32.const 10
      local.set 76
      local.get 76
      call $putchar
      drop
    end
    i32.const 16
    local.set 77
    local.get 3
    local.get 77
    i32.add
    local.set 78
    local.get 78
    global.set 0
    return)
  (func $put_int (type 1) (param i32)
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
    i32.const 10
    local.set 15
    local.get 3
    i32.load offset=12
    local.set 16
    local.get 16
    call $print_uint
    local.get 15
    call $putchar
    drop
    i32.const 16
    local.set 17
    local.get 3
    local.get 17
    i32.add
    local.set 18
    local.get 18
    global.set 0
    return)
  (func $put_str (type 1) (param i32)
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
  (func $_start (type 6)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64 i64 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 i32 i32 i32 i32 i32)
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
    i32.const 1040
    local.set 4
    i32.const 16
    local.set 5
    local.get 2
    local.get 5
    i32.add
    local.set 6
    local.get 6
    local.set 7
    i32.const 4
    local.set 8
    i32.const 8
    local.set 9
    local.get 7
    local.get 9
    i32.add
    local.set 10
    i32.const 0
    local.set 11
    local.get 11
    i64.load offset=1032
    local.set 12
    local.get 10
    local.get 12
    i64.store
    local.get 11
    i64.load offset=1024
    local.set 13
    local.get 7
    local.get 13
    i64.store
    local.get 2
    local.get 8
    i32.store offset=12
    local.get 2
    i32.load offset=12
    local.set 14
    local.get 7
    local.get 14
    call $init_by_array
    local.get 4
    call $put_str
    local.get 2
    local.get 3
    i32.store offset=44
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 1000
        local.set 15
        local.get 2
        i32.load offset=44
        local.set 16
        local.get 16
        local.set 17
        local.get 15
        local.set 18
        local.get 17
        local.get 18
        i32.lt_s
        local.set 19
        i32.const 1
        local.set 20
        local.get 19
        local.get 20
        i32.and
        local.set 21
        local.get 21
        i32.eqz
        br_if 1 (;@1;)
        call $genrand_int32
        local.set 22
        local.get 22
        call $put_int
        local.get 2
        i32.load offset=44
        local.set 23
        i32.const 1
        local.set 24
        local.get 23
        local.get 24
        i32.add
        local.set 25
        local.get 2
        local.get 25
        i32.store offset=44
        br 0 (;@2;)
      end
    end
    i32.const 0
    local.set 26
    i32.const 1072
    local.set 27
    local.get 27
    call $put_str
    local.get 2
    local.get 26
    i32.store offset=44
    block  ;; label = @1
      loop  ;; label = @2
        i32.const 1000
        local.set 28
        local.get 2
        i32.load offset=44
        local.set 29
        local.get 29
        local.set 30
        local.get 28
        local.set 31
        local.get 30
        local.get 31
        i32.lt_s
        local.set 32
        i32.const 1
        local.set 33
        local.get 32
        local.get 33
        i32.and
        local.set 34
        local.get 34
        i32.eqz
        br_if 1 (;@1;)
        call $genrand_real2
        local.set 35
        local.get 35
        call $put_float
        local.get 2
        i32.load offset=44
        local.set 36
        i32.const 1
        local.set 37
        local.get 36
        local.get 37
        i32.add
        local.set 38
        local.get 2
        local.get 38
        i32.store offset=44
        br 0 (;@2;)
      end
    end
    i32.const 48
    local.set 39
    local.get 2
    local.get 39
    i32.add
    local.set 40
    local.get 40
    global.set 0
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 69152))
  (export "memory" (memory 0))
  (export "_start" (func $_start))
  (data (;0;) (i32.const 1024) "#\01\00\004\02\00\00E\03\00\00V\04\00\001000 outputs of genrand_int32()\00\0a1000 outputs of genrand_real2()\00")
  (data (;1;) (i32.const 1108) "q\02\00\00\00\00\00\00\df\b0\08\99"))
