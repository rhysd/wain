(module
  (type (;0;) (func (param i32) (result i32)))
  (type (;1;) (func (param f64)))
  (type (;2;) (func (param f64 f64) (result i32)))
  (type (;3;) (func (param f64 f64 f64 f64)))
  (type (;4;) (func))
  (import "env" "putchar" (func $putchar (type 0)))
  (func $print_density (type 1) (param f64)
    (local i32 i32 i32 f64 f64 i32 i32 i32 i32 f64 f64 i32 i32 i32 i32 f64 f64 i32 i32 i32 i32 i32 i32 i32)
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
    f64.const 0x1p+3 (;=8;)
    local.set 4
    local.get 3
    local.get 0
    f64.store offset=8
    local.get 3
    f64.load offset=8
    local.set 5
    local.get 5
    local.get 4
    f64.gt
    local.set 6
    i32.const 1
    local.set 7
    local.get 6
    local.get 7
    i32.and
    local.set 8
    block  ;; label = @1
      block  ;; label = @2
        local.get 8
        i32.eqz
        br_if 0 (;@2;)
        i32.const 32
        local.set 9
        local.get 9
        call $putchar
        drop
        br 1 (;@1;)
      end
      f64.const 0x1p+2 (;=4;)
      local.set 10
      local.get 3
      f64.load offset=8
      local.set 11
      local.get 11
      local.get 10
      f64.gt
      local.set 12
      i32.const 1
      local.set 13
      local.get 12
      local.get 13
      i32.and
      local.set 14
      block  ;; label = @2
        block  ;; label = @3
          local.get 14
          i32.eqz
          br_if 0 (;@3;)
          i32.const 46
          local.set 15
          local.get 15
          call $putchar
          drop
          br 1 (;@2;)
        end
        f64.const 0x1p+1 (;=2;)
        local.set 16
        local.get 3
        f64.load offset=8
        local.set 17
        local.get 17
        local.get 16
        f64.gt
        local.set 18
        i32.const 1
        local.set 19
        local.get 18
        local.get 19
        i32.and
        local.set 20
        block  ;; label = @3
          block  ;; label = @4
            local.get 20
            i32.eqz
            br_if 0 (;@4;)
            i32.const 42
            local.set 21
            local.get 21
            call $putchar
            drop
            br 1 (;@3;)
          end
          i32.const 43
          local.set 22
          local.get 22
          call $putchar
          drop
        end
      end
    end
    i32.const 16
    local.set 23
    local.get 3
    local.get 23
    i32.add
    local.set 24
    local.get 24
    global.set 0
    return)
  (func $mandelconverge (type 2) (param f64 f64) (result i32)
    (local i32 i32 i32 i32 f64 f64 i32 i32 i32 i32 i32 i32 i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 i32 i32 i32 i32)
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
    local.get 4
    local.get 0
    f64.store offset=56
    local.get 4
    local.get 1
    f64.store offset=48
    local.get 4
    f64.load offset=56
    local.set 6
    local.get 4
    local.get 6
    f64.store offset=40
    local.get 4
    f64.load offset=48
    local.set 7
    local.get 4
    local.get 7
    f64.store offset=32
    local.get 4
    local.get 5
    i32.store offset=28
    loop  ;; label = @1
      i32.const 0
      local.set 8
      i32.const 255
      local.set 9
      local.get 4
      i32.load offset=28
      local.set 10
      local.get 10
      local.set 11
      local.get 9
      local.set 12
      local.get 11
      local.get 12
      i32.le_u
      local.set 13
      i32.const 1
      local.set 14
      local.get 13
      local.get 14
      i32.and
      local.set 15
      local.get 8
      local.set 16
      block  ;; label = @2
        local.get 15
        i32.eqz
        br_if 0 (;@2;)
        f64.const 0x1p+2 (;=4;)
        local.set 17
        local.get 4
        f64.load offset=56
        local.set 18
        local.get 4
        f64.load offset=56
        local.set 19
        local.get 18
        local.get 19
        f64.mul
        local.set 20
        local.get 4
        f64.load offset=48
        local.set 21
        local.get 4
        f64.load offset=48
        local.set 22
        local.get 21
        local.get 22
        f64.mul
        local.set 23
        local.get 20
        local.get 23
        f64.add
        local.set 24
        local.get 24
        local.get 17
        f64.lt
        local.set 25
        local.get 25
        local.set 16
      end
      local.get 16
      local.set 26
      i32.const 1
      local.set 27
      local.get 26
      local.get 27
      i32.and
      local.set 28
      block  ;; label = @2
        local.get 28
        i32.eqz
        br_if 0 (;@2;)
        f64.const 0x1p+1 (;=2;)
        local.set 29
        local.get 4
        f64.load offset=56
        local.set 30
        local.get 4
        f64.load offset=56
        local.set 31
        local.get 30
        local.get 31
        f64.mul
        local.set 32
        local.get 4
        f64.load offset=48
        local.set 33
        local.get 4
        f64.load offset=48
        local.set 34
        local.get 33
        local.get 34
        f64.mul
        local.set 35
        local.get 32
        local.get 35
        f64.sub
        local.set 36
        local.get 4
        f64.load offset=40
        local.set 37
        local.get 36
        local.get 37
        f64.add
        local.set 38
        local.get 4
        local.get 38
        f64.store offset=16
        local.get 4
        f64.load offset=56
        local.set 39
        local.get 29
        local.get 39
        f64.mul
        local.set 40
        local.get 4
        f64.load offset=48
        local.set 41
        local.get 40
        local.get 41
        f64.mul
        local.set 42
        local.get 4
        f64.load offset=32
        local.set 43
        local.get 42
        local.get 43
        f64.add
        local.set 44
        local.get 4
        local.get 44
        f64.store offset=8
        local.get 4
        f64.load offset=16
        local.set 45
        local.get 4
        local.get 45
        f64.store offset=56
        local.get 4
        f64.load offset=8
        local.set 46
        local.get 4
        local.get 46
        f64.store offset=48
        local.get 4
        i32.load offset=28
        local.set 47
        i32.const 1
        local.set 48
        local.get 47
        local.get 48
        i32.add
        local.set 49
        local.get 4
        local.get 49
        i32.store offset=28
        br 1 (;@1;)
      end
    end
    local.get 4
    i32.load offset=28
    local.set 50
    local.get 50
    return)
  (func $mandel (type 3) (param f64 f64 f64 f64)
    (local i32 i32 i32 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 f64 i32 i32 i32 f64 f64 f64 i32 i32 i32 f64 f64 i32 f64 f64 f64 f64 i32 f64 f64 f64 i32 i32)
    global.get 0
    local.set 4
    i32.const 96
    local.set 5
    local.get 4
    local.get 5
    i32.sub
    local.set 6
    local.get 6
    global.set 0
    f64.const 0x1.4p+5 (;=40;)
    local.set 7
    f64.const 0x1.38p+6 (;=78;)
    local.set 8
    local.get 6
    local.get 0
    f64.store offset=88
    local.get 6
    local.get 1
    f64.store offset=80
    local.get 6
    local.get 2
    f64.store offset=72
    local.get 6
    local.get 3
    f64.store offset=64
    local.get 6
    f64.load offset=88
    local.set 9
    local.get 6
    local.get 9
    f64.store offset=56
    local.get 6
    f64.load offset=88
    local.set 10
    local.get 6
    f64.load offset=72
    local.set 11
    local.get 11
    local.get 8
    f64.mul
    local.set 12
    local.get 10
    local.get 12
    f64.add
    local.set 13
    local.get 6
    local.get 13
    f64.store offset=48
    local.get 6
    f64.load offset=72
    local.set 14
    local.get 6
    local.get 14
    f64.store offset=40
    local.get 6
    f64.load offset=80
    local.set 15
    local.get 6
    local.get 15
    f64.store offset=32
    local.get 6
    f64.load offset=80
    local.set 16
    local.get 6
    f64.load offset=64
    local.set 17
    local.get 17
    local.get 7
    f64.mul
    local.set 18
    local.get 16
    local.get 18
    f64.add
    local.set 19
    local.get 6
    local.get 19
    f64.store offset=24
    local.get 6
    f64.load offset=64
    local.set 20
    local.get 6
    local.get 20
    f64.store offset=16
    local.get 6
    f64.load offset=32
    local.set 21
    local.get 6
    local.get 21
    f64.store offset=8
    block  ;; label = @1
      loop  ;; label = @2
        local.get 6
        f64.load offset=8
        local.set 22
        local.get 6
        f64.load offset=24
        local.set 23
        local.get 22
        local.get 23
        f64.lt
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
        local.get 6
        f64.load offset=56
        local.set 27
        local.get 6
        local.get 27
        f64.store
        block  ;; label = @3
          loop  ;; label = @4
            local.get 6
            f64.load
            local.set 28
            local.get 6
            f64.load offset=48
            local.set 29
            local.get 28
            local.get 29
            f64.lt
            local.set 30
            i32.const 1
            local.set 31
            local.get 30
            local.get 31
            i32.and
            local.set 32
            local.get 32
            i32.eqz
            br_if 1 (;@3;)
            local.get 6
            f64.load
            local.set 33
            local.get 6
            f64.load offset=8
            local.set 34
            local.get 33
            local.get 34
            call $mandelconverge
            local.set 35
            local.get 35
            f64.convert_i32_u
            local.set 36
            local.get 36
            call $print_density
            local.get 6
            f64.load offset=40
            local.set 37
            local.get 6
            f64.load
            local.set 38
            local.get 38
            local.get 37
            f64.add
            local.set 39
            local.get 6
            local.get 39
            f64.store
            br 0 (;@4;)
          end
        end
        i32.const 10
        local.set 40
        local.get 40
        call $putchar
        drop
        local.get 6
        f64.load offset=16
        local.set 41
        local.get 6
        f64.load offset=8
        local.set 42
        local.get 42
        local.get 41
        f64.add
        local.set 43
        local.get 6
        local.get 43
        f64.store offset=8
        br 0 (;@2;)
      end
    end
    i32.const 96
    local.set 44
    local.get 6
    local.get 44
    i32.add
    local.set 45
    local.get 45
    global.set 0
    return)
  (func $_start (type 4)
    (local f64 f64 f64 f64)
    f64.const -0x1.2666666666666p+1 (;=-2.3;)
    local.set 0
    f64.const -0x1.4cccccccccccdp+0 (;=-1.3;)
    local.set 1
    f64.const 0x1.999999999999ap-5 (;=0.05;)
    local.set 2
    f64.const 0x1.1eb851eb851ecp-4 (;=0.07;)
    local.set 3
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $mandel
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
