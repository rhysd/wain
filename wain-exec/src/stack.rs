use crate::value::Value;
use std::convert::TryInto;
use std::f32;
use std::f64;
use wain_ast::ValType;

// Vec<Value> consumes too much space since its element size is always 64bits.
// To use space more efficiently, here use u32 for storing values as bytes.

pub struct ValueStack {
    values: Vec<u8>, // actual values per byte
    types: Vec<ValType>,
}

pub trait PushPop {
    fn pop(stack: &mut ValueStack) -> Self;
    fn push(stack: &mut ValueStack, v: Self);
}

impl PushPop for i32 {
    fn pop(stack: &mut ValueStack) -> Self {
        i32::from_le_bytes(stack.pop_4_bytes(ValType::I32))
    }
    fn push(stack: &mut ValueStack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::I32);
    }
}

impl PushPop for i64 {
    fn pop(stack: &mut ValueStack) -> Self {
        i64::from_le_bytes(stack.pop_8_bytes(ValType::I64))
    }
    fn push(stack: &mut ValueStack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::I64);
    }
}

impl PushPop for f32 {
    fn pop(stack: &mut ValueStack) -> Self {
        f32::from_le_bytes(stack.pop_4_bytes(ValType::F32))
    }
    fn push(stack: &mut ValueStack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::F32);
    }
}

impl PushPop for f64 {
    fn pop(stack: &mut ValueStack) -> Self {
        f64::from_le_bytes(stack.pop_8_bytes(ValType::F64))
    }
    fn push(stack: &mut ValueStack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::F64);
    }
}

impl PushPop for Value {
    fn pop(stack: &mut ValueStack) -> Self {
        assert!(!stack.types.is_empty());
        match stack.types[stack.types.len() - 1] {
            ValType::I32 => Value::I32(PushPop::pop(stack)),
            ValType::I64 => Value::I64(PushPop::pop(stack)),
            ValType::F32 => Value::F32(PushPop::pop(stack)),
            ValType::F64 => Value::F64(PushPop::pop(stack)),
        }
    }
    fn push(stack: &mut ValueStack, v: Self) {
        match v {
            Value::I32(i) => PushPop::push(stack, i),
            Value::I64(i) => PushPop::push(stack, i),
            Value::F32(f) => PushPop::push(stack, f),
            Value::F64(f) => PushPop::push(stack, f),
        }
    }
}

pub trait ReadWrite {
    fn read(stack: &ValueStack, addr: usize) -> Self;
    fn write(stack: &mut ValueStack, addr: usize, v: Self);
}

impl ReadWrite for i32 {
    fn read(stack: &ValueStack, addr: usize) -> Self {
        i32::from_le_bytes(stack.read_4_bytes(addr))
    }
    fn write(stack: &mut ValueStack, addr: usize, v: Self) {
        stack.write_bytes(addr, &v.to_le_bytes());
    }
}

impl ReadWrite for i64 {
    fn read(stack: &ValueStack, addr: usize) -> Self {
        i64::from_le_bytes(stack.read_8_bytes(addr))
    }
    fn write(stack: &mut ValueStack, addr: usize, v: Self) {
        stack.write_bytes(addr, &v.to_le_bytes());
    }
}

impl ReadWrite for f32 {
    fn read(stack: &ValueStack, addr: usize) -> Self {
        f32::from_le_bytes(stack.read_4_bytes(addr))
    }
    fn write(stack: &mut ValueStack, addr: usize, v: Self) {
        stack.write_bytes(addr, &v.to_le_bytes());
    }
}

impl ReadWrite for f64 {
    fn read(stack: &ValueStack, addr: usize) -> Self {
        f64::from_le_bytes(stack.read_8_bytes(addr))
    }
    fn write(stack: &mut ValueStack, addr: usize, v: Self) {
        stack.write_bytes(addr, &v.to_le_bytes());
    }
}

impl ValueStack {
    pub fn new() -> Self {
        ValueStack {
            values: vec![],
            types: vec![],
        }
    }

    // Note: Here I don't use std::slice::from_raw since its unsafe

    fn push_bytes(&mut self, bytes: &[u8], ty: ValType) {
        self.types.push(ty);
        self.values.extend_from_slice(bytes);
    }

    pub fn push<V: PushPop>(&mut self, v: V) {
        PushPop::push(self, v);
    }

    fn pop_4_bytes(&mut self, ty: ValType) -> [u8; 4] {
        assert_eq!(self.types.pop().expect("pop 4 bytes"), ty);
        let len = self.values.len();
        let b = self.values[len - 4..]
            .try_into()
            .expect("4 bytes for i32 value");
        self.values.truncate(len - 4);
        b
    }

    fn pop_8_bytes(&mut self, ty: ValType) -> [u8; 8] {
        assert_eq!(self.types.pop().expect("pop 8 bytes"), ty);
        let len = self.values.len();
        let b = self.values[len - 8..]
            .try_into()
            .expect("4 bytes for 64bit value");
        self.values.truncate(len - 8);
        b
    }

    pub fn pop<V: PushPop>(&mut self) -> V {
        PushPop::pop(self)
    }

    fn read_4_bytes(&self, addr: usize) -> [u8; 4] {
        self.values[addr..addr + 4]
            .try_into()
            .expect("read 4 bytes")
    }

    fn read_8_bytes(&self, addr: usize) -> [u8; 8] {
        self.values[addr..addr + 8]
            .try_into()
            .expect("read 8 bytes")
    }

    fn write_bytes(&mut self, addr: usize, bytes: &[u8]) {
        for i in 0..bytes.len() {
            self.values[addr + i] = bytes[i];
        }
    }

    pub fn write<V: ReadWrite>(&mut self, addr: usize, v: V) {
        ReadWrite::write(self, addr, v)
    }

    pub fn read<V: ReadWrite>(&self, addr: usize) -> V {
        ReadWrite::read(self, addr)
    }

    pub fn top_addr(&self) -> usize {
        self.values.len()
    }
}

pub struct CallFrame {
    base_addr: usize,
    local_offsets: Box<[usize]>,
}

impl CallFrame {
    fn new(stack: &ValueStack, params: &[ValType], locals: &[ValType]) -> Self {
        let mut offsets = Vec::with_capacity(params.len() + locals.len());
        let mut idx = 0;
        for p in params {
            offsets.push(idx);
            idx += p.bytes();
        }
        for l in locals {
            offsets.push(idx);
            idx += l.bytes();
        }
        Self {
            base_addr: stack.top_addr(),
            local_offsets: offsets.into_boxed_slice(),
        }
    }

    fn local_addr(&self, idx: u32) -> usize {
        assert!((idx as usize) < self.local_offsets.len());
        self.base_addr + self.local_offsets[idx as usize]
    }
}

pub struct CallFrames {
    frames: Vec<CallFrame>,
}

impl CallFrames {
    pub fn new() -> Self {
        Self { frames: vec![] }
    }

    pub fn push(&mut self, stack: &ValueStack, params: &[ValType], locals: &[ValType]) {
        self.frames.push(CallFrame::new(stack, params, locals));
    }

    pub fn pop(&mut self) {
        self.frames.pop().expect("pop call frame");
    }

    pub fn frame(&self) -> &CallFrame {
        assert!(!self.frames.is_empty());
        &self.frames[self.frames.len() - 1]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn i32_value() {
        let mut s = ValueStack::new();
        s.push(0i32);
        s.push(1i32);
        s.push(-1i32);
        s.push(i32::max_value());
        s.push(i32::min_value());

        assert_eq!(s.pop::<i32>(), i32::min_value());
        assert_eq!(s.pop::<i32>(), i32::max_value());
        assert_eq!(s.pop::<i32>(), -1);
        assert_eq!(s.pop::<i32>(), 1);
        assert_eq!(s.pop::<i32>(), 0);
    }

    #[test]
    fn i64_value() {
        let mut s = ValueStack::new();
        s.push(0i64);
        s.push(1i64);
        s.push(-1i64);
        s.push(i32::max_value() as i64);
        s.push(i32::min_value() as i64);
        s.push(i64::max_value());
        s.push(i64::min_value());

        assert_eq!(s.pop::<i64>(), i64::min_value());
        assert_eq!(s.pop::<i64>(), i64::max_value());
        assert_eq!(s.pop::<i64>(), i32::min_value() as i64);
        assert_eq!(s.pop::<i64>(), i32::max_value() as i64);
        assert_eq!(s.pop::<i64>(), -1);
        assert_eq!(s.pop::<i64>(), 1);
        assert_eq!(s.pop::<i64>(), 0);
    }

    #[test]
    fn f32_value() {
        let mut s = ValueStack::new();
        s.push(0.0f32);
        s.push(3.14f32);
        s.push(-1.0f32);
        s.push(f32::INFINITY);
        s.push(f32::NEG_INFINITY);
        s.push(f32::NAN);

        assert!(s.pop::<f32>().is_nan());
        assert_eq!(s.pop::<f32>(), f32::NEG_INFINITY);
        assert_eq!(s.pop::<f32>(), f32::INFINITY);
        assert_eq!(s.pop::<f32>(), -1.0);
        assert_eq!(s.pop::<f32>(), 3.14);
        assert_eq!(s.pop::<f32>(), 0.0);
    }

    #[test]
    fn f64_value() {
        let mut s = ValueStack::new();
        s.push(0.0f64);
        s.push(3.14f64);
        s.push(-1.0f64);
        s.push(f64::INFINITY);
        s.push(f64::NEG_INFINITY);
        s.push(f64::NAN);

        assert!(s.pop::<f64>().is_nan());
        assert_eq!(s.pop::<f64>(), f64::NEG_INFINITY);
        assert_eq!(s.pop::<f64>(), f64::INFINITY);
        assert_eq!(s.pop::<f64>(), -1.0);
        assert_eq!(s.pop::<f64>(), 3.14);
        assert_eq!(s.pop::<f64>(), 0.0);
    }

    #[test]
    fn any_value() {
        let i32_s = [0, 1, -1, i32::max_value(), i32::min_value()];
        let i64_s = [
            0,
            1,
            -1,
            i32::max_value() as i64,
            i32::min_value() as i64,
            i64::max_value(),
            i64::min_value(),
        ];
        let f32_s = [0.0, 3.14, -1.0, f32::INFINITY, f32::NEG_INFINITY, f32::NAN];
        let f64_s = [0.0, 3.14, -1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN];

        let mut s = ValueStack::new();
        for (((i32v, i64v), f32v), f64v) in i32_s
            .iter()
            .cycle()
            .zip(i64_s.iter().cycle())
            .zip(f32_s.iter().cycle())
            .zip(f64_s.iter().cycle())
            .take(100)
        {
            s.push(*i32v);
            s.push(*i64v);
            s.push(*f32v);
            s.push(*f64v);
        }

        for (((i32v, i64v), f32v), f64v) in i32_s
            .iter()
            .cycle()
            .zip(i64_s.iter().cycle())
            .zip(f32_s.iter().cycle())
            .zip(f64_s.iter().cycle())
            .take(100)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
        {
            if f64v.is_nan() {
                match s.pop() {
                    Value::F64(v) => assert!(v.is_nan()),
                    v => panic!("not match: {:?}", v),
                }
            } else {
                assert_eq!(s.pop::<Value>(), Value::F64(*f64v));
            }
            if f32v.is_nan() {
                match s.pop() {
                    Value::F32(v) => assert!(v.is_nan()),
                    v => panic!("not match: {:?}", v),
                }
            } else {
                assert_eq!(s.pop::<Value>(), Value::F32(*f32v));
            }
            assert_eq!(s.pop::<Value>(), Value::I64(*i64v));
            assert_eq!(s.pop::<Value>(), Value::I32(*i32v));
        }
    }
}
