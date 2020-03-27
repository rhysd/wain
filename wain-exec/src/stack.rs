use crate::value::{LittleEndian, Value};
use std::convert::{TryFrom, TryInto};
use std::f32;
use std::f64;
use std::fmt;
use std::mem::size_of;
use wain_ast::ValType;

// Vec<Value> consumes too much space since its element size is always 64bits.
// To use space more efficiently, here use u32 for storing values as bytes.

pub struct Stack {
    bytes: Vec<u8>,      // Bytes buffer for actual values
    types: Vec<ValType>, // this stack is necessary to pop arbitrary value
}

pub trait StackAccess: Sized {
    fn pop(stack: &mut Stack) -> Self {
        let v = Self::top(stack);
        stack.erase_top(size_of::<Self>());
        v
    }
    fn push(stack: &mut Stack, v: Self);
    fn top(stack: &mut Stack) -> Self;
}

impl StackAccess for i32 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::I32);
    }
    fn top(stack: &mut Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::I32);
        i32::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for i64 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::I64);
    }
    fn top(stack: &mut Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::I64);
        i64::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for f32 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::F32);
    }
    fn top(stack: &mut Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::F32);
        f32::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for f64 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::F64);
    }
    fn top(stack: &mut Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::F64);
        f64::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for Value {
    fn pop(stack: &mut Stack) -> Self {
        match stack.types[stack.types.len() - 1] {
            ValType::I32 => Value::I32(StackAccess::pop(stack)),
            ValType::I64 => Value::I64(StackAccess::pop(stack)),
            ValType::F32 => Value::F32(StackAccess::pop(stack)),
            ValType::F64 => Value::F64(StackAccess::pop(stack)),
        }
    }
    fn push(stack: &mut Stack, v: Self) {
        match v {
            Value::I32(i) => StackAccess::push(stack, i),
            Value::I64(i) => StackAccess::push(stack, i),
            Value::F32(f) => StackAccess::push(stack, f),
            Value::F64(f) => StackAccess::push(stack, f),
        }
    }
    fn top(stack: &mut Stack) -> Self {
        match stack.types[stack.types.len() - 1] {
            ValType::I32 => Value::I32(StackAccess::top(stack)),
            ValType::I64 => Value::I64(StackAccess::top(stack)),
            ValType::F32 => Value::F32(StackAccess::top(stack)),
            ValType::F64 => Value::F64(StackAccess::top(stack)),
        }
    }
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            bytes: vec![],
            types: vec![],
        }
    }

    // Note: Here I don't use std::slice::from_raw since its unsafe

    fn top_type(&self) -> ValType {
        self.types[self.types.len() - 1]
    }

    fn push_bytes(&mut self, bytes: &[u8], ty: ValType) {
        self.types.push(ty);
        self.bytes.extend_from_slice(bytes);
    }

    pub fn push<V: StackAccess>(&mut self, v: V) {
        StackAccess::push(self, v);
    }

    fn top_bytes<'a, T>(&'a self) -> T
    where
        T: TryFrom<&'a [u8]>,
        T::Error: fmt::Debug,
    {
        let len = self.bytes.len() - size_of::<T>();
        self.bytes[len..].try_into().expect("top bytes")
    }

    fn erase_top(&mut self, len: usize) {
        self.types.pop();
        self.bytes.truncate(self.bytes.len() - len);
    }

    pub fn pop<V: StackAccess>(&mut self) -> V {
        StackAccess::pop(self)
    }

    pub fn top<V: StackAccess>(&mut self) -> V {
        StackAccess::top(self)
    }

    pub fn write<V: LittleEndian>(&mut self, addr: usize, v: V) {
        LittleEndian::write(&mut self.bytes, addr, v)
    }

    pub fn read<V: LittleEndian>(&self, addr: usize) -> V {
        LittleEndian::read(&self.bytes, addr)
    }

    pub fn write_any(&mut self, addr: usize, v: Value) {
        match v {
            Value::I32(i) => self.write(addr, i),
            Value::I64(i) => self.write(addr, i),
            Value::F32(f) => self.write(addr, f),
            Value::F64(f) => self.write(addr, f),
        }
    }

    fn top_addr(&self) -> usize {
        self.bytes.len()
    }

    fn top_idx(&self) -> usize {
        self.types.len()
    }

    pub fn restore(&mut self, addr: usize, type_idx: usize) {
        self.bytes.truncate(addr);
        self.types.truncate(type_idx);
    }

    pub fn push_label(&self, ty: &Option<ValType>) -> Label {
        Label {
            addr: self.top_addr(),
            type_idx: self.top_idx(),
            has_result: ty.is_some(),
        }
    }

    pub fn pop_label(&mut self, label: Label) {
        // Part of 'br' instruction: https://webassembly.github.io/spec/core/exec/instructions.html#exec-br
        if label.has_result {
            let v: Value = self.pop();
            self.restore(label.addr, label.type_idx);
            self.push(v);
        } else {
            self.restore(label.addr, label.type_idx);
        }
    }

    pub fn extend_zero_values(&mut self, types: &[ValType]) {
        self.types.extend_from_slice(types);
        let bytes = types.iter().fold(0, |acc, t| acc + t.bytes());
        self.bytes.resize(self.bytes.len() + bytes, 0);
    }
}

// Activations of function frames
// This class is outside Machine because it has shorter lifetime. It only lives while the current
// function is being invoked
pub struct CallFrame<'func> {
    pub base_addr: usize,
    pub base_idx: usize,
    local_addrs: Box<[usize]>, // Calculate local addresses in advance for random access
    params: &'func [ValType],
    locals: &'func [ValType],
}

impl<'f> CallFrame<'f> {
    pub fn new(stack: &Stack, params: &'f [ValType], locals: &'f [ValType]) -> Self {
        let mut addrs = Vec::with_capacity(params.len() + locals.len());

        // Note: Params were already pushed to stack
        let params_bytes = params.iter().fold(0, |acc, p| acc + p.bytes());
        let base_addr = stack.top_addr() - params_bytes;
        let base_idx = stack.top_idx() - params.len();

        let mut addr = 0;
        for p in params {
            addrs.push(base_addr + addr);
            addr += p.bytes();
        }
        for l in locals {
            addrs.push(base_addr + addr);
            addr += l.bytes();
        }

        Self {
            base_addr,
            base_idx,
            local_addrs: addrs.into_boxed_slice(),
            params,
            locals,
        }
    }

    pub fn local_addr(&self, localidx: u32) -> usize {
        self.local_addrs[localidx as usize]
    }

    pub fn local_type(&self, localidx: u32) -> ValType {
        let idx = localidx as usize;
        if idx < self.params.len() {
            self.params[idx]
        } else if idx < self.params.len() + self.locals.len() {
            self.locals[idx - self.params.len()]
        } else {
            // Unreachable thanks to validation
            unreachable!("local type out of bounds")
        }
    }
}

pub struct Label {
    addr: usize,
    type_idx: usize,
    has_result: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn i32_value() {
        let mut s = Stack::new();
        s.push(0i32);
        assert_eq!(s.top::<i32>(), 0);
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
        let mut s = Stack::new();
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
        let mut s = Stack::new();
        s.push(0.0f32);
        assert_eq!(s.top::<f32>(), 0.0);
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
        let mut s = Stack::new();
        s.push(0.0f64);
        assert_eq!(s.top::<f64>(), 0.0f64);
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

        let mut s = Stack::new();
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
                assert_eq!(s.top::<Value>(), Value::F64(*f64v));
                assert_eq!(s.pop::<Value>(), Value::F64(*f64v));
            }
            if f32v.is_nan() {
                match s.pop() {
                    Value::F32(v) => assert!(v.is_nan()),
                    v => panic!("not match: {:?}", v),
                }
            } else {
                assert_eq!(s.top::<Value>(), Value::F32(*f32v));
                assert_eq!(s.pop::<Value>(), Value::F32(*f32v));
            }
            assert_eq!(s.top::<Value>(), Value::I64(*i64v));
            assert_eq!(s.pop::<Value>(), Value::I64(*i64v));
            assert_eq!(s.top::<Value>(), Value::I32(*i32v));
            assert_eq!(s.pop::<Value>(), Value::I32(*i32v));
        }
    }
}
