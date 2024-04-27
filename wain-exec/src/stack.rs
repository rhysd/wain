use crate::value::{LittleEndian, Value};
use std::cmp::Ordering;
use std::fmt;
use std::mem;
use std::mem::size_of;
use wain_ast::{AsValType, ValType};

// Vec<Value> consumes too much space since its element size is always 16bytes.
// To use space more efficiently, here use u8 for storing values as bytes.

#[derive(Default)]
pub struct Stack {
    bytes: Vec<u8>,      // Bytes buffer for actual values
    types: Vec<ValType>, // this stack is necessary to pop arbitrary value
    frame: CallFrame,
}

pub trait StackAccess: Sized {
    fn pop(stack: &mut Stack) -> Self {
        let v = Self::top(stack);
        stack.erase_top(size_of::<Self>());
        v
    }
    fn push(stack: &mut Stack, v: Self);
    fn top(stack: &Stack) -> Self;
}

impl StackAccess for i32 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::I32);
    }
    fn top(stack: &Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::I32);
        i32::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for i64 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::I64);
    }
    fn top(stack: &Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::I64);
        i64::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for f32 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::F32);
    }
    fn top(stack: &Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::F32);
        f32::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for f64 {
    fn push(stack: &mut Stack, v: Self) {
        stack.push_bytes(&v.to_le_bytes(), ValType::F64);
    }
    fn top(stack: &Stack) -> Self {
        assert_eq!(stack.top_type(), ValType::F64);
        f64::from_le_bytes(stack.top_bytes())
    }
}

impl StackAccess for Value {
    fn pop(stack: &mut Stack) -> Self {
        match stack.types[stack.len() - 1] {
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
    fn top(stack: &Stack) -> Self {
        match stack.types[stack.len() - 1] {
            ValType::I32 => Value::I32(StackAccess::top(stack)),
            ValType::I64 => Value::I64(StackAccess::top(stack)),
            ValType::F32 => Value::F32(StackAccess::top(stack)),
            ValType::F64 => Value::F64(StackAccess::top(stack)),
        }
    }
}

impl Stack {
    // Note: Here I don't use std::slice::from_raw since its unsafe

    fn top_type(&self) -> ValType {
        self.types[self.len() - 1]
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
        let len = self.top_addr() - size_of::<T>();
        self.bytes[len..].try_into().expect("top bytes")
    }

    fn erase_top(&mut self, len: usize) {
        self.types.pop();
        self.bytes.truncate(self.top_addr() - len);
    }

    pub fn pop<V: StackAccess>(&mut self) -> V {
        StackAccess::pop(self)
    }

    pub fn top<V: StackAccess>(&self) -> V {
        StackAccess::top(self)
    }

    pub fn write_top_bytes<V: LittleEndian>(&mut self, v: V) {
        let addr = self.top_addr() - size_of::<V>();
        LittleEndian::write(&mut self.bytes, addr, v);
    }

    pub fn write_top_type(&mut self, t: ValType) {
        let idx = self.len() - 1;
        self.types[idx] = t;
    }

    pub fn write_top<T: StackAccess, V: LittleEndian + AsValType>(&mut self, v: V) {
        // Expect optimizations by compiler since conditions of these if statements can be
        // calculated at compile time
        //
        // Note: The same value as size_of::<T>() can be obtained by self.top_type().bytes(), but
        // it's runtime value preventing compiler from optimization.
        match size_of::<T>().cmp(&size_of::<V>()) {
            Ordering::Equal => {}
            Ordering::Greater => {
                let len = self.top_addr() - (size_of::<T>() - size_of::<V>());
                self.bytes.truncate(len);
            }
            Ordering::Less => {
                let len = self.top_addr() + (size_of::<V>() - size_of::<T>());
                self.bytes.resize(len, 0);
            }
        }
        self.write_top_bytes(v);
        self.write_top_type(V::VAL_TYPE);
    }

    fn write<V: LittleEndian>(&mut self, addr: usize, v: V) {
        LittleEndian::write(&mut self.bytes, addr, v);
    }

    fn read<V: LittleEndian>(&self, addr: usize) -> V {
        LittleEndian::read(&self.bytes, addr)
    }

    pub fn write_local(&mut self, localidx: u32) {
        let addr = self.local_addr(localidx);
        match self.local_type(localidx) {
            ValType::I32 => self.write(addr, self.top::<i32>()),
            ValType::I64 => self.write(addr, self.top::<i64>()),
            ValType::F32 => self.write(addr, self.top::<f32>()),
            ValType::F64 => self.write(addr, self.top::<f64>()),
        }
    }

    pub fn read_local(&mut self, localidx: u32) {
        let addr = self.local_addr(localidx);
        match self.local_type(localidx) {
            ValType::I32 => self.push(self.read::<i32>(addr)),
            ValType::I64 => self.push(self.read::<i64>(addr)),
            ValType::F32 => self.push(self.read::<f32>(addr)),
            ValType::F64 => self.push(self.read::<f64>(addr)),
        }
    }

    fn top_addr(&self) -> usize {
        self.bytes.len()
    }

    // len() returns number of values pushed on this stack
    fn len(&self) -> usize {
        self.types.len()
    }

    fn restore(&mut self, addr: usize, type_idx: usize, has_result: bool) {
        if has_result {
            let v: Value = self.pop();
            self.bytes.truncate(addr);
            self.types.truncate(type_idx);
            self.push(v);
        } else {
            self.bytes.truncate(addr);
            self.types.truncate(type_idx);
        }
    }

    pub fn push_label(&self) -> Label {
        Label {
            addr: self.top_addr(),
            type_idx: self.len(),
        }
    }

    pub fn pop_label(&mut self, label: &Label, has_result: bool) {
        // Part of 'br' instruction: https://webassembly.github.io/spec/core/exec/instructions.html#exec-br
        self.restore(label.addr, label.type_idx, has_result)
    }

    pub fn push_frame(&mut self, params: &[ValType], locals: &[ValType]) -> CallFrame {
        let mut local_addrs = Vec::with_capacity(params.len() + locals.len());

        // Note: Params were already pushed to stack
        let params_bytes = params.iter().fold(0, |acc, p| acc + p.bytes());
        let base_addr = self.top_addr() - params_bytes;
        let base_idx = self.len() - params.len();

        self.types.extend_from_slice(locals);

        let mut addr = base_addr;
        for p in &self.types[base_idx..] {
            local_addrs.push(addr);
            addr += p.bytes();
        }

        self.bytes.resize(addr, 0);

        mem::replace(
            &mut self.frame,
            CallFrame {
                base_addr,
                base_idx,
                local_addrs,
            },
        )
    }

    pub fn pop_frame(&mut self, prev_frame: CallFrame, has_result: bool) {
        self.restore(self.frame.base_addr, self.frame.base_idx, has_result);
        self.frame = prev_frame;
    }

    fn local_addr(&self, localidx: u32) -> usize {
        self.frame.local_addrs[localidx as usize]
    }

    fn local_type(&self, localidx: u32) -> ValType {
        let idx = localidx as usize;
        self.types[self.frame.base_idx + idx]
    }
}

// Activations of function frames
// This class is outside Machine because it has shorter lifetime. It only lives while the current
// function is being invoked
#[derive(Default)]
pub struct CallFrame {
    base_addr: usize,
    base_idx: usize,
    local_addrs: Vec<usize>, // Calculate local addresses in advance for random access
}

pub struct Label {
    addr: usize,
    type_idx: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn i32_value() {
        let mut s = Stack::default();
        s.push(0i32);
        assert_eq!(s.top::<i32>(), 0);
        s.push(1i32);
        s.push(-1i32);
        s.push(i32::MAX);
        s.push(i32::MIN);

        assert_eq!(s.pop::<i32>(), i32::MIN);
        assert_eq!(s.pop::<i32>(), i32::MAX);
        assert_eq!(s.pop::<i32>(), -1);
        assert_eq!(s.pop::<i32>(), 1);
        assert_eq!(s.pop::<i32>(), 0);
    }

    #[test]
    fn i64_value() {
        let mut s = Stack::default();
        s.push(0i64);
        s.push(1i64);
        s.push(-1i64);
        s.push(i32::MAX as i64);
        s.push(i32::MIN as i64);
        s.push(i64::MAX);
        s.push(i64::MIN);

        assert_eq!(s.pop::<i64>(), i64::MIN);
        assert_eq!(s.pop::<i64>(), i64::MAX);
        assert_eq!(s.pop::<i64>(), i32::MIN as i64);
        assert_eq!(s.pop::<i64>(), i32::MAX as i64);
        assert_eq!(s.pop::<i64>(), -1);
        assert_eq!(s.pop::<i64>(), 1);
        assert_eq!(s.pop::<i64>(), 0);
    }

    #[test]
    fn f32_value() {
        let mut s = Stack::default();
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
        let mut s = Stack::default();
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
        let i32_s = [0, 1, -1, i32::MAX, i32::MIN];
        let i64_s = [0, 1, -1, i32::MAX as i64, i32::MIN as i64, i64::MAX, i64::MIN];
        let f32_s = [0.0, 3.14, -1.0, f32::INFINITY, f32::NEG_INFINITY, f32::NAN];
        let f64_s = [0.0, 3.14, -1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN];

        let mut s = Stack::default();
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
