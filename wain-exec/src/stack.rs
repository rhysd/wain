use std::convert::TryInto;
use std::f32;
use std::f64;
use wain_ast::ValType;

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum AnyValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

// Vec<AnyValue> consumes too much space since its element size is always 64bits.
// To use space more efficiently, here use u32 for storing values as bytes.

pub struct Stack {
    values: Vec<u8>, // actual values per 4 bytes
    types: Vec<ValType>,
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            values: vec![],
            types: vec![],
        }
    }

    // Note: Here I don't use std::slice::from_raw since its unsafe

    pub fn push_i32(&mut self, i: i32) {
        self.types.push(ValType::I32);
        let b = i.to_le_bytes();
        self.values.extend_from_slice(&b);
    }

    pub fn push_i64(&mut self, i: i64) {
        self.types.push(ValType::I64);
        let b = i.to_le_bytes();
        self.values.extend_from_slice(&b);
    }

    pub fn push_f32(&mut self, f: f32) {
        self.types.push(ValType::F32);
        let b = f.to_le_bytes();
        self.values.extend_from_slice(&b);
    }

    pub fn push_f64(&mut self, f: f64) {
        self.types.push(ValType::F64);
        let b = f.to_le_bytes();
        self.values.extend_from_slice(&b);
    }

    fn pop_4bytes(&mut self) -> [u8; 4] {
        let len = self.values.len();
        let b = self.values[len - 4..]
            .try_into()
            .expect("4 bytes for i32 value");
        self.values.truncate(len - 4);
        b
    }

    fn pop_8bytes(&mut self) -> [u8; 8] {
        let len = self.values.len();
        let b = self.values[len - 8..]
            .try_into()
            .expect("4 bytes for 64bit value");
        self.values.truncate(len - 8);
        b
    }

    pub fn pop_i32(&mut self) -> i32 {
        assert_eq!(self.types.pop().expect("pop i32 value"), ValType::I32);
        i32::from_le_bytes(self.pop_4bytes())
    }

    pub fn pop_i64(&mut self) -> i64 {
        assert_eq!(self.types.pop().expect("pop i64 value"), ValType::I64);
        i64::from_le_bytes(self.pop_8bytes())
    }

    pub fn pop_f32(&mut self) -> f32 {
        assert_eq!(self.types.pop().expect("pop f32 value"), ValType::F32);
        f32::from_le_bytes(self.pop_4bytes())
    }

    pub fn pop_f64(&mut self) -> f64 {
        assert_eq!(self.types.pop().expect("pop f64 value"), ValType::F64);
        f64::from_le_bytes(self.pop_8bytes())
    }

    pub fn pop(&mut self) -> AnyValue {
        match self.types[self.types.len() - 1] {
            ValType::I32 => AnyValue::I32(self.pop_i32()),
            ValType::I64 => AnyValue::I64(self.pop_i64()),
            ValType::F32 => AnyValue::F32(self.pop_f32()),
            ValType::F64 => AnyValue::F64(self.pop_f64()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn i32_value() {
        let mut s = Stack::new();
        s.push_i32(0);
        s.push_i32(1);
        s.push_i32(-1);
        s.push_i32(i32::max_value());
        s.push_i32(i32::min_value());

        assert_eq!(s.pop_i32(), i32::min_value());
        assert_eq!(s.pop_i32(), i32::max_value());
        assert_eq!(s.pop_i32(), -1);
        assert_eq!(s.pop_i32(), 1);
        assert_eq!(s.pop_i32(), 0);
    }

    #[test]
    fn i64_value() {
        let mut s = Stack::new();
        s.push_i64(0);
        s.push_i64(1);
        s.push_i64(-1);
        s.push_i64(i32::max_value() as i64);
        s.push_i64(i32::min_value() as i64);
        s.push_i64(i64::max_value());
        s.push_i64(i64::min_value());

        assert_eq!(s.pop_i64(), i64::min_value());
        assert_eq!(s.pop_i64(), i64::max_value());
        assert_eq!(s.pop_i64(), i32::min_value() as i64);
        assert_eq!(s.pop_i64(), i32::max_value() as i64);
        assert_eq!(s.pop_i64(), -1);
        assert_eq!(s.pop_i64(), 1);
        assert_eq!(s.pop_i64(), 0);
    }

    #[test]
    fn f32_value() {
        let mut s = Stack::new();
        s.push_f32(0.0);
        s.push_f32(3.14);
        s.push_f32(-1.0);
        s.push_f32(f32::INFINITY);
        s.push_f32(f32::NEG_INFINITY);
        s.push_f32(f32::NAN);

        assert!(s.pop_f32().is_nan());
        assert_eq!(s.pop_f32(), f32::NEG_INFINITY);
        assert_eq!(s.pop_f32(), f32::INFINITY);
        assert_eq!(s.pop_f32(), -1.0);
        assert_eq!(s.pop_f32(), 3.14);
        assert_eq!(s.pop_f32(), 0.0);
    }

    #[test]
    fn f64_value() {
        let mut s = Stack::new();
        s.push_f64(0.0);
        s.push_f64(3.14);
        s.push_f64(-1.0);
        s.push_f64(f64::INFINITY);
        s.push_f64(f64::NEG_INFINITY);
        s.push_f64(f64::NAN);

        assert!(s.pop_f64().is_nan());
        assert_eq!(s.pop_f64(), f64::NEG_INFINITY);
        assert_eq!(s.pop_f64(), f64::INFINITY);
        assert_eq!(s.pop_f64(), -1.0);
        assert_eq!(s.pop_f64(), 3.14);
        assert_eq!(s.pop_f64(), 0.0);
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
            s.push_i32(*i32v);
            s.push_i64(*i64v);
            s.push_f32(*f32v);
            s.push_f64(*f64v);
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
                    AnyValue::F64(v) => assert!(v.is_nan()),
                    v => panic!("not match: {:?}", v),
                }
            } else {
                assert_eq!(s.pop(), AnyValue::F64(*f64v));
            }
            if f32v.is_nan() {
                match s.pop() {
                    AnyValue::F32(v) => assert!(v.is_nan()),
                    v => panic!("not match: {:?}", v),
                }
            } else {
                assert_eq!(s.pop(), AnyValue::F32(*f32v));
            }
            assert_eq!(s.pop(), AnyValue::I64(*i64v));
            assert_eq!(s.pop(), AnyValue::I32(*i32v));
        }
    }
}
