use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::mem::size_of;
use wain_ast::ValType;

#[cfg_attr(test, derive(Debug))]
#[derive(Clone, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    pub fn valtype(&self) -> ValType {
        match self {
            Value::I32(_) => ValType::I32,
            Value::I64(_) => ValType::I64,
            Value::F32(_) => ValType::F32,
            Value::F64(_) => ValType::F64,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::I32(v) => write!(f, "{}i32", v),
            Value::I64(v) => write!(f, "{}i64", v),
            Value::F32(v) => write!(f, "{}f32", v),
            Value::F64(v) => write!(f, "{}f64", v),
        }
    }
}

pub trait LittleEndian {
    fn read(buf: &[u8], addr: usize) -> Self;
    fn write(buf: &mut [u8], addr: usize, v: Self);
}

fn read_bytes<'a, A>(buf: &'a [u8], addr: usize) -> A
where
    A: TryFrom<&'a [u8]>,
    A::Error: fmt::Debug,
{
    buf[addr..addr + size_of::<A>()]
        .try_into()
        .expect("read bytes for value")
}

// TODO: copy_from_slice is slower here. I need to investigate the reason
#[allow(clippy::manual_memcpy)]
fn write_bytes(buf: &mut [u8], addr: usize, bytes: &[u8]) {
    for i in 0..bytes.len() {
        buf[addr + i] = bytes[i];
    }
}

macro_rules! impl_le_rw {
    ($t:ty) => {
        impl LittleEndian for $t {
            fn read(buf: &[u8], addr: usize) -> Self {
                <$t>::from_le_bytes(read_bytes(buf, addr))
            }
            fn write(buf: &mut [u8], addr: usize, v: Self) {
                write_bytes(buf, addr, &v.to_le_bytes());
            }
        }
    };
}

impl_le_rw!(i8);
impl_le_rw!(i16);
impl_le_rw!(i32);
impl_le_rw!(i64);
impl_le_rw!(f32);
impl_le_rw!(f64);
// unsigned integers for load/store instructions
impl LittleEndian for u8 {
    fn read(buf: &[u8], addr: usize) -> Self {
        buf[addr]
    }
    fn write(buf: &mut [u8], addr: usize, v: Self) {
        buf[addr] = v;
    }
}
impl_le_rw!(u16);
impl_le_rw!(u32);

// Trait to handle f32 and f64 in the same way
pub(crate) trait Float: Clone + Copy + PartialEq + PartialOrd {
    const ZERO: Self;
    fn is_nan(self) -> bool;
    fn min(self, other: Self) -> Self;
    fn max(self, other: Self) -> Self;
    fn is_sign_negative(self) -> bool;
}

macro_rules! impl_float {
    ($($ty:ty)*) => {
        $(
            impl Float for $ty {
                const ZERO: Self = 0.0;
                fn is_nan(self) -> bool {
                    self.is_nan()
                }
                fn min(self, other: Self) -> Self {
                    self.min(other)
                }
                fn max(self, other: Self) -> Self {
                    self.max(other)
                }
                fn is_sign_negative(self) -> bool {
                    self.is_sign_negative()
                }
            }
        )*
    };
}

impl_float!(f32 f64);
