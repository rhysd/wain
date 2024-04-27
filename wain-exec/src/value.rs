use std::fmt;
use std::mem::size_of;
use std::ops;
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

macro_rules! impl_from_values {
    ($($ty:ty => $variant:ident),*) => {
        $(
            impl From<$ty> for Value {
                fn from(v: $ty) -> Value {
                    Value::$variant(v)
                }
            }
        )*
    };
}
impl_from_values!(i32 => I32, i64 => I64, f32 => F32, f64 => F64);

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
    type UInt: Copy + ops::BitOr<Output = Self::UInt> + ops::BitAnd<Output = Self::UInt>;
    const ARITHMETIC_NAN: Self::UInt;
    fn is_nan(self) -> bool;
    fn min(self, other: Self) -> Self;
    fn max(self, other: Self) -> Self;
    fn to_bits(self) -> Self::UInt;
    fn from_bits(_: Self::UInt) -> Self;
    fn to_arithmetic_nan(self) -> Self {
        Self::from_bits(self.to_bits() | Self::ARITHMETIC_NAN)
    }
}

macro_rules! impl_float {
    ($float:ty, $uint:ty) => {
        impl Float for $float {
            type UInt = $uint;
            const ARITHMETIC_NAN: Self::UInt = 1 << <$float>::MANTISSA_DIGITS - 2;
            fn is_nan(self) -> bool {
                self.is_nan()
            }
            fn min(self, other: Self) -> Self {
                self.min(other)
            }
            fn max(self, other: Self) -> Self {
                self.max(other)
            }
            fn to_bits(self) -> Self::UInt {
                self.to_bits()
            }
            fn from_bits(b: Self::UInt) -> Self {
                Self::from_bits(b)
            }
        }
    };
}

impl_float!(f32, u32);
impl_float!(f64, u64);
