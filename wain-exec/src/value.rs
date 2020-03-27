use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::mem::size_of;

#[cfg_attr(test, derive(Debug, PartialEq))]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
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
