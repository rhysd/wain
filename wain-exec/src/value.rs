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

pub trait ReadWrite {
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

impl ReadWrite for i32 {
    fn read(buf: &[u8], addr: usize) -> Self {
        i32::from_le_bytes(read_bytes(buf, addr))
    }
    fn write(buf: &mut [u8], addr: usize, v: Self) {
        write_bytes(buf, addr, &v.to_le_bytes());
    }
}
impl ReadWrite for f32 {
    fn read(buf: &[u8], addr: usize) -> Self {
        f32::from_le_bytes(read_bytes(buf, addr))
    }
    fn write(buf: &mut [u8], addr: usize, v: Self) {
        write_bytes(buf, addr, &v.to_le_bytes());
    }
}
impl ReadWrite for i64 {
    fn read(buf: &[u8], addr: usize) -> Self {
        i64::from_le_bytes(read_bytes(buf, addr))
    }
    fn write(buf: &mut [u8], addr: usize, v: Self) {
        write_bytes(buf, addr, &v.to_le_bytes());
    }
}
impl ReadWrite for f64 {
    fn read(buf: &[u8], addr: usize) -> Self {
        f64::from_le_bytes(read_bytes(buf, addr))
    }
    fn write(buf: &mut [u8], addr: usize, v: Self) {
        write_bytes(buf, addr, &v.to_le_bytes());
    }
}
