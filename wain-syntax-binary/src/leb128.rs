// Integers are encoded with LEB128 (Little Endian Base 128)
// - https://webassembly.github.io/spec/core/binary/values.html#integers
// - https://en.wikipedia.org/wiki/LEB128

use crate::error::ErrorKind;

type Result<T> = ::std::result::Result<T, Box<ErrorKind>>;

// Note: Self must be Sized because trait function returns Self
pub trait Leb128: Sized {
    fn read_leb128(bytes: &[u8]) -> Result<(Self, usize)>;
}

impl Leb128 for u64 {
    fn read_leb128(bytes: &[u8]) -> Result<(Self, usize)> {
        read_64(bytes, false)
    }
}

impl Leb128 for u32 {
    fn read_leb128(bytes: &[u8]) -> Result<(Self, usize)> {
        read_32(bytes, false)
    }
}

impl Leb128 for i64 {
    fn read_leb128(bytes: &[u8]) -> Result<(Self, usize)> {
        let (u, size) = read_64(bytes, true)?;
        Ok((u as i64, size))
    }
}

impl Leb128 for i32 {
    fn read_leb128(bytes: &[u8]) -> Result<(Self, usize)> {
        let (u, size) = read_32(bytes, true)?;
        Ok((u as i32, size))
    }
}

fn read_64(bytes: &[u8], signed: bool) -> Result<(u64, usize)> {
    let mut ret = 0;

    for (idx, b) in bytes.iter().copied().enumerate() {
        // 7bits * 9 = 63bits
        // unsigned:
        //   Next 1byte (idx=10) must be 0 or 1
        // signed:
        //   Next 1byte (idx=10) must be 0 (for max int) or 0b01111111 (for min int)
        if idx > 9 || idx == 9 && (!signed && b > 1 || signed && b != 0 && b != 0x7f) {
            let ty = if signed { "i64" } else { "u64" };
            return Err(Box::new(ErrorKind::IntOverflow { ty, got: None }));
        }

        ret |= ((b & 0b0111_1111) as u64) << (idx * 7);

        if b & 0b1000_0000 == 0 {
            let len = idx + 1;
            // For negative signed integers, sign bit must be extended to fill significant bits
            if signed && len < 10 && b & 0b0100_0000 != 0 {
                ret |= !0 << (len * 7);
            }
            return Ok((ret, len));
        }
    }

    Err(Box::new(ErrorKind::UnexpectedEof {
        expected: "part of LEB128-encoded integer",
    }))
}

fn read_32(bytes: &[u8], signed: bool) -> Result<(u32, usize)> {
    let mut ret = 0;

    for (idx, b) in bytes.iter().copied().enumerate() {
        // 7bits * 4 = 28bits.
        // unsigned:
        //   Next byte must be <= 0b1111
        // signed:
        //   Next byte must be <= 0b0111 for positive values and >= 0b1111000 for negative values
        if idx > 4 || idx == 4 && (!signed && b > 0b1111 || signed && b > 0b0111 && b < 0b111_1000) {
            let ty = if signed { "i32" } else { "u32" };
            return Err(Box::new(ErrorKind::IntOverflow { ty, got: None }));
        }

        ret |= ((b & 0b0111_1111) as u32) << (idx * 7);

        if b & 0b1000_0000 == 0 {
            let len = idx + 1;
            // For negative signed integers, sign bit must be extended
            if signed && len < 5 && b & 0b0100_0000 != 0 {
                ret |= !0 << (len * 7);
            }
            return Ok((ret, len));
        }
    }

    Err(Box::new(ErrorKind::UnexpectedEof {
        expected: "part of LEB128-encoded integer",
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn i32_value_ok() {
        for (input, expected) in vec![
            (vec![0x00], 0),
            (vec![0x80, 0x01], 128),
            (vec![0xc0, 0xc4, 0x07], 123456),
            (vec![0x01], 1),
            (vec![0xff, 0xff, 0xff, 0xff, 0x07], i32::MAX),
            (vec![0xc0, 0xbb, 0x78], -123456),
            (vec![0x7f], -1),
            (vec![0x80, 0x7f], -128),
            (vec![0x80, 0x80, 0x80, 0x80, 0x78], i32::MIN),
        ] {
            let (i, s) = i32::read_leb128(&input).unwrap();
            assert_eq!(i, expected, "expected {} but got {} for {:?}", expected, i, input,);
            assert_eq!(
                s,
                input.len(),
                "expected size {} but got {} for {:?}",
                input.len(),
                s,
                input,
            );
        }
    }

    #[test]
    fn i64_value_ok() {
        for (input, expected) in vec![
            (vec![0x00], 0),
            (vec![0x80, 0x01], 128),
            (vec![0xc0, 0xc4, 0x07], 123456),
            (vec![0x01], 1),
            (vec![0xff, 0xff, 0xff, 0xff, 0x07], i32::MAX as i64),
            (
                vec![
                    0b10011111, 0b11111111, 0b11000111, 0b11000100, 0b11010110, 0b11111101, 0b00000000,
                ],
                4318196531103,
            ),
            (
                vec![0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x0],
                i64::MAX,
            ),
            (vec![0xc0, 0xbb, 0x78], -123456),
            (vec![0x7f], -1),
            (vec![0x80, 0x7f], -128),
            (
                vec![
                    0b11100001, 0b10000000, 0b10111000, 0b10111011, 0b10101001, 0b10000010, 0b1111111,
                ],
                -4318196531103,
            ),
            (vec![0x80, 0x80, 0x80, 0x80, 0x08], i32::MIN as u32 as i64),
            (
                vec![0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7f],
                i64::MIN,
            ),
        ] {
            let (i, s) = i64::read_leb128(&input).unwrap();
            assert_eq!(i, expected, "expected {} but got {} for {:?}", expected, i, input,);
            assert_eq!(
                s,
                input.len(),
                "expected size {} but got {} for {:?}",
                input.len(),
                s,
                input,
            );
        }
    }

    #[test]
    fn u32_value_ok() {
        for (input, expected) in [
            (vec![0x00], 0),
            (vec![0x80, 0x01], 128),
            (vec![0xc0, 0xc4, 0x07], 123456),
            (vec![0x01], 1),
            (vec![0xff, 0xff, 0xff, 0xff, 0x0f], u32::MAX),
        ] {
            let (i, s) = u32::read_leb128(&input).unwrap();
            assert_eq!(i, expected, "expected {} but got {} for {:?}", expected, i, input,);
            assert_eq!(
                s,
                input.len(),
                "expected size {} but got {} for {:?}",
                input.len(),
                s,
                input,
            );
        }
    }

    #[test]
    fn u64_value_ok() {
        for (input, expected) in vec![
            (vec![0x00], 0),
            (vec![0x80, 0x01], 128),
            (vec![0xc0, 0xc4, 0x07], 123456),
            (vec![0x01], 1),
            (vec![0xff, 0xff, 0xff, 0xff, 0x0f], u32::MAX as u64),
            (vec![0x9f, 0xff, 0xc5, 0xaf, 0x91, 0xa2, 0x1c], 124318196531103),
            (
                vec![0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1],
                u64::MAX,
            ),
        ] {
            let (i, s) = u64::read_leb128(&input).unwrap();
            assert_eq!(i, expected, "expected {} but got {} for {:?}", expected, i, input,);
            assert_eq!(
                s,
                input.len(),
                "expected size {} but got {} for {:?}",
                input.len(),
                s,
                input,
            );
        }
    }

    #[test]
    fn other_bytes_follow() {
        // other bytes follow
        let (i, s) = i32::read_leb128(&[0xc0, 0xbb, 0x78, 0x12, 0x34, 0xff]).unwrap();
        assert_eq!(i, -123456, "0x{:x}", i);
        assert_eq!(s, 3);
    }

    #[test]
    fn overflow_error() {
        let b = [0xff, 0xff, 0xff, 0xff, 0x10]; // i32 max + 1
        assert!(matches!(
            *i32::read_leb128(&b).unwrap_err(),
            ErrorKind::IntOverflow { .. }
        ));
        let b = [0x80, 0x80, 0x80, 0x80, 0x77]; // i32 min - 1
        assert!(matches!(
            *i32::read_leb128(&b).unwrap_err(),
            ErrorKind::IntOverflow { .. }
        ));
        let b = [0xff, 0xff, 0xff, 0xff, 0x10]; // u32 max + 1
        assert!(matches!(
            *u32::read_leb128(&b).unwrap_err(),
            ErrorKind::IntOverflow { .. }
        ));
        let b = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x1]; // i64 max + 1
        assert!(matches!(
            *i64::read_leb128(&b).unwrap_err(),
            ErrorKind::IntOverflow { .. }
        ));
        let b = [0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7e]; // i64 min - 1
        assert!(matches!(
            *i64::read_leb128(&b).unwrap_err(),
            ErrorKind::IntOverflow { .. }
        ));
        let b = [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x2]; // u64 max + 1
        assert!(matches!(
            *u64::read_leb128(&b).unwrap_err(),
            ErrorKind::IntOverflow { .. }
        ));
    }

    #[test]
    fn unexpected_eof() {
        let b = [0xc0, 0xc4];
        assert!(matches!(
            *i32::read_leb128(&b).unwrap_err(),
            ErrorKind::UnexpectedEof { .. }
        ));
        let b = [0xff, 0xff, 0xff, 0xff];
        assert!(matches!(
            *i64::read_leb128(&b).unwrap_err(),
            ErrorKind::UnexpectedEof { .. }
        ));
    }
}
