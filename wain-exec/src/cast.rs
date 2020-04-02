// Float to int conversion. When the float value cannot be represented as target integer type,
// it causes undefined behavior. This is a bug of rustc. We need to handle it until the bug is fixed.
//   https://github.com/rust-lang/rust/issues/10184

// https://github.com/rust-num/num-traits/blob/1eb80e123b1afdf83c491a3c8754c49f61af743d/src/cast.rs#L321-L340
macro_rules! float_to_uint {
    ($name:ident, $float:ty, $int:ty) => {
        pub fn $name(f: $float) -> $int {
            const MAX: $float = <$int>::max_value() as $float;
            if f.is_nan() || f <= -1.0 {
                0
            } else if f >= MAX {
                <$int>::max_value()
            } else {
                f as $int
            }
        }
    };
}

float_to_uint!(f32_to_u32, f32, u32);
float_to_uint!(f32_to_u64, f32, u64);
float_to_uint!(f64_to_u64, f64, u64);
pub fn f64_to_u32(f: f64) -> u32 {
    // Max value can be represented exactly
    const MAX: f64 = u32::max_value() as f64 + 1.0;
    if f.is_nan() || f <= -1.0 {
        0
    } else if f >= MAX {
        u32::max_value()
    } else {
        f as u32
    }
}

// https://github.com/rust-num/num-traits/blob/1eb80e123b1afdf83c491a3c8754c49f61af743d/src/cast.rs#L291-L313
macro_rules! float_to_int {
    ($name:ident, $float:ty, $int:ty) => {
        pub fn $name(f: $float) -> $int {
            const MAX: $float = <$int>::max_value() as $float;
            const MIN: $float = <$int>::min_value() as $float;
            if f.is_nan() {
                0
            } else if f >= MAX {
                <$int>::max_value()
            } else if f < MIN {
                <$int>::min_value()
            } else {
                f as $int
            }
        }
    };
}

float_to_int!(f32_to_i32, f32, i32);
float_to_int!(f32_to_i64, f32, i64);
float_to_int!(f64_to_i64, f64, i64);
pub fn f64_to_i32(f: f64) -> i32 {
    // Max and min values can be represented exactly
    const MAX: f64 = i32::max_value() as f64 + 1.0;
    const MIN: f64 = i32::min_value() as f64 - 1.0;
    if f.is_nan() {
        0
    } else if f <= MIN {
        i32::min_value()
    } else if f >= MAX {
        i32::max_value()
    } else {
        f as i32
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{f32, f64};

    #[test]
    fn float_to_uint() {
        assert_eq!(f32_to_u32(-1.0), 0);
        assert_eq!(f32_to_u32(0.0), 0);
        assert_eq!(f32_to_u32(u32::max_value() as f32), u32::max_value());
        assert_eq!(f32_to_u32(u32::max_value() as f32 + 1.0), u32::max_value());
        assert_eq!(
            f32_to_u32(u32::max_value() as f32 + 10000.0),
            u32::max_value()
        );
        assert_eq!(f32_to_u32(f32::NAN), 0);

        assert_eq!(f32_to_u64(-1.0), 0);
        assert_eq!(f32_to_u64(0.0), 0);
        assert_eq!(f32_to_u64(u64::max_value() as f32), u64::max_value());
        assert_eq!(f32_to_u64(u64::max_value() as f32 + 1.0), u64::max_value());
        assert_eq!(
            f32_to_u64(u64::max_value() as f32 + 10000.0),
            u64::max_value()
        );
        assert_eq!(f32_to_u64(f32::NAN), 0);

        assert_eq!(f64_to_u32(-1.0), 0);
        assert_eq!(f64_to_u32(0.0), 0);
        assert_eq!(f64_to_u32(u32::max_value() as f64), u32::max_value());
        assert_eq!(f64_to_u32(u32::max_value() as f64 + 1.0), u32::max_value());
        assert_eq!(
            f64_to_u32(u32::max_value() as f64 + 10000.0),
            u32::max_value()
        );
        assert_eq!(f64_to_u32(f64::NAN), 0);

        assert_eq!(f64_to_u64(-1.0), 0);
        assert_eq!(f64_to_u64(0.0), 0);
        assert_eq!(f64_to_u64(u64::max_value() as f64), u64::max_value());
        assert_eq!(f64_to_u64(u64::max_value() as f64 + 1.0), u64::max_value());
        assert_eq!(
            f64_to_u64(u64::max_value() as f64 + 10000.0),
            u64::max_value()
        );
        assert_eq!(f64_to_u64(f64::NAN), 0);
    }

    #[test]
    fn float_to_int() {
        assert_eq!(f32_to_i32(0.0), 0);
        assert_eq!(f32_to_i32(i32::min_value() as f32), i32::min_value());
        assert_eq!(f32_to_i32(i32::min_value() as f32 - 1.0), i32::min_value());
        assert_eq!(
            f32_to_i32(i32::min_value() as f32 - 10000.0),
            i32::min_value()
        );
        assert_eq!(f32_to_i32(i32::max_value() as f32), i32::max_value());
        assert_eq!(f32_to_i32(i32::max_value() as f32 + 1.0), i32::max_value());
        assert_eq!(
            f32_to_i32(i32::max_value() as f32 + 10000.0),
            i32::max_value()
        );
        assert_eq!(f32_to_i32(f32::NAN), 0);

        assert_eq!(f32_to_i64(0.0), 0);
        assert_eq!(f32_to_i64(i64::min_value() as f32), i64::min_value());
        assert_eq!(f32_to_i64(i64::min_value() as f32 - 1.0), i64::min_value());
        assert_eq!(
            f32_to_i64(i64::min_value() as f32 - 10000.0),
            i64::min_value()
        );
        assert_eq!(f32_to_i64(i64::max_value() as f32), i64::max_value());
        assert_eq!(f32_to_i64(i64::max_value() as f32 + 1.0), i64::max_value());
        assert_eq!(
            f32_to_i64(i64::max_value() as f32 + 10000.0),
            i64::max_value()
        );
        assert_eq!(f32_to_i64(f32::NAN), 0);

        assert_eq!(f64_to_i32(0.0), 0);
        assert_eq!(f64_to_i32(i32::min_value() as f64), i32::min_value());
        assert_eq!(f64_to_i32(i32::min_value() as f64 - 1.0), i32::min_value());
        assert_eq!(
            f64_to_i32(i32::min_value() as f64 - 10000.0),
            i32::min_value()
        );
        assert_eq!(f64_to_i32(i32::max_value() as f64), i32::max_value());
        assert_eq!(f64_to_i32(i32::max_value() as f64 + 1.0), i32::max_value());
        assert_eq!(
            f64_to_i32(i32::max_value() as f64 + 10000.0),
            i32::max_value()
        );
        assert_eq!(f64_to_i32(f64::NAN), 0);

        assert_eq!(f64_to_i64(0.0), 0);
        assert_eq!(f64_to_i64(i64::min_value() as f64), i64::min_value());
        assert_eq!(f64_to_i64(i64::min_value() as f64 - 1.0), i64::min_value());
        assert_eq!(
            f64_to_i64(i64::min_value() as f64 - 10000.0),
            i64::min_value()
        );
        assert_eq!(f64_to_i64(i64::max_value() as f64), i64::max_value());
        assert_eq!(f64_to_i64(i64::max_value() as f64 + 1.0), i64::max_value());
        assert_eq!(
            f64_to_i64(i64::max_value() as f64 + 10000.0),
            i64::max_value()
        );
        assert_eq!(f64_to_i64(f64::NAN), 0);
    }
}
