// Float to int conversion. When the float value cannot be represented as target integer type,
// it causes undefined behavior. This is a bug of rustc. We need to handle it until the bug is fixed.
//   https://github.com/rust-lang/rust/issues/10184

// >= and <= are always available for comparing max/min values. Thanks @kariya-mitsuru for discussion
// https://mobile.twitter.com/Linda_pp/status/1245587716537909250
macro_rules! float_to_int {
    ($name:ident, $float:ty, $int:ty) => {
        pub fn $name(f: $float) -> $int {
            const MAX: $float = <$int>::max_value() as $float;
            const MIN: $float = <$int>::min_value() as $float;
            if f.is_nan() {
                0
            } else if f >= MAX {
                <$int>::max_value()
            } else if f <= MIN {
                <$int>::min_value()
            } else {
                f as $int
            }
        }
    };
}

float_to_int!(f32_to_u32, f32, u32);
float_to_int!(f32_to_u64, f32, u64);
float_to_int!(f64_to_u32, f64, u32);
float_to_int!(f64_to_u64, f64, u64);
float_to_int!(f32_to_i32, f32, i32);
float_to_int!(f32_to_i64, f32, i64);
float_to_int!(f64_to_i32, f64, i32);
float_to_int!(f64_to_i64, f64, i64);

#[cfg(test)]
mod tests {
    use super::*;
    use std::{f32, f64};

    #[test]
    fn float_to_uint() {
        assert_eq!(f32_to_u32(-1.0), 0);
        assert_eq!(f32_to_u32(0.5), 0);
        assert_eq!(f32_to_u32(0.0), 0);
        assert_eq!(f32_to_u32(u32::max_value() as f32), u32::max_value());
        assert_eq!(f32_to_u32(u32::max_value() as f32 + 0.5), u32::max_value());
        assert_eq!(f32_to_u32(u32::max_value() as f32 + 1.0), u32::max_value());
        assert_eq!(
            f32_to_u32(u32::max_value() as f32 + 10000.0),
            u32::max_value()
        );
        assert_eq!(f32_to_u32(f32::NAN), 0);

        assert_eq!(f32_to_u64(-1.0), 0);
        assert_eq!(f32_to_u64(0.5), 0);
        assert_eq!(f32_to_u64(0.0), 0);
        assert_eq!(f32_to_u64(u64::max_value() as f32), u64::max_value());
        assert_eq!(f32_to_u64(u64::max_value() as f32 + 0.5), u64::max_value());
        assert_eq!(f32_to_u64(u64::max_value() as f32 + 1.0), u64::max_value());
        assert_eq!(
            f32_to_u64(u64::max_value() as f32 + 10000.0),
            u64::max_value()
        );
        assert_eq!(f32_to_u64(f32::NAN), 0);

        assert_eq!(f64_to_u32(-1.0), 0);
        assert_eq!(f64_to_u32(0.5), 0);
        assert_eq!(f64_to_u32(0.0), 0);
        assert_eq!(f64_to_u32(u32::max_value() as f64), u32::max_value());
        assert_eq!(f64_to_u32(u32::max_value() as f64 + 0.5), u32::max_value());
        assert_eq!(f64_to_u32(u32::max_value() as f64 + 1.0), u32::max_value());
        assert_eq!(
            f64_to_u32(u32::max_value() as f64 + 10000.0),
            u32::max_value()
        );
        assert_eq!(f64_to_u32(f64::NAN), 0);

        assert_eq!(f64_to_u64(-1.0), 0);
        assert_eq!(f64_to_u64(0.5), 0);
        assert_eq!(f64_to_u64(0.0), 0);
        assert_eq!(f64_to_u64(u64::max_value() as f64), u64::max_value());
        assert_eq!(f64_to_u64(u64::max_value() as f64 + 0.5), u64::max_value());
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
        assert_eq!(f32_to_i32(i32::min_value() as f32 - 0.5), i32::min_value());
        assert_eq!(
            f32_to_i32(i32::min_value() as f32 - 10000.0),
            i32::min_value()
        );
        assert_eq!(f32_to_i32(i32::max_value() as f32), i32::max_value());
        assert_eq!(f32_to_i32(i32::max_value() as f32 + 0.5), i32::max_value());
        assert_eq!(f32_to_i32(i32::max_value() as f32 + 1.0), i32::max_value());
        assert_eq!(
            f32_to_i32(i32::max_value() as f32 + 10000.0),
            i32::max_value()
        );
        assert_eq!(f32_to_i32(f32::NAN), 0);

        assert_eq!(f32_to_i64(0.0), 0);
        assert_eq!(f32_to_i64(i64::min_value() as f32), i64::min_value());
        assert_eq!(f32_to_i64(i64::min_value() as f32 - 1.0), i64::min_value());
        assert_eq!(f32_to_i64(i64::min_value() as f32 - 0.5), i64::min_value());
        assert_eq!(
            f32_to_i64(i64::min_value() as f32 - 10000.0),
            i64::min_value()
        );
        assert_eq!(f32_to_i64(i64::max_value() as f32), i64::max_value());
        assert_eq!(f32_to_i64(i64::max_value() as f32 + 0.5), i64::max_value());
        assert_eq!(f32_to_i64(i64::max_value() as f32 + 1.0), i64::max_value());
        assert_eq!(
            f32_to_i64(i64::max_value() as f32 + 10000.0),
            i64::max_value()
        );
        assert_eq!(f32_to_i64(f32::NAN), 0);

        assert_eq!(f64_to_i32(0.0), 0);
        assert_eq!(f64_to_i32(i32::min_value() as f64), i32::min_value());
        assert_eq!(f64_to_i32(i32::min_value() as f64 - 0.5), i32::min_value());
        assert_eq!(f64_to_i32(i32::min_value() as f64 - 1.0), i32::min_value());
        assert_eq!(
            f64_to_i32(i32::min_value() as f64 - 10000.0),
            i32::min_value()
        );
        assert_eq!(f64_to_i32(i32::max_value() as f64), i32::max_value());
        assert_eq!(f64_to_i32(i32::max_value() as f64 + 0.5), i32::max_value());
        assert_eq!(f64_to_i32(i32::max_value() as f64 + 1.0), i32::max_value());
        assert_eq!(
            f64_to_i32(i32::max_value() as f64 + 10000.0),
            i32::max_value()
        );
        assert_eq!(f64_to_i32(f64::NAN), 0);

        assert_eq!(f64_to_i64(0.0), 0);
        assert_eq!(f64_to_i64(i64::min_value() as f64), i64::min_value());
        assert_eq!(f64_to_i64(i64::min_value() as f64 - 0.5), i64::min_value());
        assert_eq!(f64_to_i64(i64::min_value() as f64 - 1.0), i64::min_value());
        assert_eq!(
            f64_to_i64(i64::min_value() as f64 - 10000.0),
            i64::min_value()
        );
        assert_eq!(f64_to_i64(i64::max_value() as f64), i64::max_value());
        assert_eq!(f64_to_i64(i64::max_value() as f64 + 0.5), i64::max_value());
        assert_eq!(f64_to_i64(i64::max_value() as f64 + 1.0), i64::max_value());
        assert_eq!(
            f64_to_i64(i64::max_value() as f64 + 10000.0),
            i64::max_value()
        );
        assert_eq!(f64_to_i64(f64::NAN), 0);
    }
}
