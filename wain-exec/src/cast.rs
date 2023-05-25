// Float to int conversion. When the float value cannot be represented as target integer type,
// it causes undefined behavior. This is a bug of rustc. We need to handle it until the bug is fixed.
//   https://github.com/rust-lang/rust/issues/10184

macro_rules! float_to_int {
    ($name:ident, $float:ty, $int:ty) => {
        pub fn $name(f: $float) -> Option<$int> {
            const MAX: $float = <$int>::MAX as $float;
            const MIN: $float = <$int>::MIN as $float;
            // Conceptually, f can be represented by the targe type
            // if MIN - 1.0 < f < MAX + 1.0 is satisfied.
            // 1. f < MAX + 1.0 check.
            //    MAX + 1.0 can be represented exactly by the source type whether MAX can be
            //    represented exactly or not.  (Because MAX + 1.0 is always a power of 2.)
            //    So, we can simply check it with f < MAX + 1.0.
            // 2. MIN - 1.0 < f check.
            //    MIN can be represented exactly by any source types (because MIN is
            //    always a power of two.), but MIN - 1.0 is not.
            //    a) If MIN - 1.0 can be represented exactly too,
            //       then we can simply check it with MIN - 1.0 < f.
            //       (In this case, of cource, MIN - 1.0 != MIN.)
            //    b) If MIN - 1.0 can not be represented exactly,
            //       then MIN - 1.0 = MIN.  (Because of "round to nearest even" behavior.)
            //       So, we can check it with MIN <= f.
            let gt_min = if MIN != MIN - 1.0 { MIN - 1.0 < f } else { MIN <= f };

            if gt_min && f < MAX + 1.0 {
                Some(f as $int)
            } else {
                None
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

// The functions below are preserved for future use.
// (Wasm 1.1 has the instruction inn.trunc_sat_fmm_sx that should use these functions.)
// >= and <= are always available for comparing max/min values. Thanks @kariya-mitsuru for discussion
// https://mobile.twitter.com/Linda_pp/status/1245587716537909250
macro_rules! float_to_int_sat {
    ($name:ident, $float:ty, $int:ty) => {
        #[allow(dead_code)]
        pub fn $name(f: $float) -> $int {
            const MAX: $float = <$int>::MAX as $float;
            const MIN: $float = <$int>::MIN as $float;
            if f.is_nan() {
                0
            } else if f >= MAX {
                <$int>::MAX
            } else if f <= MIN {
                <$int>::MIN
            } else {
                f as $int
            }
        }
    };
}

float_to_int_sat!(f32_to_u32_sat, f32, u32);
float_to_int_sat!(f32_to_u64_sat, f32, u64);
float_to_int_sat!(f64_to_u32_sat, f64, u32);
float_to_int_sat!(f64_to_u64_sat, f64, u64);
float_to_int_sat!(f32_to_i32_sat, f32, i32);
float_to_int_sat!(f32_to_i64_sat, f32, i64);
float_to_int_sat!(f64_to_i32_sat, f64, i32);
float_to_int_sat!(f64_to_i64_sat, f64, i64);

#[cfg(test)]
mod tests {
    use super::*;
    use std::{f32, f64};

    trait NextAfter {
        fn next_after(self, _: Self) -> Self;
    }

    impl NextAfter for f32 {
        fn next_after(self, o: f32) -> f32 {
            let b = self.to_bits();
            f32::from_bits(if self.abs() < o.abs() { b + 1 } else { b - 1 })
        }
    }

    impl NextAfter for f64 {
        fn next_after(self, o: f64) -> f64 {
            let b = self.to_bits();
            f64::from_bits(if self.abs() < o.abs() { b + 1 } else { b - 1 })
        }
    }

    #[test]
    fn float_to_uint() {
        assert_eq!(f32_to_u32(-1.0), None);
        assert_eq!(f32_to_u32(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f32_to_u32(-0.0), Some(0));
        assert_eq!(f32_to_u32(0.0), Some(0));
        assert_eq!(f32_to_u32(1.0.next_after(0.0)), Some(0));
        assert_eq!(f32_to_u32(1.0), Some(1));
        assert_eq!(f32_to_u32((u32::MAX as f32).next_after(0.0)), Some(0xffff_ff00));
        assert_eq!(f32_to_u32(u32::MAX as f32), None);
        assert_eq!(f32_to_u32(f32::INFINITY), None);
        assert_eq!(f32_to_u32(f32::NEG_INFINITY), None);
        assert_eq!(f32_to_u32(f32::NAN), None);
        assert_eq!(f32_to_u32(-f32::NAN), None);

        assert_eq!(f32_to_u64(-1.0), None);
        assert_eq!(f32_to_u64(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f32_to_u64(-0.0), Some(0));
        assert_eq!(f32_to_u64(0.0), Some(0));
        assert_eq!(f32_to_u64(1.0.next_after(0.0)), Some(0));
        assert_eq!(f32_to_u64(1.0), Some(1));
        assert_eq!(
            f32_to_u64((u64::MAX as f32).next_after(0.0)),
            Some(0xffff_ff00_0000_0000)
        );
        assert_eq!(f32_to_u64(u64::MAX as f32), None);
        assert_eq!(f32_to_u64(f32::INFINITY), None);
        assert_eq!(f32_to_u64(f32::NEG_INFINITY), None);
        assert_eq!(f32_to_u64(f32::NAN), None);
        assert_eq!(f32_to_u64(-f32::NAN), None);

        assert_eq!(f64_to_u32(-1.0), None);
        assert_eq!(f64_to_u32(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f64_to_u32(-0.0), Some(0));
        assert_eq!(f64_to_u32(0.0), Some(0));
        assert_eq!(f64_to_u32(1.0.next_after(0.0)), Some(0));
        assert_eq!(f64_to_u32(1.0), Some(1));
        assert_eq!(f64_to_u32((u32::MAX as f64 + 1.0).next_after(0.0)), Some(u32::MAX));
        assert_eq!(f64_to_u32(u32::MAX as f64 + 1.0), None);
        assert_eq!(f64_to_u32(f64::INFINITY), None);
        assert_eq!(f64_to_u32(f64::NEG_INFINITY), None);
        assert_eq!(f64_to_u32(f64::NAN), None);
        assert_eq!(f64_to_u32(-f64::NAN), None);

        assert_eq!(f64_to_u64(-1.0), None);
        assert_eq!(f64_to_u64(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f64_to_u64(-0.0), Some(0));
        assert_eq!(f64_to_u64(0.0), Some(0));
        assert_eq!(f64_to_u64(1.0.next_after(0.0)), Some(0));
        assert_eq!(f64_to_u64(1.0), Some(1));
        assert_eq!(
            f64_to_u64((u64::MAX as f64).next_after(0.0)),
            Some(0xffff_ffff_ffff_f800)
        );
        assert_eq!(f64_to_u64(u64::MAX as f64), None);
        assert_eq!(f64_to_u64(f64::INFINITY), None);
        assert_eq!(f64_to_u64(f64::NEG_INFINITY), None);
        assert_eq!(f64_to_u64(f64::NAN), None);
        assert_eq!(f64_to_u64(-f64::NAN), None);
    }

    #[test]
    fn float_to_int() {
        assert_eq!(f32_to_i32((i32::MIN as f32).next_after(f32::NEG_INFINITY)), None);
        assert_eq!(f32_to_i32(i32::MIN as f32), Some(i32::MIN));
        assert_eq!(f32_to_i32(-1.0), Some(-1));
        assert_eq!(f32_to_i32(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f32_to_i32(-0.0), Some(0));
        assert_eq!(f32_to_i32(0.0), Some(0));
        assert_eq!(f32_to_i32(1.0.next_after(0.0)), Some(0));
        assert_eq!(f32_to_i32(1.0), Some(1));
        assert_eq!(f32_to_i32((i32::MAX as f32).next_after(0.0)), Some(0x7fff_ff80));
        assert_eq!(f32_to_i32(i32::MAX as f32), None);
        assert_eq!(f32_to_i32(f32::INFINITY), None);
        assert_eq!(f32_to_i32(f32::NEG_INFINITY), None);
        assert_eq!(f32_to_i32(f32::NAN), None);
        assert_eq!(f32_to_i32(-f32::NAN), None);

        assert_eq!(f32_to_i64((i64::MIN as f32).next_after(f32::NEG_INFINITY)), None);
        assert_eq!(f32_to_i64(i64::MIN as f32), Some(i64::MIN));
        assert_eq!(f32_to_i64(-1.0), Some(-1));
        assert_eq!(f32_to_i64(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f32_to_i64(-0.0), Some(0));
        assert_eq!(f32_to_i64(0.0), Some(0));
        assert_eq!(f32_to_i64(1.0.next_after(0.0)), Some(0));
        assert_eq!(f32_to_i64(1.0), Some(1));
        assert_eq!(
            f32_to_i64((i64::MAX as f32).next_after(0.0)),
            Some(0x7fff_ff80_0000_0000)
        );
        assert_eq!(f32_to_i64(i64::MAX as f32), None);
        assert_eq!(f32_to_i64(f32::INFINITY), None);
        assert_eq!(f32_to_i64(f32::NEG_INFINITY), None);
        assert_eq!(f32_to_i64(f32::NAN), None);
        assert_eq!(f32_to_i64(-f32::NAN), None);

        assert_eq!(f64_to_i32(i32::MIN as f64 - 1.0), None);
        assert_eq!(f64_to_i32((i32::MIN as f64 - 1.0).next_after(-0.0)), Some(i32::MIN));
        assert_eq!(f64_to_i32(-1.0), Some(-1));
        assert_eq!(f64_to_i32(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f64_to_i32(-0.0), Some(0));
        assert_eq!(f64_to_i32(0.0), Some(0));
        assert_eq!(f64_to_i32(1.0.next_after(0.0)), Some(0));
        assert_eq!(f64_to_i32(1.0), Some(1));
        assert_eq!(f64_to_i32((i32::MAX as f64 + 1.0).next_after(0.0)), Some(i32::MAX));
        assert_eq!(f64_to_i32(i32::MAX as f64 + 1.0), None);
        assert_eq!(f64_to_i32(f64::INFINITY), None);
        assert_eq!(f64_to_i32(f64::NEG_INFINITY), None);
        assert_eq!(f64_to_i32(f64::NAN), None);
        assert_eq!(f64_to_i32(-f64::NAN), None);

        assert_eq!(f64_to_i64((i64::MIN as f64).next_after(f64::NEG_INFINITY)), None);
        assert_eq!(f64_to_i64(i64::MIN as f64), Some(i64::MIN));
        assert_eq!(f64_to_i64(-1.0), Some(-1));
        assert_eq!(f64_to_i64(-(1.0.next_after(-0.0))), Some(0));
        assert_eq!(f64_to_i64(-0.0), Some(0));
        assert_eq!(f64_to_i64(0.0), Some(0));
        assert_eq!(f64_to_i64(1.0.next_after(0.0)), Some(0));
        assert_eq!(f64_to_i64(1.0), Some(1));
        assert_eq!(
            f64_to_i64((i64::MAX as f64).next_after(0.0)),
            Some(0x7fff_ffff_ffff_fc00)
        );
        assert_eq!(f64_to_i64(i64::MAX as f64), None);
        assert_eq!(f64_to_i64(f64::INFINITY), None);
        assert_eq!(f64_to_i64(f64::NEG_INFINITY), None);
        assert_eq!(f64_to_i64(f64::NAN), None);
        assert_eq!(f64_to_i64(-f64::NAN), None);
    }

    #[test]
    fn float_to_uint_sat() {
        assert_eq!(f32_to_u32_sat(-1.0), 0);
        assert_eq!(f32_to_u32_sat(0.5), 0);
        assert_eq!(f32_to_u32_sat(0.0), 0);
        assert_eq!(f32_to_u32_sat(u32::MAX as f32), u32::MAX);
        assert_eq!(f32_to_u32_sat(u32::MAX as f32 + 0.5), u32::MAX);
        assert_eq!(f32_to_u32_sat(u32::MAX as f32 + 1.0), u32::MAX);
        assert_eq!(f32_to_u32_sat(u32::MAX as f32 + 10000.0), u32::MAX);
        assert_eq!(f32_to_u32_sat(f32::NAN), 0);

        assert_eq!(f32_to_u64_sat(-1.0), 0);
        assert_eq!(f32_to_u64_sat(0.5), 0);
        assert_eq!(f32_to_u64_sat(0.0), 0);
        assert_eq!(f32_to_u64_sat(u64::MAX as f32), u64::MAX);
        assert_eq!(f32_to_u64_sat(u64::MAX as f32 + 0.5), u64::MAX);
        assert_eq!(f32_to_u64_sat(u64::MAX as f32 + 1.0), u64::MAX);
        assert_eq!(f32_to_u64_sat(u64::MAX as f32 + 10000.0), u64::MAX);
        assert_eq!(f32_to_u64_sat(f32::NAN), 0);

        assert_eq!(f64_to_u32_sat(-1.0), 0);
        assert_eq!(f64_to_u32_sat(0.5), 0);
        assert_eq!(f64_to_u32_sat(0.0), 0);
        assert_eq!(f64_to_u32_sat(u32::MAX as f64), u32::MAX);
        assert_eq!(f64_to_u32_sat(u32::MAX as f64 + 0.5), u32::MAX);
        assert_eq!(f64_to_u32_sat(u32::MAX as f64 + 1.0), u32::MAX);
        assert_eq!(f64_to_u32_sat(u32::MAX as f64 + 10000.0), u32::MAX);
        assert_eq!(f64_to_u32_sat(f64::NAN), 0);

        assert_eq!(f64_to_u64_sat(-1.0), 0);
        assert_eq!(f64_to_u64_sat(0.5), 0);
        assert_eq!(f64_to_u64_sat(0.0), 0);
        assert_eq!(f64_to_u64_sat(u64::MAX as f64), u64::MAX);
        assert_eq!(f64_to_u64_sat(u64::MAX as f64 + 0.5), u64::MAX);
        assert_eq!(f64_to_u64_sat(u64::MAX as f64 + 1.0), u64::MAX);
        assert_eq!(f64_to_u64_sat(u64::MAX as f64 + 10000.0), u64::MAX);
        assert_eq!(f64_to_u64_sat(f64::NAN), 0);
    }

    #[test]
    fn float_to_int_sat() {
        assert_eq!(f32_to_i32_sat(0.0), 0);
        assert_eq!(f32_to_i32_sat(i32::MIN as f32), i32::MIN);
        assert_eq!(f32_to_i32_sat(i32::MIN as f32 - 1.0), i32::MIN);
        assert_eq!(f32_to_i32_sat(i32::MIN as f32 - 0.5), i32::MIN);
        assert_eq!(f32_to_i32_sat(i32::MIN as f32 - 10000.0), i32::MIN);
        assert_eq!(f32_to_i32_sat(i32::MAX as f32), i32::MAX);
        assert_eq!(f32_to_i32_sat(i32::MAX as f32 + 0.5), i32::MAX);
        assert_eq!(f32_to_i32_sat(i32::MAX as f32 + 1.0), i32::MAX);
        assert_eq!(f32_to_i32_sat(i32::MAX as f32 + 10000.0), i32::MAX);
        assert_eq!(f32_to_i32_sat(f32::NAN), 0);

        assert_eq!(f32_to_i64_sat(0.0), 0);
        assert_eq!(f32_to_i64_sat(i64::MIN as f32), i64::MIN);
        assert_eq!(f32_to_i64_sat(i64::MIN as f32 - 1.0), i64::MIN);
        assert_eq!(f32_to_i64_sat(i64::MIN as f32 - 0.5), i64::MIN);
        assert_eq!(f32_to_i64_sat(i64::MIN as f32 - 10000.0), i64::MIN);
        assert_eq!(f32_to_i64_sat(i64::MAX as f32), i64::MAX);
        assert_eq!(f32_to_i64_sat(i64::MAX as f32 + 0.5), i64::MAX);
        assert_eq!(f32_to_i64_sat(i64::MAX as f32 + 1.0), i64::MAX);
        assert_eq!(f32_to_i64_sat(i64::MAX as f32 + 10000.0), i64::MAX);
        assert_eq!(f32_to_i64_sat(f32::NAN), 0);

        assert_eq!(f64_to_i32_sat(0.0), 0);
        assert_eq!(f64_to_i32_sat(i32::MIN as f64), i32::MIN);
        assert_eq!(f64_to_i32_sat(i32::MIN as f64 - 0.5), i32::MIN);
        assert_eq!(f64_to_i32_sat(i32::MIN as f64 - 1.0), i32::MIN);
        assert_eq!(f64_to_i32_sat(i32::MIN as f64 - 10000.0), i32::MIN);
        assert_eq!(f64_to_i32_sat(i32::MAX as f64), i32::MAX);
        assert_eq!(f64_to_i32_sat(i32::MAX as f64 + 0.5), i32::MAX);
        assert_eq!(f64_to_i32_sat(i32::MAX as f64 + 1.0), i32::MAX);
        assert_eq!(f64_to_i32_sat(i32::MAX as f64 + 10000.0), i32::MAX);
        assert_eq!(f64_to_i32_sat(f64::NAN), 0);

        assert_eq!(f64_to_i64_sat(0.0), 0);
        assert_eq!(f64_to_i64_sat(i64::MIN as f64), i64::MIN);
        assert_eq!(f64_to_i64_sat(i64::MIN as f64 - 0.5), i64::MIN);
        assert_eq!(f64_to_i64_sat(i64::MIN as f64 - 1.0), i64::MIN);
        assert_eq!(f64_to_i64_sat(i64::MIN as f64 - 10000.0), i64::MIN);
        assert_eq!(f64_to_i64_sat(i64::MAX as f64), i64::MAX);
        assert_eq!(f64_to_i64_sat(i64::MAX as f64 + 0.5), i64::MAX);
        assert_eq!(f64_to_i64_sat(i64::MAX as f64 + 1.0), i64::MAX);
        assert_eq!(f64_to_i64_sat(i64::MAX as f64 + 10000.0), i64::MAX);
        assert_eq!(f64_to_i64_sat(f64::NAN), 0);
    }
}
