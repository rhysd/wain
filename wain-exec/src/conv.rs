// Float to int conversion. When the float value cannot be represented as target integer type,
// it causes undefined behavior. This is a bug of rustc. We need to handle it until the bug is fixed.
//   https://github.com/rust-lang/rust/issues/10184

macro_rules! float_to_uint {
    ($name:ident, $float:ty, $int:ty, $op:tt) => {
        pub fn $name(f: $float) -> $int {
            const MAX: $float = <$int>::max_value() as $float;
            if f.is_nan() {
                0
            } else if f $op MAX {
                <$int>::max_value()
            } else {
                f as $int
            }
        }
    }
}

// u32_max: 4294967295 -> f32: 4294967300
float_to_uint!(f32_to_u32, f32, u32, >=);
// u64_max: 18446744073709551615 -> f32: 18446744000000000000
float_to_uint!(f32_to_u64, f32, u64, >);
// u32_max: 4294967295 -> f64: 4294967295
float_to_uint!(f64_to_u32, f64, u32, >);
// u64_max: 18446744073709551615 -> f64: 18446744073709552000
float_to_uint!(f64_to_u64, f64, u64, >=);

macro_rules! float_to_int {
    ($name:ident, $float:ty, $int:ty, $op_max:tt, $op_min:tt) => {
        pub fn $name(f: $float) -> $int {
            const MAX: $float = <$int>::max_value() as $float;
            const MIN: $float = <$int>::min_value() as $float;
            if f.is_nan() {
                0
            } else if f $op_max MAX {
                <$int>::max_value()
            } else if f $op_min MIN {
                <$int>::min_value()
            } else {
                f as $int
            }
        }
    }
}

// i32_max: 2147483647 -> f32: 2147483600
// i32_min: -2147483648 -> f32: -2147483600
float_to_int!(f32_to_i32, f32, i32, >, <);
// i64_max: 9223372036854775807 -> f32: 9223372000000000000
// i64_min: -9223372036854775808 -> f32: -9223372000000000000
float_to_int!(f32_to_i64, f32, i64, >, <);
// i32_max: 2147483647 -> f64: 2147483647
// i32_min: -2147483648 -> f64: -2147483648
float_to_int!(f64_to_i32, f64, i32, >, <);
// i64_max: 9223372036854775807 -> f64: 9223372036854776000
// i64_min: -9223372036854775808 -> f64: -9223372036854776000
float_to_int!(f64_to_i64, f64, i64, >=, <=);
