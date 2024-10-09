use std::f64;
use std::f64::consts::{FRAC_PI_2, FRAC_PI_4, PI};

#[no_mangle]
pub extern "C" fn opensolid_bounds_sin(
    in_low: f64,
    in_high: f64,
    out_low: *mut f64,
    out_high: *mut f64,
) {
    let sin_low = f64::sin(in_low);
    let sin_high = f64::sin(in_high);
    let low = if contains_sinusoidal_extreme(in_low, in_high, -FRAC_PI_4) {
        -1.0
    } else {
        f64::min(sin_low, sin_high)
    };
    let high = if contains_sinusoidal_extreme(in_low, in_high, FRAC_PI_4) {
        1.0
    } else {
        f64::max(sin_low, sin_high)
    };
    unsafe {
        *out_low = low;
        *out_high = high;
    }
}

#[no_mangle]
pub extern "C" fn opensolid_bounds_cos(
    in_low: f64,
    in_high: f64,
    out_low: *mut f64,
    out_high: *mut f64,
) {
    let cos_low = f64::cos(in_low);
    let cos_high = f64::cos(in_high);
    let low = if contains_sinusoidal_extreme(in_low, in_high, FRAC_PI_2) {
        -1.0
    } else {
        f64::min(cos_low, cos_high)
    };
    let high = if contains_sinusoidal_extreme(in_low, in_high, 0.0) {
        1.0
    } else {
        f64::max(cos_low, cos_high)
    };
    unsafe {
        *out_low = low;
        *out_high = high;
    }
}

fn contains_sinusoidal_extreme(low: f64, high: f64, location: f64) -> bool {
    let low_index = f64::floor((low - location) / (2.0 * PI));
    let high_index = f64::floor((high - location) / (2.0 * PI));
    low_index != high_index
}
