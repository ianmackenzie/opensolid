use std::ops::{Add, Mul, Sub};

#[derive(Copy, Clone)]
pub struct Bounds {
    pub low: f64,
    pub high: f64,
}

impl Bounds {
    pub fn singleton(value: f64) -> Bounds {
        Bounds {
            low: value,
            high: value,
        }
    }

    pub fn hull2(a: f64, b: f64) -> Bounds {
        Bounds {
            low: f64::min(a, b),
            high: f64::max(a, b),
        }
    }
}

impl Add<Bounds> for f64 {
    type Output = Bounds;

    fn add(self, rhs: Bounds) -> Bounds {
        Bounds {
            low: self + rhs.low,
            high: self + rhs.high,
        }
    }
}

impl Add for Bounds {
    type Output = Bounds;

    fn add(self, rhs: Bounds) -> Bounds {
        Bounds {
            low: self.low + rhs.low,
            high: self.high + rhs.high,
        }
    }
}

impl Sub for Bounds {
    type Output = Bounds;
    fn sub(self, rhs: Bounds) -> Bounds {
        Bounds {
            low: self.low - rhs.high,
            high: self.high - rhs.low,
        }
    }
}

impl Mul<Bounds> for f64 {
    type Output = Bounds;

    fn mul(self, rhs: Bounds) -> Bounds {
        if self >= 0.0 {
            Bounds {
                low: self * rhs.low,
                high: self * rhs.high,
            }
        } else {
            Bounds {
                low: self * rhs.high,
                high: self * rhs.low,
            }
        }
    }
}

impl Mul<f64> for Bounds {
    type Output = Bounds;

    fn mul(self, rhs: f64) -> Bounds {
        if rhs >= 0.0 {
            Bounds {
                low: self.low * rhs,
                high: self.high * rhs,
            }
        } else {
            Bounds {
                low: self.high * rhs,
                high: self.low * rhs,
            }
        }
    }
}

impl Mul for Bounds {
    type Output = Bounds;
    fn mul(self, rhs: Bounds) -> Bounds {
        let ll = self.low * rhs.low;
        let lh = self.low * rhs.high;
        let hl = self.high * rhs.low;
        let hh = self.high * rhs.high;
        Bounds {
            low: ll.min(lh).min(hl).min(hh),
            high: ll.max(lh).max(hl).max(hh),
        }
    }
}
