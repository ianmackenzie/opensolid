#[derive(Copy, Clone)]
pub struct Constant {
    pub value: f64,
}

impl Constant {
    pub fn new(value: f64) -> Constant {
        Constant { value }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        self.value.to_bits() == other.value.to_bits()
    }
}

impl Eq for Constant {}

impl PartialOrd for Constant {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.to_bits().partial_cmp(&other.value.to_bits())
    }
}

impl Ord for Constant {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.to_bits().cmp(&other.value.to_bits())
    }
}

impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.to_bits().hash(state);
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum Expression {
    Argument(i64),
    Constant(Constant),
    Negate(Box<Expression>),
    Sum(Box<Expression>, Box<Expression>),
    Difference(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Quotient(Box<Expression>, Box<Expression>),
    Squared(Box<Expression>),
    SquareRoot(Box<Expression>),
    Sine(Box<Expression>),
    Cosine(Box<Expression>),
    QuadraticSpline(Constant, Constant, Constant, Box<Expression>),
    CubicSpline(Constant, Constant, Constant, Constant, Box<Expression>),
    QuarticSpline(
        Constant,
        Constant,
        Constant,
        Constant,
        Constant,
        Box<Expression>,
    ),
    QuinticSpline(
        Constant,
        Constant,
        Constant,
        Constant,
        Constant,
        Constant,
        Box<Expression>,
    ),
    BezierCurve(Vec<Constant>, Box<Expression>),
}

impl Expression {
    fn to_c(self) -> *mut Expression {
        Box::into_raw(Box::new(self))
    }

    fn from_c(ptr: *mut Expression) -> Box<Expression> {
        unsafe { Box::from_raw(ptr) }
    }
}

#[no_mangle]
pub extern "C" fn opensolid_expression_constant(value: f64) -> *mut Expression {
    let constant = Constant::new(value);
    Expression::Constant(constant).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_argument(index: i64) -> *mut Expression {
    Expression::Argument(index).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_negate(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    Expression::Negate(arg).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_sum(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    if *lhs <= *rhs {
        Expression::Sum(lhs, rhs).to_c()
    } else {
        Expression::Sum(rhs, lhs).to_c()
    }
}

#[no_mangle]
pub extern "C" fn opensolid_expression_difference(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    Expression::Difference(lhs, rhs).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_product(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    if *lhs <= *rhs {
        Expression::Product(lhs, rhs).to_c()
    } else {
        Expression::Product(rhs, lhs).to_c()
    }
}

#[no_mangle]
pub extern "C" fn opensolid_expression_quotient(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    Expression::Quotient(lhs, rhs).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_squared(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    Expression::Squared(arg).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_sqrt(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    Expression::SquareRoot(arg).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_sin(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    Expression::Sine(arg).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_cos(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    Expression::Cosine(arg).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_quadratic_spline(
    p1: f64,
    p2: f64,
    p3: f64,
    t: *mut Expression,
) -> *mut Expression {
    Expression::QuadraticSpline(
        Constant::new(p1),
        Constant::new(p2),
        Constant::new(p3),
        Expression::from_c(t),
    )
    .to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_cubic_spline(
    p1: f64,
    p2: f64,
    p3: f64,
    p4: f64,
    t: *mut Expression,
) -> *mut Expression {
    Expression::CubicSpline(
        Constant::new(p1),
        Constant::new(p2),
        Constant::new(p3),
        Constant::new(p4),
        Expression::from_c(t),
    )
    .to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_quartic_spline(
    p1: f64,
    p2: f64,
    p3: f64,
    p4: f64,
    p5: f64,
    t: *mut Expression,
) -> *mut Expression {
    Expression::QuarticSpline(
        Constant::new(p1),
        Constant::new(p2),
        Constant::new(p3),
        Constant::new(p4),
        Constant::new(p5),
        Expression::from_c(t),
    )
    .to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_quintic_spline(
    p1: f64,
    p2: f64,
    p3: f64,
    p4: f64,
    p5: f64,
    p6: f64,
    t: *mut Expression,
) -> *mut Expression {
    Expression::QuinticSpline(
        Constant::new(p1),
        Constant::new(p2),
        Constant::new(p3),
        Constant::new(p4),
        Constant::new(p5),
        Constant::new(p6),
        Expression::from_c(t),
    )
    .to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_expression_bezier_curve(
    num_control_points: i64,
    control_points: *const f64,
    t: *mut Expression,
) -> *mut Expression {
    let control_points_vec =
        unsafe { std::slice::from_raw_parts(control_points, num_control_points as usize) }
            .iter()
            .map(|value| Constant::new(*value))
            .collect();
    Expression::BezierCurve(control_points_vec, Expression::from_c(t)).to_c()
}
