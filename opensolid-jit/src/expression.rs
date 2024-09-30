pub struct Constant(pub f64);

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for Constant {}

impl std::hash::Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}

#[derive(PartialEq, Eq, Hash)]
pub enum Expression {
    Argument(i64),
    Constant(Constant),
    Negate(Box<Expression>),
    Sum(Box<Expression>, Box<Expression>),
    Difference(Box<Expression>, Box<Expression>),
    Product(Box<Expression>, Box<Expression>),
    Quotient(Box<Expression>, Box<Expression>),
    SquareRoot(Box<Expression>),
    Sine(Box<Expression>),
    Cosine(Box<Expression>),
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
pub extern "C" fn opensolid_jit_constant(value: f64) -> *mut Expression {
    let constant = Constant(value);
    Expression::Constant(constant).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_jit_argument(index: i64) -> *mut Expression {
    Expression::Argument(index).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_jit_negate(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    if let Expression::Constant(constant) = *arg {
        Expression::Constant(Constant(-constant.0)).to_c()
    } else {
        Expression::Negate(arg).to_c()
    }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_sum(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    Expression::Sum(lhs, rhs).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_jit_difference(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    Expression::Difference(lhs, rhs).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_jit_product(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    Expression::Product(lhs, rhs).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_jit_quotient(
    ptr1: *mut Expression,
    ptr2: *mut Expression,
) -> *mut Expression {
    let lhs = Expression::from_c(ptr1);
    let rhs = Expression::from_c(ptr2);
    Expression::Quotient(lhs, rhs).to_c()
}

#[no_mangle]
pub extern "C" fn opensolid_jit_sqrt(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    if let Expression::Constant(constant) = *arg {
        Expression::Constant(Constant(constant.0.sqrt())).to_c()
    } else {
        Expression::SquareRoot(arg).to_c()
    }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_sin(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    if let Expression::Constant(constant) = *arg {
        Expression::Constant(Constant(constant.0.sin())).to_c()
    } else {
        Expression::Sine(arg).to_c()
    }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_cos(ptr: *mut Expression) -> *mut Expression {
    let arg = Expression::from_c(ptr);
    if let Expression::Constant(constant) = *arg {
        Expression::Constant(Constant(constant.0.cos())).to_c()
    } else {
        Expression::Cosine(arg).to_c()
    }
}
