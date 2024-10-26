mod bounds;
mod bounds_function_compiler;
mod builtins;
mod expression;
mod jit;
mod module_compiler;
mod value_function_compiler;

#[cfg(test)]
mod tests {
    use crate::expression::{Constant, Expression};
    use crate::jit;
    use std::f64::consts::PI;

    #[test]
    fn test_basic_arithmetic() {
        let expression = Expression::Quotient(
            Box::new(Expression::Argument(0)),
            Box::new(Expression::Sum(
                Box::new(Expression::Argument(0)),
                Box::new(Expression::Constant(Constant::new(1.0))),
            )),
        );
        let compiled = jit::value_function(1, &[&expression]);
        let function = unsafe { std::mem::transmute::<_, fn(f64) -> f64>(compiled) };
        assert_eq!(function(3.0), 0.75);
    }

    #[test]
    fn test_sqrt() {
        let expression = Expression::SquareRoot(Box::new(Expression::Sum(
            Box::new(Expression::Argument(0)),
            Box::new(Expression::Constant(Constant::new(1.0))),
        )));
        let compiled = jit::value_function(1, &[&expression]);
        let function = unsafe { std::mem::transmute::<_, fn(f64) -> f64>(compiled) };
        assert_eq!(function(8.0), 3.0);
    }

    #[test]
    fn test_curve2d() {
        let x = Expression::Product(
            Box::new(Expression::Constant(Constant::new(3.0))),
            Box::new(Expression::Argument(0)),
        );
        let y = Expression::SquareRoot(Box::new(Expression::Argument(0)));
        let compiled = jit::value_function(1, &[&x, &y]);
        let function = unsafe { std::mem::transmute::<_, fn(f64, *mut f64)>(compiled) };
        let mut output: [f64; 2] = [0.0, 0.0];
        function(0.5, output.as_mut_ptr());
        assert_eq!(output[0], 1.5);
        assert_eq!(output[1], (0.5 as f64).sqrt());
    }

    #[test]
    fn test_bounds() {
        let x = Expression::Argument(0);
        let x_squared = Expression::Product(Box::new(x.clone()), Box::new(x));
        let x_squared_plus_one = Expression::Sum(
            Box::new(x_squared.clone()),
            Box::new(Expression::Constant(Constant::new(1.0))),
        );
        let expression = Expression::Quotient(
            Box::new(x_squared.clone()),
            Box::new(x_squared_plus_one.clone()),
        );
        let compiled = jit::bounds_function(1, &[&expression]);
        let function = unsafe { std::mem::transmute::<_, fn(f64, f64, *mut f64) -> ()>(compiled) };
        let mut output: [f64; 2] = [0.0, 0.0];
        function(1.0, 2.0, output.as_mut_ptr());
        assert_eq!(output[0], 0.2);
        assert_eq!(output[1], 2.0);
    }

    #[test]
    fn test_sin_bounds() {
        let expression = Expression::Sine(Box::new(Expression::Argument(0)));
        let compiled = jit::bounds_function(1, &[&expression]);
        let function = unsafe { std::mem::transmute::<_, fn(f64, f64, *mut f64) -> ()>(compiled) };
        let mut output: [f64; 2] = [0.0, 0.0];
        function(PI / 4.0, 5.0 * PI / 4.0, output.as_mut_ptr());
        assert_eq!(output[0], -0.7071067811865475);
        assert_eq!(output[1], 1.0);
    }
}
