mod builtins;
mod expression;
mod function_compiler;
mod jit;
mod module_compiler;

#[cfg(test)]
mod tests {
    use crate::expression::{Constant, Expression};
    use crate::jit;

    #[test]
    fn test_basic_arithmetic() {
        let expression = Expression::Quotient(
            Box::new(Expression::Argument(0)),
            Box::new(Expression::Sum(
                Box::new(Expression::Argument(0)),
                Box::new(Expression::Constant(Constant(1.0))),
            )),
        );
        let compiled = jit::compile_value(1, &[&expression]);
        let function = unsafe { std::mem::transmute::<_, fn(f64) -> f64>(compiled) };
        assert_eq!(function(3.0), 0.75);
    }

    #[test]
    fn test_sqrt() {
        let expression = Expression::SquareRoot(Box::new(Expression::Sum(
            Box::new(Expression::Argument(0)),
            Box::new(Expression::Constant(Constant(1.0))),
        )));
        let compiled = jit::compile_value(1, &[&expression]);
        let function = unsafe { std::mem::transmute::<_, fn(f64) -> f64>(compiled) };
        assert_eq!(function(8.0), 3.0);
    }

    #[test]
    fn test_curve2d() {
        let x = Expression::Product(
            Box::new(Expression::Constant(Constant(3.0))),
            Box::new(Expression::Argument(0)),
        );
        let y = Expression::SquareRoot(Box::new(Expression::Argument(0)));
        let compiled = jit::compile_value(1, &[&x, &y]);
        let function = unsafe { std::mem::transmute::<_, fn(f64, *mut f64)>(compiled) };
        let mut output: [f64; 2] = [0.0, 0.0];
        function(0.5, output.as_mut_ptr());
        assert_eq!(output[0], 1.5);
        assert_eq!(output[1], (0.5 as f64).sqrt());
    }
}
