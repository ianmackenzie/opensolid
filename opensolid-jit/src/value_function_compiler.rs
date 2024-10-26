use std::collections::HashMap;

use cranelift::prelude::{
    Block, FunctionBuilder, FunctionBuilderContext, InstBuilder, Value, Variable,
};
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::{FuncRef, Function, Inst, MemFlags};

use crate::builtins::Builtins;
use crate::expression::Expression;

pub struct ValueFunctionCompiler<'a> {
    function_builder: FunctionBuilder<'a>,
    mem_flags: MemFlags,
    builtins: Builtins,
    output_argument_index: usize,
    entry_block: Block,
    evaluated: HashMap<&'a Expression, Variable>,
}

impl<'a> ValueFunctionCompiler<'a> {
    pub fn new(
        builtins: Builtins,
        mem_flags: MemFlags,
        output_argument_index: usize,
        function: &'a mut Function,
        builder_context: &'a mut FunctionBuilderContext,
    ) -> ValueFunctionCompiler<'a> {
        let mut function_builder = FunctionBuilder::new(function, builder_context);
        let entry_block = function_builder.create_block();
        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);

        ValueFunctionCompiler {
            function_builder,
            mem_flags,
            builtins,
            output_argument_index,
            entry_block,
            evaluated: HashMap::new(),
        }
    }

    fn constant_value(&mut self, value: f64) -> Value {
        self.function_builder.ins().f64const(value)
    }

    fn call(&mut self, func_ref: FuncRef, args: &[Value]) -> Inst {
        self.function_builder.ins().call(func_ref, args)
    }

    fn define_value(&mut self, expression: &'a Expression, value: Value) -> Value {
        let variable = Variable::from_u32(self.evaluated.len() as u32);
        self.function_builder.declare_var(variable, F64);
        self.function_builder.def_var(variable, value);
        self.evaluated.insert(expression, variable);
        self.function_builder.use_var(variable)
    }

    pub fn compute_value(&mut self, expression: &'a Expression) -> Value {
        match self.evaluated.get(&expression) {
            Some(variable) => self.function_builder.use_var(*variable),
            None => match expression {
                Expression::Argument(index) => {
                    self.function_builder.block_params(self.entry_block)[*index as usize]
                }
                Expression::Constant(constant) => self.constant_value(constant.value),
                Expression::Negate(arg) => {
                    let arg_value = self.compute_value(&arg);
                    let negated_value = self.function_builder.ins().fneg(arg_value);
                    self.define_value(expression, negated_value)
                }
                Expression::Sum(lhs, rhs) => {
                    let lhs_value = self.compute_value(&lhs);
                    let rhs_value = self.compute_value(&rhs);
                    let sum_value = self.function_builder.ins().fadd(lhs_value, rhs_value);
                    self.define_value(expression, sum_value)
                }
                Expression::Difference(lhs, rhs) => {
                    let lhs_value = self.compute_value(&lhs);
                    let rhs_value = self.compute_value(&rhs);
                    let difference_value = self.function_builder.ins().fsub(lhs_value, rhs_value);
                    self.define_value(expression, difference_value)
                }
                Expression::Product(lhs, rhs) => {
                    let lhs_value = self.compute_value(&lhs);
                    let rhs_value = self.compute_value(&rhs);
                    let product_value = self.function_builder.ins().fmul(lhs_value, rhs_value);
                    self.define_value(expression, product_value)
                }
                Expression::Quotient(lhs, rhs) => {
                    let lhs_value = self.compute_value(&lhs);
                    let rhs_value = self.compute_value(&rhs);
                    let quotient_value = self.function_builder.ins().fdiv(lhs_value, rhs_value);
                    self.define_value(expression, quotient_value)
                }
                Expression::Squared(arg) => {
                    let arg_value = self.compute_value(&arg);
                    let squared_value = self.function_builder.ins().fmul(arg_value, arg_value);
                    self.define_value(expression, squared_value)
                }
                Expression::SquareRoot(arg) => {
                    let arg_value = self.compute_value(&arg);
                    let sqrt_inst = self.call(self.builtins.sqrt, &[arg_value]);
                    let sqrt_value = self.function_builder.inst_results(sqrt_inst)[0];
                    self.define_value(expression, sqrt_value)
                }
                Expression::Sine(arg) => {
                    let arg_value = self.compute_value(&arg);
                    let sin_inst = self.call(self.builtins.sin, &[arg_value]);
                    let sin_value = self.function_builder.inst_results(sin_inst)[0];
                    self.define_value(expression, sin_value)
                }
                Expression::Cosine(arg) => {
                    let arg_value = self.compute_value(&arg);
                    let cos_inst = self.call(self.builtins.cos, &[arg_value]);
                    let cos_value = self.function_builder.inst_results(cos_inst)[0];
                    self.define_value(expression, cos_value)
                }
                Expression::QuadraticSpline(p1, p2, p3, t) => {
                    let p1_value = self.constant_value(p1.value);
                    let p2_value = self.constant_value(p2.value);
                    let p3_value = self.constant_value(p3.value);
                    let t_value = self.compute_value(t);
                    let function_arguments = [p1_value, p2_value, p3_value, t_value];
                    let function_call =
                        self.call(self.builtins.quadratic_spline, &function_arguments);
                    let spline_value = self.function_builder.inst_results(function_call)[0];
                    self.define_value(expression, spline_value)
                }
                Expression::CubicSpline(p1, p2, p3, p4, t) => {
                    let p1_value = self.constant_value(p1.value);
                    let p2_value = self.constant_value(p2.value);
                    let p3_value = self.constant_value(p3.value);
                    let p4_value = self.constant_value(p4.value);
                    let t_value = self.compute_value(t);
                    let function_arguments = [p1_value, p2_value, p3_value, p4_value, t_value];
                    let function_call = self.call(self.builtins.cubic_spline, &function_arguments);
                    let spline_value = self.function_builder.inst_results(function_call)[0];
                    self.define_value(expression, spline_value)
                }
            },
        }
    }

    pub fn return_value(mut self, value: Value) {
        self.function_builder.ins().return_(&[value]);
        self.function_builder.finalize();
    }

    pub fn write_output_value(&mut self, index: usize, value: Value) {
        let output_pointer =
            self.function_builder.block_params(self.entry_block)[self.output_argument_index];
        let byte_offset = 8 * index as i32;
        self.function_builder
            .ins()
            .store(self.mem_flags, value, output_pointer, byte_offset);
    }

    pub fn return_void(mut self) {
        self.function_builder.ins().return_(&[]);
        self.function_builder.finalize();
    }
}
