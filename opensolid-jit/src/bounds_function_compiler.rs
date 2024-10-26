use std::collections::HashMap;
use std::f64;

use cranelift::prelude::{
    Block, FunctionBuilder, FunctionBuilderContext, InstBuilder, Type, Value, Variable,
};
use cranelift_codegen::ir::condcodes::FloatCC;
use cranelift_codegen::ir::stackslot::{StackSlotData, StackSlotKind};
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::{FuncRef, Function, Inst, MemFlags, StackSlot};

use crate::builtins::Builtins;
use crate::expression::Expression;

pub struct BoundsFunctionCompiler<'a> {
    function_builder: FunctionBuilder<'a>,
    mem_flags: MemFlags,
    pointer_type: Type,
    builtins: Builtins,
    output_argument_index: usize,
    entry_block: Block,
    evaluated: HashMap<&'a Expression, (Variable, Variable)>,
    output_slot: StackSlot,
}

impl<'a> BoundsFunctionCompiler<'a> {
    pub fn new(
        builtins: Builtins,
        mem_flags: MemFlags,
        pointer_type: Type,
        output_argument_index: usize,
        function: &'a mut Function,
        builder_context: &'a mut FunctionBuilderContext,
    ) -> BoundsFunctionCompiler<'a> {
        let mut function_builder = FunctionBuilder::new(function, builder_context);
        let entry_block = function_builder.create_block();
        function_builder.append_block_params_for_function_params(entry_block);
        function_builder.switch_to_block(entry_block);
        function_builder.seal_block(entry_block);
        // Space for two f64 values on the stack,
        // to act as return values from bounds functions
        // via output pointers
        let output_slot = function_builder.create_sized_stack_slot(StackSlotData {
            // Fixed size
            kind: StackSlotKind::ExplicitSlot,
            // Two f64 values
            size: 16,
            // 2^3 = 8 bytes, alignment for one f64 value
            align_shift: 3,
        });
        BoundsFunctionCompiler {
            function_builder,
            mem_flags,
            pointer_type,
            builtins,
            output_argument_index,
            entry_block,
            evaluated: HashMap::new(),
            output_slot,
        }
    }

    fn constant_value(&mut self, value: f64) -> Value {
        self.function_builder.ins().f64const(value)
    }

    fn zero(&mut self) -> Value {
        self.constant_value(0.0)
    }

    fn negative_infinity(&mut self) -> Value {
        self.constant_value(f64::NEG_INFINITY)
    }

    fn positive_infinity(&mut self) -> Value {
        self.constant_value(f64::INFINITY)
    }

    fn fneg(&mut self, arg: Value) -> Value {
        self.function_builder.ins().fneg(arg)
    }

    fn fadd(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().fadd(lhs, rhs)
    }

    fn fsub(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().fsub(lhs, rhs)
    }

    fn fmul(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().fmul(lhs, rhs)
    }

    fn fdiv(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().fdiv(lhs, rhs)
    }

    fn fmin(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().fmin(lhs, rhs)
    }

    fn fmin4(&mut self, a: Value, b: Value, c: Value, d: Value) -> Value {
        let ab = self.fmin(a, b);
        let abc = self.fmin(ab, c);
        self.fmin(abc, d)
    }

    fn fmax(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().fmax(lhs, rhs)
    }

    fn fmax4(&mut self, a: Value, b: Value, c: Value, d: Value) -> Value {
        let ab = self.fmax(a, b);
        let abc = self.fmax(ab, c);
        self.fmax(abc, d)
    }

    fn le(&mut self, x: Value, y: Value) -> Value {
        self.function_builder
            .ins()
            .fcmp(FloatCC::LessThanOrEqual, x, y)
    }

    fn ge(&mut self, x: Value, y: Value) -> Value {
        self.function_builder
            .ins()
            .fcmp(FloatCC::GreaterThanOrEqual, x, y)
    }

    fn band(&mut self, lhs: Value, rhs: Value) -> Value {
        self.function_builder.ins().band(lhs, rhs)
    }

    fn includes_zero(&mut self, lower: Value, upper: Value) -> Value {
        let zero = self.zero();
        let lower_le_zero = self.le(lower, zero);
        let upper_ge_zero = self.ge(upper, zero);
        self.band(lower_le_zero, upper_ge_zero)
    }

    fn select(&mut self, condition: Value, if_true: Value, if_false: Value) -> Value {
        self.function_builder
            .ins()
            .select(condition, if_true, if_false)
    }

    fn sqrt(&mut self, arg: Value) -> Value {
        let inst = self.call(self.builtins.sqrt, &[arg]);
        self.function_builder.inst_results(inst)[0]
    }

    fn lower_address(&mut self) -> Value {
        self.function_builder
            .ins()
            .stack_addr(self.pointer_type, self.output_slot, 0)
    }

    fn upper_address(&mut self) -> Value {
        self.function_builder
            .ins()
            .stack_addr(self.pointer_type, self.output_slot, 8)
    }

    fn load_lower(&mut self) -> Value {
        self.function_builder
            .ins()
            .stack_load(F64, self.output_slot, 0)
    }

    fn load_upper(&mut self) -> Value {
        self.function_builder
            .ins()
            .stack_load(F64, self.output_slot, 8)
    }

    fn call(&mut self, func_ref: FuncRef, args: &[Value]) -> Inst {
        self.function_builder.ins().call(func_ref, args)
    }

    fn define_bounds(
        &mut self,
        expression: &'a Expression,
        low_value: Value,
        high_value: Value,
    ) -> (Value, Value) {
        let next_variable_index = 2 * self.evaluated.len() as u32;
        let low_variable = Variable::from_u32(next_variable_index);
        let high_variable = Variable::from_u32(next_variable_index + 1);
        self.function_builder.declare_var(low_variable, F64);
        self.function_builder.declare_var(high_variable, F64);
        self.function_builder.def_var(low_variable, low_value);
        self.function_builder.def_var(high_variable, high_value);
        self.evaluated
            .insert(expression, (low_variable, high_variable));
        (
            self.function_builder.use_var(low_variable),
            self.function_builder.use_var(high_variable),
        )
    }

    pub fn compute_bounds(&mut self, expression: &'a Expression) -> (Value, Value) {
        match self.evaluated.get(&expression) {
            Some((low_variable, high_variable)) => (
                self.function_builder.use_var(*low_variable),
                self.function_builder.use_var(*high_variable),
            ),
            None => match expression {
                Expression::Argument(index) => {
                    let arguments = self.function_builder.block_params(self.entry_block);
                    let i = *index as usize;
                    (arguments[2 * i], arguments[2 * i + 1])
                }
                Expression::Constant(constant) => {
                    let value = self.constant_value(constant.value);
                    (value, value)
                }
                Expression::Negate(arg) => {
                    let (arg_lower, arg_upper) = self.compute_bounds(&arg);
                    let lower = self.fneg(arg_upper);
                    let upper = self.fneg(arg_lower);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Sum(lhs, rhs) => {
                    let (lhs_lower, lhs_upper) = self.compute_bounds(&lhs);
                    let (rhs_lower, rhs_upper) = self.compute_bounds(&rhs);
                    let lower = self.fadd(lhs_lower, rhs_lower);
                    let upper = self.fadd(lhs_upper, rhs_upper);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Difference(lhs, rhs) => {
                    let (lhs_lower, lhs_upper) = self.compute_bounds(&lhs);
                    let (rhs_lower, rhs_upper) = self.compute_bounds(&rhs);
                    let lower = self.fsub(lhs_lower, rhs_upper);
                    let upper = self.fsub(lhs_upper, rhs_lower);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Product(lhs, rhs) => {
                    let (lhs_lower, lhs_upper) = self.compute_bounds(&lhs);
                    let (rhs_lower, rhs_upper) = self.compute_bounds(&rhs);
                    let ll = self.fmul(lhs_lower, rhs_lower);
                    let lu = self.fmul(lhs_lower, rhs_upper);
                    let ul = self.fmul(lhs_upper, rhs_lower);
                    let uu = self.fmul(lhs_upper, rhs_upper);
                    let lower = self.fmin4(ll, lu, ul, uu);
                    let upper = self.fmax4(ll, lu, ul, uu);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Quotient(lhs, rhs) => {
                    let (lhs_lower, lhs_upper) = self.compute_bounds(&lhs);
                    let (rhs_lower, rhs_upper) = self.compute_bounds(&rhs);
                    let ll = self.fdiv(lhs_lower, rhs_lower);
                    let lu = self.fdiv(lhs_lower, rhs_upper);
                    let ul = self.fdiv(lhs_upper, rhs_lower);
                    let uu = self.fdiv(lhs_upper, rhs_upper);
                    let positive_infinity = self.positive_infinity();
                    let negative_infinity = self.negative_infinity();
                    let rhs_includes_zero = self.includes_zero(rhs_lower, rhs_upper);
                    let finite_lower = self.fmin4(ll, lu, ul, uu);
                    let finite_upper = self.fmax4(ll, lu, ul, uu);
                    let lower = self.select(rhs_includes_zero, negative_infinity, finite_lower);
                    let upper = self.select(rhs_includes_zero, positive_infinity, finite_upper);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::SquareRoot(arg) => {
                    let (arg_lower, arg_upper) = self.compute_bounds(arg);
                    let zero = self.zero();
                    let clamped_lower = self.fmax(arg_lower, zero);
                    let clamped_upper = self.fmax(arg_upper, zero);
                    let lower = self.sqrt(clamped_lower);
                    let upper = self.sqrt(clamped_upper);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Squared(arg) => {
                    let (arg_lower, arg_upper) = self.compute_bounds(arg);
                    let ll = self.fmul(arg_lower, arg_lower);
                    let uu = self.fmul(arg_upper, arg_upper);
                    let arg_includes_zero = self.includes_zero(arg_lower, arg_upper);
                    let zero = self.zero();
                    let min = self.fmin(ll, uu);
                    let lower = self.select(arg_includes_zero, zero, min);
                    let upper = self.fmax(ll, uu);
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Sine(arg) => {
                    let (arg_lower, arg_upper) = self.compute_bounds(arg);
                    let sin_bounds_args = [
                        arg_lower,
                        arg_upper,
                        self.lower_address(),
                        self.upper_address(),
                    ];
                    self.call(self.builtins.sin_bounds, &sin_bounds_args);
                    let lower = self.load_lower();
                    let upper = self.load_upper();
                    self.define_bounds(expression, lower, upper)
                }
                Expression::Cosine(arg) => {
                    let (arg_lower, arg_upper) = self.compute_bounds(arg);
                    let cos_bounds_args = [
                        arg_lower,
                        arg_upper,
                        self.lower_address(),
                        self.upper_address(),
                    ];
                    self.call(self.builtins.cos_bounds, &cos_bounds_args);
                    let lower = self.load_lower();
                    let upper = self.load_upper();
                    self.define_bounds(expression, lower, upper)
                }
            },
        }
    }

    pub fn write_output_bounds(&mut self, index: usize, lower: Value, upper: Value) {
        let output_pointer =
            self.function_builder.block_params(self.entry_block)[self.output_argument_index];
        let i = index as i32;
        let lower_offset = 16 * i;
        let upper_offset = 16 * i + 8;
        self.function_builder
            .ins()
            .store(self.mem_flags, lower, output_pointer, lower_offset);
        self.function_builder
            .ins()
            .store(self.mem_flags, upper, output_pointer, upper_offset);
    }

    pub fn return_void(mut self) {
        self.function_builder.ins().return_(&[]);
        self.function_builder.finalize();
    }
}
