use std::collections::HashMap;

use cranelift::prelude::{
    AbiParam, Block, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, Value,
    Variable,
};
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::{FuncRef, Function, MemFlags};
use cranelift_jit::{JITBuilder, JITModule};

use cranelift_module::{Linkage, Module};

use crate::expression::Expression;

fn flags() -> cranelift::prelude::settings::Flags {
    let mut builder = cranelift::prelude::settings::builder();
    builder.set("use_colocated_libcalls", "false").unwrap();
    builder.set("is_pic", "false").unwrap();
    cranelift::prelude::settings::Flags::new(builder)
}

fn declare_builtin_unary(name: &str, module: &mut JITModule, function: &mut Function) -> FuncRef {
    let mut signature = module.make_signature();
    signature.params.push(AbiParam::new(F64));
    signature.returns.push(AbiParam::new(F64));
    let func_id = module
        .declare_function(name, Linkage::Import, &signature)
        .unwrap();
    module.declare_func_in_func(func_id, function)
}

pub struct Builtins {
    sqrt: FuncRef,
    sin: FuncRef,
    cos: FuncRef,
}

fn declare_builtins(module: &mut JITModule, function: &mut Function) -> Builtins {
    Builtins {
        sqrt: declare_builtin_unary("sqrt", module, function),
        sin: declare_builtin_unary("sin", module, function),
        cos: declare_builtin_unary("cos", module, function),
    }
}

pub fn compile(input_dimension: usize, outputs: &[&Expression]) -> *const u8 {
    let isa_builder = cranelift_native::builder().unwrap();
    let isa = isa_builder.finish(flags()).unwrap();
    let pointer_type = isa.pointer_type();
    let jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    let mut module = JITModule::new(jit_builder);
    let mut builder_context = FunctionBuilderContext::new();
    let mut context = module.make_context();
    let builtins = declare_builtins(&mut module, &mut context.func);
    let mem_flags = MemFlags::new();

    for _ in 0..input_dimension {
        context.func.signature.params.push(AbiParam::new(F64));
    }
    if outputs.len() == 1 {
        // 1D output means we can just return a double,
        // e.g. `double foo(double x)` for evaluating a 1D curve
        context.func.signature.returns.push(AbiParam::new(F64));
    } else {
        // 2D or 3D output means we write to an output pointer,
        // e.g. `void foo(double u, double v, double* output)` for evaluating a 3D surface
        // (assuming 'output' points to an array of 3 double values)
        context
            .func
            .signature
            .params
            .push(AbiParam::new(pointer_type));
    }

    let mut function_builder = FunctionBuilder::new(&mut context.func, &mut builder_context);
    let entry_block = function_builder.create_block();
    function_builder.append_block_params_for_function_params(entry_block);
    function_builder.switch_to_block(entry_block);
    function_builder.seal_block(entry_block);
    let mut evaluated = HashMap::new();
    for i in 0..outputs.len() {
        let return_value = compile_subexpression(
            &mut function_builder,
            &builtins,
            entry_block,
            &mut evaluated,
            outputs[i],
        );
        if outputs.len() == 1 {
            function_builder.ins().return_(&[return_value]);
        } else {
            let output_pointer = function_builder.block_params(entry_block)[input_dimension];
            let offset = 8 * i as i32;
            function_builder
                .ins()
                .store(mem_flags, return_value, output_pointer, offset);
        }
    }
    if outputs.len() > 1 {
        function_builder.ins().return_(&[]);
    }
    function_builder.finalize();
    let id = module
        .declare_anonymous_function(&context.func.signature)
        .unwrap();
    module.define_function(id, &mut context).unwrap();
    module.clear_context(&mut context);
    module.finalize_definitions().unwrap();
    module.get_finalized_function(id)
}

fn compile_subexpression<'a>(
    function_builder: &mut FunctionBuilder<'a>,
    builtins: &Builtins,
    entry_block: Block,
    evaluated: &mut HashMap<&'a Expression, Variable>,
    expression: &'a Expression,
) -> Value {
    match evaluated.get(&expression) {
        Some(variable) => function_builder.use_var(*variable),
        None => match expression {
            Expression::Argument(index) => {
                function_builder.block_params(entry_block)[*index as usize]
            }
            Expression::Constant(constant) => function_builder.ins().f64const(constant.0),
            Expression::Negate(arg) => {
                let arg_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &arg);
                let negated_value = function_builder.ins().fneg(arg_value);
                let negated_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(negated_variable, F64);
                function_builder.def_var(negated_variable, negated_value);
                evaluated.insert(expression, negated_variable);
                function_builder.use_var(negated_variable)
            }
            Expression::Sum(lhs, rhs) => {
                let lhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &lhs);
                let rhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &rhs);
                let sum_value = function_builder.ins().fadd(lhs_value, rhs_value);
                let sum_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(sum_variable, F64);
                function_builder.def_var(sum_variable, sum_value);
                evaluated.insert(expression, sum_variable);
                function_builder.use_var(sum_variable)
            }
            Expression::Difference(lhs, rhs) => {
                let lhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &lhs);
                let rhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &rhs);
                let difference_value = function_builder.ins().fsub(lhs_value, rhs_value);
                let difference_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(difference_variable, F64);
                function_builder.def_var(difference_variable, difference_value);
                evaluated.insert(expression, difference_variable);
                function_builder.use_var(difference_variable)
            }
            Expression::Product(lhs, rhs) => {
                let lhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &lhs);
                let rhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &rhs);
                let product_value = function_builder.ins().fmul(lhs_value, rhs_value);
                let product_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(product_variable, F64);
                function_builder.def_var(product_variable, product_value);
                evaluated.insert(expression, product_variable);
                function_builder.use_var(product_variable)
            }
            Expression::Quotient(lhs, rhs) => {
                let lhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &lhs);
                let rhs_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &rhs);
                let quotient_value = function_builder.ins().fdiv(lhs_value, rhs_value);
                let quotient_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(quotient_variable, F64);
                function_builder.def_var(quotient_variable, quotient_value);
                evaluated.insert(expression, quotient_variable);
                function_builder.use_var(quotient_variable)
            }
            Expression::SquareRoot(arg) => {
                let arg_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &arg);
                let sqrt_inst = function_builder.ins().call(builtins.sqrt, &[arg_value]);
                let sqrt_value = function_builder.inst_results(sqrt_inst)[0];
                let sqrt_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(sqrt_variable, F64);
                function_builder.def_var(sqrt_variable, sqrt_value);
                evaluated.insert(expression, sqrt_variable);
                function_builder.use_var(sqrt_variable)
            }
            Expression::Sine(arg) => {
                let arg_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &arg);
                let sin_inst = function_builder.ins().call(builtins.sin, &[arg_value]);
                let sin_value = function_builder.inst_results(sin_inst)[0];
                let sin_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(sin_variable, F64);
                function_builder.def_var(sin_variable, sin_value);
                evaluated.insert(expression, sin_variable);
                function_builder.use_var(sin_variable)
            }
            Expression::Cosine(arg) => {
                let arg_value =
                    compile_subexpression(function_builder, builtins, entry_block, evaluated, &arg);
                let cos_inst = function_builder.ins().call(builtins.cos, &[arg_value]);
                let cos_value = function_builder.inst_results(cos_inst)[0];
                let cos_variable = Variable::from_u32(evaluated.len() as u32);
                function_builder.declare_var(cos_variable, F64);
                function_builder.def_var(cos_variable, cos_value);
                evaluated.insert(expression, cos_variable);
                function_builder.use_var(cos_variable)
            }
        },
    }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve1d(ptr: *mut Expression) -> *const u8 {
    unsafe { compile(1, &[&*ptr]) }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface1d(ptr: *mut Expression) -> *const u8 {
    unsafe { compile(2, &[&*ptr]) }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve2d(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    unsafe { compile(1, &[&*px, &*py]) }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface2d(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    unsafe { compile(2, &[&*px, &*py]) }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve3d(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    unsafe { compile(1, &[&*px, &*py, &*pz]) }
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface3d(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    unsafe { compile(2, &[&*px, &*py, &*pz]) }
}
