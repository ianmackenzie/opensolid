use cranelift::prelude::{AbiParam, Type};
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::{FuncRef, Function};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

#[derive(Copy, Clone)]
pub struct Builtins {
    pub sqrt: FuncRef,
    pub sin: FuncRef,
    pub cos: FuncRef,
    pub bounds_sin: FuncRef,
    pub bounds_cos: FuncRef,
}

impl Builtins {
    pub fn declare_in(
        module: &mut JITModule,
        function: &mut Function,
        pointer_type: Type,
    ) -> Builtins {
        Builtins {
            sqrt: Builtins::declare_unary_value("sqrt", module, function),
            sin: Builtins::declare_unary_value("sin", module, function),
            cos: Builtins::declare_unary_value("cos", module, function),
            bounds_sin: Builtins::declare_unary_bounds(
                "opensolid_bounds_sin",
                module,
                function,
                pointer_type,
            ),
            bounds_cos: Builtins::declare_unary_bounds(
                "opensolid_bounds_cos",
                module,
                function,
                pointer_type,
            ),
        }
    }

    fn declare_unary_value(name: &str, module: &mut JITModule, function: &mut Function) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.returns.push(AbiParam::new(F64));
        let func_id = module
            .declare_function(name, Linkage::Import, &signature)
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }

    fn declare_unary_bounds(
        name: &str,
        module: &mut JITModule,
        function: &mut Function,
        pointer_type: Type,
    ) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        let func_id = module
            .declare_function(name, Linkage::Import, &signature)
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }
}
