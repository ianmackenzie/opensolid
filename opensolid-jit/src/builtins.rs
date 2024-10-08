use cranelift::prelude::AbiParam;
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::{FuncRef, Function};
use cranelift_jit::JITModule;
use cranelift_module::{Linkage, Module};

#[derive(Copy, Clone)]
pub struct Builtins {
    pub sqrt: FuncRef,
    pub sin: FuncRef,
    pub cos: FuncRef,
}

impl Builtins {
    pub fn declare_in(module: &mut JITModule, function: &mut Function) -> Builtins {
        Builtins {
            sqrt: Builtins::declare_unary("sqrt", module, function),
            sin: Builtins::declare_unary("sin", module, function),
            cos: Builtins::declare_unary("cos", module, function),
        }
    }

    fn declare_unary(name: &str, module: &mut JITModule, function: &mut Function) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.returns.push(AbiParam::new(F64));
        let func_id = module
            .declare_function(name, Linkage::Import, &signature)
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }
}
