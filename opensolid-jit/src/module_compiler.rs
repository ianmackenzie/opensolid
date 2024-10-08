use cranelift::prelude::{AbiParam, Configurable, FunctionBuilderContext, Type};
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::MemFlags;
use cranelift_codegen::Context;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

use crate::builtins::Builtins;
use crate::expression::Expression;
use crate::function_compiler::FunctionCompiler;

pub struct ModuleCompiler {
    module: JITModule,
    context: Context,
    builtins: Builtins,
    mem_flags: MemFlags,
    pointer_type: Type,
    builder_context: FunctionBuilderContext,
}

impl ModuleCompiler {
    pub fn new() -> ModuleCompiler {
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder.finish(ModuleCompiler::flags()).unwrap();
        let pointer_type = isa.pointer_type();
        let jit_builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        let mut module = JITModule::new(jit_builder);
        let builder_context = FunctionBuilderContext::new();
        let mut context = module.make_context();
        let builtins = Builtins::declare_in(&mut module, &mut context.func);
        let mem_flags = MemFlags::new();
        ModuleCompiler {
            module,
            context,
            builtins,
            mem_flags,
            pointer_type,
            builder_context,
        }
    }

    fn flags() -> cranelift::prelude::settings::Flags {
        let mut builder = cranelift::prelude::settings::builder();
        builder.set("use_colocated_libcalls", "false").unwrap();
        builder.set("is_pic", "false").unwrap();
        cranelift::prelude::settings::Flags::new(builder)
    }

    pub fn compile_value(mut self, input_dimension: usize, outputs: &[&Expression]) -> *const u8 {
        for _ in 0..input_dimension {
            self.context.func.signature.params.push(AbiParam::new(F64));
        }
        if outputs.len() == 1 {
            // 1D output means we can just return a double,
            // e.g. `double foo(double x)` for evaluating a 1D curve
            self.context.func.signature.returns.push(AbiParam::new(F64));
        } else {
            // 2D or 3D output means we write to an output pointer,
            // e.g. `void foo(double u, double v, double* output)` for evaluating a 3D surface
            // (assuming 'output' points to an array of 3 double values)
            self.context
                .func
                .signature
                .params
                .push(AbiParam::new(self.pointer_type));
        }

        let mut function_compiler = FunctionCompiler::new(
            self.builtins,
            self.mem_flags,
            input_dimension,
            &mut self.context.func,
            &mut self.builder_context,
        );
        if outputs.len() == 1 {
            let return_value = function_compiler.compute_value(outputs[0]);
            function_compiler.return_value(return_value);
        } else {
            for i in 0..outputs.len() {
                let value = function_compiler.compute_value(outputs[i]);
                function_compiler.write_output_value(i, value);
            }
            function_compiler.return_void();
        }
        self.finalize()
    }

    fn finalize(mut self) -> *const u8 {
        let id = self
            .module
            .declare_anonymous_function(&self.context.func.signature)
            .unwrap();
        self.module.define_function(id, &mut self.context).unwrap();
        self.module.clear_context(&mut self.context);
        self.module.finalize_definitions().unwrap();
        self.module.get_finalized_function(id)
    }
}
