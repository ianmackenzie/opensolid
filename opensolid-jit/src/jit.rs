use crate::expression::Expression;
use crate::module_compiler::ModuleCompiler;

pub fn compile_value(input_dimension: usize, outputs: &[&Expression]) -> *const u8 {
    ModuleCompiler::new().compile_value(input_dimension, outputs)
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve1d_value(ptr: *mut Expression) -> *const u8 {
    compile_value(1, &unsafe { [&*ptr] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface1d_value(ptr: *mut Expression) -> *const u8 {
    compile_value(2, &unsafe { [&*ptr] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve2d_value(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    compile_value(1, &unsafe { [&*px, &*py] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface2d_value(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    compile_value(2, &unsafe { [&*px, &*py] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve3d_value(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    compile_value(1, &unsafe { [&*px, &*py, &*pz] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface3d_value(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    compile_value(2, &unsafe { [&*px, &*py, &*pz] })
}
