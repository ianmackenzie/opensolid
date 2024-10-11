use crate::expression::Expression;
use crate::module_compiler::ModuleCompiler;

pub fn value_function(input_dimension: usize, outputs: &[&Expression]) -> *const u8 {
    ModuleCompiler::new().value_function(input_dimension, outputs)
}

pub fn bounds_function(input_dimension: usize, outputs: &[&Expression]) -> *const u8 {
    ModuleCompiler::new().bounds_function(input_dimension, outputs)
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve1d_value_function(ptr: *mut Expression) -> *const u8 {
    value_function(1, unsafe { &[&*ptr] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve1d_bounds_function(ptr: *mut Expression) -> *const u8 {
    bounds_function(1, unsafe { &[&*ptr] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface1d_value_function(
    ptr: *mut Expression,
) -> *const u8 {
    value_function(2, unsafe { &[&*ptr] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface1d_bounds_function(
    ptr: *mut Expression,
) -> *const u8 {
    bounds_function(2, unsafe { &[&*ptr] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve2d_value_function(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    value_function(1, unsafe { &[&*px, &*py] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve2d_bounds_function(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    bounds_function(1, unsafe { &[&*px, &*py] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface2d_value_function(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    value_function(2, unsafe { &[&*px, &*py] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface2d_bounds_function(
    px: *mut Expression,
    py: *mut Expression,
) -> *const u8 {
    bounds_function(2, unsafe { &[&*px, &*py] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve3d_value_function(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    value_function(1, unsafe { &[&*px, &*py, &*pz] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_curve3d_bounds_function(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    bounds_function(1, unsafe { &[&*px, &*py, &*pz] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface3d_value_function(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    value_function(2, unsafe { &[&*px, &*py, &*pz] })
}

#[no_mangle]
pub extern "C" fn opensolid_jit_compile_surface3d_bounds_function(
    px: *mut Expression,
    py: *mut Expression,
    pz: *mut Expression,
) -> *const u8 {
    bounds_function(2, unsafe { &[&*px, &*py, &*pz] })
}
