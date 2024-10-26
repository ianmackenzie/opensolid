use std::f64;
use std::f64::consts::{FRAC_PI_2, PI};

use cranelift::prelude::{AbiParam, Type};
use cranelift_codegen::ir::types::F64;
use cranelift_codegen::ir::{FuncRef, Function};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use crate::bounds::Bounds;

#[derive(Copy, Clone)]
pub struct Builtins {
    pub sqrt: FuncRef,
    pub sin: FuncRef,
    pub cos: FuncRef,
    pub sin_bounds: FuncRef,
    pub cos_bounds: FuncRef,
    pub quadratic_spline: FuncRef,
    pub cubic_spline: FuncRef,
    pub quadratic_spline_bounds: FuncRef,
    pub cubic_spline_bounds: FuncRef,
}

impl Builtins {
    pub fn declare_symbols(jit_builder: &mut JITBuilder) {
        jit_builder.symbol("opensolid_sin_bounds", opensolid_sin_bounds as *const u8);
        jit_builder.symbol("opensolid_cos_bounds", opensolid_cos_bounds as *const u8);
        jit_builder.symbol(
            "opensolid_quadratic_spline",
            opensolid_quadratic_spline as *const u8,
        );
        jit_builder.symbol(
            "opensolid_quadratic_spline_bounds",
            opensolid_quadratic_spline_bounds as *const u8,
        );
        jit_builder.symbol(
            "opensolid_cubic_spline",
            opensolid_cubic_spline as *const u8,
        );
        jit_builder.symbol(
            "opensolid_cubic_spline_bounds",
            opensolid_cubic_spline_bounds as *const u8,
        );
    }

    pub fn declare_functions(
        module: &mut JITModule,
        function: &mut Function,
        pointer_type: Type,
    ) -> Builtins {
        Builtins {
            sqrt: Builtins::declare_unary_value("sqrt", module, function),
            sin: Builtins::declare_unary_value("sin", module, function),
            cos: Builtins::declare_unary_value("cos", module, function),
            sin_bounds: Builtins::declare_unary_bounds(
                "opensolid_sin_bounds",
                module,
                function,
                pointer_type,
            ),
            cos_bounds: Builtins::declare_unary_bounds(
                "opensolid_cos_bounds",
                module,
                function,
                pointer_type,
            ),
            quadratic_spline: Builtins::declare_quadratic_spline(module, function),
            quadratic_spline_bounds: Builtins::declare_quadratic_spline_bounds(
                module,
                function,
                pointer_type,
            ),
            cubic_spline: Builtins::declare_cubic_spline(module, function),
            cubic_spline_bounds: Builtins::declare_cubic_spline_bounds(
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

    fn declare_quadratic_spline(module: &mut JITModule, function: &mut Function) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.returns.push(AbiParam::new(F64));
        let func_id = module
            .declare_function("opensolid_quadratic_spline", Linkage::Import, &signature)
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }

    fn declare_cubic_spline(module: &mut JITModule, function: &mut Function) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.returns.push(AbiParam::new(F64));
        let func_id = module
            .declare_function("opensolid_cubic_spline", Linkage::Import, &signature)
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }

    fn declare_quadratic_spline_bounds(
        module: &mut JITModule,
        function: &mut Function,
        pointer_type: Type,
    ) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        let func_id = module
            .declare_function(
                "opensolid_quadratic_spline_bounds",
                Linkage::Import,
                &signature,
            )
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }

    fn declare_cubic_spline_bounds(
        module: &mut JITModule,
        function: &mut Function,
        pointer_type: Type,
    ) -> FuncRef {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(F64));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        let func_id = module
            .declare_function("opensolid_cubic_spline_bounds", Linkage::Import, &signature)
            .unwrap();
        module.declare_func_in_func(func_id, function)
    }
}

#[no_mangle]
pub extern "C" fn opensolid_quadratic_spline(p1: f64, p2: f64, p3: f64, t: f64) -> f64 {
    let p12 = p1 + t * (p2 - p1);
    let p23 = p2 + t * (p3 - p2);
    p12 + t * (p23 - p12)
}

#[no_mangle]
pub extern "C" fn opensolid_cubic_spline(p1: f64, p2: f64, p3: f64, p4: f64, t: f64) -> f64 {
    let p12 = p1 + t * (p2 - p1);
    let p23 = p2 + t * (p3 - p2);
    let p34 = p3 + t * (p4 - p3);
    let p123 = p12 + t * (p23 - p12);
    let p234 = p23 + t * (p34 - p23);
    p123 + t * (p234 - p123)
}

#[no_mangle]
pub extern "C" fn opensolid_quadratic_spline_bounds(
    p1: f64,
    p2: f64,
    p3: f64,
    t_low: f64,
    t_high: f64,
    out_low: *mut f64,
    out_high: *mut f64,
) {
    let t = Bounds {
        low: t_low,
        high: t_high,
    };
    let p12 = p1 + t * (p2 - p1);
    let p23 = p2 + t * (p3 - p2);
    let bounds = p12 + t * (p23 - p12);
    unsafe {
        *out_low = bounds.low;
        *out_high = bounds.high;
    }
}

#[no_mangle]
pub extern "C" fn opensolid_cubic_spline_bounds(
    p1: f64,
    p2: f64,
    p3: f64,
    p4: f64,
    t_low: f64,
    t_high: f64,
    out_low: *mut f64,
    out_high: *mut f64,
) {
    let t = Bounds {
        low: t_low,
        high: t_high,
    };
    let p12 = p1 + t * (p2 - p1);
    let p23 = p2 + t * (p3 - p2);
    let p34 = p3 + t * (p4 - p3);
    let p123 = p12 + t * (p23 - p12);
    let p234 = p23 + t * (p34 - p23);
    let bounds = p123 + t * (p234 - p123);
    unsafe {
        *out_low = bounds.low;
        *out_high = bounds.high;
    }
}

#[no_mangle]
pub extern "C" fn opensolid_sin_bounds(
    in_low: f64,
    in_high: f64,
    out_low: *mut f64,
    out_high: *mut f64,
) {
    let sin_low = f64::sin(in_low);
    let sin_high = f64::sin(in_high);
    let low = if contains_sinusoidal_extreme(in_low, in_high, -FRAC_PI_2) {
        -1.0
    } else {
        f64::min(sin_low, sin_high)
    };
    let high = if contains_sinusoidal_extreme(in_low, in_high, FRAC_PI_2) {
        1.0
    } else {
        f64::max(sin_low, sin_high)
    };
    unsafe {
        *out_low = low;
        *out_high = high;
    }
}

#[no_mangle]
pub extern "C" fn opensolid_cos_bounds(
    in_low: f64,
    in_high: f64,
    out_low: *mut f64,
    out_high: *mut f64,
) {
    let cos_low = f64::cos(in_low);
    let cos_high = f64::cos(in_high);
    let low = if contains_sinusoidal_extreme(in_low, in_high, PI) {
        -1.0
    } else {
        f64::min(cos_low, cos_high)
    };
    let high = if contains_sinusoidal_extreme(in_low, in_high, 0.0) {
        1.0
    } else {
        f64::max(cos_low, cos_high)
    };
    unsafe {
        *out_low = low;
        *out_high = high;
    }
}

fn contains_sinusoidal_extreme(low: f64, high: f64, location: f64) -> bool {
    let low_index = f64::floor((low - location) / (2.0 * PI));
    let high_index = f64::floor((high - location) / (2.0 * PI));
    low_index != high_index
}
