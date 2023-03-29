//! Compiles strings.

use cranelift::prelude::{FunctionBuilder, InstBuilder};
use cranelift_module::{DataContext, Module};

use crate::codegen::Codegen;

use super::create_wide_pointer;

/// Compiles the provided string as a Cranelift value.
#[inline]
fn compile_string(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    str: &str,
) -> cranelift::prelude::Value {
    let mut cx = DataContext::new();
    let data = str.as_bytes().to_vec();

    cx.define(data.into_boxed_slice());

    let data_id = codegen.module.declare_anonymous_data(true, false).unwrap();
    codegen.module.define_data(data_id, &cx).unwrap();
    let data_ref = codegen
        .module
        .declare_data_in_func(data_id, &mut builder.func);
    builder.ins().global_value(codegen.pointer_type, data_ref)
}

/// Compiles a string as a `[]mut u8` slice.
///
/// Returns `None` if `to` is provided, otherwise always returns a value.
pub fn compile_string_slice(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    value: &String,
    // the address to write the value to, if any.
    to: Option<cranelift::prelude::Value>,
) -> Option<cranelift::prelude::Value> {
    let ptr = compile_string(codegen, builder, value);
    let len = builder
        .ins()
        .iconst(codegen.pointer_type, value.len() as i64);
    create_wide_pointer(codegen, builder, ptr, len, to)
}
