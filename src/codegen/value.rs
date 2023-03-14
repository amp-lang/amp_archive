use cranelift::prelude::{FunctionBuilder, InstBuilder};
use cranelift_module::{DataContext, Module};

use crate::typechecker::value::Value;

use super::Codegen;

/// Compiles the provided value into a Cranelift value.
pub fn compile_value(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    value: &Value,
) -> cranelift::prelude::Value {
    match value {
        Value::CStr(value) => {
            let mut cx = DataContext::new();
            let mut data = value.as_bytes().to_vec();
            data.push(0);
            cx.define(data.into_boxed_slice());

            let data_id = codegen.module.declare_anonymous_data(true, false).unwrap();
            codegen.module.define_data(data_id, &cx).unwrap();

            let data_ref = codegen
                .module
                .declare_data_in_func(data_id, &mut builder.func);

            builder.ins().global_value(codegen.pointer_type, data_ref)
        }
    }
}
