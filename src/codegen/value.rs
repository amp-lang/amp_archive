use cranelift::prelude::{FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind};
use cranelift_module::{DataContext, DataId, Module};

use crate::typechecker::value::Value;

use super::Codegen;

fn compile_string(codegen: &mut Codegen, str: &str, nullterm: bool) -> DataId {
    let mut cx = DataContext::new();
    let mut data = str.as_bytes().to_vec();

    if nullterm {
        data.push(0);
    }

    cx.define(data.into_boxed_slice());

    let data_id = codegen.module.declare_anonymous_data(true, false).unwrap();
    codegen.module.define_data(data_id, &cx).unwrap();
    data_id
}

/// Compiles the provided value into a Cranelift value.
pub fn compile_value(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    value: &Value,
) -> cranelift::prelude::Value {
    match value {
        Value::U8(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I8, *value as i64),
        Value::I32(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I32, *value as i64),
        Value::CStr(value) => {
            let data_id = compile_string(codegen, value, true);

            let data_ref = codegen
                .module
                .declare_data_in_func(data_id, &mut builder.func);

            builder.ins().global_value(codegen.pointer_type, data_ref)
        }
        Value::Str(value) => {
            // TODO: find better way to use big types as values
            let data_id = compile_string(codegen, value, false);

            let data_ref = codegen
                .module
                .declare_data_in_func(data_id, &mut builder.func);
            let ptr = builder.ins().global_value(codegen.pointer_type, data_ref);
            let len = builder
                .ins()
                .iconst(codegen.pointer_type, value.len() as i64);

            let stack_slot = StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                codegen.pointer_type.bytes() as u32,
            );
            let slot = builder.create_sized_stack_slot(stack_slot);
            builder.ins().stack_store(ptr, slot, 0);
            builder
                .ins()
                .stack_store(len, slot, codegen.pointer_type.bytes() as i32);

            builder.ins().stack_addr(codegen.pointer_type, slot, 0)
        }
    }
}
