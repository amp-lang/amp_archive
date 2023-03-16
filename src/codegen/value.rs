use std::collections::HashMap;

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder, StackSlotData, StackSlotKind},
};
use cranelift_module::{DataContext, DataId, Module};

use crate::typechecker::{
    func::FuncImpl,
    value::{FuncCall, Value},
    var::VarId,
    Typechecker,
};

use super::{types::compile_type, Codegen};

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

pub fn use_var(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    var: VarId,
    // the address to write the value to, if any.
    to: Option<cranelift::prelude::Value>,
) -> Option<cranelift::prelude::Value> {
    let slot = vars[&var];
    let ty = &data.vars.vars[var.0].ty;

    match to {
        Some(to) => {
            let size = builder.ins().iconst(
                codegen.pointer_type,
                ty.size(codegen.pointer_type.bytes() as usize) as i64,
            );
            let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
            builder.call_memcpy(codegen.module.target_config(), to, addr, size);
            return None;
        }
        None => {
            if ty.is_big() {
                Some(builder.ins().stack_addr(codegen.pointer_type, slot, 0))
            } else {
                Some(builder.ins().stack_load(compile_type(codegen, ty), slot, 0))
            }
        }
    }
}

/// Compiles a function call as a Cranelift value.
pub fn compile_func_call(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    call: &FuncCall,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    // the address to write the value to, if any.
    to: Option<cranelift::prelude::Value>,
) -> Option<cranelift::prelude::Value> {
    let func = &checker.funcs[call.callee.0];
    let cranelift_func = codegen
        .module
        .declare_func_in_func(codegen.funcs[&call.callee].cranelift_id, &mut builder.func);

    let mut args = Vec::new();

    let dest = if let Some(ty) = &func.signature.returns {
        if ty.is_big() {
            if let Some(value) = to {
                args.push(value);
                Some(value)
            } else {
                let stack_slot = StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    ty.size(codegen.pointer_type.bytes() as usize) as u32,
                );
                let slot = builder.create_sized_stack_slot(stack_slot);
                args.push(builder.ins().stack_addr(codegen.pointer_type, slot, 0));

                Some(builder.ins().stack_addr(codegen.pointer_type, slot, 0))
            }
        } else {
            to
        }
    } else {
        None
    };

    for arg in &call.args {
        args.push(compile_value(checker, codegen, builder, arg, vars, data, None).unwrap());
    }

    let inst = builder.ins().call(cranelift_func, &args);

    if let Some(ty) = &func.signature.returns {
        if ty.is_big() {
            if to == None {
                Some(dest.unwrap())
            } else {
                None
            }
        } else {
            Some(builder.inst_results(inst)[0])
        }
    } else {
        None
    }
}

/// Compiles the provided value into a Cranelift value.  Always returns [Some] if the `to`
/// parameter is [None].
pub fn compile_value(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    value: &Value,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    // the address to write the value to, if any.
    to: Option<cranelift::prelude::Value>,
) -> Option<cranelift::prelude::Value> {
    let value = match value {
        Value::U8(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I8, *value as i64),
        Value::U16(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I16, *value as i64),
        Value::U32(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I32, *value as i64),
        Value::U64(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I64, *value as i64),
        Value::Uint(value) => builder.ins().iconst(codegen.pointer_type, *value as i64),
        Value::I8(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I8, *value as i64),
        Value::I16(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I16, *value as i64),
        Value::I32(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I32, *value as i64),
        Value::I64(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I64, *value as i64),
        Value::Int(value) => builder.ins().iconst(codegen.pointer_type, *value as i64),
        Value::CStr(_, value) => {
            let data_id = compile_string(codegen, value, true);

            let data_ref = codegen
                .module
                .declare_data_in_func(data_id, &mut builder.func);

            builder.ins().global_value(codegen.pointer_type, data_ref)
        }
        Value::Str(_, value) => {
            // TODO: find better way to use big types as values
            let data_id = compile_string(codegen, value, false);

            let data_ref = codegen
                .module
                .declare_data_in_func(data_id, &mut builder.func);
            let ptr = builder.ins().global_value(codegen.pointer_type, data_ref);
            let len = builder
                .ins()
                .iconst(codegen.pointer_type, value.len() as i64);

            match to {
                Some(to) => {
                    builder
                        .ins()
                        .store(cranelift::prelude::MemFlags::new(), ptr, to, 0);
                    builder.ins().store(
                        cranelift::prelude::MemFlags::new(),
                        len,
                        to,
                        codegen.pointer_type.bytes() as i32,
                    );
                    return None;
                }
                None => {
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
        Value::Var(var) => use_var(codegen, builder, vars, data, *var, to)?,
        Value::FuncCall(call) => {
            compile_func_call(checker, codegen, builder, call, vars, data, to)?
        }
    };

    if let Some(to) = to {
        builder
            .ins()
            .store(cranelift::prelude::MemFlags::new(), value, to, 0);
        None
    } else {
        Some(value)
    }
}
