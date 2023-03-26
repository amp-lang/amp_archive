//! Compiles conversions between two different values.

use std::collections::HashMap;

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder},
};

use crate::{
    codegen::{types::compile_type, Codegen},
    typechecker::{func::FuncImpl, types::Type, value::Value, var::VarId, Typechecker},
};

use super::compile_value;

/// Compiles a conversion from one integer type to another.
pub fn compile_int_to_int(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    value: &Value,
    ty: &Type,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
) -> cranelift::prelude::Value {
    let signed = match value.ty(checker, &data.vars) {
        Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::Int => true,
        _ => false,
    };

    let from = value.ty(checker, &data.vars);
    let to = compile_type(codegen, checker, ty);
    let value = compile_value(checker, codegen, builder, value, vars, data, None).unwrap();

    // Ensure the signedness of the value is considered in the conversion
    if from.size(checker, codegen.pointer_type.bytes() as usize) < to.bytes() as usize {
        if signed {
            builder.ins().sextend(to, value)
        } else {
            builder.ins().uextend(to, value)
        }
    } else if from.size(checker, codegen.pointer_type.bytes() as usize) > to.bytes() as usize {
        builder.ins().ireduce(to, value)
    } else {
        value
    }
}
