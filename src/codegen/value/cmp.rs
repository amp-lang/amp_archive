//! Compiles comparisons between two different values.

use std::collections::HashMap;

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder},
};
use cranelift_module::Module;

use crate::{
    codegen::{types::compile_type, Codegen},
    typechecker::{
        func::FuncImpl, struct_::Struct, types::Type, value::Value, var::VarId, Typechecker,
    },
};

use super::compile_value;

/// Compiles a struct comparison
pub fn compile_struct_comparison(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    struct_decl: &Struct,
    left: cranelift::prelude::Value,
    right: cranelift::prelude::Value,
    neg: bool,
) -> cranelift::prelude::Value {
    let mut res = builder.ins().iconst(cranelift::prelude::types::I8, 1);

    for (i, field) in struct_decl.fields.iter().enumerate() {
        let offset =
            struct_decl.get_field_offset(checker, codegen.pointer_type.bytes() as usize, i) as i64;

        if field.ty.value.is_primitive(checker) {
            let left_ptr = builder.ins().iadd_imm(left, offset);
            let right_ptr = builder.ins().iadd_imm(right, offset);

            let cmp = if field
                .ty
                .value
                .is_big(checker, codegen.pointer_type.bytes() as usize)
            {
                let size = builder.ins().iconst(
                    codegen.pointer_type,
                    field
                        .ty
                        .value
                        .size(checker, codegen.pointer_type.bytes() as usize)
                        as i64,
                );
                let cmp =
                    builder.call_memcmp(codegen.module.target_config(), left_ptr, right_ptr, size);
                builder.ins().icmp_imm(
                    if neg {
                        cranelift::prelude::IntCC::NotEqual
                    } else {
                        cranelift::prelude::IntCC::Equal
                    },
                    cmp,
                    0,
                )
            } else {
                let ty = compile_type(codegen, checker, &field.ty.value);

                let left = builder
                    .ins()
                    .load(ty, cranelift::prelude::MemFlags::new(), left_ptr, 0);
                let right =
                    builder
                        .ins()
                        .load(ty, cranelift::prelude::MemFlags::new(), right_ptr, 0);

                builder.ins().icmp(
                    if neg {
                        cranelift::prelude::IntCC::NotEqual
                    } else {
                        cranelift::prelude::IntCC::Equal
                    },
                    left,
                    right,
                )
            };

            res = builder.ins().band(res, cmp);
        } else {
            let left = builder.ins().iadd_imm(left, offset);
            let right = builder.ins().iadd_imm(right, offset);

            if let Type::Struct(struct_id) = field.ty.value {
                let cmp = compile_struct_comparison(
                    checker,
                    codegen,
                    builder,
                    vars,
                    data,
                    &checker.structs[struct_id.0],
                    left,
                    right,
                    neg,
                );

                res = builder.ins().band(res, cmp);
            } else {
                unreachable!("no other non-primitive types are implemented");
            }
        }
    }

    res
}

pub fn compile_eq(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    left: &Value,
    right: &Value,
    neg: bool,
) -> cranelift::prelude::Value {
    let ty = left.ty(checker, &data.vars);

    if ty.is_primitive(checker) {
        let lhs = compile_value(checker, codegen, builder, left, vars, data, None).unwrap();
        let rhs = compile_value(checker, codegen, builder, right, vars, data, None).unwrap();

        if ty.is_big(checker, codegen.pointer_type.bytes() as usize) {
            let size = builder.ins().iconst(
                codegen.pointer_type,
                ty.size(checker, codegen.pointer_type.bytes() as usize) as i64,
            );

            let cmp = builder.call_memcmp(codegen.module.target_config(), lhs, rhs, size);

            builder.ins().icmp_imm(
                if neg {
                    cranelift::prelude::IntCC::NotEqual
                } else {
                    cranelift::prelude::IntCC::Equal
                },
                cmp,
                0,
            )
        } else {
            builder.ins().icmp(
                if neg {
                    cranelift::prelude::IntCC::NotEqual
                } else {
                    cranelift::prelude::IntCC::Equal
                },
                lhs,
                rhs,
            )
        }
    } else {
        // check if the type is already stored on the stack.
        let (left_ptr, right_ptr) = if ty.is_big(checker, codegen.pointer_type.bytes() as usize) {
            let lhs = compile_value(checker, codegen, builder, left, vars, data, None).unwrap();
            let rhs = compile_value(checker, codegen, builder, right, vars, data, None).unwrap();
            (lhs, rhs)
        } else {
            let left_slot =
                builder.create_sized_stack_slot(cranelift::prelude::StackSlotData::new(
                    cranelift::prelude::StackSlotKind::ExplicitSlot,
                    ty.size(checker, codegen.pointer_type.bytes() as usize) as u32,
                ));
            let right_slot =
                builder.create_sized_stack_slot(cranelift::prelude::StackSlotData::new(
                    cranelift::prelude::StackSlotKind::ExplicitSlot,
                    ty.size(checker, codegen.pointer_type.bytes() as usize) as u32,
                ));

            let left_ptr = builder.ins().stack_addr(codegen.pointer_type, left_slot, 0);
            let right_ptr = builder
                .ins()
                .stack_addr(codegen.pointer_type, right_slot, 0);

            compile_value(checker, codegen, builder, left, vars, data, Some(left_ptr));
            compile_value(
                checker,
                codegen,
                builder,
                right,
                vars,
                data,
                Some(right_ptr),
            );

            (left_ptr, right_ptr)
        };

        if let Type::Struct(struct_id) = ty {
            compile_struct_comparison(
                checker,
                codegen,
                builder,
                vars,
                data,
                &checker.structs[struct_id.0],
                left_ptr,
                right_ptr,
                neg,
            )
        } else {
            unreachable!("no other non-primitive types are implemented");
        }
    }
}
