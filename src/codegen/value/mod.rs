use std::collections::HashMap;

use cranelift::{
    codegen::ir::{ArgumentPurpose, StackSlot},
    prelude::{
        AbiParam, FunctionBuilder, InstBuilder, IntCC, MemFlags, StackSlotData, StackSlotKind,
    },
};
use cranelift_module::Module;

use crate::typechecker::{
    func::FuncImpl,
    types::Type,
    value::{Callee, FuncCall, Op, Value},
    var::VarId,
    Typechecker,
};

use self::{conv::compile_int_to_int, str::compile_string_slice};

use super::{func::compile_abi_param, types::compile_type, Codegen};

mod cmp;
mod conv;
mod str;

/// Compiles the provided value into a Cranelift value.  Always returns [Some] if the `to`
/// parameter is [None].  If the `to` parameter is [Some], then it always returns [None].
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
        Value::Bool(value) => builder
            .ins()
            .iconst(cranelift::prelude::types::I8, if *value { 1 } else { 0 }),
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
        Value::Str(value) => return compile_string_slice(codegen, builder, value, to),
        Value::Var(var) => use_var(codegen, checker, builder, vars, data, *var, to)?,
        Value::FuncCall(call) => {
            compile_func_call(checker, codegen, builder, call, vars, data, to)?
        }
        Value::Deref(value) => {
            let Type::Ptr(deref) = value.ty(checker, &data.vars)
            else {
                unreachable!()
            };

            let ptr = compile_value(checker, codegen, builder, value, vars, data, None).unwrap();

            if deref
                .ty
                .is_big(checker, codegen.pointer_type.bytes() as usize)
                .expect("must be sized")
            {
                if let Some(dest) = to {
                    let size = builder.ins().iconst(
                        codegen.pointer_type,
                        deref
                            .ty
                            .size(checker, codegen.pointer_type.bytes() as usize)
                            .expect("must be sized") as i64,
                    );
                    builder.call_memcpy(codegen.module.target_config(), dest, ptr, size);

                    return None;
                } else {
                    let slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        deref
                            .ty
                            .size(checker, codegen.pointer_type.bytes() as usize)
                            .expect("must be sized") as u32,
                    ));
                    let dest = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
                    let size = builder.ins().iconst(
                        codegen.pointer_type,
                        deref
                            .ty
                            .size(checker, codegen.pointer_type.bytes() as usize)
                            .expect("must be sized") as i64,
                    );
                    builder.call_memcpy(codegen.module.target_config(), dest, ptr, size);

                    builder.ins().stack_addr(codegen.pointer_type, slot, 0)
                }
            } else {
                let ty = compile_type(codegen, checker, &deref.ty);
                builder
                    .ins()
                    .load(ty, cranelift::prelude::MemFlags::new(), ptr, 0)
            }
        }
        Value::AddrOfVar(_, var) => builder
            .ins()
            .stack_addr(codegen.pointer_type, vars[&var], 0),
        Value::Store(_, value) => {
            dbg!(value);
            let ty = value.ty(checker, &data.vars);

            let slot = builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                ty.size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as u32,
            ));

            let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
            compile_value(checker, codegen, builder, value, vars, data, Some(addr));

            builder.ins().stack_addr(codegen.pointer_type, slot, 0)
        }
        Value::Constructor(struct_id, fields) => {
            let struct_decl = &checker.structs[struct_id.0];

            if let Some(dest) = to {
                for (id, value) in fields {
                    let addr = builder.ins().iadd_imm(
                        dest,
                        struct_decl.get_field_offset(
                            checker,
                            codegen.pointer_type.bytes() as usize,
                            *id,
                        ) as i64,
                    );

                    compile_value(checker, codegen, builder, value, vars, data, Some(addr));
                }

                return None;
            } else {
                let size = struct_decl.size(checker, codegen.pointer_type.bytes() as usize);
                let slot = builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    size as u32,
                ));

                for (id, value) in fields {
                    let dest = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
                    let addr = builder.ins().iadd_imm(
                        dest,
                        struct_decl.get_field_offset(
                            checker,
                            codegen.pointer_type.bytes() as usize,
                            *id,
                        ) as i64,
                    );

                    compile_value(checker, codegen, builder, value, vars, data, Some(addr));
                }

                if size > codegen.pointer_type.bytes() as usize {
                    builder.ins().stack_addr(codegen.pointer_type, slot, 0)
                } else {
                    let ptr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
                    let ty = cranelift::prelude::Type::int_with_byte_size(size as u16).unwrap();
                    builder
                        .ins()
                        .load(ty, cranelift::prelude::MemFlags::new(), ptr, 0)
                }
            }
        }
        Value::StructAccess(value, id, field) => {
            let struct_decl = &checker.structs[id.0];
            let offset = struct_decl.get_field_offset(
                checker,
                codegen.pointer_type.bytes() as usize,
                *field,
            );
            let field = &struct_decl.fields[*field].ty.value;

            // the value is a pointer to the struct
            let ptr = compile_value(checker, codegen, builder, value, vars, data, None).unwrap();

            let src = builder.ins().iadd_imm(ptr, offset as i64);
            let size = builder.ins().iconst(
                codegen.pointer_type,
                field
                    .size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );

            if let Some(to) = to {
                builder.call_memcpy(codegen.module.target_config(), to, src, size);
                return None;
            } else {
                if !field
                    .is_big(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized")
                {
                    let ty = compile_type(codegen, checker, field);
                    builder
                        .ins()
                        .load(ty, cranelift::prelude::MemFlags::new(), src, 0)
                } else {
                    src
                }
            }
        }
        Value::AddrOfField(_, value, id, field) => {
            let struct_decl = &checker.structs[id.0];

            let ptr = compile_value(checker, codegen, builder, value, vars, data, None).unwrap();

            let offset = struct_decl.get_field_offset(
                checker,
                codegen.pointer_type.bytes() as usize,
                *field,
            );

            builder.ins().iadd_imm(ptr, offset as i64)
        }
        Value::IntOp(op, left, right) => {
            let lhs = compile_value(checker, codegen, builder, left, vars, data, None).unwrap();
            let rhs = compile_value(checker, codegen, builder, right, vars, data, None).unwrap();

            // Whether or not the integer values are signed.
            let signed = match left.ty(checker, &data.vars) {
                Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::Int => true,
                _ => false,
            };

            match op {
                Op::Mul => builder.ins().imul(lhs, rhs),
                Op::Add => builder.ins().iadd(lhs, rhs),
                Op::Div => {
                    if signed {
                        builder.ins().sdiv(lhs, rhs)
                    } else {
                        builder.ins().udiv(lhs, rhs)
                    }
                }
                Op::Mod => {
                    if signed {
                        builder.ins().srem(lhs, rhs)
                    } else {
                        builder.ins().urem(lhs, rhs)
                    }
                }
                Op::Sub => builder.ins().isub(lhs, rhs),
                Op::LtEq => {
                    if signed {
                        builder.ins().icmp(
                            cranelift::prelude::IntCC::SignedLessThanOrEqual,
                            lhs,
                            rhs,
                        )
                    } else {
                        builder.ins().icmp(
                            cranelift::prelude::IntCC::UnsignedLessThanOrEqual,
                            lhs,
                            rhs,
                        )
                    }
                }
                Op::Lt => {
                    if signed {
                        builder
                            .ins()
                            .icmp(cranelift::prelude::IntCC::SignedLessThan, lhs, rhs)
                    } else {
                        builder
                            .ins()
                            .icmp(cranelift::prelude::IntCC::UnsignedLessThan, lhs, rhs)
                    }
                }
                Op::GtEq => {
                    if signed {
                        builder.ins().icmp(
                            cranelift::prelude::IntCC::SignedGreaterThanOrEqual,
                            lhs,
                            rhs,
                        )
                    } else {
                        builder.ins().icmp(
                            cranelift::prelude::IntCC::UnsignedGreaterThanOrEqual,
                            lhs,
                            rhs,
                        )
                    }
                }
                Op::Gt => {
                    if signed {
                        builder
                            .ins()
                            .icmp(cranelift::prelude::IntCC::SignedGreaterThan, lhs, rhs)
                    } else {
                        builder
                            .ins()
                            .icmp(cranelift::prelude::IntCC::UnsignedGreaterThan, lhs, rhs)
                    }
                }
                Op::Shl => builder.ins().ishl(lhs, rhs),
                Op::Shr => {
                    if signed {
                        builder.ins().sshr(lhs, rhs)
                    } else {
                        builder.ins().ushr(lhs, rhs)
                    }
                }
                Op::BitAnd => builder.ins().band(lhs, rhs),
                Op::BitXor => builder.ins().bxor(lhs, rhs),
                Op::BitOr => builder.ins().bor(lhs, rhs),
            }
        }
        Value::LogEq(left, right) => {
            cmp::compile_eq(checker, codegen, builder, vars, data, left, right, false)
        }
        Value::LogNe(left, right) => {
            cmp::compile_eq(checker, codegen, builder, vars, data, left, right, true)
        }
        Value::IntToInt(value, ty) => {
            compile_int_to_int(checker, codegen, builder, value, ty, vars, data)
        }
        Value::WidePtrToPtr(value, _) => {
            let ptr = compile_value(checker, codegen, builder, value, vars, data, None)
                .expect("No `to` provided");
            builder
                .ins()
                .load(codegen.pointer_type, MemFlags::new(), ptr, 0)
        }
        Value::WidePtrToWidePtr(value, _) => {
            compile_value(checker, codegen, builder, value, vars, data, to)?
        }
        Value::SliceIdx(slice, idx, ty) => {
            let slice_ptr = compile_value(checker, codegen, builder, slice, vars, data, None)
                .expect("No `to` provided");

            // the pointer part of the slice
            let ptr = builder
                .ins()
                .load(codegen.pointer_type, MemFlags::new(), slice_ptr, 0);

            let idx = compile_value(checker, codegen, builder, idx, vars, data, None).unwrap();

            let offset = builder.ins().imul_imm(
                idx,
                ty.size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );

            let final_ptr = builder.ins().iadd(ptr, offset);

            return mem_load(checker, codegen, builder, final_ptr, ty, to);
        }
        Value::PtrIdx(ptr, idx, ty) => {
            let ptr = compile_value(checker, codegen, builder, ptr, vars, data, None)
                .expect("No `to` provided");

            let idx = compile_value(checker, codegen, builder, idx, vars, data, None).unwrap();

            let offset = builder.ins().imul_imm(
                idx,
                ty.size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );

            let final_ptr = builder.ins().iadd(ptr, offset);

            return mem_load(checker, codegen, builder, final_ptr, ty, to);
        }
        Value::AddrOfSliceIdx(_, slice, idx, ty) => {
            let slice_ptr = compile_value(checker, codegen, builder, slice, vars, data, None)
                .expect("No `to` provided");

            // the pointer part of the slice
            let ptr = builder
                .ins()
                .load(codegen.pointer_type, MemFlags::new(), slice_ptr, 0);

            let idx = compile_value(checker, codegen, builder, idx, vars, data, None).unwrap();

            let offset = builder.ins().imul_imm(
                idx,
                ty.size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );

            builder.ins().iadd(ptr, offset)
        }
        Value::AddrOfPtrIdx(_, ptr, idx, ty) => {
            let ptr = compile_value(checker, codegen, builder, ptr, vars, data, None)
                .expect("No `to` provided");

            let idx = compile_value(checker, codegen, builder, idx, vars, data, None).unwrap();

            let offset = builder.ins().imul_imm(
                idx,
                ty.size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );

            builder.ins().iadd(ptr, offset)
        }
        Value::Subslice(ptr, from_idx, to_idx, item_ty) => {
            let ptr = compile_value(checker, codegen, builder, ptr, vars, data, None)
                .expect("No `to` provided");

            let from_idx =
                compile_value(checker, codegen, builder, from_idx, vars, data, None).unwrap();

            let to_idx =
                compile_value(checker, codegen, builder, to_idx, vars, data, None).unwrap();

            let from_offset = builder.ins().imul_imm(
                from_idx,
                item_ty
                    .size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );

            let new_ptr = builder.ins().iadd(ptr, from_offset);
            let size = builder.ins().isub(to_idx, from_idx);

            return create_wide_pointer(codegen, builder, new_ptr, size, to);
        }
        Value::AddrOfUnsizedField(_, wide_ptr, struct_id, _) => {
            let clif_ptr = compile_value(checker, codegen, builder, wide_ptr, vars, data, None)
                .expect("No `to` provided");

            // the pointer part of the slice
            let old_ptr = builder
                .ins()
                .load(codegen.pointer_type, MemFlags::new(), clif_ptr, 0);

            let old_len = builder
                .ins()
                .load(codegen.pointer_type, MemFlags::new(), clif_ptr, 8);

            let offset =
                checker.structs[struct_id.0].size(checker, codegen.pointer_type.bytes() as usize);

            let new_ptr = builder.ins().iadd_imm(old_ptr, offset as i64);
            let new_len = builder.ins().iadd_imm(old_len, -(offset as i64));

            return create_wide_pointer(codegen, builder, new_ptr, new_len, to);
        }
        Value::BitNot(value) => {
            let value = compile_value(checker, codegen, builder, value, vars, data, None)
                .expect("No `to` provided");

            builder.ins().bnot(value)
        }
        Value::LogNot(value) => {
            let value = compile_value(checker, codegen, builder, value, vars, data, None)
                .expect("No `to` provided");

            builder.ins().icmp_imm(IntCC::Equal, value, 0)
        }
        Value::LogAnd(left, right) => {
            let left = compile_value(checker, codegen, builder, left, vars, data, None)
                .expect("No `to` provided");

            let right = compile_value(checker, codegen, builder, right, vars, data, None)
                .expect("No `to` provided");

            builder.ins().band(left, right)
        }
        Value::LogOr(left, right) => {
            let left = compile_value(checker, codegen, builder, left, vars, data, None)
                .expect("No `to` provided");

            let right = compile_value(checker, codegen, builder, right, vars, data, None)
                .expect("No `to` provided");

            builder.ins().bor(left, right)
        }
        Value::IntNeg(value) => {
            let value = compile_value(checker, codegen, builder, value, vars, data, None)
                .expect("No `to` provided");

            builder.ins().ineg(value)
        }
        Value::FuncAddr(func_id) => {
            let func = &codegen.funcs[&func_id];

            let func_ref = codegen
                .module
                .declare_func_in_func(func.cranelift_id, &mut builder.func);
            builder.ins().func_addr(codegen.pointer_type, func_ref)
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

/// Loads a value from the specified address
pub fn mem_load(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    addr: cranelift::prelude::Value,
    ty: &Type,
    // the address to write the value to, if any.
    to: Option<cranelift::prelude::Value>,
) -> Option<cranelift::prelude::Value> {
    if ty
        .is_big(checker, codegen.pointer_type.bytes() as usize)
        .expect("must be sized")
    {
        let size = ty
            .size(checker, codegen.pointer_type.bytes() as usize)
            .expect("must be sized");
        let stack_slot = builder
            .create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, size as u32));

        let dest = if let Some(to) = to {
            to
        } else {
            builder
                .ins()
                .stack_addr(codegen.pointer_type, stack_slot, 0)
        };

        let size = builder.ins().iconst(codegen.pointer_type, size as i64);
        builder.call_memcpy(codegen.module.target_config(), dest, addr, size);

        if to == None {
            Some(dest)
        } else {
            None
        }
    } else {
        let ty = compile_type(codegen, checker, ty);
        let value = builder.ins().load(ty, MemFlags::new(), addr, 0);

        if let Some(to) = to {
            builder.ins().store(MemFlags::new(), value, to, 0);
            None
        } else {
            Some(value)
        }
    }
}

/// Uses a variable as a Cranelift value.
pub fn use_var(
    codegen: &mut Codegen,
    checker: &Typechecker,
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
                ty.size(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized") as i64,
            );
            let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
            builder.call_memcpy(codegen.module.target_config(), to, addr, size);
            return None;
        }
        None => {
            if ty
                .is_big(checker, codegen.pointer_type.bytes() as usize)
                .expect("must be sized")
            {
                Some(builder.ins().stack_addr(codegen.pointer_type, slot, 0))
            } else {
                let ty = compile_type(codegen, checker, ty);
                Some(builder.ins().stack_load(ty, slot, 0))
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
    match &call.callee {
        Callee::Func(callee) => {
            let func = &checker.funcs[callee.0];

            let mut args = Vec::new();

            let dest = if let Some(ty) = &func.signature.returns {
                if ty
                    .is_big(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized")
                {
                    if let Some(value) = to {
                        args.push(value);
                        Some(value)
                    } else {
                        let stack_slot = StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            ty.size(checker, codegen.pointer_type.bytes() as usize)
                                .expect("must be sized") as u32,
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

            let cranelift_func = codegen
                .module
                .declare_func_in_func(codegen.funcs[&callee].cranelift_id, &mut builder.func);

            let inst = if func.signature.variadic && call.args.len() > func.signature.args.len() {
                // Make signature for indirect call
                let mut signature = codegen.module.make_signature();

                if let Some(ty) = &func.signature.returns {
                    if ty
                        .is_big(checker, codegen.pointer_type.bytes() as usize)
                        .expect("must be sized")
                    {
                        signature.params.push(AbiParam::special(
                            codegen.pointer_type,
                            ArgumentPurpose::StructReturn,
                        ));
                    } else {
                        signature
                            .returns
                            .push(AbiParam::new(compile_type(codegen, checker, ty)));
                    }
                }

                for ty in &func.signature.args {
                    signature
                        .params
                        .push(compile_abi_param(checker, codegen, &ty.value.ty));
                }

                for arg in &call.args[func.signature.args.len()..] {
                    let ty = arg.ty(checker, &data.vars);
                    signature
                        .params
                        .push(compile_abi_param(checker, codegen, &ty));
                }

                let sig_ref = builder.import_signature(signature);
                let addr = builder
                    .ins()
                    .func_addr(codegen.pointer_type, cranelift_func);
                builder.ins().call_indirect(sig_ref, addr, &args)
            } else {
                builder.ins().call(cranelift_func, &args)
            };

            if let Some(ty) = &func.signature.returns {
                if ty
                    .is_big(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized")
                {
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
        Callee::Indirect(callee) => {
            let Type::Func(func_type) = callee.ty(checker, &data.vars) else { unreachable!() };

            let mut args = Vec::new();

            let dest = if let Some(ty) = &func_type.ret {
                if ty
                    .is_big(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized")
                {
                    if let Some(value) = to {
                        args.push(value);
                        Some(value)
                    } else {
                        let stack_slot = StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            ty.size(checker, codegen.pointer_type.bytes() as usize)
                                .expect("must be sized") as u32,
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

            let cranelift_func =
                compile_value(checker, codegen, builder, &callee, vars, data, None).unwrap();

            let inst = {
                // Make signature for indirect call
                let mut signature = codegen.module.make_signature();

                if let Some(ty) = &func_type.ret {
                    if ty
                        .is_big(checker, codegen.pointer_type.bytes() as usize)
                        .expect("must be sized")
                    {
                        signature.params.push(AbiParam::special(
                            codegen.pointer_type,
                            ArgumentPurpose::StructReturn,
                        ));
                    } else {
                        signature
                            .returns
                            .push(AbiParam::new(compile_type(codegen, checker, ty)));
                    }
                }

                for ty in &func_type.args {
                    signature
                        .params
                        .push(compile_abi_param(checker, codegen, &ty));
                }

                for arg in &call.args[func_type.args.len()..] {
                    let ty = arg.ty(checker, &data.vars);
                    signature
                        .params
                        .push(compile_abi_param(checker, codegen, &ty));
                }

                let sig_ref = builder.import_signature(signature);
                builder.ins().call_indirect(sig_ref, cranelift_func, &args)
            };

            if let Some(ty) = &func_type.ret {
                if ty
                    .is_big(checker, codegen.pointer_type.bytes() as usize)
                    .expect("must be sized")
                {
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
    }
}

/// Creates a wide pointer value.
pub fn create_wide_pointer(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    ptr: cranelift::prelude::Value,
    len: cranelift::prelude::Value,
    // the address to write the value to, if any.
    to: Option<cranelift::prelude::Value>,
) -> Option<cranelift::prelude::Value> {
    let dest = match to {
        Some(to) => to,
        None => {
            let stack_slot = StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                codegen.pointer_type.bytes() as u32,
            );

            let slot = builder.create_sized_stack_slot(stack_slot);

            builder.ins().stack_addr(codegen.pointer_type, slot, 0)
        }
    };

    builder
        .ins()
        .store(cranelift::prelude::MemFlags::new(), ptr, dest, 0);
    builder.ins().store(
        cranelift::prelude::MemFlags::new(),
        len,
        dest,
        codegen.pointer_type.bytes() as i32,
    );

    if let Some(_) = to {
        None
    } else {
        Some(dest)
    }
}
