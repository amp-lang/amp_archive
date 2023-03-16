use std::collections::HashMap;

use cranelift::{
    codegen::{
        ir::{ArgumentPurpose, Function},
        Context,
    },
    prelude::{
        AbiParam, FunctionBuilder, FunctionBuilderContext, InstBuilder,
        Signature as CraneliftSignature, StackSlotData, StackSlotKind,
    },
};
use cranelift_module::{Linkage, Module};

use crate::typechecker::{
    func::{Func, FuncId, FuncImpl, Signature},
    var::VarId,
};

use super::{stmnt, types, Codegen};

/// A function declared in the Cranelift context.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CraneliftFunc {
    /// The ID Cranelift assigned this function to.
    pub cranelift_id: cranelift_module::FuncId,

    /// The signature of this function.
    pub signature: CraneliftSignature,
}

/// Declares a function in the Cranelift context.
pub fn declare_func(codegen: &mut Codegen, decl: &Func) -> CraneliftFunc {
    let mut signature = codegen.module.make_signature();

    // compile return types
    for ret in &decl.signature.returns {
        // TODO: support big types
        if ret.is_big() {
            signature.params.push(AbiParam::special(
                codegen.pointer_type,
                ArgumentPurpose::StructReturn,
            ));
            println!("TEST");
        } else {
            let ty = types::compile_type(codegen, &ret);
            signature.returns.push(AbiParam::new(ty));
        }
    }

    for arg in &decl.signature.args {
        if arg.value.ty.is_big() {
            signature.params.push(AbiParam::special(
                codegen.pointer_type,
                ArgumentPurpose::StructArgument(
                    arg.value
                        .ty
                        .size(codegen.module.target_config().pointer_width.bytes() as usize)
                        as u32,
                ),
            ));
        } else {
            let ty = types::compile_type(codegen, &arg.value.ty);
            signature.params.push(AbiParam::new(ty));
        }
    }

    let cranelift_id = codegen
        .module
        .declare_function(&decl.name.value, Linkage::Export, &signature)
        .unwrap();

    CraneliftFunc {
        cranelift_id,
        signature,
    }
}

pub fn compile_func(
    codegen: &mut Codegen,
    context: &mut Context,
    func_context: &mut FunctionBuilderContext,
    id: FuncId,
    signature: &Signature,
    data: &FuncImpl,
) {
    let mut function = Function::new();
    function.signature = codegen.funcs[&id].signature.clone();

    context.func = function;
    let mut builder = FunctionBuilder::new(&mut context.func, func_context);

    let mut vars = HashMap::new();

    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);

    let arg_offset = if let Some(returns) = &signature.returns {
        if returns.is_big() {
            1
        } else {
            0
        }
    } else {
        0
    };

    for (idx, var) in data.vars.vars.iter().enumerate() {
        let slot = StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            var.ty
                .size(codegen.module.target_config().pointer_width.bytes() as usize)
                as u32,
        );
        let slot = builder.create_sized_stack_slot(slot);
        vars.insert(VarId(idx), slot);

        if let Some(arg) = var.argument {
            let arg = builder.block_params(entry_block)[arg_offset + arg];

            if var.ty.is_big() {
                let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
                let size = builder.ins().iconst(
                    codegen.pointer_type,
                    var.ty.size(codegen.pointer_type.bytes() as usize) as i64,
                );
                builder.call_memcpy(codegen.module.target_config(), addr, arg, size);
                // builder.ins().store(MemFlags::new(), arg, addr, 0);
            } else {
                // let ty = types::compile_type(codegen, &var.ty);
                builder.ins().stack_store(arg, slot, 0);
            }
            // use_var(codegen, &mut builder, &vars, data, VarId(arg), Some(addr));
        }
    }

    stmnt::compile_block(codegen, &mut builder, &vars, signature, data);

    builder.seal_all_blocks();
    builder.finalize();

    codegen
        .module
        .define_function(codegen.funcs[&id].cranelift_id, context)
        .unwrap();
}
