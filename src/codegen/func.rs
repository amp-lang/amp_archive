use std::{collections::HashMap, fmt::Write};

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
    decl::Modifier,
    func::{Func, FuncId, FuncImpl, Signature},
    types::Type,
    var::VarId,
    Typechecker,
};

use super::{stmnt, types::compile_type, Codegen};

/// A function declared in the Cranelift context.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CraneliftFunc {
    /// The ID Cranelift assigned this function to.
    pub cranelift_id: cranelift_module::FuncId,

    /// The signature of this function.
    pub signature: CraneliftSignature,
}

/// Mangles the name of a function.
pub fn mangle_func(decl: &Func) -> String {
    if let Some(extern_name) = &decl.extern_name {
        return extern_name.clone();
    }

    if decl.name.value.items.len() == 1 {
        format!(
            "__f_{}{}",
            decl.name.value.short_name().len(),
            decl.name.value.short_name()
        )
    } else {
        let mut name = String::new();

        for (_, item) in decl.name.value.items.iter().enumerate() {
            write!(name, "_{}{}", item.len(), item).unwrap();
        }

        format!("__n_{}", name)
    }
}

/// Declares a function in the Cranelift context.
pub fn declare_func(codegen: &mut Codegen, checker: &Typechecker, decl: &Func) -> CraneliftFunc {
    let mut signature = codegen.module.make_signature();

    // compile return types
    for ret in &decl.signature.returns {
        if ret.is_big(checker, codegen.pointer_type.bytes() as usize) {
            signature.params.push(AbiParam::special(
                codegen.pointer_type,
                ArgumentPurpose::StructReturn,
            ))
        } else {
            signature
                .returns
                .push(AbiParam::new(compile_type(codegen, checker, &ret)))
        }
    }

    for arg in &decl.signature.args {
        signature
            .params
            .push(compile_abi_param(checker, codegen, &arg.value.ty));
    }

    let cranelift_id = if decl.modifiers.contains(&Modifier::Export) || decl.func_impl.is_none() {
        codegen
            .module
            .declare_function(
                &mangle_func(decl),
                if decl.modifiers.contains(&Modifier::Export) {
                    Linkage::Export
                } else {
                    Linkage::Local
                },
                &signature,
            )
            .unwrap()
    } else {
        codegen
            .module
            .declare_anonymous_function(&signature)
            .unwrap()
    };

    CraneliftFunc {
        cranelift_id,
        signature,
    }
}

/// Compiles a parameter type for the Cranelift ABI.
pub fn compile_abi_param(checker: &Typechecker, codegen: &mut Codegen, ty: &Type) -> AbiParam {
    if ty.is_big(checker, codegen.pointer_type.bytes() as usize) {
        AbiParam::special(
            codegen.pointer_type,
            ArgumentPurpose::StructArgument(
                ty.size(checker, codegen.pointer_type.bytes() as usize) as u32,
            ),
        )
    } else {
        AbiParam::new(compile_type(codegen, checker, &ty))
    }
}

pub fn compile_func(
    checker: &Typechecker,
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
        if returns.is_big(checker, codegen.pointer_type.bytes() as usize) {
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
            var.ty.size(
                checker,
                codegen.module.target_config().pointer_width.bytes() as usize,
            ) as u32,
        );
        let slot = builder.create_sized_stack_slot(slot);
        vars.insert(VarId(idx), slot);

        if let Some(arg) = var.argument {
            let arg = builder.block_params(entry_block)[arg_offset + arg];

            if var
                .ty
                .is_big(checker, codegen.pointer_type.bytes() as usize)
            {
                let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
                let size = builder.ins().iconst(
                    codegen.pointer_type,
                    var.ty.size(checker, codegen.pointer_type.bytes() as usize) as i64,
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

    stmnt::compile_block(
        checker,
        codegen,
        &mut builder,
        &vars,
        signature,
        data,
        &data.block,
        true,
    );

    builder.seal_all_blocks();
    builder.finalize();

    codegen
        .module
        .define_function(codegen.funcs[&id].cranelift_id, context)
        .unwrap();
}
