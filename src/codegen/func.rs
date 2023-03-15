use cranelift::{
    codegen::{ir::Function, Context},
    prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext, Signature},
};
use cranelift_module::{FuncId, Linkage, Module};

use crate::typechecker::{
    func::{FuncDecl, FuncDef},
    symbol::SymbolId,
};

use super::{stmnt, types, Codegen};

/// A function declared in the Cranelift context.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CraneliftFunc {
    /// The ID Cranelift assigned this function to.
    pub cranelift_id: FuncId,

    /// The signature of this function.
    pub signature: Signature,
}

/// Declares a function in the Cranelift context.
pub fn declare_func(codegen: &mut Codegen, decl: &FuncDecl) -> CraneliftFunc {
    let mut signature = codegen.module.make_signature();

    for arg in &decl.signature.args {
        let ty = types::compile_type(codegen, &arg.ty);
        signature.params.push(AbiParam::new(ty));
    }

    // compile return types
    for ret in &decl.signature.returns {
        let ty = types::compile_type(codegen, &ret);
        signature.returns.push(AbiParam::new(ty));
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
    id: SymbolId,
    def: &FuncDef,
) {
    let mut function = Function::new();
    function.signature = codegen.funcs[&id].signature.clone();

    context.func = function;
    let mut builder = FunctionBuilder::new(&mut context.func, func_context);

    let entry_block = builder.create_block();
    builder.switch_to_block(entry_block);

    stmnt::compile_block(codegen, &mut builder, &def.block);

    builder.seal_all_blocks();
    builder.finalize();

    codegen
        .module
        .define_function(codegen.funcs[&id].cranelift_id, context)
        .unwrap();
}
