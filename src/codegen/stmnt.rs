use std::collections::HashMap;

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder},
};
use cranelift_module::Module;

use crate::typechecker::{
    func::{FuncImpl, Signature},
    stmnt::{Return, Stmnt, VarDecl},
    value::FuncCall,
    var::VarId,
    Typechecker,
};

use super::Codegen;

pub fn compile_func_call(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    call: &FuncCall,
) {
    let func = codegen
        .module
        .declare_func_in_func(codegen.funcs[&call.callee].cranelift_id, &mut builder.func);

    let mut args = Vec::new();

    for arg in &call.args {
        args.push(
            super::value::compile_value(checker, codegen, builder, arg, vars, data, None)
                .expect("no `to` provided"),
        );
    }

    builder.ins().call(func, &args);
}

pub fn compile_return(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    signature: &Signature,
    data: &FuncImpl,
    ret: &Return,
) {
    if let Some(returns) = &signature.returns {
        if returns.is_big() {
            let addr = builder.block_params(builder.current_block().unwrap())[0];
            super::value::compile_value(
                checker,
                codegen,
                builder,
                ret.value.as_ref().unwrap(),
                vars,
                data,
                Some(addr),
            );
            builder.ins().return_(&[]);
        } else {
            let mut args = Vec::new();

            ret.value.iter().for_each(|value| {
                args.push(
                    super::value::compile_value(checker, codegen, builder, value, vars, data, None)
                        .expect("no `to` provided"),
                );
            });

            builder.ins().return_(&args);
        }
    } else {
        builder.ins().return_(&[]);
    }
}

pub fn compile_var_decl(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    decl: &VarDecl,
) {
    if let Some(value) = &decl.value {
        let slot = vars[&decl.var];
        let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
        super::value::compile_value(checker, codegen, builder, value, vars, data, Some(addr));
    }
}

/// Returns true if the statement returns.
pub fn compile_statement(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    signature: &Signature,
    data: &FuncImpl,
    stmnt: &Stmnt,
) -> bool {
    match stmnt {
        Stmnt::FuncCall(func_call) => {
            compile_func_call(checker, codegen, builder, vars, data, func_call);
        }
        Stmnt::Return(ret) => {
            compile_return(checker, codegen, builder, vars, signature, data, ret);
            return true;
        }
        Stmnt::VarDecl(decl) => {
            compile_var_decl(checker, codegen, builder, vars, data, decl);
        }
    }

    false
}

pub fn compile_block(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    signature: &Signature,
    data: &FuncImpl,
) {
    let mut returns = false;
    for stmnt in &data.block.value {
        returns =
            compile_statement(checker, codegen, builder, vars, signature, data, stmnt) || returns;
    }

    // TODO: check if returned

    if !returns {
        builder.ins().return_(&[]);
    }
}
