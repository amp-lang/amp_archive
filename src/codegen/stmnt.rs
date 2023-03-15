use std::collections::HashMap;

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder},
};
use cranelift_module::Module;

use crate::typechecker::{
    func::FuncImpl,
    stmnt::{Return, Stmnt, VarDecl},
    value::FuncCall,
    var::VarId,
};

use super::Codegen;

pub fn compile_func_call(
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
            super::value::compile_value(codegen, builder, arg, vars, data, None)
                .expect("no `to` provided"),
        );
    }

    builder.ins().call(func, &args);
}

pub fn compile_return(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    ret: &Return,
) {
    let mut args = Vec::new();

    ret.value.iter().for_each(|value| {
        args.push(
            super::value::compile_value(codegen, builder, value, vars, data, None)
                .expect("no `to` provided"),
        );
    });

    builder.ins().return_(&args);
}

pub fn compile_var_decl(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    decl: &VarDecl,
) {
    if let Some(value) = &decl.value {
        let slot = vars[&decl.var];
        let addr = builder.ins().stack_addr(codegen.pointer_type, slot, 0);
        super::value::compile_value(codegen, builder, value, vars, data, Some(addr));
    }
}

/// Returns true if the statement returns.
pub fn compile_statement(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    stmnt: &Stmnt,
) -> bool {
    match stmnt {
        Stmnt::FuncCall(func_call) => {
            compile_func_call(codegen, builder, vars, data, func_call);
        }
        Stmnt::Return(ret) => {
            compile_return(codegen, builder, vars, data, ret);
            return true;
        }
        Stmnt::VarDecl(decl) => {
            compile_var_decl(codegen, builder, vars, data, decl);
        }
    }

    false
}

pub fn compile_block(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
) {
    let mut returns = false;
    for stmnt in &data.block.value {
        returns = compile_statement(codegen, builder, vars, data, stmnt) || returns;
    }

    // TODO: check if returned

    if !returns {
        builder.ins().return_(&[]);
    }
}
