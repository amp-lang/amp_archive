use cranelift::prelude::{FunctionBuilder, InstBuilder};
use cranelift_module::Module;

use crate::typechecker::{
    stmnt::{Block, Return, Stmnt},
    value::FuncCall,
};

use super::Codegen;

pub fn compile_func_call(codegen: &mut Codegen, builder: &mut FunctionBuilder, call: &FuncCall) {
    let func = codegen
        .module
        .declare_func_in_func(codegen.funcs[&call.callee].cranelift_id, &mut builder.func);

    let mut args = Vec::new();

    for arg in &call.args {
        args.push(super::value::compile_value(codegen, builder, arg));
    }

    builder.ins().call(func, &args);
}

pub fn compile_return(codegen: &mut Codegen, builder: &mut FunctionBuilder, ret: &Return) {
    let mut args = Vec::new();

    ret.value.iter().for_each(|value| {
        args.push(super::value::compile_value(codegen, builder, value));
    });

    builder.ins().return_(&args);
}

/// Returns true if the statement returns.
pub fn compile_statement(
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    stmnt: &Stmnt,
) -> bool {
    match stmnt {
        Stmnt::FuncCall(func_call) => {
            compile_func_call(codegen, builder, func_call);
        }
        Stmnt::Return(ret) => {
            compile_return(codegen, builder, ret);
            return true;
        }
    }

    false
}

pub fn compile_block(codegen: &mut Codegen, builder: &mut FunctionBuilder, block: &Block) {
    for stmnt in &block.value {
        compile_statement(codegen, builder, stmnt);
    }

    // TODO: check if returned

    // builder.ins().return_(&[]);
}
