use cranelift::prelude::{FunctionBuilder, InstBuilder};
use cranelift_module::Module;

use crate::typechecker::{
    stmnt::{Block, Stmnt},
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

pub fn compile_statement(codegen: &mut Codegen, builder: &mut FunctionBuilder, stmnt: &Stmnt) {
    match stmnt {
        Stmnt::FuncCall(func_call) => {
            compile_func_call(codegen, builder, func_call);
        }
    }
}

pub fn compile_block(codegen: &mut Codegen, builder: &mut FunctionBuilder, block: &Block) {
    for stmnt in &block.value {
        compile_statement(codegen, builder, stmnt);
    }

    // TODO: check if returned

    builder.ins().return_(&[]);
}
