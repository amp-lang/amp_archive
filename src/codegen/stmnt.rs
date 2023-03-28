use std::collections::HashMap;

use cranelift::{
    codegen::ir::StackSlot,
    prelude::{FunctionBuilder, InstBuilder},
};
use cranelift_module::Module;

use crate::typechecker::{
    func::{FuncImpl, Signature},
    stmnt::{Assign, Block, IfBranch, Return, Stmnt, VarDecl},
    value::FuncCall,
    var::VarId,
    Typechecker,
};

use super::{
    types::compile_type,
    value::{compile_func_call, compile_value},
    Codegen,
};

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
        if returns.is_big(checker, codegen.pointer_type.bytes() as usize) {
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

pub fn compile_assign(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    data: &FuncImpl,
    assign: &Assign,
) {
    let dest = compile_value(checker, codegen, builder, &assign.dest, vars, data, None)
        .expect("no `to` provided");

    super::value::compile_value(
        checker,
        codegen,
        builder,
        &assign.value,
        vars,
        data,
        Some(dest),
    );
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
            compile_func_call(checker, codegen, builder, func_call, vars, data, None);
        }
        Stmnt::Return(ret) => {
            compile_return(checker, codegen, builder, vars, signature, data, ret);
            return true;
        }
        Stmnt::VarDecl(decl) => {
            compile_var_decl(checker, codegen, builder, vars, data, decl);
        }
        Stmnt::Assign(assign) => compile_assign(checker, codegen, builder, vars, data, assign),
        Stmnt::While(while_) => {
            // TODO: implement break statements.
            if let Some(cond) = &while_.cond {
                let cond_block = builder.create_block();
                let body_block = builder.create_block();
                let end_block = builder.create_block();

                builder.ins().jump(cond_block, &[]);

                builder.switch_to_block(cond_block);
                let cond =
                    super::value::compile_value(checker, codegen, builder, cond, vars, data, None)
                        .expect("no `to` provided");
                builder.ins().brif(cond, body_block, &[], end_block, &[]);

                builder.switch_to_block(body_block);
                compile_block(
                    checker,
                    codegen,
                    builder,
                    vars,
                    signature,
                    data,
                    &while_.body,
                    false,
                );
                builder.ins().jump(cond_block, &[]);

                builder.switch_to_block(end_block);
            } else {
                let body_block = builder.create_block();
                let end_block = builder.create_block();

                builder.ins().jump(body_block, &[]);

                builder.switch_to_block(body_block);
                compile_block(
                    checker,
                    codegen,
                    builder,
                    vars,
                    signature,
                    data,
                    &while_.body,
                    false,
                );
                builder.ins().jump(body_block, &[]);

                builder.switch_to_block(end_block);
            }
        }
        Stmnt::If(if_) => {
            let cond_block = builder.create_block();
            let body_block = builder.create_block();
            let end_block = builder.create_block();

            // create blocks for branches
            let branch_blocks = {
                let mut blocks = Vec::new();
                for branch in &if_.branches {
                    match branch {
                        IfBranch::Else(_) => {
                            blocks.push((builder.create_block(), builder.create_block()));
                            break;
                        }
                        IfBranch::ElseIf(_) => {
                            blocks.push((builder.create_block(), builder.create_block()));
                        }
                    }
                }
                blocks
            };

            // check condition
            builder.ins().jump(cond_block, &[]);
            builder.switch_to_block(cond_block);
            let cond =
                super::value::compile_value(checker, codegen, builder, &if_.cond, vars, data, None)
                    .expect("no `to` provided");
            builder.ins().brif(
                cond,
                body_block,
                &[],
                if let Some(next_branch) = branch_blocks.get(0) {
                    // check the next condition...
                    next_branch.0
                } else {
                    // ...or exit the if statement
                    end_block
                },
                &[],
            );

            // compile body
            builder.switch_to_block(body_block);

            if !compile_block(
                checker, codegen, builder, vars, signature, data, &if_.body, false,
            ) {
                builder.ins().jump(end_block, &[]);
            }

            if branch_blocks.len() == 0 {
                builder.switch_to_block(end_block);
            }

            // compile branches
            let branches = branch_blocks.iter().enumerate();
            for (idx, branch) in branches {
                let (cond_block, body_block) = *branch;

                // build condition block
                builder.switch_to_block(cond_block);

                let branch = &if_.branches[idx];
                match branch {
                    IfBranch::ElseIf(else_if) => {
                        let cond = super::value::compile_value(
                            checker,
                            codegen,
                            builder,
                            &else_if.cond,
                            vars,
                            data,
                            None,
                        )
                        .expect("no `to` provided");
                        builder.ins().brif(
                            cond,
                            body_block,
                            &[],
                            if let Some(next_branch) = branch_blocks.get(idx + 1) {
                                next_branch.0
                            } else {
                                end_block
                            },
                            &[],
                        );
                    }
                    IfBranch::Else(_) => {
                        builder.ins().jump(body_block, &[]);
                    }
                }

                // build body
                builder.switch_to_block(body_block);

                match branch {
                    IfBranch::ElseIf(else_if) => {
                        if !compile_block(
                            checker,
                            codegen,
                            builder,
                            vars,
                            signature,
                            data,
                            &else_if.body,
                            false,
                        ) {
                            builder.ins().jump(end_block, &[]);
                        }
                    }
                    IfBranch::Else(else_) => {
                        if !compile_block(
                            checker,
                            codegen,
                            builder,
                            vars,
                            signature,
                            data,
                            &else_.body,
                            false,
                        ) {
                            builder.ins().jump(end_block, &[]);
                        }
                    }
                }
                builder.switch_to_block(end_block);
            }
        }
    }

    false
}

/// Returns `true` if the block is filled.
pub fn compile_block(
    checker: &Typechecker,
    codegen: &mut Codegen,
    builder: &mut FunctionBuilder,
    vars: &HashMap<VarId, StackSlot>,
    signature: &Signature,
    data: &FuncImpl,
    block: &Block,
    must_return: bool,
) -> bool {
    let mut filled = false;
    for stmnt in &block.value {
        filled =
            compile_statement(checker, codegen, builder, vars, signature, data, stmnt) || filled;
    }

    if !filled && must_return {
        if let Some(returns) = &signature.returns {
            if returns.is_big(checker, codegen.pointer_type.bytes() as usize) {
                builder.ins().return_(&[]);
            } else {
                let ty = compile_type(codegen, checker, returns);

                let value = if ty.is_int() {
                    builder.ins().iconst(ty, 0)
                } else {
                    unreachable!();
                };

                builder.ins().return_(&[value]);
            }
        } else {
            builder.ins().return_(&[]);
        }
        filled = true;
    }

    filled
}
