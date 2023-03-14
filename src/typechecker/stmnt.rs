use crate::{ast, error::Error};

use super::{module::Module, value::FuncCall, Typechecker};

/// A statement of code.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Stmnt {
    FuncCall(FuncCall),
}

impl Stmnt {
    /// Checks if a statement is value.
    pub fn check(
        checker: &mut Typechecker,
        module: &mut Module,
        stmnt: &ast::Expr,
    ) -> Result<Self, Error> {
        match stmnt {
            ast::Expr::Call(call) => {
                let func_call = FuncCall::check(checker, module, call)?;

                Ok(Stmnt::FuncCall(func_call))
            }
            _ => Err(Error::InvalidStatement(stmnt.span())),
        }
    }
}

/// A block in Amp.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Block {
    pub value: Vec<Stmnt>,
}

impl Block {
    /// Checks an entire block for validity.
    pub fn check(
        checker: &mut Typechecker,
        module: &mut Module,
        block: &ast::Block,
    ) -> Result<Self, Error> {
        let mut value = Vec::new();

        for stmnt in &block.value {
            let stmnt = Stmnt::check(checker, module, stmnt)?;
            value.push(stmnt);
        }

        Ok(Self { value })
    }
}
