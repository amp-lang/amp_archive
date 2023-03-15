use crate::{ast, error::Error};

use super::{
    func::FuncDecl,
    module::Module,
    value::{FuncCall, GenericValue, Value},
    Typechecker,
};

/// A return statement.

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Return {
    pub value: Option<Value>,
}

impl Return {
    pub fn check(
        checker: &mut Typechecker,
        module: &mut Module,
        decl: &FuncDecl,
        return_: &ast::Return,
    ) -> Result<Self, Error> {
        let value = if let Some(value) = &return_.value {
            if decl.signature.returns == None {
                return Err(Error::InvalidReturnValue {
                    decl: decl.decl_span,
                    name: decl
                        .signature
                        .returns
                        .clone()
                        .map_or("{nothing}".to_string(), |value| value.name()),
                    offending: value.span(),
                });
            }

            let value = GenericValue::check(module, value)
                .ok_or(Error::InvalidValue(value.span()))?
                .coerce(&decl.signature.returns.clone().unwrap())
                .ok_or(Error::InvalidReturnValue {
                    decl: decl.decl_span,
                    name: decl.signature.returns.clone().unwrap().name(),
                    offending: value.span(),
                })?;

            Some(value)
        } else {
            None
        };

        Ok(Return { value })
    }
}

/// A statement of code.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Stmnt {
    FuncCall(FuncCall),
    Return(Return),
}

impl Stmnt {
    /// Checks if a statement is value.
    pub fn check(
        checker: &mut Typechecker,
        module: &mut Module,
        func: &FuncDecl,
        stmnt: &ast::Expr,
    ) -> Result<Self, Error> {
        match stmnt {
            ast::Expr::Call(call) => {
                let func_call = FuncCall::check(checker, module, call)?;

                Ok(Stmnt::FuncCall(func_call))
            }
            ast::Expr::Return(return_) => {
                let return_ = Return::check(checker, module, func, return_)?;

                Ok(Stmnt::Return(return_))
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
        func: &FuncDecl,
        block: &ast::Block,
    ) -> Result<Self, Error> {
        let mut value = Vec::new();

        for stmnt in &block.value {
            let stmnt = Stmnt::check(checker, module, func, stmnt)?;
            value.push(stmnt);
        }

        Ok(Self { value })
    }
}
