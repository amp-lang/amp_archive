use crate::{ast, error::Error};

use super::{
    func::Func,
    scope::Scope,
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
        checker: &Typechecker,
        scope: &mut Scope,
        func: &Func,
        return_: &ast::Return,
    ) -> Result<Self, Error> {
        let value = if let Some(value) = &return_.value {
            if func.signature.returns == None {
                return Err(Error::InvalidReturnValue {
                    decl: func.span,
                    name: func
                        .signature
                        .returns
                        .clone()
                        .map_or("{nothing}".to_string(), |value| value.name()),
                    offending: value.span(),
                });
            }

            let value = GenericValue::check(scope, value)
                .ok_or(Error::InvalidValue(value.span()))?
                .coerce(&func.signature.returns.clone().unwrap())
                .ok_or(Error::InvalidReturnValue {
                    decl: func.span,
                    name: func.signature.returns.clone().unwrap().name(),
                    offending: value.span(),
                })?;

            Some(value)
        } else {
            if func.signature.returns != None {
                return Err(Error::InvalidReturnValue {
                    decl: func.span,
                    name: func
                        .signature
                        .returns
                        .clone()
                        .map(|value| value.name())
                        .unwrap(),
                    offending: return_.span,
                });
            }

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
        checker: &Typechecker,
        scope: &mut Scope,
        func: &Func,
        stmnt: &ast::Expr,
    ) -> Result<Self, Error> {
        match stmnt {
            ast::Expr::Call(call) => {
                let func_call = FuncCall::check(checker, scope, call)?;

                Ok(Stmnt::FuncCall(func_call))
            }
            ast::Expr::Return(return_) => {
                let return_ = Return::check(checker, scope, func, return_)?;

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
        checker: &Typechecker,
        scope: &mut Scope,
        func: &Func,
        block: &ast::Block,
    ) -> Result<Self, Error> {
        let mut value = Vec::new();

        for stmnt in &block.value {
            let stmnt = Stmnt::check(checker, scope, func, stmnt)?;
            value.push(stmnt);
        }

        Ok(Self { value })
    }
}
