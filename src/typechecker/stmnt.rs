use crate::{ast, error::Error, typechecker::var::Var};

use super::{
    func::Func,
    scope::Scope,
    types::Type,
    value::{FuncCall, GenericValue, Value},
    var::{VarId, Vars},
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
        vars: &Vars,
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

            let value = GenericValue::check(scope, vars, value)?
                .coerce(vars, &func.signature.returns.clone().unwrap())
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

/// A variable declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VarDecl {
    pub var: VarId,
    pub value: Option<Value>,
}

impl VarDecl {
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        decl: &ast::Var,
    ) -> Result<Self, Error> {
        if decl.ty == None && decl.value == None {
            return Err(Error::CannotInferVarType(decl.span));
        }

        let (ty, value) = if let Some(ty) = &decl.ty {
            let ty = Type::check(scope, ty)?;

            if let Some(value) = &decl.value {
                let value = GenericValue::check(scope, vars, value)?
                    .coerce(vars, &ty)
                    .ok_or(Error::InvalidValue(value.span()))?; // TODO: make special variable value error

                (ty, Some(value))
            } else {
                (ty, None)
            }
        } else {
            let value =
                GenericValue::check(scope, vars, &decl.value.clone().expect("Cannot be none"))?;

            let value = value.coerce_default();

            (value.ty(vars), Some(value))
        };

        let id = vars.declare_var(Var::new(decl.span, decl.name.value.clone(), ty));
        scope.define_var(decl.name.value.clone(), id);

        Ok(Self { var: id, value })
    }
}

/// A statement of code.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Stmnt {
    FuncCall(FuncCall),
    Return(Return),
    VarDecl(VarDecl),
}

impl Stmnt {
    /// Checks if a statement is value.
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        stmnt: &ast::Expr,
    ) -> Result<Self, Error> {
        match stmnt {
            ast::Expr::Call(call) => {
                let func_call = FuncCall::check(checker, scope, vars, call)?;

                Ok(Stmnt::FuncCall(func_call))
            }
            ast::Expr::Return(return_) => {
                let return_ = Return::check(checker, scope, vars, func, return_)?;

                Ok(Stmnt::Return(return_))
            }
            ast::Expr::Var(var) => {
                let var = VarDecl::check(checker, scope, vars, func, var)?;

                Ok(Stmnt::VarDecl(var))
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
        vars: &mut Vars,
        func: &Func,
        block: &ast::Block,
    ) -> Result<Self, Error> {
        let mut value = Vec::new();

        for stmnt in &block.value {
            let stmnt = Stmnt::check(checker, scope, vars, func, stmnt)?;
            value.push(stmnt);
        }

        Ok(Self { value })
    }
}
