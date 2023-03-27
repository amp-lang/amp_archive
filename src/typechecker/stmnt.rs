use crate::{
    ast::{self},
    error::Error,
    typechecker::{types::Ptr, var::Var},
};

use super::{
    func::Func,
    scope::Scope,
    types::{Mutability, Type},
    value::{FuncCall, GenericValue, Value},
    var::{VarId, Vars},
    Typechecker,
};

/// A return statement.
#[derive(Clone, Debug, PartialEq, Eq)]
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
                        .map_or("{nothing}".to_string(), |value| value.name(checker)),
                    offending: value.span(),
                });
            }

            let generic_value = GenericValue::check(checker, scope, vars, value)?;
            dbg!(&generic_value.default_type(checker, vars));

            let value = generic_value
                .coerce(checker, vars, &func.signature.returns.clone().unwrap())
                .ok_or(Error::InvalidReturnValue {
                    decl: func.span,
                    name: func.signature.returns.clone().unwrap().name(checker),
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
                        .map(|value| value.name(checker))
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
#[derive(Clone, Debug, PartialEq, Eq)]
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
            // var my_name; // cannot infer the type of the variable without looking ahead
            return Err(Error::CannotInferVarType(decl.span));
        }

        let (ty, value) = if let Some(ty) = &decl.ty {
            let ty = Type::check(scope, ty)?;

            if let Some(value) = &decl.value {
                let value = GenericValue::check(checker, scope, vars, value)?
                    .coerce(checker, vars, &ty)
                    .ok_or(Error::InvalidValue(value.span()))?; // TODO: make special variable value error

                (ty, Some(value))
            } else {
                (ty, None)
            }
        } else {
            let value = GenericValue::check(
                checker,
                scope,
                vars,
                decl.value.as_ref().expect("Cannot be none"),
            )?;

            let value = value.coerce_default(checker, vars);

            (value.ty(checker, vars), Some(value))
        };

        let id = vars.declare_var(Var::new(decl.span, decl.name.value.clone(), ty));
        scope.define_var(decl.name.value.clone(), id);

        Ok(Self { var: id, value })
    }
}

/// Assigns a value to a variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assign {
    pub dest: Value,
    pub value: Value,
}

impl Assign {
    /// Checks an assignment expression and returns the assignment.
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        assign: &ast::Binary,
    ) -> Result<Self, Error> {
        // get pointer to destination
        let dest = match assign.left.as_ref() {
            ast::Expr::Unary(ast::Unary {
                op: ast::UnaryOp::Deref,
                expr,
                ..
            }) => {
                let value = GenericValue::check(checker, scope, vars, &expr)?;
                let Type::Ptr(Ptr { mutability, .. }) = value.default_type(checker, vars) else {
                    return Err(Error::InvalidDeref(expr.span()))
                };

                if mutability != Mutability::Mut {
                    return Err(Error::CannotChangeImmutable(expr.span()));
                }

                value.coerce_default(checker, vars)
            }
            _ => GenericValue::check(checker, scope, vars, &assign.left)?
                .as_ref(checker, vars, Mutability::Mut)
                .ok_or(Error::CannotChangeImmutable(assign.left.span()))?
                .coerce_default(checker, vars),
        };

        match dest {
            Value::Store(_, _) => return Err(Error::InvalidAssignment(assign.span)),
            _ => {}
        }

        // get type of destination
        let Type::Ptr(ptr) = dest.ty(checker, vars) else { unreachable!() };

        let value = GenericValue::check(checker, scope, vars, &assign.right)?
            .coerce(checker, vars, &ptr.ty)
            .ok_or(Error::CannotAssignType {
                decl: assign.right.span(),
                expected: ptr.ty.name(checker),
                offending: assign.right.span(),
            })?;

        Ok(Self { dest, value })
    }
}

/// A while loop.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct While {
    pub cond: Option<Value>,
    pub body: Block,
}

impl While {
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        ast: &ast::While,
    ) -> Result<Self, Error> {
        let cond = if let Some(cond) = &ast.cond {
            Some(
                GenericValue::check(checker, scope, vars, cond)?
                    .coerce(checker, vars, &Type::Bool)
                    .ok_or(Error::InvalidCondition(cond.span()))?,
            )
        } else {
            None
        };
        let body = Block::check(checker, scope, vars, func, &ast.body)?;

        Ok(Self { cond, body })
    }
}

/// An `else if` statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ElseIf {
    pub cond: Value,
    pub body: Block,
}

impl ElseIf {
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        ast: &ast::ElseIf,
    ) -> Result<Self, Error> {
        let cond = GenericValue::check(checker, scope, vars, &ast.cond)?
            .coerce(checker, vars, &Type::Bool)
            .ok_or(Error::InvalidCondition(ast.cond.span()))?;

        let body = Block::check(checker, scope, vars, func, &ast.body)?;

        Ok(Self { cond, body })
    }
}

/// An else statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Else {
    pub body: Block,
}

impl Else {
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        ast: &ast::Else,
    ) -> Result<Self, Error> {
        let body = Block::check(checker, scope, vars, func, &ast.body)?;

        Ok(Self { body })
    }
}

/// A branch of an [If] statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IfBranch {
    ElseIf(ElseIf),
    Else(Else),
}

/// An `if` statement.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct If {
    pub cond: Value,
    pub body: Block,
    pub branches: Vec<IfBranch>,
}

impl If {
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        ast: &ast::If,
    ) -> Result<Self, Error> {
        let cond = GenericValue::check(checker, scope, vars, &ast.cond)?
            .coerce(checker, vars, &Type::Bool)
            .ok_or(Error::InvalidCondition(ast.cond.span()))?;

        let body = Block::check(checker, scope, vars, func, &ast.body)?;

        let mut branches = Vec::new();

        for branch in &ast.branches {
            match branch {
                ast::IfBranch::ElseIf(else_if) => {
                    let else_if = ElseIf::check(checker, scope, vars, func, else_if)?;

                    branches.push(IfBranch::ElseIf(else_if));
                }
                ast::IfBranch::Else(else_) => {
                    let else_ = Else::check(checker, scope, vars, func, else_)?;

                    branches.push(IfBranch::Else(else_));
                }
            }
        }

        Ok(Self {
            cond,
            body,
            branches,
        })
    }
}

/// A statement of code.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmnt {
    FuncCall(FuncCall),
    Return(Return),
    VarDecl(VarDecl),
    Assign(Assign),
    While(While),
    If(If),
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
            ast::Expr::Binary(binary) => match binary.op {
                ast::BinaryOp::Eq => {
                    let assign = Assign::check(checker, scope, vars, func, binary)?;

                    Ok(Stmnt::Assign(assign))
                }
                _ => Err(Error::InvalidStatement(stmnt.span())),
            },
            ast::Expr::While(while_) => {
                let while_ = While::check(checker, scope, vars, func, while_)?;

                Ok(Stmnt::While(while_))
            }
            ast::Expr::If(if_) => {
                let if_ = If::check(checker, scope, vars, func, if_)?;

                Ok(Stmnt::If(if_))
            }
            _ => Err(Error::InvalidStatement(stmnt.span())),
        }
    }
}

/// A block in Amp.
#[derive(Clone, Debug, PartialEq, Eq)]
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
        let mut scope = Scope::new(Some(scope)); // the new scope for the block
        let mut value = Vec::new();

        for stmnt in &block.value {
            let stmnt = Stmnt::check(checker, &mut scope, vars, func, stmnt)?;
            value.push(stmnt);
        }

        Ok(Self { value })
    }
}
