use crate::{ast, error::Error, span::Spanned, typechecker::var::Var};

use super::{
    func::Func,
    scope::Scope,
    struct_::StructId,
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

            let value = GenericValue::check(checker, scope, vars, value)?
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

            let value = value.coerce_default();

            (value.ty(checker, vars), Some(value))
        };

        let id = vars.declare_var(Var::new(decl.span, decl.name.value.clone(), ty));
        scope.define_var(decl.name.value.clone(), id);

        Ok(Self { var: id, value })
    }
}

/// The destination of an assignment.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignDest {
    Var(VarId),
    Deref(Value),

    /// Assigns to a field of a struct.
    StructField(Value, StructId, usize),
}

impl AssignDest {
    /// Checks a destination to assign.
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &mut Vars,
        func: &Func,
        dest: &ast::Expr,
    ) -> Result<Self, Error> {
        match &dest {
            ast::Expr::Iden(name) => {
                let var = scope
                    .resolve_var(&name.value)
                    .ok_or(Error::UndeclaredVariable(Spanned::new(
                        name.span,
                        name.value.clone(),
                    )))?;

                Ok(Self::Var(var))
            }
            ast::Expr::Unary(unary) => match unary.op {
                ast::UnaryOp::Deref => {
                    let value = GenericValue::check(checker, scope, vars, &unary.expr)?;

                    match value.default_type(checker, vars) {
                        Type::Ptr(ptr) => {
                            if ptr.mutability != Mutability::Mut {
                                return Err(Error::CannotChangeImmutable(unary.span));
                            }
                        }
                        _ => return Err(Error::InvalidDeref(unary.span)),
                    }

                    Ok(Self::Deref(value.coerce_default()))
                }
                _ => Err(Error::InvalidAssignment(dest.span())),
            },
            ast::Expr::Binary(binary) => match binary.op {
                ast::BinaryOp::Dot => {
                    let left = GenericValue::check(checker, scope, vars, &binary.left)?;

                    // get id of struct type
                    let id = left
                        .resolve_struct(checker, vars)
                        .ok_or(Error::AccessNonStruct(binary.left.span()))?;

                    let iden = match binary.right.as_ref() {
                        ast::Expr::Iden(iden) => iden,
                        _ => return Err(Error::ExpectedFieldName(dest.span())),
                    };

                    let struct_decl = &checker.structs[id.0];

                    if let Some((field_id, _)) = struct_decl.get_field(&iden.value) {
                        if left.is_pointer(checker, vars) {
                            Ok(Self::StructField(
                                // already a pointer
                                left.coerce_default(),
                                id,
                                field_id,
                            ))
                        } else {
                            Ok(Self::StructField(
                                // get address of value to assign to
                                left.as_ref(Mutability::Mut).coerce_default(),
                                id,
                                field_id,
                            ))
                        }
                    } else {
                        return Err(Error::UnknownStructField(iden.span));
                    }
                }
                _ => Err(Error::InvalidAssignment(dest.span())),
            },
            _ => Err(Error::InvalidAssignment(dest.span())),
        }
    }
}

/// Assigns a value to a variable.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Assign {
    pub dest: AssignDest,
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
        let dest = AssignDest::check(checker, scope, vars, func, &assign.left)?;

        match &dest {
            AssignDest::Var(var) => {
                let value = GenericValue::check(checker, scope, vars, &assign.right)?
                    .coerce(checker, vars, &vars.vars[var.0].ty)
                    .ok_or(Error::CannotAssignType {
                        decl: vars.vars[var.0].span,
                        expected: vars.vars[var.0].ty.name(checker),
                        offending: assign.right.span(),
                    })?;

                Ok(Self { dest, value })
            }
            AssignDest::Deref(deref) => {
                let Type::Ptr(ptr) = deref.ty(checker, vars) else { unreachable!() };

                let value = GenericValue::check(checker, scope, vars, &assign.right)?
                    .coerce(checker, vars, &ptr.ty)
                    .ok_or(Error::CannotAssignType {
                        decl: assign.right.span(),
                        expected: ptr.ty.name(checker),
                        offending: assign.right.span(),
                    })?;

                Ok(Self { dest, value })
            }
            AssignDest::StructField(_, id, field) => {
                let struct_decl = &checker.structs[id.0];

                let value = GenericValue::check(checker, scope, vars, &assign.right)?
                    .coerce(checker, vars, &struct_decl.fields[*field].ty.value)
                    .ok_or(Error::CannotAssignType {
                        decl: assign.right.span(),
                        expected: struct_decl.fields[*field].ty.value.name(checker),
                        offending: assign.right.span(),
                    })?;

                Ok(Self { dest, value })
            }
        }
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

/// A statement of code.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmnt {
    FuncCall(FuncCall),
    Return(Return),
    VarDecl(VarDecl),
    Assign(Assign),
    While(While),
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
        let mut value = Vec::new();

        for stmnt in &block.value {
            let stmnt = Stmnt::check(checker, scope, vars, func, stmnt)?;
            value.push(stmnt);
        }

        Ok(Self { value })
    }
}
