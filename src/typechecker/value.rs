use std::collections::HashMap;

use crate::{
    ast::{self},
    error::Error,
    span::Spanned,
};

use super::{
    decl::Modifier,
    func::FuncId,
    scope::{Scope, ScopeValue, Subscope},
    struct_::StructId,
    types::{self, Func, Mutability, Ptr, Slice, Type},
    var::{VarId, Vars},
    Typechecker,
};

/// The callee of a function call.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Callee {
    Func(FuncId),
    Indirect(Box<Value>),
}

impl Callee {
    /// Returns the return type of the callee.
    pub fn return_type(&self, checker: &Typechecker, vars: &Vars) -> Option<Type> {
        match self {
            Callee::Func(func) => checker.funcs[func.0 as usize].signature.returns.clone(),
            Callee::Indirect(value) => match &value.ty(checker, vars) {
                Type::Func(func) => func.ret.as_ref().map(|v| v.as_ref().clone()),
                _ => unreachable!(),
            },
        }
    }
}

/// A function call value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncCall {
    pub callee: Callee,
    pub args: Vec<Value>,
}

impl FuncCall {
    /// Checks the type signature of the provided function call.
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        call: &ast::Call,
    ) -> Result<Self, Error> {
        let callee = GenericValue::check(checker, scope, vars, &call.callee, false)?;

        match callee {
            GenericValue::FuncAddr(callee) => {
                let func = &checker.funcs[callee.0 as usize];

                if func.signature.args.len() != call.args.args.len() {
                    if func.signature.variadic && call.args.args.len() < func.signature.args.len()
                        || !func.signature.variadic
                    {
                        return Err(Error::InvalidArgumentCount {
                            decl: func.span,
                            decl_type: func.signature.name(checker),
                            offending: call.span,
                        });
                    }
                }

                let mut args = Vec::new();

                for (idx, arg) in call.args.args.iter().enumerate() {
                    if idx < func.signature.args.len() {
                        let generic_value = GenericValue::check(checker, scope, vars, arg, false)?;
                        let value = generic_value
                            .coerce(checker, vars, &func.signature.args[idx].value.ty)
                            .ok_or(Error::ExpectedArgumentOfType {
                                decl: func.span,
                                name: func.signature.args[idx].value.ty.name(checker),
                                offending: arg.span(),
                            })?;
                        args.push(value);
                    } else {
                        let generic_value = GenericValue::check(checker, scope, vars, arg, false)?;
                        let value = generic_value.coerce_default(checker, vars);
                        args.push(value);
                    }
                }

                Ok(Self {
                    callee: Callee::Func(callee),
                    args,
                })
            }
            value => {
                let ty = value.default_type(checker, vars).resolve_aliases(checker);

                let Type::Func(func_ty) = ty else {
                    return Err(Error::CannotCallNonFunction(call.callee.span()));
                };

                if func_ty.args.len() != call.args.args.len() {
                    // TODO: do better diagnostics for this
                    return Err(Error::InvalidArgumentCount {
                        decl: call.span,
                        decl_type: func_ty.name(checker),
                        offending: call.span,
                    });
                }

                let mut args = Vec::new();

                for (idx, arg) in call.args.args.iter().enumerate() {
                    let generic_value = GenericValue::check(checker, scope, vars, arg, false)?;
                    // TODO: do better diagnostics for this
                    let value = generic_value
                        .coerce(checker, vars, &func_ty.args[idx])
                        .ok_or(Error::ExpectedArgumentOfType {
                            decl: call.span,
                            name: func_ty.args[idx].name(checker),
                            offending: arg.span(),
                        })?;
                    args.push(value);
                }

                Ok(Self {
                    callee: Callee::Indirect(Box::new(value.coerce_default(checker, vars))),
                    args,
                })
            }
        }
    }
}

/// A value in a scope or a scope value.  Responsible for resolving access operations.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueOrNamespace<'a> {
    /// A namespace
    Namespace(&'a Subscope),

    /// A generic value.
    Value(GenericValue),
}

impl<'a> ValueOrNamespace<'a> {
    /// Checks an access operation, assumes the operator is a [ast::BinaryOp::Dot].
    pub fn check_access(
        checker: &Typechecker,
        scope: &'a mut Scope,
        vars: &Vars,
        left: &ast::Expr,
        right: &ast::Expr,
    ) -> Result<Self, Error> {
        let lhs = Self::check_value(checker, scope, vars, left)?;
        let rhs = match right {
            ast::Expr::Iden(iden) => iden,
            _ => return Err(Error::ExpectedFieldName(right.span())),
        };

        match lhs {
            ValueOrNamespace::Namespace(namespace) => {
                let value = namespace
                    .get_value(&rhs.value)
                    .ok_or(Error::UndeclaredNamespaceMember(right.span()))?;
                match value {
                    ScopeValue::Func(func) => {
                        Ok(ValueOrNamespace::Value(GenericValue::FuncAddr(*func)))
                    }
                    ScopeValue::Var(var) => Ok(ValueOrNamespace::Value(GenericValue::Var(*var))),
                    ScopeValue::Namespace(namespace) => Ok(ValueOrNamespace::Namespace(namespace)),
                }
            }
            ValueOrNamespace::Value(lhs) => {
                let lhs_ty = lhs.default_type(checker, vars);

                let id = lhs
                    .resolve_struct(checker, vars)
                    .ok_or(Error::AccessNonStruct(left.span()))?;

                let struct_decl = &checker.structs[id.0 as usize];

                if let Some((field_id, _)) = struct_decl.get_field(&rhs.value) {
                    let field = &struct_decl.fields[field_id];

                    if !field.modifiers.contains(&Modifier::Export)
                        && struct_decl.declared_in != checker.current_module
                    {
                        return Err(Error::CannotAccessPrivateField(rhs.span));
                    }

                    Ok(ValueOrNamespace::Value(GenericValue::StructAccess(
                        Box::new(match lhs_ty {
                            Type::Ptr(Ptr { mutability, ty }) => {
                                if ty.is_sized(checker) {
                                    lhs
                                } else {
                                    if field_id == struct_decl.fields.len() - 1 {
                                        return Ok(ValueOrNamespace::Value(
                                            GenericValue::UnsizedStructAccess(
                                                Box::new(lhs),
                                                id,
                                                field_id,
                                            ),
                                        ));
                                    }

                                    GenericValue::WidePtrToPtr(
                                        Box::new(lhs),
                                        Type::Ptr(Ptr::new(mutability, ty.as_ref().clone())),
                                    )
                                }
                            }
                            _ => lhs.as_ref(checker, vars, Mutability::Mut).unwrap(),
                        }),
                        id,
                        field_id,
                    )))
                } else {
                    return Err(Error::UnknownStructField(rhs.span));
                }
            }
        }
    }

    /// Checks an access expression.
    pub fn check_value(
        checker: &Typechecker,
        scope: &'a mut Scope,
        vars: &Vars,
        value: &ast::Expr,
    ) -> Result<Self, Error> {
        match value {
            ast::Expr::Iden(iden) => {
                let value = scope
                    .get_value(&iden.value)
                    .ok_or(Error::UndeclaredVariable(Spanned::new(
                        iden.span,
                        iden.value.clone(),
                    )))?;
                match value {
                    ScopeValue::Func(func) => {
                        Ok(ValueOrNamespace::Value(GenericValue::FuncAddr(*func)))
                    }
                    ScopeValue::Var(var) => Ok(ValueOrNamespace::Value(GenericValue::Var(*var))),
                    ScopeValue::Namespace(namespace) => Ok(ValueOrNamespace::Namespace(namespace)),
                }
            }
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::Dot,
                left,
                right,
                ..
            }) => Self::check_access(checker, scope, vars, &left, &right),
            value => {
                let value = GenericValue::check(checker, scope, vars, value, false)?;
                Ok(ValueOrNamespace::Value(value))
            }
        }
    }
}

/// A `{value}` literal value before it is coerced to a specific type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenericValue {
    /// Creates a boolean literal.
    Bool(bool),

    /// Creates an integer literal.
    Int(i64),

    /// Creates a string literal as a `~const [u8]`.
    Str(String),

    /// Loads the value of a variable.
    Var(VarId),

    /// Calls a function.
    FuncCall(FuncCall),

    /// Loads a value from a pointer.
    Deref(Box<GenericValue>),

    /// Same as `~const var` or `~mut var`.
    AddrOfVar(Mutability, VarId),

    /// Same as `~const {value}` or `~mut {value}`.  Stores a literal on the stack without storing
    /// it in a variable.
    Store(Mutability, Box<GenericValue>),

    /// A constructor of a struct.
    Constructor(StructId, HashMap<usize, Value>),

    /// A field access of a struct.
    StructAccess(Box<GenericValue>, StructId, usize),

    /// Outputs the address of a field of a struct.
    AddrOfField(Mutability, Box<GenericValue>, StructId, usize),

    /// Performs an integer operation.
    IntOp(Op, Box<GenericValue>, Box<GenericValue>),

    /// Compares two values for equality.
    LogEq(Box<GenericValue>, Box<GenericValue>),

    /// Compares two values for inequality.
    LogNe(Box<GenericValue>, Box<GenericValue>),

    /// Converts an integer to another integer type.
    IntToInt(Box<GenericValue>, Type),

    /// Converts a slice to a pointer type.
    WidePtrToPtr(Box<GenericValue>, Type),

    /// Converts a slice to a different slice type.  Basically a no-op, as slice types are only
    /// used at compile time.
    WidePtrToWidePtr(Box<GenericValue>, Type),

    /// Index a slice.
    SliceIdx(Box<GenericValue>, Box<GenericValue>, Type),

    /// Index a pointer.
    PtrIdx(Box<GenericValue>, Box<GenericValue>, Type),

    /// Implicitly converts a value to the provided type.
    Coerce(Box<GenericValue>, Type),

    /// Outputs the address of a slice index.
    AddrOfSliceIdx(Mutability, Box<GenericValue>, Box<GenericValue>, Type),

    /// Outputs the address of a pointer index.
    AddrOfPtrIdx(Mutability, Box<GenericValue>, Box<GenericValue>, Type),

    /// Creates a "subslice" of a pointer.
    ///
    /// Subslice(pointer: ~const T, from: uint, to: uint, type)
    Subslice(
        Box<GenericValue>,
        Box<GenericValue>,
        Box<GenericValue>,
        Type,
    ),

    /// Accesses an unsized field on an unsized struct.  Should never be outputted by `coerce` or
    /// `coerce_default`.
    UnsizedStructAccess(Box<GenericValue>, StructId, usize),

    /// Outputs a wide pointer to the address of an unsized field on an unsized struct.
    AddrOfUnsizedField(Mutability, Box<GenericValue>, StructId, usize),

    /// Performs a bitwise NOT operation on an integer.
    BitNot(Box<GenericValue>),

    /// Negates a boolean value. (`true` => `false`, `false` => `true`)
    LogNot(Box<GenericValue>),

    /// Performs a logical AND operation on two booleans.
    LogAnd(Box<GenericValue>, Box<GenericValue>),

    /// Performs a logical OR operation on two booleans.
    LogOr(Box<GenericValue>, Box<GenericValue>),

    /// Negates an integer.
    IntNeg(Box<GenericValue>),

    /// Outputs the address of a function.
    FuncAddr(FuncId),
}

impl GenericValue {
    /// Returns the ID of the struct type if the value acts as a struct (e.g. a struct type or a
    /// struct pointer).
    pub fn resolve_struct(&self, checker: &Typechecker, vars: &Vars) -> Option<StructId> {
        match self.default_type(checker, vars) {
            Type::Struct(id) => Some(id),
            Type::Ptr(ptr) => match *ptr.ty {
                Type::Struct(id) => Some(id),
                _ => return None,
            },
            _ => return None,
        }
    }

    /// Returns `true` if the value is a pointer.
    ///
    /// TODO: check if useful, if not, remove
    pub fn is_pointer(&self, checker: &Typechecker, vars: &Vars) -> bool {
        match self.default_type(checker, vars) {
            Type::Ptr(_) => true,
            _ => false,
        }
    }

    /// Returns `true` if this is an integer value.
    pub fn is_int(&self, checker: &Typechecker, vars: &Vars) -> bool {
        self.default_type(checker, vars).is_int(checker)
    }

    /// Checks a dereference expression (`*{value}`).
    pub fn check_deref(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        expr: &ast::Expr,
    ) -> Result<Self, Error> {
        let value = Self::check(checker, scope, vars, expr, false)?;

        match value.default_type(checker, vars) {
            Type::Ptr(_) => Ok(GenericValue::Deref(Box::new(value))),
            _ => Err(Error::InvalidDeref(expr.span())),
        }
    }

    /// Checks a reference operator.
    ///
    /// ```amp
    /// var my_var = 42;
    /// MyFunction(~mut my_var);
    /// ```
    pub fn check_ref(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        mutability: Mutability,
        expr: &ast::Expr,
    ) -> Result<Self, Error> {
        Ok(Self::check(checker, scope, vars, expr, true)?
            .as_ref(checker, vars, mutability)
            .ok_or(Error::CannotReferenceAsMut(expr.span()))?)
    }

    /// Gets a reference to this value. If the value is an immediate value (for example, `~mut 8`),
    /// it will be stored on the stack.  Returns `None` if the value cannot be referenced as
    /// mutable.
    pub fn as_ref(
        &self,
        checker: &Typechecker,
        vars: &Vars,
        mutability: Mutability,
    ) -> Option<Self> {
        Some(match self {
            Self::Var(var) => Self::AddrOfVar(mutability, *var),
            Self::StructAccess(target, id, field) => {
                let ty = target.default_type(checker, vars);

                match ty {
                    Type::Ptr(ptr) => {
                        if mutability > ptr.mutability {
                            return None;
                        }
                    }
                    _ => {}
                }

                Self::AddrOfField(
                    mutability,
                    Box::new(if !target.is_pointer(checker, vars) {
                        target.as_ref().as_ref(checker, vars, mutability)?
                    } else {
                        target.as_ref().clone()
                    }),
                    *id,
                    *field,
                )
            }
            Self::SliceIdx(target, index, ty) => {
                let Type::Ptr(ptr) = target.default_type(checker, vars) else { unreachable!() };

                if mutability > ptr.mutability {
                    return None;
                }

                Self::AddrOfSliceIdx(mutability, target.clone(), index.clone(), ty.clone())
            }
            Self::PtrIdx(target, index, ty) => {
                let Type::Ptr(ptr) = target.default_type(checker, vars) else { unreachable!() };

                if mutability > ptr.mutability {
                    return None;
                }

                Self::AddrOfPtrIdx(mutability, target.clone(), index.clone(), ty.clone())
            }
            Self::UnsizedStructAccess(value, field, ty) => {
                let Type::Ptr(ptr) = value.default_type(checker, vars) else { unreachable!() };

                if mutability > ptr.mutability {
                    return None;
                }

                Self::AddrOfUnsizedField(mutability, value.clone(), *field, ty.clone())
            }
            value => Self::Store(mutability, Box::new(value.clone())),
        })
    }

    pub fn check_math_expr(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        left: &ast::Expr,
        right: &ast::Expr,
    ) -> Result<(GenericValue, GenericValue), Error> {
        let lhs = Self::check(checker, scope, vars, left, false)?;
        let rhs = Self::check(checker, scope, vars, right, false)?;

        let lhs_ty = lhs.default_type(checker, vars);

        if let Some(_) = rhs.clone().coerce(checker, vars, &lhs_ty) {
        } else {
            return Err(Error::InvalidExprTypes {
                left: Spanned::new(left.span(), lhs_ty.name(checker)),
                right: Spanned::new(right.span(), rhs.default_type(checker, vars).name(checker)),
                offending: right.span(),
            });
        }

        Ok((lhs, rhs))
    }

    /// Returns the default type for a generic value.  Never returns [Type::TypeAlias]
    pub fn default_type(&self, checker: &Typechecker, vars: &Vars) -> Type {
        match self {
            Self::Bool(_) => Type::Bool,
            Self::Int(_) => Type::Int,
            Self::Str(_) => Type::slice_ptr(Mutability::Mut, Type::U8),
            Self::Var(var) => vars.vars[var.0 as usize].ty.clone(),
            Self::FuncCall(func_call) => func_call.callee.return_type(checker, vars).unwrap(),
            Self::Deref(value) => {
                let ty = value.default_type(checker, vars);
                match ty {
                    Type::Ptr(ptr) => *ptr.ty,
                    _ => unreachable!(),
                }
            }
            Self::AddrOfVar(mutability, var) => {
                let ty = vars.vars[var.0 as usize].ty.clone();
                Type::Ptr(Ptr::new(*mutability, ty))
            }
            Self::Store(mutability, value) => {
                Type::Ptr(Ptr::new(*mutability, value.default_type(checker, vars)))
            }
            Self::Constructor(struct_id, _) => Type::Struct(*struct_id),
            Self::StructAccess(_, id, field) => {
                checker.structs[id.0].fields[*field].ty.value.clone()
            }
            Self::AddrOfField(mutability, _, id, field) => {
                let ty = checker.structs[id.0].fields[*field].ty.value.clone();
                Type::Ptr(Ptr::new(*mutability, ty))
            }
            Self::IntOp(_, lhs, _) => lhs.default_type(checker, vars),
            Self::LogEq(_, _) => Type::Bool,
            Self::LogNe(_, _) => Type::Bool,
            Self::IntToInt(_, ty) => ty.clone(),
            Self::WidePtrToPtr(_, ty) => ty.clone(),
            Self::WidePtrToWidePtr(_, ty) => ty.clone(),
            Self::SliceIdx(_, _, ty) => ty.clone(),
            Self::PtrIdx(_, _, ty) => ty.clone(),
            Self::Coerce(_, ty) => ty.clone(),
            Self::AddrOfSliceIdx(mutability, _, _, ty) => {
                Type::Ptr(Ptr::new(*mutability, ty.clone()))
            }
            Self::AddrOfPtrIdx(mutability, _, _, ty) => {
                Type::Ptr(Ptr::new(*mutability, ty.clone()))
            }
            Self::Subslice(ptr, _, _, ty) => {
                let Type::Ptr(Ptr { mutability, .. }) = ptr.default_type(checker, vars) else { unreachable!() };

                Type::slice_ptr(mutability, ty.clone())
            }
            Self::UnsizedStructAccess(_, struct_id, field) => {
                checker.structs[struct_id.0].fields[*field].ty.value.clone()
            }
            Self::AddrOfUnsizedField(mut_, _, struct_id, field) => {
                let ty = checker.structs[struct_id.0].fields[*field].ty.value.clone();

                Type::Ptr(Ptr::new(*mut_, ty.clone()))
            }
            Self::BitNot(item) => item.default_type(checker, vars),
            Self::LogNot(_) => Type::Bool,
            Self::LogAnd(_, _) => Type::Bool,
            Self::LogOr(_, _) => Type::Bool,
            Self::IntNeg(value) => value.default_type(checker, vars),
            Self::FuncAddr(f) => {
                let func = &checker.funcs[f.0 as usize];
                Type::Func(Func::from_signature(&func.signature))
            }
        }.resolve_aliases(checker)
    }

    /// Converts an ast value into a generic value, if it is a value.
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        expr: &ast::Expr,
        is_ref: bool,
    ) -> Result<Self, Error> {
        let value = match expr {
            ast::Expr::Bool(bool) => Ok(GenericValue::Bool(bool.value)),
            ast::Expr::Int(int) => Ok(GenericValue::Int(int.value)),
            ast::Expr::Str(str) => Ok(GenericValue::Str(str.value.clone())),
            ast::Expr::Iden(iden) => {
                let value = ValueOrNamespace::check_value(
                    checker,
                    scope,
                    vars,
                    &ast::Expr::Iden(iden.clone()),
                )?;

                match value {
                    ValueOrNamespace::Value(value) => Ok(value),
                    ValueOrNamespace::Namespace(_) => {
                        Err(Error::ExpectedValueGotNamespace(iden.span))
                    }
                }
            }
            ast::Expr::Call(call) => {
                let func_call = FuncCall::check(checker, scope, vars, call)?;

                if func_call.callee.return_type(checker, vars) == None {
                    return Err(Error::VoidAsValue(call.span));
                }

                Ok(GenericValue::FuncCall(func_call))
            }
            ast::Expr::Unary(unary) => match unary.op {
                ast::UnaryOp::Deref => Self::check_deref(checker, scope, vars, &unary.expr),
                ast::UnaryOp::ConstRef => {
                    Self::check_ref(checker, scope, vars, Mutability::Const, &unary.expr)
                }
                ast::UnaryOp::LogNot => {
                    let value = Self::check(checker, scope, vars, &unary.expr, false)?;

                    if value.clone().coerce(checker, vars, &Type::Bool).is_none() {
                        return Err(Error::InvalidLogNot(expr.span()));
                    }

                    Ok(GenericValue::LogNot(Box::new(value)))
                }
                ast::UnaryOp::MutRef => {
                    Self::check_ref(checker, scope, vars, Mutability::Mut, &unary.expr)
                }
                ast::UnaryOp::Tilde => {
                    let value = Self::check(checker, scope, vars, &unary.expr, false)?;

                    if !value.is_int(checker, vars) {
                        return Err(Error::InvalidBitNot(expr.span()));
                    }

                    Ok(GenericValue::BitNot(Box::new(value)))
                }
                ast::UnaryOp::Neg => {
                    let value = Self::check(checker, scope, vars, &unary.expr, false)?;

                    let ty = value.default_type(checker, vars);

                    match ty {
                        Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::Int => {}
                        _ => return Err(Error::InvalidNeg(expr.span())),
                    }

                    Ok(GenericValue::IntNeg(Box::new(value)))
                }
            },
            ast::Expr::Constructor(constructor) => {
                let ty = types::check_type_decl_path(scope, &*constructor.ty)?;
                let id = ty
                    .get_struct_id(checker)
                    .ok_or(Error::CannotConstructNonStruct(constructor.ty.span()))?;

                let struct_decl = &checker.structs[id.0 as usize];

                if !struct_decl.can_construct && struct_decl.declared_in != checker.current_module {
                    return Err(Error::CannotConstructPrivateStruct(constructor.ty.span()));
                }

                let mut fields = HashMap::new();

                for field in &constructor.fields {
                    let (field_id, field_decl) = struct_decl
                        .get_field(&field.name.value)
                        .ok_or(Error::UnknownStructField(field.span))?;

                    if fields.contains_key(&field_id) {
                        return Err(Error::DuplicateFieldDefinition(field.span));
                    }

                    let value = Self::check(checker, scope, vars, &field.value, false)?
                        .coerce(checker, vars, &field_decl.ty.value)
                        .ok_or(Error::ExpectedFieldOfType {
                            decl: field.span,
                            name: field_decl.ty.value.name(checker),
                            offending: field.span,
                        })?;

                    fields.insert(field_id, value);
                }

                Ok(GenericValue::Constructor(id, fields))
            }
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::Dot,
                left,
                right,
                ..
            }) => {
                let res = ValueOrNamespace::check_access(checker, scope, vars, left, right)?;

                match res {
                    ValueOrNamespace::Value(value) => Ok(value),
                    ValueOrNamespace::Namespace(_) => {
                        Err(Error::ExpectedValueGotNamespace(expr.span()))
                    }
                }
            }
            // Comparison operators
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::LogEq,
                left,
                right,
                ..
            }) => {
                let (lhs, rhs) = Self::check_math_expr(checker, scope, vars, left, right)?;

                Ok(Self::LogEq(Box::new(lhs), Box::new(rhs)))
            }
            // Comparison operators
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::LogNe,
                left,
                right,
                ..
            }) => {
                let (lhs, rhs) = Self::check_math_expr(checker, scope, vars, left, right)?;

                Ok(Self::LogNe(Box::new(lhs), Box::new(rhs)))
            }
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::LogAnd,
                left,
                right,
                ..
            }) => {
                let (lhs, rhs) = Self::check_math_expr(checker, scope, vars, left, right)?;

                if lhs.clone().coerce(checker, vars, &Type::Bool).is_none() {
                    return Err(Error::InvalidLogAnd(left.span()));
                }

                if rhs.clone().coerce(checker, vars, &Type::Bool).is_none() {
                    return Err(Error::InvalidLogAnd(right.span()));
                }

                Ok(GenericValue::LogAnd(Box::new(lhs), Box::new(rhs)))
            }
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::LogOr,
                left,
                right,
                ..
            }) => {
                let (lhs, rhs) = Self::check_math_expr(checker, scope, vars, left, right)?;

                if lhs.clone().coerce(checker, vars, &Type::Bool).is_none() {
                    return Err(Error::InvalidLogOr(left.span()));
                }

                if rhs.clone().coerce(checker, vars, &Type::Bool).is_none() {
                    return Err(Error::InvalidLogOr(right.span()));
                }

                Ok(GenericValue::LogOr(Box::new(lhs), Box::new(rhs)))
            }
            ast::Expr::Binary(ast::Binary {
                span,
                op,
                left,
                right,
            }) => {
                let op = match op {
                    ast::BinaryOp::Add => Op::Add,
                    ast::BinaryOp::Sub => Op::Sub,
                    ast::BinaryOp::Mul => Op::Mul,
                    ast::BinaryOp::Div => Op::Div,
                    ast::BinaryOp::Mod => Op::Mod,
                    ast::BinaryOp::LtEq => Op::LtEq,
                    ast::BinaryOp::Lt => Op::Lt,
                    ast::BinaryOp::GtEq => Op::GtEq,
                    ast::BinaryOp::Gt => Op::Gt,
                    ast::BinaryOp::Shl => Op::Shl,
                    ast::BinaryOp::Shr => Op::Shr,
                    ast::BinaryOp::BitAnd => Op::BitAnd,
                    ast::BinaryOp::BitXor => Op::BitXor,
                    ast::BinaryOp::BitOr => Op::BitOr,
                    _ => unreachable!("purposefully put last"),
                };

                if op == Op::Shl || op == Op::Shr {
                    let lhs = Self::check(checker, scope, vars, left, false)?;
                    let rhs = Self::check(checker, scope, vars, right, false)?;

                    if lhs.is_int(checker, vars) && rhs.is_int(checker, vars) {
                        return Ok(GenericValue::IntOp(op, Box::new(lhs), Box::new(rhs)));
                    } else {
                        return Err(Error::NonNumberMath {
                            ty: Spanned::new(
                                left.span(),
                                lhs.default_type(checker, vars).name(checker),
                            ),
                            offending: *span,
                        });
                    }
                }

                let (lhs, rhs) = Self::check_math_expr(checker, scope, vars, left, right)?;

                if lhs.is_int(checker, vars) {
                    return Ok(GenericValue::IntOp(op, Box::new(lhs), Box::new(rhs)));
                } else {
                    return Err(Error::NonNumberMath {
                        ty: Spanned::new(
                            left.span(),
                            lhs.default_type(checker, vars).name(checker),
                        ),
                        offending: *span,
                    });
                }
            }
            ast::Expr::As(as_) => {
                let value = Self::check(checker, scope, vars, &as_.expr, false)?;

                let ty = Type::check(checker, scope, &as_.ty)?;

                if value.clone().coerce(checker, vars, &ty).is_some() {
                    Ok(GenericValue::Coerce(Box::new(value), ty))
                } else {
                    Ok(value.clone().convert(checker, vars, &ty).ok_or(
                        Error::InvalidConversion {
                            from: value.default_type(checker, vars).name(checker),
                            to: ty.name(checker),
                            offending: as_.span,
                        },
                    )?)
                }
            }
            ast::Expr::Idx(idx) => {
                // NOTE: the left hand side of index operators are always references
                let value = Self::check(checker, scope, vars, &idx.expr, false)?;

                let index = Self::check(checker, scope, vars, &idx.index, false)?;

                // check if `index` is a `uint`
                index
                    .clone()
                    .coerce(checker, vars, &Type::Uint)
                    .ok_or(Error::ExpectedUintIndex(idx.index.span()))?;

                if let Some(to) = &idx.to {
                    let to_value = Self::check(checker, scope, vars, to, false)?;

                    // check if `to` is a `uint`
                    to_value
                        .clone()
                        .coerce(checker, vars, &Type::Uint)
                        .ok_or(Error::ExpectedUintIndex(to.span()))?;

                    let ty = value.default_type(checker, vars);
                    let (ptr, item_ty) = match &ty {
                        Type::Ptr(ptr) => match ptr.ty.as_ref() {
                            Type::Slice(slice) => (
                                Self::WidePtrToPtr(
                                    Box::new(value),
                                    Type::Ptr(Ptr::new(ptr.mutability, slice.ty.as_ref().clone())),
                                ),
                                slice.ty.as_ref().clone(),
                            ),
                            _ => (value, ty),
                        },
                        _ => {
                            return Err(Error::CannotIndex {
                                ty: ty.name(checker),
                                offending: idx.expr.span(),
                            })
                        }
                    };

                    return Ok(Self::Subslice(
                        // slice is always a pointer
                        Box::new(ptr),
                        Box::new(index),
                        Box::new(to_value),
                        item_ty,
                    ));
                }

                let ty = value.default_type(checker, vars);
                match &ty {
                    Type::Ptr(ptr) => match ptr.ty.as_ref() {
                        Type::Slice(slice) => Ok(Self::SliceIdx(
                            Box::new(value),
                            Box::new(index),
                            slice.ty.as_ref().clone(),
                        )),
                        _ => {
                            if ptr.ty.is_sized(checker) {
                                Ok(Self::PtrIdx(
                                    Box::new(value),
                                    Box::new(index),
                                    ptr.ty.as_ref().clone(),
                                ))
                            } else {
                                Err(Error::CannotIndex {
                                    ty: ty.name(checker),
                                    offending: idx.expr.span(),
                                })
                            }
                        }
                    },
                    _ => {
                        return Err(Error::CannotIndex {
                            ty: ty.name(checker),
                            offending: idx.expr.span(),
                        })
                    }
                }
            }
            _ => return Err(Error::InvalidValue(expr.span())),
        }?;

        if !is_ref {
            let ty = value.default_type(checker, vars);

            if !ty.is_sized(checker) {
                return Err(Error::OwnedUnsizedType(expr.span()));
            } else {
                Ok(value)
            }
        } else {
            Ok(value)
        }
    }

    /// Converts a value to a different type.  Assumes any non-conversions (for example,
    /// `bool as bool`) have been checked.
    pub fn convert(self, checker: &Typechecker, vars: &Vars, to: &Type) -> Option<Self> {
        // assume any basic conversions (i.e. bool => bool have already been covered.)
        match (
            self.default_type(checker, vars),
            to.clone().resolve_aliases(checker),
        ) {
            (left_ty, right_ty) if left_ty.is_int(checker) && right_ty.is_int(checker) => {
                Some(Self::IntToInt(Box::new(self), right_ty.clone()))
            }
            (ty, Type::Ptr(ptr)) if ty.is_int(checker) => {
                if ptr.is_wide(checker) {
                    return None;
                }

                Some(Self::IntToInt(Box::new(self), to.clone()))
            }
            // => Some(Self::IntToInt(Box::new(self), to.clone())),
            (Type::Ptr(left), Type::Ptr(right)) => {
                match (left.is_wide(checker), right.is_wide(checker)) {
                    (true, true) => Some(Self::WidePtrToWidePtr(Box::new(self), to.clone())),
                    (true, false) => Some(Self::WidePtrToPtr(Box::new(self), to.clone())),
                    (false, false) => Some(Self::IntToInt(Box::new(self), to.clone())),

                    // Cannot convert thin pointer to wide pointer
                    (false, true) => None,
                }
            }
            (Type::Ptr(_), to) if to.is_int(checker) => {
                Some(Self::IntToInt(Box::new(self), to.clone()))
            }
            _ => None,
        }
    }

    /// Returns the default value type for this generic value.
    pub fn coerce_default(self, checker: &Typechecker, vars: &Vars) -> Value {
        match self {
            GenericValue::Bool(bool) => Value::Bool(bool),
            GenericValue::Int(int) => Value::Int(int as i64),
            GenericValue::Str(str) => Value::Str(str),
            GenericValue::Var(var) => Value::Var(var),
            GenericValue::FuncCall(call) => Value::FuncCall(call),
            GenericValue::Deref(val) => Value::Deref(Box::new(val.coerce_default(checker, vars))),
            GenericValue::AddrOfVar(mutability, var) => Value::AddrOfVar(mutability, var),
            GenericValue::Store(mutability, var) => {
                Value::Store(mutability, Box::new(var.coerce_default(checker, vars)))
            }
            GenericValue::Constructor(struct_id, fields) => Value::Constructor(struct_id, fields),
            GenericValue::StructAccess(value, id, field) => {
                Value::StructAccess(Box::new(value.coerce_default(checker, vars)), id, field)
            }
            GenericValue::AddrOfField(mutability, value, id, field) => Value::AddrOfField(
                mutability,
                Box::new(value.coerce_default(checker, vars)),
                id,
                field,
            ),
            GenericValue::IntOp(op, lhs, rhs) => {
                if op == Op::Shl || op == Op::Shr {
                    return Value::IntOp(
                        op,
                        Box::new(lhs.coerce_default(checker, vars)),
                        Box::new(rhs.coerce_default(checker, vars)),
                    );
                }

                let ty = lhs.default_type(checker, vars);
                Value::IntOp(
                    op,
                    Box::new(lhs.coerce_default(checker, vars)),
                    Box::new(rhs.coerce(checker, vars, &ty).expect("verified previously")),
                )
            }
            GenericValue::LogEq(lhs, rhs) => {
                let ty = lhs.default_type(checker, vars);
                Value::LogEq(
                    Box::new(lhs.coerce_default(checker, vars)),
                    Box::new(rhs.coerce(checker, vars, &ty).expect("verified previously")),
                )
            }
            GenericValue::LogNe(lhs, rhs) => {
                let ty = lhs.default_type(checker, vars);
                Value::LogNe(
                    Box::new(lhs.coerce_default(checker, vars)),
                    Box::new(rhs.coerce(checker, vars, &ty).expect("verified previously")),
                )
            }
            GenericValue::IntToInt(from, ty) => {
                Value::IntToInt(Box::new(from.coerce_default(checker, vars)), ty)
            }
            GenericValue::WidePtrToPtr(from, ty) => {
                Value::WidePtrToPtr(Box::new(from.coerce_default(checker, vars)), ty)
            }
            GenericValue::WidePtrToWidePtr(from, ty) => {
                Value::WidePtrToWidePtr(Box::new(from.coerce_default(checker, vars)), ty)
            }
            GenericValue::SliceIdx(slice, idx, ty) => Value::SliceIdx(
                Box::new(slice.coerce_default(checker, vars)),
                Box::new(
                    idx.coerce(checker, vars, &Type::Uint)
                        .expect("verified earlier"),
                ),
                ty,
            ),
            GenericValue::PtrIdx(ptr, idx, ty) => Value::PtrIdx(
                Box::new(ptr.coerce_default(checker, vars)),
                Box::new(
                    idx.coerce(checker, vars, &Type::Uint)
                        .expect("verified earlier"),
                ),
                ty,
            ),
            GenericValue::Coerce(value, ty) => {
                value.coerce(checker, vars, &ty).expect("confirmed earlier")
            }
            GenericValue::AddrOfSliceIdx(mutability, value, idx, ty) => Value::AddrOfSliceIdx(
                mutability,
                Box::new(value.coerce_default(checker, vars)),
                Box::new(
                    idx.coerce(checker, vars, &Type::Uint)
                        .expect("verified earlier"),
                ),
                ty,
            ),
            GenericValue::AddrOfPtrIdx(mutability, value, idx, ty) => Value::AddrOfPtrIdx(
                mutability,
                Box::new(value.coerce_default(checker, vars)),
                Box::new(
                    idx.coerce(checker, vars, &Type::Uint)
                        .expect("verified earlier"),
                ),
                ty,
            ),
            GenericValue::Subslice(value, start, end, ty) => Value::Subslice(
                Box::new(value.coerce_default(checker, vars)),
                Box::new(
                    start
                        .coerce(checker, vars, &Type::Uint)
                        .expect("verified earlier"),
                ),
                Box::new(
                    end.coerce(checker, vars, &Type::Uint)
                        .expect("verified earlier"),
                ),
                ty,
            ),
            GenericValue::UnsizedStructAccess(..) => unreachable!("unsized value"),
            GenericValue::AddrOfUnsizedField(mut_, value, struct_id, field) => {
                Value::AddrOfUnsizedField(
                    mut_,
                    Box::new(value.coerce_default(checker, vars)),
                    struct_id,
                    field,
                )
            }
            GenericValue::BitNot(value) => {
                Value::BitNot(Box::new(value.coerce_default(checker, vars)))
            }
            GenericValue::LogNot(value) => {
                Value::LogNot(Box::new(value.coerce_default(checker, vars)))
            }
            GenericValue::LogAnd(left, right) => Value::LogAnd(
                Box::new(left.coerce_default(checker, vars)),
                Box::new(right.coerce_default(checker, vars)),
            ),
            GenericValue::LogOr(left, right) => Value::LogOr(
                Box::new(left.coerce_default(checker, vars)),
                Box::new(right.coerce_default(checker, vars)),
            ),
            GenericValue::IntNeg(value) => {
                Value::IntNeg(Box::new(value.coerce_default(checker, vars)))
            }
            GenericValue::FuncAddr(func_id) => Value::FuncAddr(func_id),
        }
    }

    /// Attempts to coerce this generic value into a value of the specified type.
    pub fn coerce(self, checker: &Typechecker, vars: &Vars, ty: &Type) -> Option<Value> {
        match (self, &ty.clone().resolve_aliases(checker)) {
            (GenericValue::Bool(bool), Type::Bool) => Some(Value::Bool(bool)),
            (GenericValue::Int(int), Type::I8) => Some(Value::I8(int as i8)),
            (GenericValue::Int(int), Type::I16) => Some(Value::I16(int as i16)),
            (GenericValue::Int(int), Type::I32) => Some(Value::I32(int as i32)),
            (GenericValue::Int(int), Type::I64) => Some(Value::I64(int as i64)),
            (GenericValue::Int(int), Type::Int) => Some(Value::Int(int as i64)),
            (GenericValue::Int(int), Type::U8) => Some(Value::U8(int as u8)),
            (GenericValue::Int(int), Type::U16) => Some(Value::U16(int as u16)),
            (GenericValue::Int(int), Type::U32) => Some(Value::U32(int as u32)),
            (GenericValue::Int(int), Type::U64) => Some(Value::U64(int as u64)),
            (GenericValue::Int(int), Type::Uint) => Some(Value::Uint(int as u64)),
            (GenericValue::Str(str), Type::Ptr(Ptr { ty, .. })) => {
                match ty.clone().resolve_aliases(checker) {
                    Type::Slice(Slice { ty }) => {
                        if ty.is_equivalent(checker, &Type::U8) {
                            Some(Value::Str(str))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (GenericValue::Var(var), ty) => {
                if vars.vars[var.0].ty.is_equivalent(checker, ty) {
                    Some(Value::Var(var))
                } else {
                    None
                }
            }
            (GenericValue::FuncCall(call), ty) => {
                if call
                    .callee
                    .return_type(checker, vars)
                    .unwrap()
                    .is_equivalent(checker, ty)
                {
                    Some(Value::FuncCall(call))
                } else {
                    None
                }
            }
            (GenericValue::Deref(val), ty) => match val.default_type(checker, vars) {
                Type::Ptr(ptr) => {
                    if ptr.ty.is_equivalent(checker, ty) {
                        Some(Value::Deref(Box::new(val.coerce_default(checker, vars))))
                    } else {
                        None
                    }
                }
                _ => unreachable!(),
            },
            (GenericValue::AddrOfVar(mut_, var), Type::Ptr(ptr)) => {
                if vars.vars[var.0].ty.is_equivalent(checker, &*ptr.ty) {
                    Some(Value::AddrOfVar(mut_, var))
                } else {
                    None
                }
            }
            (GenericValue::Store(mut_, val), Type::Ptr(ty)) if mut_ >= ty.mutability => Some(
                Value::Store(ty.mutability, Box::new(val.coerce(checker, vars, &ty.ty)?)),
            ),
            (GenericValue::Constructor(struct_id, fields), Type::Struct(struct_ty))
                if struct_id == *struct_ty =>
            {
                Some(Value::Constructor(struct_id, fields))
            }
            (GenericValue::StructAccess(value, id, field), ty) => {
                let struct_decl = &checker.structs[id.0];

                if struct_decl.fields[field]
                    .ty
                    .value
                    .is_equivalent(checker, ty)
                {
                    Some(Value::StructAccess(
                        Box::new(value.coerce_default(checker, vars)),
                        id,
                        field,
                    ))
                } else {
                    None
                }
            }
            (GenericValue::AddrOfField(mutability, value, id, field), ty) => {
                let struct_decl = &checker.structs[id.0];

                if Type::Ptr(Ptr::new(
                    mutability,
                    struct_decl.fields[field].ty.value.clone(),
                ))
                .is_equivalent(checker, ty)
                {
                    Some(Value::AddrOfField(
                        mutability,
                        Box::new(value.coerce_default(checker, vars)),
                        id,
                        field,
                    ))
                } else {
                    None
                }
            }
            (GenericValue::IntOp(op, lhs, rhs), ty) => match op {
                Op::LtEq | Op::Lt | Op::GtEq | Op::Gt => {
                    if ty.is_equivalent(checker, &Type::Bool) {
                        let ty = lhs.default_type(checker, vars);
                        Some(Value::IntOp(
                            op,
                            Box::new(lhs.coerce_default(checker, vars)),
                            Box::new(rhs.coerce(checker, vars, &ty).expect("verified previously")),
                        ))
                    } else {
                        None
                    }
                }
                _ => Some(Value::IntOp(
                    op,
                    Box::new(lhs.coerce(checker, vars, ty)?),
                    Box::new(rhs.coerce(checker, vars, ty)?),
                )),
            },
            (GenericValue::LogEq(lhs, rhs), Type::Bool) => {
                let left = lhs.coerce_default(checker, vars);
                let left_ty = left.ty(checker, vars);
                Some(Value::LogEq(
                    Box::new(left),
                    Box::new(rhs.coerce(checker, vars, &left_ty).unwrap()),
                ))
            }
            (GenericValue::LogNe(lhs, rhs), Type::Bool) => {
                let left = lhs.coerce_default(checker, vars);
                let left_ty = left.ty(checker, vars);
                Some(Value::LogNe(
                    Box::new(left),
                    Box::new(rhs.coerce(checker, vars, &left_ty).unwrap()),
                ))
            }
            (GenericValue::IntToInt(val, ty), to) if ty.is_equivalent(checker, to) => Some(
                Value::IntToInt(Box::new(val.coerce_default(checker, vars)), ty),
            ),
            (GenericValue::WidePtrToPtr(val, ty), to) if ty.is_equivalent(checker, to) => Some(
                Value::WidePtrToPtr(Box::new(val.coerce_default(checker, vars)), ty),
            ),
            (GenericValue::WidePtrToWidePtr(val, ty), to) if ty.is_equivalent(checker, to) => Some(
                Value::WidePtrToWidePtr(Box::new(val.coerce_default(checker, vars)), ty),
            ),
            (GenericValue::SliceIdx(slice, idx, ty), to) if ty.is_equivalent(checker, to) => {
                Some(Value::SliceIdx(
                    Box::new(slice.coerce_default(checker, vars)),
                    Box::new(idx.coerce(checker, vars, &Type::Uint)?),
                    ty,
                ))
            }
            (GenericValue::PtrIdx(slice, idx, ty), to) if ty.is_equivalent(checker, to) => {
                Some(Value::PtrIdx(
                    Box::new(slice.coerce_default(checker, vars)),
                    Box::new(idx.coerce(checker, vars, &Type::Uint)?),
                    ty,
                ))
            }
            (GenericValue::Coerce(value, ty), to) if ty.is_equivalent(checker, to) => {
                value.coerce(checker, vars, to)
            }
            (GenericValue::AddrOfSliceIdx(mutability, slice, idx, ty), to) => {
                let ty = Type::Ptr(Ptr::new(mutability, ty.clone()));

                if ty.is_equivalent(checker, to) {
                    Some(Value::AddrOfSliceIdx(
                        mutability,
                        Box::new(slice.coerce_default(checker, vars)),
                        Box::new(idx.coerce(checker, vars, &Type::Uint)?),
                        ty,
                    ))
                } else {
                    None
                }
            }
            (GenericValue::AddrOfPtrIdx(mutability, ptr, idx, ty), to) => {
                let ty = Type::Ptr(Ptr::new(mutability, ty.clone()));

                if ty.is_equivalent(checker, to) {
                    Some(Value::AddrOfPtrIdx(
                        mutability,
                        Box::new(ptr.coerce_default(checker, vars)),
                        Box::new(idx.coerce(checker, vars, &Type::Uint)?),
                        ty,
                    ))
                } else {
                    None
                }
            }
            (GenericValue::Subslice(ptr, start, end, item_ty), Type::Ptr(Ptr { ty, .. })) => {
                match ty.clone().resolve_aliases(checker) {
                    Type::Slice(Slice { ty }) => {
                        if ty.is_equivalent(checker, &Type::U8) {
                            Some(Value::Subslice(
                                Box::new(ptr.coerce_default(checker, vars)),
                                Box::new(start.coerce(checker, vars, &Type::Uint).unwrap()),
                                Box::new(end.coerce(checker, vars, &Type::Uint).unwrap()),
                                item_ty,
                            ))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            (GenericValue::AddrOfUnsizedField(mutability, value, struct_id, field), to) => {
                let struct_decl = &checker.structs[struct_id.0];
                let ty = Type::Ptr(Ptr::new(
                    mutability,
                    struct_decl.fields[field].ty.value.clone(),
                ));

                if ty.is_equivalent(checker, to) {
                    Some(Value::AddrOfUnsizedField(
                        mutability,
                        Box::new(value.coerce_default(checker, vars)),
                        struct_id,
                        field,
                    ))
                } else {
                    None
                }
            }
            (GenericValue::BitNot(value), ty) if ty.is_int(checker) => {
                if let Some(value) = value.coerce(checker, vars, ty) {
                    Some(Value::BitNot(Box::new(value)))
                } else {
                    None
                }
            }
            (GenericValue::LogAnd(left, right), Type::Bool) => Some(Value::LogAnd(
                Box::new(left.coerce_default(checker, vars)),
                Box::new(right.coerce_default(checker, vars)),
            )),
            (GenericValue::LogOr(left, right), Type::Bool) => Some(Value::LogOr(
                Box::new(left.coerce_default(checker, vars)),
                Box::new(right.coerce_default(checker, vars)),
            )),
            (GenericValue::LogNot(value), Type::Bool) => {
                Some(Value::LogNot(Box::new(value.coerce_default(checker, vars))))
            }
            (GenericValue::FuncAddr(func_id), Type::Func(to)) => {
                let left_func = Func::from_signature(&checker.funcs[func_id.0].signature);

                if left_func.is_equivalent(checker, to) {
                    Some(Value::FuncAddr(func_id))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

/// An operation on a number.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LtEq,
    Lt,
    GtEq,
    Gt,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
}

/// A value expression in an Amp module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    /// A boolean value.
    Bool(bool),

    /// A string literal as a byte slice: `~const [u8]` or `~mut [u8]`.
    Str(String),

    /// An 8-bit unsigned integer.
    U8(u8),

    /// A 16-bit unsigned integer.
    U16(u16),

    /// A 32-bit unsigned integer.
    U32(u32),

    /// A 64-bit unsigned integer.
    U64(u64),

    /// A pointer sized integer.
    Uint(u64),

    /// An 8-bit unsigned integer.
    I8(i8),

    /// A 16-bit unsigned integer.
    I16(i16),

    /// A 32-bit unsigned integer.
    I32(i32),

    /// A 64-bit unsigned integer.
    I64(i64),

    /// A pointer sized integer.
    Int(i64),

    /// Reads the value of a variable.
    Var(VarId),

    /// Calls a function.
    FuncCall(FuncCall),

    /// Loads a value from a pointer.
    Deref(Box<Value>),

    /// Returns the address of the provided variable.
    AddrOfVar(Mutability, VarId),

    /// Stores a value on the stack, outputting the address as a pointer.
    Store(Mutability, Box<Value>),

    /// Creates a new struct valuie
    Constructor(StructId, HashMap<usize, Value>),

    /// Accesses a field in a struct.
    StructAccess(Box<Value>, StructId, usize),

    /// Outputs the address of a field.
    AddrOfField(Mutability, Box<Value>, StructId, usize),

    /// Performs an integer operation.
    IntOp(Op, Box<Value>, Box<Value>),

    /// Compares two values.
    LogEq(Box<Value>, Box<Value>),

    /// Compares two values.
    LogNe(Box<Value>, Box<Value>),

    /// Converts an integer type to another integer type.
    IntToInt(Box<Value>, Type),

    /// Converts a slice to a pointer
    WidePtrToPtr(Box<Value>, Type),

    /// Converts a slice to a different slice type.
    WidePtrToWidePtr(Box<Value>, Type),

    /// Indexes into a slice.
    SliceIdx(Box<Value>, Box<Value>, Type),

    /// Indexes into a pointer.
    PtrIdx(Box<Value>, Box<Value>, Type),

    /// Outputs the address of an index in a slice.
    AddrOfSliceIdx(Mutability, Box<Value>, Box<Value>, Type),

    /// Outputs the address of an index in a pointer.
    AddrOfPtrIdx(Mutability, Box<Value>, Box<Value>, Type),

    /// Creates a subslice of a slice.
    ///
    /// The first parameter is always a pointer.
    Subslice(Box<Value>, Box<Value>, Box<Value>, Type),

    /// Outputs the address of an unsized field in an unsized struct.
    AddrOfUnsizedField(Mutability, Box<Value>, StructId, usize),

    /// Performs a bitwise NOT on an integer.
    BitNot(Box<Value>),

    /// Performs a logical NOT on a boolean.
    LogNot(Box<Value>),

    /// Performs a logical AND on two booleans.
    LogAnd(Box<Value>, Box<Value>),

    /// Performs a logical OR on two booleans.
    LogOr(Box<Value>, Box<Value>),

    /// Negates an integer.
    IntNeg(Box<Value>),

    /// Returns the address of a function.
    FuncAddr(FuncId),
}

impl Value {
    /// Returns the type of this value.
    pub fn ty(&self, checker: &Typechecker, vars: &Vars) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::Str(_) => Type::slice_ptr(Mutability::Mut, Type::U8),
            Value::U8(_) => Type::U8,
            Value::U16(_) => Type::U16,
            Value::U32(_) => Type::U32,
            Value::U64(_) => Type::U64,
            Value::Uint(_) => Type::Uint,
            Value::I8(_) => Type::I8,
            Value::I16(_) => Type::I16,
            Value::I32(_) => Type::I32,
            Value::I64(_) => Type::I64,
            Value::Int(_) => Type::Int,
            Value::Var(var) => vars.vars[var.0].ty.clone(),
            Value::FuncCall(call) => call.callee.return_type(checker, vars).unwrap(),
            Value::Deref(ptr) => match ptr.ty(checker, vars) {
                Type::Ptr(ptr) => *ptr.ty,
                _ => unreachable!(),
            },
            Value::AddrOfVar(mutability, var) => {
                Type::Ptr(Ptr::new(*mutability, vars.vars[var.0].ty.clone()))
            }
            Value::Store(mutability, val) => {
                Type::Ptr(Ptr::new(*mutability, val.ty(checker, vars)))
            }
            Value::Constructor(struct_, _) => Type::Struct(*struct_),
            Value::StructAccess(_, id, field) => {
                checker.structs[id.0].fields[*field].ty.value.clone()
            }
            Value::AddrOfField(mutability, _, id, field) => Type::Ptr(Ptr::new(
                *mutability,
                checker.structs[id.0].fields[*field].ty.value.clone(),
            )),
            Value::IntOp(op, left, _) => match op {
                Op::LtEq | Op::Lt | Op::GtEq | Op::Gt => Type::Bool,
                _ => left.ty(checker, vars),
            },
            Value::LogEq(_, _) => Type::Bool,
            Value::LogNe(_, _) => Type::Bool,
            Value::IntToInt(_, ty) => ty.clone(),
            Value::WidePtrToPtr(_, ty) => ty.clone(),
            Value::WidePtrToWidePtr(_, ty) => ty.clone(),
            Value::SliceIdx(_, _, ty) => ty.clone(),
            Value::PtrIdx(_, _, ty) => ty.clone(),
            Value::AddrOfSliceIdx(mutability, _, _, ty) => {
                Type::Ptr(Ptr::new(*mutability, ty.clone()))
            }
            Value::AddrOfPtrIdx(mutability, _, _, ty) => {
                Type::Ptr(Ptr::new(*mutability, ty.clone()))
            }
            Value::Subslice(ptr, _, _, ty) => {
                let Type::Ptr(Ptr { mutability, .. }) = ptr.ty(checker, vars) else { unreachable!() };

                Type::slice_ptr(mutability, ty.clone())
            }
            Value::AddrOfUnsizedField(mutability, _, id, field) => {
                Type::Ptr(Ptr::new(
                    *mutability,
                    checker.structs[id.0].fields[*field].ty.value.clone(),
                ))
            }
            Value::BitNot(value) => value.ty(checker, vars),
            Value::LogNot(value) => value.ty(checker, vars),
            Value::LogAnd(..) => Type::Bool,
            Value::LogOr(..) => Type::Bool,
            Value::IntNeg(value) => value.ty(checker, vars),
            Value::FuncAddr(id) => {
                let func = &checker.funcs[id.0];
                Type::Func(Func::from_signature(&func.signature))
            },
        }.resolve_aliases(checker)
    }
}
