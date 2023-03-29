use std::collections::HashMap;

use crate::{
    ast::{self},
    error::Error,
    span::Spanned,
};

use super::{
    func::FuncId,
    path::Path,
    scope::Scope,
    struct_::StructId,
    types::{self, Mutability, Ptr, Slice, Type},
    var::{VarId, Vars},
    Typechecker,
};

/// A function call value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncCall {
    pub callee: FuncId,
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
        // TODO: make this code simpler somehow
        let callee = {
            let path = Path::check_path_expr(&call.callee)?;
            let res = scope.resolve_func(&path);
            res.ok_or(Error::UndeclaredFunction(Spanned::new(
                call.callee.span(),
                path.to_string(),
            )))?
        };

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
                let generic_value = GenericValue::check(checker, scope, vars, arg)?;
                let value = generic_value
                    .coerce(checker, vars, &func.signature.args[idx].value.ty)
                    .ok_or(Error::ExpectedArgumentOfType {
                        decl: func.span,
                        name: func.signature.args[idx].value.ty.name(checker),
                        offending: arg.span(),
                    })?;
                args.push(value);
            } else {
                let generic_value = GenericValue::check(checker, scope, vars, arg)?;
                let value = generic_value.coerce_default(checker, vars);
                args.push(value);
            }
        }

        Ok(Self { callee, args })
    }
}

/// A `{value}` literal value before it is coerced to a specific type.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenericValue {
    Bool(bool),
    Int(i64),
    Str(String),
    Var(VarId),
    FuncCall(FuncCall),
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
        let value = Self::check(checker, scope, vars, expr)?;

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
        Ok(Self::check(checker, scope, vars, expr)?
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

                        // TODO: check if sized here, then implement some type of unsized struct
                        // field access:
                        // wide_ptr.addr := wide_ptr.addr + field_offset
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
        let lhs = Self::check(checker, scope, vars, left)?;
        let rhs = Self::check(checker, scope, vars, right)?;

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
            Self::FuncCall(func_call) => checker.funcs[func_call.callee.0 as usize]
                .signature
                .returns
                .clone()
                .unwrap(),
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
        }.resolve_aliases(checker)
    }

    /// Converts an ast value into a generic value, if it is a value.
    pub fn check(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        expr: &ast::Expr,
    ) -> Result<Self, Error> {
        match expr {
            ast::Expr::Bool(bool) => Ok(GenericValue::Bool(bool.value)),
            ast::Expr::Int(int) => Ok(GenericValue::Int(int.value)),
            ast::Expr::Str(str) => Ok(GenericValue::Str(str.value.clone())),
            ast::Expr::Iden(iden) => {
                let var = scope
                    .resolve_var(&iden.value)
                    .ok_or(Error::UndeclaredVariable(Spanned::new(
                        iden.span,
                        iden.value.clone(),
                    )))?;
                Ok(GenericValue::Var(var))
            }
            ast::Expr::Call(call) => {
                let func_call = FuncCall::check(checker, scope, vars, call)?;

                let func = &checker.funcs[func_call.callee.0 as usize];
                if func.signature.returns == None {
                    return Err(Error::VoidAsValue(expr.span()));
                }

                Ok(GenericValue::FuncCall(func_call))
            }
            ast::Expr::Unary(unary) => match unary.op {
                ast::UnaryOp::Deref => Self::check_deref(checker, scope, vars, &unary.expr),
                ast::UnaryOp::ConstRef => {
                    Self::check_ref(checker, scope, vars, Mutability::Const, &unary.expr)
                }
                ast::UnaryOp::MutRef => {
                    Self::check_ref(checker, scope, vars, Mutability::Mut, &unary.expr)
                }
                _ => todo!("implement bitwise not operator"),
            },
            ast::Expr::Constructor(constructor) => {
                let ty = types::check_type_decl_path(scope, &*constructor.ty)?;
                let id = ty
                    .get_struct_id(checker)
                    .ok_or(Error::CannotConstructNonStruct(constructor.ty.span()))?;

                let struct_decl = &checker.structs[id.0 as usize];

                let mut fields = HashMap::new();

                for field in &constructor.fields {
                    let (field_id, field_decl) = struct_decl
                        .get_field(&field.name.value)
                        .ok_or(Error::UnknownStructField(field.span))?;

                    if fields.contains_key(&field_id) {
                        return Err(Error::DuplicateFieldDefinition(field.span));
                    }

                    let value = Self::check(checker, scope, vars, &field.value)?
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
                let lhs = Self::check(checker, scope, vars, left)?;

                let id = lhs
                    .resolve_struct(checker, vars)
                    .ok_or(Error::AccessNonStruct(left.span()))?;

                let ast::Expr::Iden(iden) = right.as_ref() else {
                    return Err(Error::ExpectedFieldName(right.span()));
                };

                let struct_decl = &checker.structs[id.0 as usize];

                if let Some(value) = struct_decl.get_field(&iden.value) {
                    let (field_id, _) = value;

                    // let ty = field_decl.ty.value.name(checker);

                    Ok(Self::StructAccess(
                        Box::new(if lhs.is_pointer(checker, vars) {
                            lhs
                        } else {
                            lhs.as_ref(checker, vars, Mutability::Mut).unwrap()
                        }),
                        id,
                        field_id,
                    ))
                } else {
                    return Err(Error::UnknownStructField(iden.span));
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
                    _ => unreachable!("purposefully put last"),
                };

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
                let value = Self::check(checker, scope, vars, &as_.expr)?;

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
                let value = Self::check(checker, scope, vars, &idx.expr)?;

                let index = Self::check(checker, scope, vars, &idx.index)?;

                // check if `index` is a `uint`
                index
                    .clone()
                    .coerce(checker, vars, &Type::Uint)
                    .ok_or(Error::ExpectedUintIndex(idx.index.span()))?;

                if let Some(to) = &idx.to {
                    let to_value = Self::check(checker, scope, vars, to)?;

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
                        // get address of slice
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
                let func = &checker.funcs[call.callee.0 as usize];
                if func
                    .signature
                    .returns
                    .as_ref()
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
}

/// A logical comparison of two values.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cmp {
    Eq,
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
            Value::FuncCall(call) => checker.funcs[call.callee.0 as usize]
                .signature
                .returns
                .clone()
                .expect("verified as a GenericValue"),
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
        }.resolve_aliases(checker)
    }
}
