use crate::{
    ast::{self, PointerMutability},
    error::Error,
    span::Spanned,
};

use super::{
    func::FuncId,
    scope::Scope,
    types::{Mutability, Ptr, Slice, Type},
    var::{VarId, Vars},
    Typechecker,
};

/// A function call value.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        let callee = match &*call.callee {
            ast::Expr::Iden(iden) => {
                let callee = scope
                    .resolve_func(&iden.value)
                    .ok_or(Error::UndeclaredFunction(Spanned::new(
                        iden.span,
                        iden.value.clone(),
                    )))?;
                callee
            }
            _ => return Err(Error::InvalidFunctionName(call.callee.span())),
        };

        let func = &checker.funcs[callee.0 as usize];

        if func.signature.args.len() != call.args.args.len() {
            return Err(Error::InvalidArgumentCount {
                decl: func.span,
                decl_type: func.signature.name(),
                offending: call.span,
            });
        }

        let mut args = Vec::new();

        for (idx, arg) in call.args.args.iter().enumerate() {
            let generic_value = GenericValue::check(checker, scope, vars, arg)?;
            let value = generic_value
                .coerce(checker, vars, &func.signature.args[idx].value.ty)
                .ok_or(Error::ExpectedArgumentOfType {
                    decl: func.span,
                    name: func.signature.args[idx].value.ty.name(),
                    offending: arg.span(),
                })?;
            args.push(value);
        }

        Ok(Self { callee, args })
    }
}

/// A `{value}` literal value before it is coerced to a specific type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
}

impl GenericValue {
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

    pub fn check_ref(
        checker: &Typechecker,
        scope: &mut Scope,
        vars: &Vars,
        mutability: Mutability,
        expr: &ast::Expr,
    ) -> Result<Self, Error> {
        let value = Self::check(checker, scope, vars, expr)?;

        match value {
            Self::Var(var) => Ok(GenericValue::AddrOfVar(mutability, var)),
            value => Ok(GenericValue::Store(mutability, Box::new(value))),
        }
    }

    /// Returns the default type for a generic value.
    pub fn default_type(&self, checker: &Typechecker, vars: &Vars) -> Type {
        match self {
            Self::Bool(_) => Type::Bool,
            Self::Int(_) => Type::Int,
            Self::Str(_) => Type::Slice(Slice::new(Mutability::Const, Type::Int)),
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
        }
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
            _ => return Err(Error::InvalidValue(expr.span())),
        }
    }

    /// Returns the default value type for this generic value.
    pub fn coerce_default(self) -> Value {
        match self {
            GenericValue::Bool(bool) => Value::Bool(bool),
            GenericValue::Int(int) => Value::Int(int as i64),
            GenericValue::Str(str) => Value::Str(Mutability::Const, str),
            GenericValue::Var(var) => Value::Var(var),
            GenericValue::FuncCall(call) => Value::FuncCall(call),
            GenericValue::Deref(val) => Value::Deref(Box::new(val.coerce_default())),
            GenericValue::AddrOfVar(mutability, var) => Value::AddrOfVar(mutability, var),
            GenericValue::Store(mutability, var) => {
                Value::Store(mutability, Box::new(var.coerce_default()))
            }
        }
    }

    /// Attempts to coerce this generic value into a value of the specified type.
    pub fn coerce(self, checker: &Typechecker, vars: &Vars, ty: &Type) -> Option<Value> {
        match (self, ty) {
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
            (GenericValue::Str(str), Type::Ptr(ptr)) => match &*ptr.ty {
                Type::U8 => Some(Value::CStr(ptr.mutability, str)),
                _ => None,
            },
            (GenericValue::Str(str), Type::Slice(ptr)) => match &*ptr.ty {
                Type::U8 => Some(Value::Str(ptr.mutability, str)),
                _ => None,
            },
            (GenericValue::Var(var), ty) => {
                if vars.vars[var.0].ty.is_equivalent(ty) {
                    Some(Value::Var(var))
                } else {
                    None
                }
            }
            (GenericValue::FuncCall(call), ty) => {
                let func = &checker.funcs[call.callee.0 as usize];
                if func.signature.returns.as_ref().unwrap().is_equivalent(ty) {
                    None
                } else {
                    Some(Value::FuncCall(call))
                }
            }
            (GenericValue::Deref(val), ty) => match val.default_type(checker, vars) {
                Type::Ptr(ptr) => {
                    if ptr.ty.is_equivalent(ty) {
                        Some(Value::Deref(Box::new(val.coerce_default())))
                    } else {
                        None
                    }
                }
                _ => unreachable!(),
            },
            (GenericValue::AddrOfVar(mut_, var), Type::Ptr(ptr)) => {
                if vars.vars[var.0].ty.is_equivalent(&*ptr.ty) {
                    Some(Value::AddrOfVar(mut_, var))
                } else {
                    None
                }
            }
            (GenericValue::Store(mut_, val), Type::Ptr(ty)) if mut_ >= ty.mutability => Some(
                Value::Store(ty.mutability, Box::new(val.coerce(checker, vars, &ty.ty)?)),
            ),
            // TODO: coerce reference here
            _ => None,
        }
    }
}

/// A value expression in an Amp module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    /// A boolean value.
    Bool(bool),

    /// A null terminated string.
    CStr(Mutability, String),

    /// A `[]const u8` or `[]mut u8` value.
    Str(Mutability, String),

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
}

impl Value {
    /// Returns the type of this value.
    pub fn ty(&self, checker: &Typechecker, vars: &Vars) -> Type {
        match self {
            Value::Bool(_) => Type::Bool,
            Value::CStr(mut_, _) => Type::Ptr(Ptr::new(*mut_, Type::U8)),
            Value::Str(mut_, _) => Type::Slice(Slice::new(*mut_, Type::U8)),
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
        }
    }
}
