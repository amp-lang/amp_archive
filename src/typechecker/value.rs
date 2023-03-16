use crate::{ast, error::Error, span::Spanned};

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
            let generic_value = GenericValue::check(scope, vars, arg)?;
            let value = generic_value
                .coerce(vars, &func.signature.args[idx].value.ty)
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
    Int(i64),
    Str(String),
    Var(VarId),
}

impl GenericValue {
    /// Converts an ast value into a generic value, if it is a value.
    pub fn check(scope: &mut Scope, vars: &Vars, expr: &ast::Expr) -> Result<Self, Error> {
        match expr {
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
            _ => return Err(Error::InvalidValue(expr.span())),
        }
    }

    /// Returns the default value type for this generic value.
    pub fn coerce_default(self) -> Value {
        match self {
            GenericValue::Int(int) => Value::I32(int as i32), // todo: replace with `int` value
            GenericValue::Str(str) => Value::Str(Mutability::Const, str),
            GenericValue::Var(var) => Value::Var(var),
        }
    }

    /// Attempts to coerce this generic value into a value of the specified type.
    pub fn coerce(self, vars: &Vars, ty: &Type) -> Option<Value> {
        match (self, ty) {
            (GenericValue::Int(int), Type::I32) => Some(Value::I32(int as i32)),
            (GenericValue::Int(int), Type::U8) => Some(Value::U8(int as u8)),
            (GenericValue::Str(str), Type::Ptr(ptr)) => match &*ptr.ty {
                Type::U8 => Some(Value::CStr(ptr.mutability, str)),
                _ => None,
            },
            (GenericValue::Str(str), Type::Slice(ptr)) => match &*ptr.ty {
                Type::U8 => Some(Value::Str(ptr.mutability, str)),
                _ => None,
            },
            (GenericValue::Var(var), ty) => {
                if ty != &vars.vars[var.0].ty {
                    None
                } else {
                    Some(Value::Var(var))
                }
            }
            _ => None,
        }
    }
}

/// A value expression in an Amp module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    /// A null terminated string.
    CStr(Mutability, String),

    /// A `[]const u8` or `[]mut u8` value.
    Str(Mutability, String),

    /// An 8-bit unsigned integer.
    U8(u8),

    /// A 32-bit integer.
    I32(i32),

    /// Reads the value of a variable.
    Var(VarId),
}

impl Value {
    /// Returns the type of this value.
    pub fn ty(&self, vars: &Vars) -> Type {
        match self {
            Value::CStr(mut_, _) => Type::Ptr(Ptr::new(*mut_, Type::U8)),
            Value::Str(mut_, _) => Type::Slice(Slice::new(*mut_, Type::U8)),
            Value::U8(_) => Type::U8,
            Value::I32(_) => Type::I32,
            Value::Var(var) => vars.vars[var.0].ty.clone(),
        }
    }
}
