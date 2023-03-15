use crate::{ast, error::Error, span::Spanned};

use super::{func::FuncId, scope::Scope, types::Type, Typechecker};

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
            let value = GenericValue::check(scope, arg)
                .ok_or(Error::InvalidValue(arg.span()))?
                .coerce(&func.signature.args[idx].ty)
                .ok_or(Error::ExpectedArgumentOfType {
                    decl: func.span,
                    name: func.signature.args[idx].ty.name(),
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
}

impl GenericValue {
    /// Converts an ast value into a generic value, if it is a value.
    pub fn check(_module: &mut Scope, expr: &ast::Expr) -> Option<Self> {
        match expr {
            ast::Expr::Int(int) => Some(GenericValue::Int(int.value)),
            ast::Expr::Str(str) => Some(GenericValue::Str(str.value.clone())),
            _ => None,
        }
    }

    /// Attempts to coerce this generic value into a value of the specified type.
    pub fn coerce(self, ty: &Type) -> Option<Value> {
        match (self, ty) {
            (GenericValue::Int(int), Type::I32) => Some(Value::I32(int as i32)),
            (GenericValue::Int(int), Type::U8) => Some(Value::U8(int as u8)),
            (GenericValue::Str(str), Type::Ptr(ptr)) => match &*ptr.ty {
                Type::U8 => Some(Value::CStr(str)),
                _ => None,
            },
            _ => None,
        }
    }
}

/// A value expression in an Amp module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    /// A null terminated string.
    CStr(String),

    /// An 8-bit unsigned integer.
    U8(u8),

    /// A 32-bit integer.
    I32(i32),
}
