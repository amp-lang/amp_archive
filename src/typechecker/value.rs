use crate::{ast, error::Error, span::Spanned};

use super::{
    module::Module,
    symbol::{Symbol, SymbolId},
    types::Type,
    Typechecker,
};

/// A function call value.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncCall {
    pub callee: SymbolId,
    pub args: Vec<Value>,
}

impl FuncCall {
    pub fn check(
        checker: &mut Typechecker,
        module: &mut Module,
        call: &ast::Call,
    ) -> Result<Self, Error> {
        let callee = match &*call.callee {
            ast::Expr::Iden(iden) => {
                let callee =
                    module
                        .resolve_symbol(&iden.value)
                        .ok_or(Error::UndeclaredFunction(Spanned::new(
                            iden.span,
                            iden.value.clone(),
                        )))?;
                callee
            }
            _ => return Err(Error::InvalidFunctionName(call.callee.span())),
        };

        let func = &checker.symbols[callee.0 as usize];

        let decl = match func {
            Symbol::FuncDecl(func) => &func,
            Symbol::FuncDef(func) => &func.decl,
            // TODO: error if the symbol is not a function
        };

        if decl.signature.args.len() != call.args.args.len() {
            return Err(Error::InvalidArgumentCount {
                decl: decl.decl_span,
                decl_type: decl.signature.name(),
                offending: call.span,
            });
        }

        let mut args = Vec::new();

        for (idx, arg) in call.args.args.iter().enumerate() {
            let value = Value::check_expected(module, arg, &decl.signature.args[idx].ty)
                .ok_or(Error::InvalidValue(arg.span()))?;
            args.push(value);
        }

        Ok(Self { callee, args })
    }
}

/// A value expression in an Amp module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    /// A null terminated string.
    CStr(String),
}

impl Value {
    /// Checks if the value matches the expected type.
    pub fn check_expected(_module: &mut Module, expr: &ast::Expr, expected: &Type) -> Option<Self> {
        // TODO: check for things other than strings.
        match expr {
            ast::Expr::Str(str) => {
                match expected {
                    // TODO: check for slice type
                    Type::Ptr(pointee) => match &*pointee.ty {
                        Type::U8 => {}
                        _ => return None,
                    },
                    _ => return None,
                }

                Some(Value::CStr(str.value.clone()))
            }
            _ => None,
        }
    }
}
