use crate::{ast, error::Error};

use super::scope::Scope;

/// The mutability of a type.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Mutability {
    Mut,
    Const,
}

/// A pointer type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ptr {
    pub mutability: Mutability,
    pub ty: Box<Type>,
}

/// A type expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    I32,
    U8,
    Ptr(Ptr),
}

impl Type {
    /// Returns the visualized name of type.
    pub fn name(&self) -> String {
        match self {
            Type::I32 => "i32".to_string(),
            Type::U8 => "u8".to_string(),
            Type::Ptr(ptr) => {
                let mutability = match ptr.mutability {
                    Mutability::Const => "const",
                    Mutability::Mut => "mut",
                };

                format!("~{} {}", mutability, ptr.ty.name())
            }
        }
    }

    /// Checks for a type in the given module.
    ///
    /// TODO: check imports and declared types
    pub fn check(module: &mut Scope, ty: &ast::Type) -> Result<Self, Error> {
        match ty {
            ast::Type::Named(name) => match name.value.as_str() {
                "i32" => Ok(Type::I32),
                "u8" => Ok(Type::U8),
                _ => return Err(Error::UnknownNamedType(name.clone())),
            },
            ast::Type::Pointer(ptr) => {
                let mutability = match ptr.mutability {
                    ast::PointerMutability::Const(_) => Mutability::Const,
                    ast::PointerMutability::Mut(_) => Mutability::Mut,
                };

                Ok(Type::Ptr(Ptr {
                    mutability,
                    ty: Box::new(Type::check(module, &ptr.ty)?),
                }))
            }
        }
    }
}
