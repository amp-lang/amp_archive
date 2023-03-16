use crate::{ast, error::Error};

use super::scope::Scope;

/// The mutability of a type.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutability {
    Mut = 1,
    Const = 0,
}

/// A pointer type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Ptr {
    pub mutability: Mutability,
    pub ty: Box<Type>,
}

impl Ptr {
    /// Creates a new pointer type.
    pub fn new(mutability: Mutability, ty: Type) -> Self {
        Self {
            mutability,
            ty: Box::new(ty),
        }
    }

    /// Returns the visualized name of the pointer type.
    pub fn name(&self) -> String {
        let mutability = match self.mutability {
            Mutability::Const => "const",
            Mutability::Mut => "mut",
        };

        format!("~{} {}", mutability, self.ty.name())
    }
}

/// A slice type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Slice {
    pub mutability: Mutability,
    pub ty: Box<Type>,
}

impl Slice {
    /// Creates a new pointer type.
    pub fn new(mutability: Mutability, ty: Type) -> Self {
        Self {
            mutability,
            ty: Box::new(ty),
        }
    }

    /// Returns the visualized name of the pointer type.
    pub fn name(&self) -> String {
        let mutability = match self.mutability {
            Mutability::Const => "const",
            Mutability::Mut => "mut",
        };

        format!("[]{} {}", mutability, self.ty.name())
    }
}

/// A type expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Bool,
    I8,
    I16,
    I32,
    I64,
    Int,
    U8,
    U16,
    U32,
    U64,
    Uint,
    Ptr(Ptr),
    Slice(Slice),
}

impl Type {
    /// Returns the visualized name of type.
    pub fn name(&self) -> String {
        match self {
            Type::Bool => "bool".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::Int => "int".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Uint => "uint".to_string(),
            Type::Ptr(ptr) => ptr.name(),
            Type::Slice(slice) => slice.name(),
        }
    }

    /// Returns `true` if this type needs to be passed through a pointer rather than by value.
    pub fn is_big(&self) -> bool {
        match self {
            Type::Slice(_) => true,
            _ => false,
        }
    }

    /// Returns the size of the type in bytes.
    pub fn size(&self, ptr_size: usize) -> usize {
        match self {
            Type::Bool => 1,
            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::Int => ptr_size,
            Type::U8 => 1,
            Type::U16 => 2,
            Type::U32 => 4,
            Type::U64 => 8,
            Type::Uint => ptr_size,
            Type::Ptr(_) => ptr_size,
            Type::Slice(_) => ptr_size * 2,
        }
    }

    /// Checks for a type in the given module.
    ///
    /// TODO: check imports and declared types
    pub fn check(module: &mut Scope, ty: &ast::Type) -> Result<Self, Error> {
        match ty {
            ast::Type::Named(name) => match name.value.as_str() {
                "bool" => Ok(Type::Bool),
                "i8" => Ok(Type::I8),
                "i16" => Ok(Type::I16),
                "i32" => Ok(Type::I32),
                "i64" => Ok(Type::I64),
                "int" => Ok(Type::Int),
                "u8" => Ok(Type::U8),
                "u16" => Ok(Type::U16),
                "u32" => Ok(Type::U32),
                "u64" => Ok(Type::U64),
                "uint" => Ok(Type::Uint),
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
            ast::Type::Array(array) => {
                let mutability = match array.length {
                    ast::ArrayLength::Static(_) => todo!("Implement array types"),
                    ast::ArrayLength::Slice(ast::PointerMutability::Const(_)) => Mutability::Const,
                    ast::ArrayLength::Slice(ast::PointerMutability::Mut(_)) => Mutability::Mut,
                };

                Ok(Type::Slice(Slice {
                    mutability,
                    ty: Box::new(Type::check(module, &array.ty)?),
                }))
            }
        }
    }

    /// Returns `true` if the two types are equivalent (i.e, are represented the same in memory and
    /// have the same function).
    pub fn is_equivalent(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Bool, Type::Bool) => true,
            (Type::I8, Type::I8) => true,
            (Type::I16, Type::I16) => true,
            (Type::I32, Type::I32) => true,
            (Type::I64, Type::I64) => true,
            (Type::Int, Type::Int) => true,
            (Type::U8, Type::U8) => true,
            (Type::U16, Type::U16) => true,
            (Type::U32, Type::U32) => true,
            (Type::U64, Type::U64) => true,
            (Type::Uint, Type::Uint) => true,
            (Type::Ptr(ptr), Type::Ptr(other)) => {
                ptr.ty == other.ty && ptr.mutability >= other.mutability
            }
            (Type::Slice(slice), Type::Slice(other)) => {
                slice.ty == other.ty && slice.mutability >= other.mutability
            }
            _ => false,
        }
    }
}
