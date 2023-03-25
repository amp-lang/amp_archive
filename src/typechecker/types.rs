use crate::{
    ast::{self, Iden},
    error::Error,
};

use super::{
    path::Path,
    scope::{Scope, TypeDecl},
    struct_::StructId,
    Typechecker,
};

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
    pub fn name(&self, checker: &Typechecker) -> String {
        let mutability = match self.mutability {
            Mutability::Const => "const",
            Mutability::Mut => "mut",
        };

        format!("~{} {}", mutability, self.ty.name(checker))
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
    pub fn name(&self, checker: &Typechecker) -> String {
        let mutability = match self.mutability {
            Mutability::Const => "const",
            Mutability::Mut => "mut",
        };

        format!("[]{} {}", mutability, self.ty.name(checker))
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
    Struct(StructId),
}

impl Type {
    /// Returns the visualized name of type.
    pub fn name(&self, checker: &Typechecker) -> String {
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
            Type::Ptr(ptr) => ptr.name(checker),
            Type::Slice(slice) => slice.name(checker),
            Type::Struct(struct_) => checker.structs[struct_.0].name.value.to_string(),
        }
    }

    /// Returns `true` if this type needs to be passed through a pointer rather than by value.
    pub fn is_big(&self, checker: &Typechecker, ptr_size: usize) -> bool {
        match self {
            // slice is always ptr_size * 2, so it's guaranteed a big type
            Self::Slice(_) => true,
            // for C abi
            Self::Struct(struct_) => checker.structs[struct_.0].size(checker, ptr_size) > ptr_size,
            _ => false,
        }
    }

    /// Returns `true` if this value is an integer type.
    pub fn is_int(&self) -> bool {
        match self {
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Int
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Uint => true,
            _ => false,
        }
    }

    /// Returns the size of the type in bytes.
    pub fn size(&self, checker: &Typechecker, ptr_size: usize) -> usize {
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
            Type::Struct(struct_) => checker.structs[struct_.0].size(checker, ptr_size),
        }
    }

    /// Returns `true` if the type is a primitive type.
    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Struct(_) => false,
            _ => true,
        }
    }

    /// Checks for a type in the given scope.
    ///
    /// TODO: check imports and declared types
    pub fn check(scope: &mut Scope, ty: &ast::Type) -> Result<Self, Error> {
        match ty {
            ast::Type::Named(name) => {
                let path = Path::check(name);
                if let Some(_) = path.namespace() {
                    let value = scope
                        .resolve_type(&path)
                        .ok_or(Error::UnknownNamedType(Iden {
                            span: name.span,
                            value: path.to_string(),
                        }))?;
                    match value {
                        TypeDecl::Struct(struct_) => Ok(Type::Struct(struct_)),
                    }
                } else {
                    match path.short_name() {
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
                        _ => {
                            let value =
                                scope
                                    .resolve_type(&path)
                                    .ok_or(Error::UnknownNamedType(Iden {
                                        span: name.span,
                                        value: path.to_string(),
                                    }))?;
                            match value {
                                TypeDecl::Struct(struct_) => Ok(Type::Struct(struct_)),
                            }
                        }
                    }
                }
            }
            ast::Type::Pointer(ptr) => {
                let mutability = match ptr.mutability {
                    ast::PointerMutability::Const(_) => Mutability::Const,
                    ast::PointerMutability::Mut(_) => Mutability::Mut,
                };

                Ok(Type::Ptr(Ptr {
                    mutability,
                    ty: Box::new(Type::check(scope, &ptr.ty)?),
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
                    ty: Box::new(Type::check(scope, &array.ty)?),
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
            (Type::Struct(struct_), Type::Struct(other)) => struct_ == other,
            _ => false,
        }
    }
}

/// Checks a type declaration path.
pub fn check_type_decl_path(scope: &mut Scope, expr: &ast::Expr) -> Result<TypeDecl, Error> {
    let path = Path::check_path_expr(expr)?;
    scope
        .resolve_type(&path)
        .ok_or(Error::InvalidTypePath(expr.span()))
}
