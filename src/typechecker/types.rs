use crate::{
    ast::{self, Iden},
    error::Error,
};

use super::{
    path::Path,
    scope::{Scope, TypeDecl},
    struct_::{check_struct_size, StructId},
    type_alias::TypeAliasId,
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

    /// Returns `true` if this type is a wide pointer.
    #[inline]
    pub fn is_wide(&self, checker: &Typechecker) -> bool {
        !self.ty.is_sized(checker)
    }
}

/// A slice type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Slice {
    pub ty: Box<Type>,
}

impl Slice {
    /// Creates a new pointer type.
    pub fn new(ty: Type) -> Self {
        Self { ty: Box::new(ty) }
    }

    /// Returns the visualized name of the pointer type.
    pub fn name(&self, checker: &Typechecker) -> String {
        format!("[{}]", self.ty.name(checker))
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
    TypeAlias(TypeAliasId),
}

impl Type {
    /// Creates a `~const [T]` or `~mut [T]` type.
    pub fn slice_ptr(mutability: Mutability, ty: Type) -> Self {
        Self::Ptr(Ptr::new(mutability, Type::Slice(Slice::new(ty))))
    }

    /// Resolves the aliases of this type.  Guarantees that this type is not [TypeAlias]
    pub fn resolve_aliases(self, checker: &Typechecker) -> Self {
        match self {
            Type::TypeAlias(ty) => checker.type_aliases[ty.0]
                .value
                .clone()
                .unwrap()
                .resolve_aliases(checker),
            Type::Ptr(ptr) => Type::Ptr(Ptr {
                mutability: ptr.mutability,
                ty: Box::new(ptr.ty.resolve_aliases(checker)),
            }),
            Type::Slice(slice) => Type::Slice(Slice {
                ty: Box::new(slice.ty.resolve_aliases(checker)),
            }),
            _ => self,
        }
    }

    /// Returns the visualized name of type.
    pub fn name(&self, checker: &Typechecker) -> String {
        match self.clone().resolve_aliases(checker) {
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
            _ => unreachable!(),
        }
    }

    /// Returns `true` if this type needs to be passed through a pointer rather than by value.
    pub fn is_big(&self, checker: &Typechecker, ptr_size: usize) -> Option<bool> {
        if self.is_primitive(checker) {
            return Some(false);
        }
        Some(self.size(checker, ptr_size)? > ptr_size)
    }

    /// Returns `true` if this value is an integer type.
    pub fn is_int(&self, checker: &Typechecker) -> bool {
        match self.clone().resolve_aliases(checker) {
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
    pub fn size(&self, checker: &Typechecker, ptr_size: usize) -> Option<usize> {
        Some(match self.clone().resolve_aliases(checker) {
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
            Type::Ptr(ptr) => {
                if ptr.is_wide(checker) {
                    ptr_size * 2 // wide pointer size
                } else {
                    ptr_size
                }
            }
            Type::Slice(_) => return None,
            // TODO: check if unsized
            Type::Struct(struct_id) => {
                let struct_ = &checker.structs[struct_id.0];
                if check_struct_size(checker, struct_).expect("TODO: don't expect this") {
                    checker.structs[struct_id.0].size(checker, ptr_size)
                } else {
                    return None;
                }
            }
            _ => unreachable!(),
        })
    }

    /// Returns `true` if the type is a primitive type.
    pub fn is_primitive(&self, checker: &Typechecker) -> bool {
        match self.clone().resolve_aliases(checker) {
            Type::Bool
            | Type::I8
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

    /// Checks for a type in the given scope.
    ///
    /// TODO: check imports and declared types
    pub fn check(checker: &Typechecker, scope: &mut Scope, ty: &ast::Type) -> Result<Self, Error> {
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
                        TypeDecl::TypeAlias(struct_) => Ok(Type::TypeAlias(struct_)),
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
                                TypeDecl::TypeAlias(ty) => Ok(Type::TypeAlias(ty)),
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
                    ty: Box::new(Type::check(checker, scope, &ptr.ty)?),
                }))
            }
            // TODO: check for array types
            ast::Type::Array(array) => Ok(Type::Slice(Slice {
                ty: Box::new({
                    let ty = Type::check(checker, scope, &array.ty)?;
                    // TODO: return `Err` variant
                    if ty.is_sized(checker) {
                        ty
                    } else {
                        return Err(Error::OwnedUnsizedType(array.ty.span()));
                    }
                }),
            })),
        }
    }

    /// Returns `true` if the two types are equivalent (i.e, are represented the same in memory and
    /// have the same function).
    pub fn is_equivalent(&self, checker: &Typechecker, other: &Self) -> bool {
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
                ptr.ty.is_equivalent(checker, &other.ty) && ptr.mutability >= other.mutability
            }
            (Type::Slice(slice), Type::Slice(other)) => slice.ty.is_equivalent(checker, &other.ty),
            (Type::Struct(struct_), Type::Struct(other)) => struct_ == other,
            (Type::TypeAlias(ty), other) => checker.type_aliases[ty.0]
                .value
                .as_ref()
                .unwrap()
                .is_equivalent(checker, other),
            _ => false,
        }
    }

    /// Returns the ID of the provided struct type, if it is a struct type.
    pub fn get_struct_id(&self, checker: &Typechecker) -> Option<StructId> {
        match self {
            Type::Struct(struct_) => Some(*struct_),
            Type::TypeAlias(ty) => checker.type_aliases[ty.0]
                .value
                .as_ref()
                .unwrap()
                .get_struct_id(checker),
            _ => None,
        }
    }

    /// Returns the pointer value of the provided type, if it is a pointer type.
    pub fn get_pointer(&self, checker: &Typechecker) -> Option<Ptr> {
        match self {
            Type::Ptr(ptr) => Some(ptr.clone()),
            Type::TypeAlias(ty) => checker.type_aliases[ty.0]
                .value
                .as_ref()
                .unwrap()
                .get_pointer(checker),
            _ => None,
        }
    }

    /// Returns the slice value of the provided type, if it is a slice type.
    pub fn get_slice(&self, checker: &Typechecker) -> Option<Slice> {
        match self {
            Type::Slice(slice) => Some(slice.clone()),
            Type::TypeAlias(ty) => checker.type_aliases[ty.0]
                .value
                .as_ref()
                .unwrap()
                .get_slice(checker),
            _ => None,
        }
    }

    /// Returns `true` if the size of this type is known at compile time.
    pub fn is_sized(&self, checker: &Typechecker) -> bool {
        self.size(checker, 8).is_some()
    }
}

/// Checks a type declaration path.
pub fn check_type_decl_path(scope: &mut Scope, expr: &ast::Expr) -> Result<TypeDecl, Error> {
    let path = Path::check_path_expr(expr)?;
    scope
        .resolve_type(&path)
        .ok_or(Error::InvalidTypePath(expr.span()))
}
