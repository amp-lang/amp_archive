//! Type aliases

use crate::{
    ast,
    span::{Span, Spanned},
};

use super::{decl::Modifier, path::Path, scope::TypeDecl, types::Type, Typechecker};

/// A unique identifier for a type alias.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct TypeAliasId(pub usize);

/// A named type alias.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeAlias {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub name: Spanned<Path>,

    /// Always defined after types are checked.
    pub value: Option<Type>,
}

pub fn check_type_alias_decl(decl: &ast::TypeAlias) -> Result<TypeAlias, super::Error> {
    let alias = TypeAlias {
        span: decl.span,
        modifiers: decl.modifiers.iter().map(|m| Modifier::check(m)).collect(),
        name: Spanned::new(decl.name.span, Path::check(&decl.name)),
        value: None,
    };

    Ok(alias)
}

pub fn check_type_alias_def(
    checker: &mut Typechecker,
    scope: &mut super::scope::Scope,
    ast: &ast::TypeAlias,
) -> Result<(), super::Error> {
    let item = scope
        .resolve_type(&Path::check(&ast.name))
        .expect("Typechecker confirms this type exists");
    let TypeDecl::TypeAlias(id) = item else { unreachable!("Typechecker confirms this is a type alias") };

    checker.type_aliases[id.0].value = Some(Type::check(scope, &ast.value)?);
    Ok(())
}
