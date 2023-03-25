use std::collections::HashSet;

use crate::{
    ast,
    error::Error,
    span::{Span, Spanned},
    typechecker::scope::TypeDecl,
};

use super::{decl::Modifier, scope::Scope, types::Type, Typechecker};

/// A unique identifier for a struct.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructId(pub usize);

/// A field in a struct.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Field {
    pub span: Span,
    pub name: Spanned<String>,
    pub ty: Spanned<Type>,
}

/// A struct type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    /// The location of the struct's declaration.
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub name: Spanned<String>,
    pub fields: Vec<Field>,
}

/// Rounds a number up to the nearest multiple of another number.
fn round_up(num_to_round: usize, multiple: usize) -> usize {
    if multiple == 0 {
        return num_to_round;
    }

    let remainder = num_to_round % multiple;
    if remainder == 0 {
        num_to_round
    } else {
        num_to_round + multiple - remainder
    }
}

impl Struct {
    /// Returns the size, in bytes, of this [Struct].
    pub fn size(&self, checker: &Typechecker, ptr_size: usize) -> usize {
        // calculate size with field padding
        let mut size = 0;

        let mut fields = self.fields.iter().peekable();
        while let Some(field) = fields.next() {
            size += field.ty.value.size(checker, ptr_size);

            if let Some(next_field) = fields.peek() {
                let next_size = next_field.ty.value.size(checker, ptr_size);
                size = round_up(size, next_size);
            }
        }

        size = round_up(size, ptr_size); // align struct to pointer size for system
        size
    }

    /// Gets the field with the provided name.
    pub fn get_field(&self, name: &str) -> Option<(usize, &Field)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name.value == name)
    }

    /// Gets the offset of a field.
    ///
    /// Fails if the field does not exist.
    pub fn get_field_offset(&self, checker: &Typechecker, ptr_size: usize, target: usize) -> usize {
        // calculate size with field padding
        let mut offset = 0;

        let mut fields = self.fields.iter().enumerate().peekable();
        while let Some((idx, field)) = fields.next() {
            if idx == target {
                return offset;
            }

            offset += field.ty.value.size(checker, ptr_size);

            if let Some((_, next_field)) = fields.peek() {
                let next_size = next_field.ty.value.size(checker, ptr_size);
                offset = round_up(offset, next_size);
            }
        }

        offset
    }
}

/// Checks a struct declaration, simply checks if the name is already defined in the current scope.
pub fn check_struct_decl(decl: &ast::Struct) -> Result<Struct, Error> {
    let struct_ = Struct {
        span: decl.span,
        modifiers: decl.modifiers.iter().map(|m| Modifier::check(m)).collect(),
        name: Spanned::new(decl.name.span, decl.name.value.clone()),
        fields: Vec::new(),
    };

    Ok(struct_)
}

/// Checks the struct definition.
pub fn check_struct_def(
    checker: &mut Typechecker,
    scope: &mut Scope,
    ast: &ast::Struct,
) -> Result<(), Error> {
    let item = scope
        .resolve_type(&ast.name.value)
        .expect("Typechecker confirms this type exists");
    let TypeDecl::Struct(id) = item;
    let struct_ = &mut checker.structs[id.0 as usize];

    let mut field_names = HashSet::new();
    for item in &ast.fields.fields {
        // TODO: check if field makes struct infinitely sized
        if field_names.contains(&item.name.value) {
            return Err(Error::DuplicateField(Spanned::new(
                item.name.span,
                item.name.value.clone(),
            )));
        }
        field_names.insert(&item.name.value);
        struct_.fields.push(Field {
            span: item.span,
            name: Spanned::new(item.name.span, item.name.value.clone()),
            ty: Spanned::new(item.ty.span(), Type::check(scope, &item.ty)?),
        });
    }

    Ok(())
}
