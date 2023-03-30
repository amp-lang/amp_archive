use std::collections::HashSet;

use crate::{
    ast,
    error::Error,
    span::{Span, Spanned},
    typechecker::scope::TypeDecl,
};

use super::{decl::Modifier, path::Path, scope::Scope, types::Type, Typechecker};

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
    pub name: Spanned<Path>,
    pub fields: Vec<Field>,
    pub sized: bool,
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
    /// Returns the size, in bytes, of this [Struct].  If the struct is unsized, it returns the
    /// size of the struct excluding the unsized field.
    pub fn size(&self, checker: &Typechecker, ptr_size: usize) -> usize {
        // calculate size with field padding
        let mut size = 0;

        let mut fields = self.fields.iter().peekable();
        while let Some(field) = fields.next() {
            size += field.ty.value.size(checker, ptr_size).unwrap();

            if let Some(next_field) = fields.peek() {
                if let Some(next_size) = next_field.ty.value.size(checker, ptr_size) {
                    size = round_up(size, next_size);
                } else {
                    break;
                }
            }
        }

        // Round up to alignment of struct
        // TODO: when/if we have alignment configuration for structs, we should use that here
        size = size.next_power_of_two();
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
        if !self.sized && target == self.fields.len() - 1 {
            return self.size(checker, ptr_size);
        }

        // calculate size with field padding
        let mut offset = 0;

        let mut fields = self.fields.iter().enumerate().peekable();
        while let Some((idx, field)) = fields.next() {
            if idx == target {
                return offset;
            }

            offset += field.ty.value.size(checker, ptr_size).unwrap();

            if let Some((_, next_field)) = fields.peek() {
                let next_size = next_field.ty.value.size(checker, ptr_size);
                offset = round_up(offset, next_size.unwrap());
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
        name: Spanned::new(decl.name.span, Path::check(&decl.name)),
        fields: Vec::new(),
        sized: true,
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
        .resolve_type(&Path::check(&ast.name))
        .expect("Typechecker confirms this type exists");
    let TypeDecl::Struct(id) = item else { unreachable!("Typechecker confirms this is a struct") };

    let mut field_names = HashSet::new();
    for item in &ast.fields.fields {
        if field_names.contains(&item.name.value) {
            return Err(Error::DuplicateField(Spanned::new(
                item.name.span,
                item.name.value.clone(),
            )));
        }
        field_names.insert(&item.name.value);

        let ty = Type::check(checker, scope, &item.ty)?;

        // Make sure field isn't exposing a private type
        // TODO: check if field is private
        match ty {
            Type::Struct(struct_ty) => {
                let ty = &checker.structs[struct_ty.0];

                if !ty.modifiers.contains(&Modifier::Export) {
                    return Err(Error::ExposedPrivateType {
                        name: Spanned::new(ty.name.span, ty.name.value.to_string()),
                        offending: item.span,
                    });
                }
            }
            _ => {}
        }

        checker.structs[id.0 as usize].fields.push(Field {
            span: item.span,
            name: Spanned::new(item.name.span, item.name.value.clone()),
            ty: Spanned::new(item.ty.span(), ty),
        });
    }

    Ok(())
}

/// Checks the size of a struct.  Returns `true` if the struct is sized.
pub fn check_struct_size(checker: &Typechecker, struct_: &Struct) -> Result<bool, Error> {
    if !struct_.sized {
        // the struct for sure already checked
        return Ok(false);
    }

    let mut fields = struct_.fields.iter().peekable();
    let mut sized = true;

    while let Some(field) = fields.next() {
        match field.ty.value.clone().resolve_aliases(checker) {
            Type::Struct(struct_ty) => {
                // check if type is sized
                let ty = &checker.structs[struct_ty.0];

                // TODO: check if struct is infinite sized
                if !check_struct_size(checker, ty)? {
                    sized = false;

                    if fields.peek().is_some() {
                        return Err(Error::BadUnsizedStructField(field.span));
                    }
                }
            }
            Type::Slice(_) => {
                sized = false;

                if fields.peek().is_some() {
                    return Err(Error::BadUnsizedStructField(field.span));
                }
            }
            _ => {}
        }
    }

    Ok(sized)
}
