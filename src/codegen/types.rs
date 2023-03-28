use crate::typechecker::{types::Type, Typechecker};

use super::Codegen;

/// Compiles a type into a Cranelift type.
pub fn compile_type(
    codegen: &mut Codegen,
    checker: &Typechecker,
    ty: &Type,
) -> cranelift::prelude::Type {
    match ty {
        Type::Bool => cranelift::prelude::types::I8,
        Type::I8 => cranelift::prelude::types::I8,
        Type::I16 => cranelift::prelude::types::I16,
        Type::I32 => cranelift::prelude::types::I32,
        Type::I64 => cranelift::prelude::types::I64,
        Type::Int => codegen.pointer_type,
        Type::U8 => cranelift::prelude::types::I8,
        Type::U16 => cranelift::prelude::types::I16,
        Type::U32 => cranelift::prelude::types::I32,
        Type::U64 => cranelift::prelude::types::I64,
        Type::Uint => codegen.pointer_type,
        Type::Ptr(_) => codegen.pointer_type,
        Type::Struct(struct_) => {
            let ty = cranelift::prelude::Type::int_with_byte_size(
                checker.structs[struct_.0].size(checker, codegen.pointer_type.bytes() as usize)
                    as u16,
            )
            .expect("struct must not be big");
            ty
        }
        Type::TypeAlias(alias) => compile_type(
            codegen,
            checker,
            &checker.type_aliases[alias.0].value.as_ref().unwrap(),
        ),
        _ => unreachable!("compile_type: {:?}", ty),
    }
}
