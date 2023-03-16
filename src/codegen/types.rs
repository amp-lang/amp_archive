use crate::typechecker::types::Type;

use super::Codegen;

/// Compiles a type into a Cranelift type.
pub fn compile_type(codegen: &mut Codegen, ty: &Type) -> cranelift::prelude::Type {
    match ty {
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
        _ => unreachable!("compile_type: {:?}", ty),
    }
}
