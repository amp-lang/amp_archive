use crate::typechecker::types::Type;

use super::Codegen;

/// Compiles a type into a Cranelift type.
pub fn compile_type(codegen: &mut Codegen, ty: &Type) -> cranelift::prelude::Type {
    match ty {
        Type::I32 => cranelift::prelude::types::I32,
        Type::U8 => cranelift::prelude::types::I8,
        Type::Ptr(_) => codegen.pointer_type,
        _ => unreachable!("compile_type: {:?}", ty),
    }
}
