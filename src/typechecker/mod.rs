use crate::{ast, error::Error};

use self::{
    func::{Func, FuncId},
    scope::{Scope, TypeDecl},
    struct_::{Struct, StructId},
};

pub mod func;
pub mod scope;
pub mod stmnt;
pub mod struct_;
pub mod types;
pub mod value;
pub mod var;

/// The state of the typechecker.
#[derive(Clone, Debug, PartialEq)]
pub struct Typechecker {
    /// The functions declared by all modules.
    pub funcs: Vec<Func>,

    /// The structs declared by all modules.
    pub structs: Vec<Struct>,
}

impl Typechecker {
    /// Creates a new [Typechecker] context.
    pub fn new() -> Self {
        Self {
            funcs: Vec::new(),
            structs: Vec::new(),
        }
    }

    /// Declares a function in this [Typechecker] context.
    pub fn declare_func(&mut self, func: Func, scope: &mut Scope) -> Result<FuncId, Error> {
        if let Some(id) = scope.resolve_func(&func.name.value) {
            return Err(Error::DuplicateSymbol {
                original: self.funcs[id.0 as usize].span,
                name: func.name.clone(),
            });
        }

        let id = FuncId(self.funcs.len());

        scope.define_func(func.name.value.clone(), id);
        self.funcs.push(func);

        Ok(id)
    }

    pub fn declare_struct(
        &mut self,
        struct_: Struct,
        scope: &mut Scope,
    ) -> Result<StructId, Error> {
        if let Some(id) = scope.resolve_type(&struct_.name.value) {
            let TypeDecl::Struct(id) = id;

            return Err(Error::DuplicateSymbol {
                original: self.structs[id.0 as usize].span,
                name: struct_.name,
            });
        }

        let id = StructId(self.structs.len());

        scope.define_type(struct_.name.value.clone(), TypeDecl::Struct(id));
        self.structs.push(struct_.clone());

        Ok(id)
    }

    /// Recursively typechecks an AST module.
    pub fn check(&mut self, module_ast: &ast::Source) -> Result<(), Error> {
        // TODO: (should be implemented in order of)
        // - check imports
        // - check namespaces
        // - check type declarations
        // - check const declarations/definitions including const functions
        // - check static declarations
        // - check function declarations
        // - check function definitions

        let mut global_scope = Scope::new(None);

        // Check struct declarations
        for item in &module_ast.decls {
            match item {
                ast::Decl::Struct(decl) => {
                    struct_::check_struct_decl(self, &mut global_scope, decl)?;
                }
                _ => {}
            }
        }

        // Check struct definitions
        for item in &module_ast.decls {
            match item {
                ast::Decl::Struct(def) => {
                    struct_::check_struct_def(self, &mut global_scope, def)?;
                }
                _ => {}
            }
        }

        // Check function declarations
        for item in &module_ast.decls {
            match item {
                ast::Decl::Func(func) => {
                    func::check_func_decl(self, &mut global_scope, &func)?;
                }
                _ => {}
            }
        }

        // Check function definitions
        for item in &module_ast.decls {
            match item {
                ast::Decl::Func(func) => {
                    let mut scope = Scope::new(Some(&global_scope));
                    func::check_func_def(self, &mut scope, func)?;
                }
                _ => {}
            }
        }

        Ok(())
    }
}
