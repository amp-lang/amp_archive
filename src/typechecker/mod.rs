use crate::{ast, error::Error};

use self::{
    func::{Func, FuncId},
    scope::Scope,
};

pub mod func;
pub mod scope;
pub mod stmnt;
pub mod types;
pub mod value;

/// The state of the typechecker.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Typechecker {
    /// The functions declared by all modules.
    pub funcs: Vec<Func>,
}

impl Typechecker {
    /// Creates a new [Typechecker] context.
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
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

    /// Recursively typechecks an AST module.
    pub fn check(&mut self, module_ast: &ast::Source) -> Result<(), Error> {
        // TODO: (should be implemented in order of)
        // - check namespaces
        // - check imports
        // - check type declarations
        // - check const declarations/definitions including const functions
        // - check static declarations
        // - check function declarations
        // - check function definitions

        let mut global_scope = Scope::new(None);

        // Check function declarations
        for item in &module_ast.decls {
            match item {
                ast::Decl::Func(func) => {
                    func::check_func_decl(self, &mut global_scope, &func)?;
                }
            }
        }

        // Check function definitions
        for item in &module_ast.decls {
            match item {
                ast::Decl::Func(func) => {
                    func::check_func_def(self, &mut global_scope, func)?;
                }
            }
        }

        Ok(())
    }
}
