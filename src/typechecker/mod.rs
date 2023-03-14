use std::collections::HashMap;

use crate::{ast, error::Error};

use self::{
    func::FuncDecl,
    module::Module,
    symbol::{Symbol, SymbolId},
};

pub mod func;
pub mod module;
pub mod stmnt;
pub mod symbol;
pub mod types;
pub mod value;

/// The state of the typechecker.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Typechecker {
    /// The modules declared in this context.
    pub modules: Vec<Module>,

    /// The symbols declared globally.
    pub symbols: Vec<Symbol>,

    pub symbol_names: HashMap<String, SymbolId>,
}

impl Typechecker {
    /// Creates a new [Typechecker] context.
    pub fn new() -> Self {
        Self {
            modules: Vec::new(),
            symbols: Vec::new(),
            symbol_names: HashMap::new(),
        }
    }

    pub fn declare_func(&mut self, func: FuncDecl, module: &mut Module) -> Result<SymbolId, Error> {
        if let Some(id) = self.symbol_names.get(&func.name.value) {
            return Err(Error::DuplicateSymbol {
                original: self.symbols[id.0 as usize].decl_span(),
                name: func.name.clone(),
            });
        }

        let id = SymbolId(self.symbols.len() as u32);
        let name = func.name.value.clone();

        self.symbols.push(Symbol::FuncDecl(func));
        self.symbol_names.insert(name.clone(), SymbolId(id.0));
        module.symbols.insert(name, id);

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

        let mut module = Module::new();

        // Check function declarations
        for item in &module_ast.decls {
            match item {
                ast::Decl::Func(func) => {
                    func::check_func_decl(self, &mut module, &func)?;
                }
            }
        }

        // Check function definitions
        for item in &module_ast.decls {
            match item {
                ast::Decl::Func(func) => {
                    func::check_func_def(self, &mut module, &func)?;
                }
            }
        }

        Ok(())
    }
}
