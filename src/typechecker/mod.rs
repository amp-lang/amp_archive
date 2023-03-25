use std::path::PathBuf;

use codespan_reporting::files::SimpleFiles;

use crate::{ast, error::Error, import::Importer, parser::Parser, scanner::Scanner, span::FileId};

use self::{
    func::{Func, FuncId},
    module::{Module, ModuleId},
    scope::{Scope, TypeDecl},
    struct_::{Struct, StructId},
};

pub mod decl;
pub mod func;
pub mod module;
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

    /// Returns the type with the provided name, if any.
    pub fn get_type_by_name(&self, name: &String) -> Option<TypeDecl> {
        for (idx, struct_) in self.structs.iter().enumerate() {
            if &struct_.name.value == name {
                return Some(TypeDecl::Struct(StructId(idx)));
            }
        }

        None
    }

    pub fn declare_struct(&mut self, struct_: Struct) -> Result<StructId, Error> {
        if let Some(id) = self.get_type_by_name(&struct_.name.value) {
            let TypeDecl::Struct(id) = id;

            return Err(Error::DuplicateSymbol {
                original: self.structs[id.0 as usize].span,
                name: struct_.name,
            });
        }

        let id = StructId(self.structs.len());

        self.structs.push(struct_);

        Ok(id)
    }

    pub fn resolve_module(
        modules: &mut Vec<Module>,
        parent: &PathBuf,
        files: &mut SimpleFiles<String, String>,
        importer: &Importer,
        import: &ast::Import,
    ) -> Result<ModuleId, Error> {
        let path = importer
            .resolve(parent, &import.path.value)
            .ok_or(Error::CannotResolveImport(import.path.clone()))?;

        for (id, module) in modules.iter().enumerate() {
            if same_file::is_same_file(&module.path, &path).expect("files have been verified") {
                return Ok(ModuleId(id));
            }
        }

        let source = std::fs::read_to_string(&path)
            .map_err(|_| Error::CannotReadImport(import.path.clone()))?;
        let file = files.add(path.to_string_lossy().to_string(), source.clone());

        let module_id = ModuleId(modules.len());

        let scanner = Scanner::new(FileId(file as u32), &source);
        let mut module = Module::new(
            module_id,
            path.to_string_lossy().to_string(),
            Parser::new(scanner)
                .parse::<ast::Source>()
                .expect("Cannot have an empty source")?,
        );

        modules.push(module.clone());
        module.resolve_imports(modules, files, importer)?;
        modules[module_id.0 as usize] = module;

        Ok(module_id)
    }

    /// Checks the modules in the [Typechecker] for validity.
    pub fn check(
        &mut self,
        mut module: Module,
        files: &mut SimpleFiles<String, String>,
        importer: &Importer,
    ) -> Result<(), Error> {
        let mut modules = vec![module.clone()];

        // Resolve imports
        module.resolve_imports(&mut modules, files, importer)?;
        modules[0] = module;

        // Check type declarations
        for module in &mut modules {
            module.check_struct_decls(self)?;
        }

        // Check type defintions
        for module in &modules {
            module.check_struct_defs(self, &modules)?;
        }

        // Check function declarations
        let mut i = 0;
        while i < modules.len() {
            let mut module = modules[i].clone();
            module.check_func_decls(self, &modules)?;
            modules[i] = module;
            i += 1;
        }

        // Check function definitions
        for module in &modules {
            module.check_func_defs(self, &modules)?;
        }

        Ok(())
    }

    // /// Recursively typechecks an AST module.
    // pub fn check(&mut self, module_ast: &ast::Source) -> Result<(), Error> {
    //     // TODO: (should be implemented in order of)
    //     // - check imports
    //     // - check namespaces
    //     // - check type declarations
    //     // - check const declarations/definitions including const functions
    //     // - check static declarations
    //     // - check function declarations
    //     // - check function definitions

    //     let mut global_scope = Scope::new(None);

    //     // Check struct declarations
    //     for item in &module_ast.decls {
    //         match item {
    //             ast::Decl::Struct(decl) => {
    //                 struct_::check_struct_decl(self, &mut global_scope, decl)?;
    //             }
    //             _ => {}
    //         }
    //     }

    //     // Check struct definitions
    //     for item in &module_ast.decls {
    //         match item {
    //             ast::Decl::Struct(def) => {
    //                 struct_::check_struct_def(self, &mut global_scope, def)?;
    //             }
    //             _ => {}
    //         }
    //     }

    //     // Check function declarations
    //     for item in &module_ast.decls {
    //         match item {
    //             ast::Decl::Func(func) => {
    //                 func::check_func_decl(self, &mut global_scope, &func)?;
    //             }
    //             _ => {}
    //         }
    //     }

    //     // Check function definitions
    //     for item in &module_ast.decls {
    //         match item {
    //             ast::Decl::Func(func) => {
    //                 let mut scope = Scope::new(Some(&global_scope));
    //                 func::check_func_def(self, &mut scope, func)?;
    //             }
    //             _ => {}
    //         }
    //     }

    //     Ok(())
    // }
}
