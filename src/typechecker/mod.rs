use std::path::PathBuf;

use codespan_reporting::files::SimpleFiles;

use crate::{
    ast,
    error::Error,
    import::Importer,
    parser::Parser,
    scanner::Scanner,
    span::{FileId, Spanned},
};

use self::{
    func::{Func, FuncId},
    module::{Module, ModuleId},
    path::Path,
    scope::{Scope, TypeDecl},
    struct_::{Struct, StructId},
    type_alias::{TypeAlias, TypeAliasId},
};

pub mod decl;
pub mod func;
pub mod module;
pub mod namespace;
pub mod path;
pub mod scope;
pub mod stmnt;
pub mod struct_;
pub mod type_alias;
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

    /// The type aliases declared by all modules.
    pub type_aliases: Vec<TypeAlias>,
}

impl Typechecker {
    /// Creates a new [Typechecker] context.
    pub fn new() -> Self {
        Self {
            funcs: Vec::new(),
            structs: Vec::new(),
            type_aliases: Vec::new(),
        }
    }

    /// Returns the function with the provided name, if any.
    pub fn get_func_by_extern_name(&self, name: &String) -> Option<FuncId> {
        for (idx, func) in self.funcs.iter().enumerate() {
            if func.extern_name.as_ref() == Some(name) {
                return Some(FuncId(idx));
            }
        }

        None
    }

    /// Returns the function with the provided name, if any.
    pub fn get_func_by_name(&self, name: &Path) -> Option<FuncId> {
        for (idx, func) in self.funcs.iter().enumerate() {
            if &func.name.value == name {
                return Some(FuncId(idx));
            }
        }

        None
    }

    /// Declares a function in this [Typechecker] context.
    pub fn declare_func(&mut self, func: Func, scope: &mut Scope) -> Result<FuncId, Error> {
        if let Some(extern_name) = &func.extern_name {
            if let Some(id) = self.get_func_by_extern_name(extern_name) {
                let other_func = &self.funcs[id.0];

                if other_func.defined && func.defined
                    || !func.signature.is_equivalent(self, &other_func.signature)
                {
                    return Err(Error::DuplicateSymbol {
                        original: other_func.span,
                        name: Spanned::new(func.name.span, func.name.value.to_string()),
                    });
                }
            }
        } else {
            if let Some(id) = self.get_func_by_name(&func.name.value) {
                let other_func = &self.funcs[id.0];

                if other_func.defined && func.defined
                    || !func.signature.is_equivalent(self, &other_func.signature)
                {
                    return Err(Error::DuplicateSymbol {
                        original: other_func.span,
                        name: Spanned::new(func.name.span, func.name.value.to_string()),
                    });
                }
            }
        }

        let id = FuncId(self.funcs.len());

        if !scope.define_func(func.name.value.clone(), id) {
            return Err(Error::UndeclaredNamespace(Spanned::new(
                func.name.span,
                func.name.value.namespace().unwrap().to_string(),
            )));
        }
        self.funcs.push(func);

        Ok(id)
    }

    /// Declares a struct type in this [Typechecker] context.
    pub fn declare_struct(&mut self, struct_: Struct, scope: &Scope) -> Result<StructId, Error> {
        if let Some(id) = scope.resolve_type(&struct_.name.value) {
            let span = match id {
                TypeDecl::Struct(id) => self.structs[id.0 as usize].span,
                TypeDecl::TypeAlias(id) => self.type_aliases[id.0 as usize].span,
            };

            return Err(Error::DuplicateSymbol {
                original: span,
                name: Spanned::new(struct_.name.span, struct_.name.value.to_string()),
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

    /// Declares a type alias in this [Typechecker] context.
    pub fn declare_type_alias(
        &mut self,
        type_alias: TypeAlias,
        scope: &Scope,
    ) -> Result<TypeAliasId, Error> {
        if let Some(id) = scope.resolve_type(&type_alias.name.value) {
            let span = match id {
                TypeDecl::Struct(id) => self.structs[id.0 as usize].span,
                TypeDecl::TypeAlias(id) => self.type_aliases[id.0 as usize].span,
            };

            return Err(Error::DuplicateSymbol {
                original: span,
                name: Spanned::new(type_alias.name.span, type_alias.name.value.to_string()),
            });
        }

        let id = TypeAliasId(self.type_aliases.len());
        self.type_aliases.push(type_alias);

        Ok(id)
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

        // Check namespace declarations
        for module in &mut modules {
            module.declare_namespaces();
        }

        // Check type declarations
        let mut i = 0;
        while i < modules.len() {
            let mut module = modules[i].clone();
            module.check_type_decls(self, &modules)?;
            modules[i] = module;
            i += 1;
        }

        // Check type definitions
        for module in &modules {
            module.check_type_defs(self, &modules)?;
        }

        // TODO: check type sizes for loops

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
}
