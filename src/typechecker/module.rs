use std::path::Path;

use codespan_reporting::files::SimpleFiles;

use crate::{
    ast::{self, Modifier},
    error::Error,
    import::Importer,
    typechecker::scope::TypeDecl,
};

use super::{
    func::FuncId,
    scope::Scope,
    struct_::{check_struct_decl, check_struct_def, StructId},
    Typechecker,
};

/// An identifier for a module.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ModuleId(pub usize);

/// An item exported by a module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Export {
    Func(FuncId),
    Struct(StructId),

    /// A module that was imported, for example:
    ///
    /// ```amp
    /// export import "Std";
    /// ```
    Import(ModuleId),
}

/// A module of Amp code.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module {
    /// The ID representing this module.
    pub id: ModuleId,

    /// The absolute path of the module.
    pub path: String,

    /// The AST of the module.
    pub ast: ast::Source,

    /// The modules imported by this module.
    pub imports: Vec<ModuleId>,

    /// The items exported by this module.
    pub exports: Vec<Export>,

    /// All structs declared in this module.
    pub structs: Vec<StructId>,
}

impl Module {
    /// Creates a new [Module] from an AST.
    pub fn new(module: ModuleId, path: String, ast: ast::Source) -> Self {
        Self {
            id: module,
            ast,
            path,
            imports: Vec::new(),
            exports: Vec::new(),
            structs: Vec::new(),
        }
    }

    /// Checks the module for errors.
    fn load_exports_into_scope(
        &self,
        checker: &Typechecker,
        scope: &mut Scope,
        modules: &mut Vec<Module>,
        imported_exported_modules: &mut Vec<ModuleId>,
    ) {
        for export in &self.exports {
            match export {
                Export::Func(id) => {
                    let func = &checker.funcs[id.0];
                    scope.define_func(func.name.value.clone(), *id);
                }
                Export::Struct(id) => {
                    let struct_ = &checker.structs[id.0];
                    let decl = TypeDecl::Struct(*id);
                    scope.define_type(struct_.name.value.clone(), decl);
                }
                Export::Import(id) => {
                    if imported_exported_modules.contains(id) {
                        continue;
                    }

                    imported_exported_modules.push(*id);
                    let imported_module = &modules[id.0];

                    for export in &imported_module.exports {
                        match export {
                            Export::Func(id) => {
                                let func = &checker.funcs[id.0];
                                scope.define_func(func.name.value.clone(), *id);
                            }
                            Export::Struct(id) => {
                                let struct_ = &checker.structs[id.0];
                                let decl = TypeDecl::Struct(*id);
                                scope.define_type(struct_.name.value.clone(), decl);
                            }
                            Export::Import(_) => unreachable!(),
                        }
                    }
                }
            }
        }
    }

    /// Creates the scope for a module to use.
    pub fn make_scope(&self, checker: &Typechecker, modules: &mut Vec<Module>) -> Scope {
        let mut scope = Scope::new(None);

        // a list of modules imported through an export
        let mut imported_exported_modules = vec![self.id];

        // Load structs into scope
        for id in &self.structs {
            let decl = TypeDecl::Struct(*id);
            scope.define_type(checker.structs[id.0].name.value.clone(), decl);
        }

        for import in &self.imports {
            let imported_module = modules[import.0].clone();

            for export in &imported_module.exports {
                match export {
                    Export::Func(id) => {
                        let func = &checker.funcs[id.0];
                        scope.define_func(func.name.value.clone(), *id);
                    }
                    Export::Struct(id) => {
                        let struct_ = &checker.structs[id.0];
                        let decl = TypeDecl::Struct(*id);
                        scope.define_type(struct_.name.value.clone(), decl);
                    }
                    Export::Import(id) => {
                        imported_exported_modules.push(*id);
                        let imported_module = modules[id.0].clone();

                        imported_module.load_exports_into_scope(
                            checker,
                            &mut scope,
                            modules,
                            &mut imported_exported_modules,
                        )
                    }
                }
            }
        }

        scope
    }

    /// Resolves the imports of a module.  Returns a list of imported modules.
    pub fn resolve_imports(
        &mut self,
        modules: &mut Vec<Module>,
        files: &mut SimpleFiles<String, String>,
        importer: &Importer,
    ) -> Result<Vec<Module>, Error> {
        let imported = Vec::new();
        let parent = Path::new(&self.path).parent().unwrap().to_path_buf();

        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Import(import) => {
                    let id =
                        Typechecker::resolve_module(modules, &parent, files, importer, import)?;
                    self.imports.push(id);
                }
                _ => {}
            }
        }

        Ok(imported)
    }

    /// Checks the struct declarations of a module.
    pub fn check_struct_decls(&mut self, checker: &mut Typechecker) -> Result<(), Error> {
        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Struct(struct_) => {
                    let decl = check_struct_decl(struct_)?;
                    let id = checker.declare_struct(decl.clone())?;

                    for item in &struct_.modifiers {
                        match item {
                            Modifier::Export(_) => {
                                self.exports.push(Export::Struct(id));
                            }
                        }
                    }

                    self.structs.push(id);
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Checks the struct definitions in the module.
    pub fn check_struct_defs(
        &self,
        checker: &mut Typechecker,
        modules: &mut Vec<Module>,
    ) -> Result<(), Error> {
        let mut scope = self.make_scope(checker, modules);

        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Struct(struct_) => {
                    check_struct_def(checker, &mut scope, struct_)?;
                }
                _ => {}
            }
        }

        Ok(())
    }
}
