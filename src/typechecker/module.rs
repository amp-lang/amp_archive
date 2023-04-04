use std::path::Path;

use codespan_reporting::files::SimpleFiles;

use crate::{
    ast::{self, Modifier},
    error::Error,
    import::Importer,
    typechecker::scope::TypeDecl,
};

use super::{
    func::{check_func_decl, check_func_def, FuncId},
    namespace::Namespace,
    scope::Scope,
    struct_::{check_struct_decl, check_struct_def, check_struct_size, StructId},
    type_alias::{check_type_alias_decl, check_type_alias_def, TypeAliasId},
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
    TypeAlias(TypeAliasId),

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

    /// A list of all functions declared in this module.
    pub funcs: Vec<FuncId>,

    /// A list of type aliases declared in the module.
    pub type_aliases: Vec<TypeAliasId>,

    /// A list of namespace strings declared in the module.
    pub namespaces: Vec<Namespace>,
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
            funcs: Vec::new(),
            type_aliases: Vec::new(),
            namespaces: Vec::new(),
        }
    }

    /// Checks the module for errors.
    fn load_exports_into_scope(
        &self,
        checker: &Typechecker,
        scope: &mut Scope,
        modules: &Vec<Module>,
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
                Export::TypeAlias(id) => {
                    let type_alias = &checker.type_aliases[id.0];
                    let decl = TypeDecl::TypeAlias(*id);
                    scope.define_type(type_alias.name.value.clone(), decl);
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
                            Export::TypeAlias(id) => {
                                let type_alias = &checker.type_aliases[id.0];
                                let decl = TypeDecl::TypeAlias(*id);
                                scope.define_type(type_alias.name.value.clone(), decl);
                            }
                            Export::Import(import) => {
                                if imported_exported_modules.contains(import) {
                                    continue;
                                }

                                imported_exported_modules.push(*import);
                                let imported_module = modules[import.0].clone();

                                imported_module.load_exports_into_scope(
                                    checker,
                                    scope,
                                    modules,
                                    imported_exported_modules,
                                )
                            }
                        }
                    }
                }
            }
        }
    }

    fn declare_imported_namespaces(
        &self,
        imports_searched: &mut Vec<ModuleId>,
        modules: &Vec<Module>,
        scope: &mut Scope,
    ) {
        for import in &self.imports {
            if imports_searched.contains(import) {
                continue;
            }
            imports_searched.push(*import);
            let imported_module = &modules[import.0];

            for namespace in &imported_module.namespaces {
                scope.declare_namespace(namespace.name.clone());
            }

            imported_module.declare_imported_namespaces(imports_searched, modules, scope);
        }
    }

    /// Creates the scope for a module to use.
    pub fn make_scope<'a>(&self, checker: &Typechecker, modules: &Vec<Module>) -> Scope<'a> {
        let mut scope = Scope::new(None);

        // a list of modules imported through an export

        for namespace in &self.namespaces {
            scope.declare_namespace(namespace.name.clone());
        }

        let mut imports_searched = vec![self.id];
        self.declare_imported_namespaces(&mut imports_searched, modules, &mut scope);

        // Load structs into scope
        for id in &self.structs {
            let decl = TypeDecl::Struct(*id);
            scope.define_type(checker.structs[id.0].name.value.clone(), decl);
        }

        // Load structs into scope
        for id in &self.funcs {
            let res = scope.define_func(checker.funcs[id.0].name.value.clone(), *id);
            debug_assert!(res);
        }

        // Load type aliases into scope
        for id in &self.type_aliases {
            let res = scope.define_type(
                checker.type_aliases[id.0].name.value.clone(),
                TypeDecl::TypeAlias(*id),
            );
            debug_assert!(res);
        }

        let mut imported_exported_modules = vec![self.id];
        for import in &self.imports {
            let imported_module = modules[import.0].clone();

            for export in &imported_module.exports {
                match export {
                    Export::Func(id) => {
                        let func = &checker.funcs[id.0];
                        let res = scope.define_func(func.name.value.clone(), *id);
                        debug_assert!(res);
                    }
                    Export::Struct(id) => {
                        let struct_ = &checker.structs[id.0];
                        let decl = TypeDecl::Struct(*id);
                        scope.define_type(struct_.name.value.clone(), decl);
                    }
                    Export::TypeAlias(id) => {
                        let type_alias = &checker.type_aliases[id.0];
                        let decl = TypeDecl::TypeAlias(*id);
                        scope.define_type(type_alias.name.value.clone(), decl);
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

    /// Declares the namespaces of a module.
    pub fn declare_namespaces(&mut self) {
        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Namespace(namespace) => {
                    self.namespaces.push(Namespace {
                        span: namespace.span,
                        name: namespace.name.value.clone(),
                    });
                }
                _ => {}
            }
        }
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

                    // export import
                    for item in &import.modifiers {
                        match item {
                            Modifier::Export(_) => {
                                self.exports.push(Export::Import(id));
                                break;
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(imported)
    }

    /// Checks the type declarations of a module.
    pub fn check_type_decls(
        &mut self,
        checker: &mut Typechecker,
        modules: &Vec<Module>,
    ) -> Result<(), Error> {
        let mut scope = self.make_scope(checker, modules);

        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Struct(struct_) => {
                    let decl = check_struct_decl(checker, struct_)?;
                    let id = checker.declare_struct(decl.clone(), &mut scope)?;

                    for item in &struct_.modifiers {
                        match item {
                            Modifier::Export(_) => {
                                self.exports.push(Export::Struct(id));
                                break;
                            }
                        }
                    }

                    self.structs.push(id);
                }
                ast::Decl::TypeAlias(ty) => {
                    let decl = check_type_alias_decl(ty)?;
                    let id = checker.declare_type_alias(decl.clone(), &scope)?;

                    for item in &ty.modifiers {
                        match item {
                            Modifier::Export(_) => {
                                self.exports.push(Export::TypeAlias(id));
                                break;
                            }
                        }
                    }

                    self.type_aliases.push(id);
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Checks the type definitions in the module.
    pub fn check_type_defs(
        &self,
        checker: &mut Typechecker,
        modules: &Vec<Module>,
    ) -> Result<(), Error> {
        let mut scope = self.make_scope(checker, modules);

        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Struct(struct_) => {
                    check_struct_def(checker, &mut scope, struct_)?;
                }
                ast::Decl::TypeAlias(ty) => {
                    check_type_alias_def(checker, &mut scope, ty)?;
                }
                _ => {}
            }
        }

        Ok(())
    }

    /// Checks the type sizes in the module.
    pub fn check_type_sizes(&self, checker: &mut Typechecker) -> Result<(), Error> {
        for struct_id in &self.structs {
            let mut struct_ = checker.structs[struct_id.0].clone();
            let sized = check_struct_size(checker, &struct_, &mut Vec::new())?;
            struct_.sized = sized;
            checker.structs[struct_id.0] = struct_;
        }

        Ok(())
    }

    /// Checks the function declarations in the module.
    pub fn check_func_decls(
        &mut self,
        checker: &mut Typechecker,
        modules: &Vec<Module>,
    ) -> Result<(), Error> {
        let mut scope = self.make_scope(checker, modules);

        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Func(func) => {
                    let id = check_func_decl(checker, &mut scope, &func)?;

                    for item in &func.modifiers {
                        match item {
                            Modifier::Export(_) => {
                                self.exports.push(Export::Func(id));
                                break;
                            }
                        }
                    }

                    self.funcs.push(id);
                }
                _ => {}
            }
        }

        Ok(())
    }

    pub fn check_func_defs(
        &self,
        checker: &mut Typechecker,
        modules: &Vec<Module>,
    ) -> Result<(), Error> {
        let mut scope = self.make_scope(checker, modules);

        for decl in &self.ast.decls {
            match decl {
                ast::Decl::Func(func) => {
                    check_func_def(checker, &mut scope, func)?;
                }
                _ => {}
            }
        }

        Ok(())
    }
}
