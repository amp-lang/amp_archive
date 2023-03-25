use std::collections::HashMap;

use super::{func::FuncId, path::Path, struct_::StructId, var::VarId};

/// The a type declared in a scope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeDecl {
    /// A `struct` type.
    Struct(StructId),
}

/// A scope of symbols and variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub namespaces: HashMap<String, Scope<'a>>,
    pub funcs: HashMap<String, FuncId>,
    pub vars: HashMap<String, VarId>,
    pub types: HashMap<String, TypeDecl>,
}

impl<'a> Scope<'a> {
    /// Creates a new [Scope].
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            parent,
            namespaces: HashMap::new(),
            funcs: HashMap::new(),
            vars: HashMap::new(),
            types: HashMap::new(),
        }
    }

    /// Declares a namespace in the current [Scope].
    #[inline]
    pub fn declare_namespace(&mut self, name: String) {
        self.namespaces.insert(name, Scope::new(None));
    }

    /// Resolves a namespace in the current scope or parent scopes.
    pub fn resolve_namespace(&self, name: &str) -> Option<&Scope> {
        self.namespaces
            .get(name)
            .or_else(|| self.parent.and_then(|p| p.resolve_namespace(name)))
    }

    /// Recursively searches for a function with the provided name, if any.
    pub fn resolve_func(&self, name: &Path) -> Option<FuncId> {
        if let Some(namespace) = name.namespace() {
            if let Some(namespace) = self.resolve_namespace(namespace) {
                let res = namespace.funcs.get(name.short_name()).copied();
                res
            } else {
                None
            }
        } else if let Some(id) = self.funcs.get(name.short_name()) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.resolve_func(name)
        } else {
            None
        }
    }

    /// Recursively searches for a variable with the provided name, if any.
    pub fn resolve_var(&self, name: &str) -> Option<VarId> {
        if let Some(id) = self.vars.get(name) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.resolve_var(name)
        } else {
            None
        }
    }

    /// Recursively searches for a type with the provided name, if any.
    pub fn resolve_type(&self, name: &Path) -> Option<TypeDecl> {
        if let Some(namespace) = name.namespace() {
            if let Some(namespace) = self.resolve_namespace(namespace) {
                let res = namespace.types.get(name.short_name()).copied();
                res
            } else {
                None
            }
        } else if let Some(id) = self.types.get(name.short_name()) {
            Some(*id)
        } else if let Some(parent) = &self.parent {
            parent.resolve_type(name)
        } else {
            None
        }
    }

    /// Defines a function in this scope.
    ///
    /// Returns `false` if the namespace for the function didn't exist.
    pub fn define_func(&mut self, name: Path, id: FuncId) -> bool {
        if let Some(namespace) = name.namespace() {
            if let Some(scope) = self.namespaces.get_mut(namespace) {
                scope.funcs.insert(name.short_name().to_string(), id);
                true
            } else {
                false
            }
        } else {
            self.funcs.insert(name.short_name().to_string(), id);
            true
        }
    }

    /// Defines a function in this scope.
    pub fn define_var(&mut self, name: String, id: VarId) {
        self.vars.insert(name, id);
    }

    /// Defines a type in this scope.
    ///
    /// Returns `false` if the type's namespace didn't exist.
    pub fn define_type(&mut self, name: Path, id: TypeDecl) -> bool {
        if let Some(namespace) = name.namespace() {
            if let Some(scope) = self.namespaces.get_mut(namespace) {
                scope.types.insert(name.short_name().to_string(), id);
                true
            } else {
                false
            }
        } else {
            self.types.insert(name.short_name().to_string(), id);
            true
        }
    }
}
