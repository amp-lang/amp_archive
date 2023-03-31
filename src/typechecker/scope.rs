use std::collections::HashMap;

use super::{
    func::FuncId, path::Path, struct_::StructId, type_alias::TypeAliasId, var::VarId, Typechecker,
};

/// The a type declared in a scope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeDecl {
    /// A `struct` type.
    Struct(StructId),

    /// A type alias.
    TypeAlias(TypeAliasId),
}

impl TypeDecl {
    /// Gets the [StructId] of this [TypeDecl] if it is a `struct`.
    pub fn get_struct_id(&self, checker: &Typechecker) -> Option<StructId> {
        match self {
            Self::Struct(id) => Some(*id),
            Self::TypeAlias(id) => checker.type_aliases[id.0]
                .value
                .as_ref()
                .unwrap()
                .get_struct_id(checker),
        }
    }
}

/// Any non-type value that can be stored in a scope (types are resolved separately).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScopeValue<'a> {
    Namespace(Scope<'a>),
    Func(FuncId),
    Var(VarId),
}

/// A scope of symbols and variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub values: HashMap<String, ScopeValue<'a>>,
    pub types: HashMap<String, TypeDecl>,
}

impl<'a> Scope<'a> {
    /// Creates a new [Scope].
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            parent,
            values: HashMap::new(),
            types: HashMap::new(),
        }
    }

    /// Declares a namespace in the current [Scope].
    #[inline]
    pub fn declare_namespace(&mut self, name: String) {
        self.values
            .insert(name, ScopeValue::Namespace(Scope::new(None)));
    }

    /// Resolves a namespace in the current scope or parent scopes.
    pub fn resolve_namespace(&self, name: &str) -> Option<&Scope> {
        if let Some(ScopeValue::Namespace(scope)) = self.values.get(name) {
            Some(scope)
        } else if let Some(parent) = self.parent {
            parent.resolve_namespace(name)
        } else {
            None
        }
    }

    /// Resolves a namespace in the current scope or parent scopes.
    ///
    /// # Safety
    /// Ensure that the returned [Scope] is not used after the parent [Scope] is dropped.
    fn resolve_namespace_mut<'n>(&'n mut self, name: &str) -> Option<&'n mut Scope> {
        if let Some(ScopeValue::Namespace(scope)) = self.values.get_mut(name) {
            unsafe { Some(&mut *(scope as *const Scope as *mut Scope)) }
        } else if let Some(_) = self.parent {
            // TODO: cannot assign to parent namespace
            // NOTE: shouldn't be an issue yet, as declarations are only in the root scope
            None
        } else {
            None
        }
    }

    /// Recursively searches for a function with the provided name, if any.
    pub fn resolve_func(&self, name: &Path) -> Option<FuncId> {
        // TODO: check variables for functions
        if let Some(namespace) = name.namespace() {
            if let Some(namespace) = self.resolve_namespace(namespace) {
                namespace.resolve_func(&Path::new(name.short_name().to_string()))
            } else {
                None
            }
        } else if let Some(id) = self.values.get(name.short_name()) {
            match id {
                ScopeValue::Func(id) => Some(*id),
                _ => None,
            }
        } else if let Some(parent) = &self.parent {
            parent.resolve_func(name)
        } else {
            None
        }
    }

    /// Recursively searches for a variable with the provided name, if any.
    pub fn resolve_var(&self, name: &str) -> Option<VarId> {
        if let Some(id) = self.values.get(name) {
            match id {
                ScopeValue::Var(id) => Some(*id),
                _ => None,
            }
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
            if let Some(scope) = self.resolve_namespace_mut(namespace) {
                scope
                    .values
                    .insert(name.short_name().to_string(), ScopeValue::Func(id));
                true
            } else {
                false
            }
        } else {
            self.values
                .insert(name.short_name().to_string(), ScopeValue::Func(id));
            true
        }
    }

    /// Defines a function in this scope.
    pub fn define_var(&mut self, name: String, id: VarId) {
        self.values.insert(name, ScopeValue::Var(id));
    }

    /// Defines a type in this scope.
    ///
    /// Returns `false` if the type's namespace didn't exist.
    pub fn define_type(&mut self, name: Path, id: TypeDecl) -> bool {
        if let Some(namespace) = name.namespace() {
            if let Some(scope) = self.resolve_namespace_mut(namespace) {
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
