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

/// A subscope of symbols and variables without a parent.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Subscope {
    pub values: HashMap<String, ScopeValue>,
    pub types: HashMap<String, TypeDecl>,
}

impl Subscope {
    /// Declares a namespace in the current [Scope].
    #[inline]
    pub fn declare_namespace(&mut self, name: String) {
        self.values
            .insert(name, ScopeValue::Namespace(Subscope::default()));
    }

    /// Resolves a namespace in the current scope or parent scopes.
    pub fn resolve_namespace(&self, name: &str) -> Option<&Subscope> {
        match self.values.get(name)? {
            ScopeValue::Namespace(scope) => Some(scope),
            _ => None,
        }
    }

    /// Resolves a namespace in the current scope or parent scopes.
    ///
    /// # Safety
    /// Ensure that the returned [Scope] is not used after the parent [Scope] is dropped.
    fn resolve_namespace_mut(&mut self, name: &str) -> Option<&mut Subscope> {
        if let ScopeValue::Namespace(scope) = self.values.get_mut(name)? {
            Some(scope)
        } else {
            None
        }
    }

    /// Recursively searches for a function with the provided name, if any.
    pub fn resolve_func(&self, name: &Path) -> Option<FuncId> {
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

    /// Gets a value with the specified name.
    pub fn get_value(&self, name: &str) -> Option<&ScopeValue> {
        if let Some(value) = self.values.get(name) {
            Some(value)
        } else {
            None
        }
    }
}

/// Any non-type value that can be stored in a scope (types are resolved separately).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ScopeValue {
    Namespace(Subscope),
    Func(FuncId),
    Var(VarId),
}

/// A scope of symbols and variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub subscope: Subscope,
}

impl<'a> Scope<'a> {
    /// Creates a new [Scope].
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            parent,
            subscope: Subscope::default(),
        }
    }

    /// Declares a namespace in the current [Scope].
    #[inline]
    pub fn declare_namespace(&mut self, name: String) {
        self.subscope.declare_namespace(name);
    }

    /// Resolves a namespace in the current scope or parent scopes.
    pub fn resolve_namespace(&self, name: &str) -> Option<&Subscope> {
        match self.subscope.values.get(name) {
            Some(ScopeValue::Namespace(namespace)) => Some(namespace),
            Some(_) => None,
            _ => self
                .parent
                .as_ref()
                .and_then(|parent| parent.resolve_namespace(name)),
        }
    }

    /// Resolves a namespace in the current scope or parent scopes.
    ///
    /// # Safety
    /// Ensure that the returned [Scope] is not used after the parent [Scope] is dropped.
    fn resolve_namespace_mut(&mut self, name: &str) -> Option<&mut Subscope> {
        match self.subscope.values.get_mut(name) {
            Some(ScopeValue::Namespace(namespace)) => Some(namespace),
            Some(_) => None,
            // TODO: currently, namespaces and functions are only declared in the same scope,
            // so we don't have issues with mutable references to parent scopes.
            _ => None,
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
        } else if let Some(id) = self.subscope.values.get(name.short_name()) {
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
        if let Some(id) = self.subscope.values.get(name) {
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
        } else if let Some(id) = self.subscope.types.get(name.short_name()) {
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
            self.subscope
                .values
                .insert(name.short_name().to_string(), ScopeValue::Func(id));
            true
        }
    }

    /// Defines a function in this scope.
    pub fn define_var(&mut self, name: String, id: VarId) {
        self.subscope.values.insert(name, ScopeValue::Var(id));
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
            self.subscope
                .types
                .insert(name.short_name().to_string(), id);
            true
        }
    }

    /// Gets a value with the specified name.
    pub fn get_value(&self, name: &str) -> Option<&ScopeValue> {
        if let Some(value) = self.subscope.values.get(name) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get_value(name)
        } else {
            None
        }
    }
}
