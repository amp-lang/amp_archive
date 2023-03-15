use std::collections::HashMap;

use super::{func::FuncId, var::VarId};

/// A scope of symbols and variables.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub funcs: HashMap<String, FuncId>,
    pub vars: HashMap<String, VarId>,
}

impl<'a> Scope<'a> {
    /// Creates a new [Scope].
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            parent,
            funcs: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    /// Recursively searches for a function with the provided name, if any.
    pub fn resolve_func(&self, name: &str) -> Option<FuncId> {
        if let Some(id) = self.funcs.get(name) {
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

    /// Defines a function in this scope.
    pub fn define_func(&mut self, name: String, id: FuncId) {
        self.funcs.insert(name, id);
    }

    /// Defines a function in this scope.
    pub fn define_var(&mut self, name: String, id: VarId) {
        self.vars.insert(name, id);
    }
}
