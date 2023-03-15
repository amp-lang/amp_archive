use crate::span::Span;

use super::types::Type;

/// A unique identifier for a variable.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarId(pub usize);

/// Information about a variable declaration.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    /// The span of the variable declaration.
    pub span: Span,

    /// The name of the variable.
    pub name: String,

    /// The type of the variable.
    pub ty: Type,
}

impl Var {
    /// Creates a new variable.
    pub fn new(span: Span, name: String, ty: Type) -> Self {
        Self { span, name, ty }
    }
}

/// A list of variables declared in a function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Vars {
    /// The variables declared in the function.
    pub vars: Vec<Var>,
}

impl Vars {
    /// Creates an empty variable list.
    pub fn new() -> Self {
        Self { vars: Vec::new() }
    }

    /// Declares a new variable.
    pub fn declare_var(&mut self, var: Var) -> VarId {
        let id = self.vars.len();
        self.vars.push(var);
        VarId(id)
    }
}
