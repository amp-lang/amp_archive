use crate::ast;

/// A modifier for a declaration.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Modifier {
    /// Exposes a declaration to other modules.
    Export,
}

impl Modifier {
    /// Converts an [ast::Modifier] into a [Modifier].
    pub fn check(modifier: &ast::Modifier) -> Self {
        match modifier {
            ast::Modifier::Export(_) => Self::Export,
        }
    }
}
