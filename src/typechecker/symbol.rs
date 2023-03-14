use crate::span::Span;

use super::func::{FuncDecl, FuncDef};

/// The unique identifier for a symbol in a typechecker.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct SymbolId(pub u32);

/// Any symbol in an Amp module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    FuncDecl(FuncDecl),
    FuncDef(FuncDef),
}

impl Symbol {
    /// Returns the span of a symbol declaration.
    pub fn decl_span(&self) -> Span {
        match self {
            Symbol::FuncDecl(decl) => decl.decl_span,
            Symbol::FuncDef(def) => def.decl.decl_span,
        }
    }
}
