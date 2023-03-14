use std::collections::HashMap;

use super::symbol::SymbolId;

/// An Amp module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module {
    /// The symbols declared in this module.
    pub symbols: HashMap<String, SymbolId>,
}

impl Module {
    /// Creates an empty [Module].
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    /// Returns the symbol ID for a symbol with the given name.
    pub fn resolve_symbol(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }
}
