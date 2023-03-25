use crate::span::Span;

/// A unique identifier for a [Namespace].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct NamespaceId(pub usize);

/// A namespace declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Namespace {
    /// The span of the [Namespace]'s declaration.
    pub span: Span,

    /// The name of the [Namespace].
    pub name: String,
}
