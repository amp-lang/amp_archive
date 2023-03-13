use std::fmt;

use codespan_reporting::diagnostic::Label;

/// A value that opaquely represents a file.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct FileId(pub u32);

impl FileId {
    /// Creates a new [FileId] from a [u32].
    pub fn new(id: usize) -> Self {
        Self(id as u32)
    }
}

/// The source location of an expression.
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    /// The ID of the file that the [Span] originated in.
    pub file_id: FileId,

    /// The starting location of the [Span].
    pub start: u32,

    /// The ending location of the [Span].
    pub end: u32,
}

impl Span {
    /// Creates a new [Span] from a [FileId], a starting location, and an ending location.
    pub fn new(file_id: FileId, start: impl Into<u32>, end: impl Into<u32>) -> Self {
        Self {
            file_id,
            start: start.into(),
            end: end.into(),
        }
    }

    /// Creates a primary label from this span.
    pub fn primary(&self) -> Label<usize> {
        Label::primary(
            self.file_id.0 as usize,
            self.start as usize..self.end as usize,
        )
    }

    /// Creates a secondary label from this span.
    pub fn secondary(&self) -> Label<usize> {
        Label::secondary(
            self.file_id.0 as usize,
            self.start as usize..self.end as usize,
        )
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({:?}:{}..{})", self.file_id.0, self.start, self.end)
    }
}
