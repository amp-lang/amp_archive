use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{Func, Import, Namespace, Struct, TypeAlias};

/// A modifier for a declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Modifier {
    /// Makes a declaration public.
    Export(Span),
}

impl Modifier {
    /// Returns the span of the modifier.
    pub fn span(&self) -> Span {
        match self {
            Self::Export(span) => *span,
        }
    }

    /// Returns the span of all modifiers in the provided list.
    pub fn modifiers_span(modifiers: &Vec<Modifier>) -> Option<Span> {
        if modifiers.is_empty() {
            None
        } else {
            let start_span = modifiers.first().unwrap().span();
            let end_span = modifiers.last().unwrap().span();
            Some(Span::new(
                start_span.file_id,
                start_span.start,
                end_span.end,
            ))
        }
    }
}

impl Parse for Modifier {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(Ok(Token::KExport)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();
            Some(Ok(Self::Export(parser.scanner().span())))
        } else {
            None
        }
    }
}

/// A declaration in an Amp module.
///
/// For example, a function declaration:
/// ```amp
/// func printf(format: ~const u8) -> i32;
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Decl {
    /// A `func`tion declaration.
    Func(Func),

    /// A `struct` declaration.
    Struct(Struct),

    /// An `import` statement.
    Import(Import),

    /// A `namespace` declaration.
    Namespace(Namespace),

    /// A `type` alias.
    TypeAlias(TypeAlias),
}

impl Parse for Decl {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let mut modifiers = Vec::new();

        // Parse modifiers
        while let Some(value) = parser.parse::<Modifier>() {
            match value {
                Ok(value) => modifiers.push(value),
                Err(err) => return Some(Err(err)),
            }
        }

        // Parse declaration after modifiers
        if let Some(value) = parser.parse::<Func>() {
            match value {
                Ok(mut value) => Some(Ok(Self::Func({
                    value.modifiers = modifiers;
                    value
                }))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(value) = parser.parse::<Struct>() {
            match value {
                Ok(mut value) => Some(Ok(Self::Struct({
                    value.modifiers = modifiers;
                    value
                }))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(value) = parser.parse::<Import>() {
            match value {
                Ok(mut value) => Some(Ok(Self::Import({
                    value.modifiers = modifiers;
                    value
                }))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(value) = parser.parse::<Namespace>() {
            if let Some(span) = Modifier::modifiers_span(&modifiers) {
                return Some(Err(Error::NamespaceModifier(span)));
            }

            match value {
                Ok(value) => Some(Ok(Self::Namespace(value))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(value) = parser.parse::<TypeAlias>() {
            match value {
                Ok(mut value) => Some(Ok(Self::TypeAlias({
                    value.modifiers = modifiers;
                    value
                }))),
                Err(err) => Some(Err(err)),
            }
        } else {
            if modifiers.len() > 0 {
                parser.scanner_mut().next();
                let span = Modifier::modifiers_span(&modifiers).unwrap();
                return Some(Err(Error::ExpectedDeclarationAfterModifier {
                    modifier: span,
                    offending: parser.scanner().span(),
                }));
            }

            None
        }
    }
}
