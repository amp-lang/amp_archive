use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{Func, Struct};

/// A modifier for a declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Modifier {
    /// Makes a declaration public.
    Export(Span),
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
    /// A function declaration.
    Func(Func),

    /// A struct declaration.
    Struct(Struct),
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
        } else {
            None
        }
    }
}
