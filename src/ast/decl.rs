use crate::{
    error::Error,
    parser::{Parse, Parser},
};

use super::{Func, Struct};

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
        if let Some(value) = parser.parse::<Func>() {
            match value {
                Ok(value) => Some(Ok(Self::Func(value))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(value) = parser.parse::<Struct>() {
            match value {
                Ok(value) => Some(Ok(Self::Struct(value))),
                Err(err) => Some(Err(err)),
            }
        } else {
            None
        }
    }
}
