use crate::{
    error::Error,
    parser::{Parse, Parser},
};

use super::Func;

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
}

impl Parse for Decl {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(value) = parser.parse::<Func>() {
            match value {
                Ok(value) => Some(Ok(Self::Func(value))),
                Err(err) => Some(Err(err)),
            }
        } else {
            None
        }
    }
}
