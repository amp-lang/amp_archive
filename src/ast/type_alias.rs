//! ```amp
//! type MyType = i32;
//! ```

use crate::{error::Error, parser::Parse, scanner::Token, span::Span};

use super::{Modifier, Path, Type};

/// A type alias.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeAlias {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub name: Path,
    pub value: Type,
}

impl Parse for TypeAlias {
    fn parse(parser: &mut crate::parser::Parser) -> Option<Result<Self, Error>> {
        if let Ok(Token::KType) = parser.scanner_mut().peek()? {
            parser.scanner_mut().next();
        } else {
            return None;
        }

        let start_pos = parser.scanner().span().start;

        let name = match Path::parse(parser) {
            Some(Ok(name)) => name,
            Some(Err(err)) => return Some(Err(err)),
            None => {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedTypeAliasName(parser.scanner().span())));
            }
        };

        match parser.scanner_mut().next() {
            Some(Ok(Token::Eq)) => {}
            Some(Ok(_)) => return Some(Err(Error::ExpectedEq(parser.scanner().span()))),
            Some(Err(err)) => return Some(Err(err)),
            None => return Some(Err(Error::ExpectedEq(parser.scanner().span()))),
        }

        let value = match parser.parse::<Type>() {
            Some(Ok(value)) => value,
            Some(Err(err)) => return Some(Err(err)),
            None => {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedTypeAliasValue(parser.scanner().span())));
            }
        };

        match parser.scanner_mut().next() {
            Some(Ok(Token::Semi)) => {}
            Some(Ok(_)) => return Some(Err(Error::ExpectedSemicolon(parser.scanner().span()))),
            Some(Err(err)) => return Some(Err(err)),
            None => return Some(Err(Error::ExpectedSemicolon(parser.scanner().span()))),
        }

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_pos,
                parser.scanner().span().end,
            ),
            modifiers: Vec::new(),
            name,
            value,
        }))
    }
}
