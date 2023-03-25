use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::Iden;

/// A namespace declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Namespace {
    pub span: Span,
    pub name: Iden,
}

impl Parse for Namespace {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Ok(Token::KNamespace) = parser.scanner_mut().peek()? {
            parser.scanner_mut().next();
        } else {
            return None;
        }

        let start_span = parser.scanner().span();

        let name = match parser.parse::<Iden>() {
            Some(Ok(value)) => value,
            Some(Err(err)) => return Some(Err(err)),
            None => {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedNamespaceName(parser.scanner().span())));
            }
        };

        // parse semicolon
        if let Some(token) = parser.scanner_mut().next() {
            match token {
                Ok(Token::Semi) => {}
                Ok(_) => return Some(Err(Error::ExpectedSemicolon(parser.scanner_mut().span()))),
                Err(err) => return Some(Err(err)),
            }
        } else {
            return Some(Err(Error::ExpectedSemicolon(parser.scanner_mut().span())));
        }

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_span.start,
                parser.scanner().span().end,
            ),
            name,
        }))
    }
}
