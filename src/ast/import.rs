use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{Modifier, Str};

/// An `import` statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Import {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub path: Str,
}

impl Parse for Import {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Ok(Token::KImport) = parser.scanner_mut().peek()? {
            parser.scanner_mut().next();
        } else {
            return None;
        }

        let start_span = parser.scanner().span();

        let path = match parser.parse::<Str>() {
            Some(Ok(value)) => value,
            Some(Err(err)) => return Some(Err(err)),
            None => {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedModuleName(parser.scanner().span())));
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
            modifiers: Vec::new(),
            path,
        }))
    }
}
