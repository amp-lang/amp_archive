use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::Expr;

/// A code block or expressions.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Block {
    pub span: Span,
    pub value: Vec<Expr>,
}

impl Parse for Block {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let mut value = Vec::new();

        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::LBrace {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }

        let start = parser.scanner().span().start;

        loop {
            let expr = if let Some(res) = parser.parse::<Expr>() {
                match res {
                    Ok(expr) => expr,
                    Err(err) => return Some(Err(err)),
                }
            } else {
                break;
            };

            // Parse semicolon
            if expr.requires_semi() {
                if let Some(res) = parser.scanner_mut().next() {
                    match res {
                        Ok(token) => {
                            if token != Token::Semi {
                                return Some(Err(Error::ExpectedSemicolon(
                                    parser.scanner().span(),
                                )));
                            }
                        }
                        Err(err) => return Some(Err(err)),
                    }
                } else {
                    return Some(Err(Error::ExpectedSemicolon(parser.scanner().span())));
                }
            }

            value.push(expr);
        }

        // Parse closing brace
        if let Some(res) = parser.scanner_mut().next() {
            match res {
                Ok(token) => {
                    if token != Token::RBrace {
                        return Some(Err(Error::ExpectedClosingBrace {
                            starts: Span::new(parser.scanner().file_id(), start, start + 1),
                            offending: parser.scanner().span(),
                        }));
                    }
                }
                Err(err) => return Some(Err(err)),
            }
        } else {
            return Some(Err(Error::ExpectedClosingBrace {
                starts: Span::new(parser.scanner().file_id(), start, start + 1),
                offending: parser.scanner().span(),
            }));
        }

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start,
                parser.scanner().span().end,
            ),
            value,
        }))
    }
}
