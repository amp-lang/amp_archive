use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

/// An identifier expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Iden {
    pub span: Span,
    pub value: String,
}

impl Parse for Iden {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token == Token::Identifier {
                    parser.scanner_mut().next();
                    return Some(Ok(Self {
                        span: parser.scanner().span(),
                        value: parser.scanner().slice().to_string(),
                    }));
                }

                return None;
            }
            Err(err) => return Some(Err(err)),
        }
    }
}

/// A string literal.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Str {
    pub span: Span,
    pub value: String,
}

impl Parse for Str {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let raw = match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token == Token::String {
                    parser.scanner_mut().next();
                    parser.scanner().slice()
                } else {
                    return None;
                }
            }
            Err(err) => return Some(Err(err)),
        };

        // TODO: parse escape codes

        Some(Ok(Self {
            span: parser.scanner().span(),
            value: raw[1..raw.len() - 1].to_string(),
        }))
    }
}
