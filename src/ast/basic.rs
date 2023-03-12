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
        let res = parser.scanner_mut().peek()?;

        match res {
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
