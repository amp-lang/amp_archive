use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

/// An argument list.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArgList<T: Parse> {
    pub span: Span,
    pub args: Vec<T>,
}

impl<T: Parse> Parse for ArgList<T> {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let start_pos = parser.scanner().span().start;

        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::LParen {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }
        let opening_paren = parser.scanner().span();

        let args = match parser
            .parse::<Vec<T>>()
            .expect("argument list is always there, as it can return an empty list")
        {
            Ok(args) => args,
            Err(err) => return Some(Err(err)),
        };

        if let Some(res) = parser.scanner_mut().next() {
            match res {
                Ok(token) => {
                    if token != Token::RParen {
                        return Some(Err(Error::ExpectedArgumentListClose {
                            opening_paren,
                            offending: parser.scanner().span(),
                        }));
                    }
                }
                Err(err) => return Some(Err(err)),
            }
        } else {
            return Some(Err(Error::UnclosedArgumentList {
                opening_paren,
                offending: parser.scanner().span(),
            }));
        }

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_pos,
                parser.scanner().span().end,
            ),
            args,
        }))
    }
}
