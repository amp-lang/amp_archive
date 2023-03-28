use crate::{
    error::Error,
    parser::{Parse, Parser},
    span::Span,
};

use super::Decl;

/// An Amp source module.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Source {
    pub span: Span,
    pub decls: Vec<Decl>,
}

impl Parse for Source {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let start_pos = parser.scanner().span().start;
        let mut decls = Vec::new();

        loop {
            if parser.scanner_mut().peek().is_none() {
                break;
            }

            let decl = match parser.parse::<Decl>() {
                Some(res) => match res {
                    Ok(decl) => decl,
                    Err(err) => return Some(Err(err)),
                },
                None => {
                    parser.scanner_mut().next();
                    return Some(Err(Error::ExpectedDeclaration(parser.scanner().span())));
                }
            };

            decls.push(decl);
        }

        let end_pos = parser.scanner().span().end;

        Some(Ok(Self {
            span: Span::new(parser.scanner().file_id(), start_pos, end_pos),
            decls,
        }))
    }
}
