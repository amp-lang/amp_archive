use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{Iden, Modifier, Type};

/// A field in a [Struct] declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructField {
    pub span: Span,
    pub name: Iden,
    pub ty: Type,
}

impl Parse for StructField {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let name = match parser.parse::<Iden>()? {
            Ok(name) => name,
            Err(err) => return Some(Err(err)),
        };

        if let Some(Ok(Token::Colon)) = parser.scanner_mut().next() {
        } else {
            return Some(Err(Error::ExpectedColon(parser.scanner().span())));
        }

        let ty = if let Some(ty) = parser.parse::<Type>() {
            match ty {
                Ok(ty) => ty,
                Err(err) => return Some(Err(err)),
            }
        } else {
            return Some(Err(Error::ExpectedFieldType(parser.scanner().span())));
        };

        Some(Ok(Self {
            span: Span::new(parser.scanner().file_id(), name.span.start, ty.span().end),
            name,
            ty,
        }))
    }
}

/// The fields in a [Struct] declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StructFieldList {
    pub span: Span,
    pub fields: Vec<StructField>,
}

impl Parse for StructFieldList {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(Ok(Token::LBrace)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();
        } else {
            return None;
        }
        let starts = parser.scanner().span();

        let fields = match parser
            .parse::<Vec<StructField>>()
            .expect("always returns a value")
        {
            Ok(fields) => fields,
            Err(err) => return Some(Err(err)),
        };

        if let Some(Ok(Token::RBrace)) = parser.scanner_mut().next() {
        } else {
            return Some(Err(Error::ExpectedClosingBrace {
                starts,
                offending: parser.scanner().span(),
            }));
        }

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                starts.start,
                parser.scanner().span().end,
            ),
            fields,
        }))
    }
}

/// A struct type declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    pub span: Span,
    pub modifiers: Vec<Modifier>,
    pub name: Iden,
    pub fields: StructFieldList,
}

impl Parse for Struct {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Ok(Token::KStruct) = parser.scanner_mut().peek()? {
            parser.scanner_mut().next();
        } else {
            return None;
        }

        let name = if let Some(ty) = parser.parse::<Iden>() {
            match ty {
                Ok(ty) => ty,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedFieldType(parser.scanner().span())));
        };

        let fields = if let Some(fields) = parser.parse::<StructFieldList>() {
            match fields {
                Ok(fields) => fields,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedStructFields(parser.scanner().span())));
        };

        Some(Ok(Self {
            span: Span::new(parser.scanner().file_id(), name.span.start, fields.span.end),
            modifiers: Vec::new(),
            name,
            fields,
        }))
    }
}
