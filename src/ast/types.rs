use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::Iden;

/// The mutability of a pointer.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum PointerMutability {
    Const(Span),
    Mut(Span),
}

impl Parse for PointerMutability {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let next = parser.scanner_mut().peek()?;

        let mutability = match next {
            Ok(token) => {
                let token = if token == Token::KMut {
                    parser.scanner_mut().next();
                    PointerMutability::Mut(parser.scanner().span())
                } else if token == Token::KConst {
                    parser.scanner_mut().next();
                    PointerMutability::Const(parser.scanner().span())
                } else {
                    return None;
                };

                token
            }
            Err(err) => return Some(Err(err)),
        };

        Some(Ok(mutability))
    }
}

/// A pointer type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PointerType {
    pub span: Span,
    pub mutability: PointerMutability,
    pub ty: Box<Type>,
}

impl Parse for PointerType {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let next = parser.scanner_mut().peek()?;
        let start_pos = parser.scanner().span().start;

        match next {
            Ok(token) => {
                if token != Token::Tilde {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }
        let tilde = parser.scanner().span();

        let mutability = if let Some(mutability) = parser.parse::<PointerMutability>() {
            match mutability {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedPointerMutability {
                tilde,
                offending: parser.scanner().span(),
            }));
        };

        let ty = if let Some(ty) = parser.parse::<Type>() {
            match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedPointerType {
                tilde,
                offending: parser.scanner().span(),
            }));
        };

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_pos,
                parser.scanner().span().end,
            ),
            mutability,
            ty: Box::new(ty),
        }))
    }
}

/// A type expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    /// A named type.
    ///
    /// ```amp
    /// i32
    /// ```
    Named(Iden),

    /// A pointer type.
    ///
    /// ```amp
    /// ~mut i32
    /// ```
    Pointer(PointerType),
}

impl Type {
    /// Returns the span of a type.
    pub fn span(&self) -> Span {
        match self {
            Self::Named(ty) => ty.span,
            Self::Pointer(ty) => ty.span,
        }
    }
}

impl Parse for Type {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(ty) = parser.parse::<PointerType>() {
            Some(Ok(Self::Pointer(match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            })))
        } else if let Some(ty) = parser.parse::<Iden>() {
            Some(Ok(Self::Named(match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            })))
        } else {
            None
        }
    }
}
