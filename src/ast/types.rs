use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{ArgList, Int, Path};

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

/// An array or slice type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayType {
    pub span: Span,
    pub ty: Box<Type>,
    pub length: Option<Int>,
}

impl Parse for ArrayType {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        // [T; 10]
        // [T]

        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::LBrack {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }
        let started = parser.scanner().span();

        let ty = if let Some(ty) = parser.parse::<Type>() {
            match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedArrayType(parser.scanner().span())));
        };

        let length = if let Some(Ok(Token::Semi)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();

            if let Some(length) = parser.parse::<Int>() {
                match length {
                    Ok(value) => Some(value),
                    Err(err) => return Some(Err(err)),
                }
            } else {
                return Some(Err(Error::ExpectedArrayLength(parser.scanner().span())));
            }
        } else {
            None
        };

        if let Some(token) = parser.scanner_mut().peek() {
            match token {
                Ok(token) => {
                    if token != Token::RBrack {
                        parser.scanner_mut().next();
                        return Some(Err(Error::UnclosedArrayType {
                            started,
                            offending: parser.scanner().span(),
                        }));
                    }

                    parser.scanner_mut().next();
                }
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::UnclosedArrayType {
                started,
                offending: parser.scanner().span(),
            }));
        }

        return Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                started.start,
                parser.scanner().span().end,
            ),
            ty: Box::new(ty),
            length,
        }));
    }
}

/// A function type.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncType {
    pub span: Span,
    pub args: ArgList<Type>,
    pub ret: Option<Box<Type>>,
}

impl Parse for FuncType {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        // func() -> i32
        // func(i32) -> i32
        // func(i32, i32) -> i32

        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::KFunc {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }
        let started = parser.scanner().span();

        let args = if let Some(args) = parser.parse::<ArgList<Type>>() {
            match args {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedFunctionTypeArgs(
                parser.scanner().span(),
            )));
        };

        let ret = if let Some(Ok(Token::Arrow)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();

            if let Some(ret) = parser.parse::<Type>() {
                match ret {
                    Ok(value) => Some(Box::new(value)),
                    Err(err) => return Some(Err(err)),
                }
            } else {
                return Some(Err(Error::ExpectedFunctionTypeRet(parser.scanner().span())));
            }
        } else {
            None
        };

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                started.start,
                parser.scanner().span().end,
            ),
            args,
            ret,
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
    Named(Path),

    /// A pointer type.
    ///
    /// ```amp
    /// ~mut i32
    /// ```
    Pointer(PointerType),

    /// An array type.
    Array(ArrayType),

    /// A function type.
    Func(FuncType),
}

impl Type {
    /// Returns the span of a type.
    pub fn span(&self) -> Span {
        match self {
            Self::Named(ty) => ty.span,
            Self::Pointer(ty) => ty.span,
            Self::Array(ty) => ty.span,
            Self::Func(ty) => ty.span,
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
        } else if let Some(ty) = parser.parse::<ArrayType>() {
            Some(Ok(Self::Array(match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            })))
        } else if let Some(ty) = parser.parse::<Path>() {
            Some(Ok(Self::Named(match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            })))
        } else if let Some(ty) = parser.parse::<FuncType>() {
            Some(Ok(Self::Func(match ty {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            })))
        } else {
            None
        }
    }
}
