use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{ArgList, Iden, Type};

/// An argument in a function declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncArg {
    pub span: Span,
    pub name: Iden,
    pub ty: Type,
}

impl Parse for FuncArg {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let start_pos = parser.scanner().span().start;

        let name = match parser.parse::<Iden>()? {
            Ok(name) => name,
            Err(err) => return Some(Err(err)),
        };
        let name_span = name.span;

        match parser.scanner_mut().peek() {
            Some(res) => match res {
                Ok(token) => {
                    if token != Token::Colon {
                        parser.scanner_mut().next();
                        return Some(Err(Error::MissingArgumentTypeAnnotation {
                            name: name_span,
                            offending: parser.scanner().span(),
                        }));
                    }

                    parser.scanner_mut().next();
                }
                Err(err) => return Some(Err(err)),
            },
            None => {
                parser.scanner_mut().next();
                return Some(Err(Error::MissingArgumentTypeAnnotation {
                    name: name_span,
                    offending: parser.scanner().span(),
                }));
            }
        }

        let ty = match parser.parse::<Type>() {
            Some(res) => match res {
                Ok(ty) => ty,
                Err(err) => return Some(Err(err)),
            },
            None => {
                parser.scanner_mut().next();
                return Some(Err(Error::MissingArgumentTypeAnnotation {
                    name: name_span,
                    offending: parser.scanner().span(),
                }));
            }
        };

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_pos,
                parser.scanner().span().end,
            ),
            name,
            ty,
        }))
    }
}

/// A function declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Func {
    pub span: Span,
    pub name: Iden,
    pub args: ArgList<FuncArg>,
    pub returns: Option<Type>,
}

impl Func {
    /// Parses the return type in a function declaration.
    ///
    /// ```amp
    /// func Test() -> int;
    /// //             ^^^
    /// ```
    fn parse_return_type(parser: &mut Parser) -> Option<Result<Type, Error>> {
        if let Some(value) = parser.scanner_mut().peek() {
            match value {
                Ok(token) => {
                    if token == Token::Arrow {
                        parser.scanner_mut().next();
                        let arrow = parser.scanner().span();

                        let ty = if let Some(ty) = parser.parse::<Type>() {
                            match ty {
                                Ok(ty) => ty,
                                Err(err) => return Some(Err(err)),
                            }
                        } else {
                            parser.scanner_mut().next();
                            return Some(Err(Error::ExpectedReturnType {
                                arrow,
                                offending: parser.scanner().span(),
                            }));
                        };

                        Some(Ok(ty))
                    } else {
                        None
                    }
                }
                Err(err) => return Some(Err(err)),
            }
        } else {
            None
        }
    }
}

impl Parse for Func {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let start_pos = parser.scanner().span().start;

        // Check for the function keyword.
        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::KFunc {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }
        let func_keyword = parser.scanner().span();

        // Parse the name of the function
        let name = if let Some(res) = parser.parse::<Iden>() {
            match res {
                Ok(name) => name,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedFunctionName {
                func_keyword,
                offending: parser.scanner().span(),
            }));
        };

        let args = if let Some(arg_list) = parser.parse::<ArgList<FuncArg>>() {
            match arg_list {
                Ok(args) => args,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedArgumentList {
                func_keyword,
                offending: parser.scanner().span(),
            }));
        };

        // Parse the optional return type.
        let returns = match Func::parse_return_type(parser) {
            Some(res) => match res {
                Ok(returns) => Some(returns),
                Err(err) => return Some(Err(err)),
            },
            None => None,
        };

        // Check for semicolon
        // TODO: parse code block
        if let Some(res) = parser.scanner_mut().next() {
            match res {
                Ok(value) => {
                    if value != Token::Semi {
                        return Some(Err(Error::ExpectedFunctionDefinition {
                            func_keyword,
                            offending: parser.scanner().span(),
                        }));
                    }
                }
                Err(err) => return Some(Err(err)),
            }
        } else {
        }

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_pos,
                parser.scanner().span().end,
            ),
            name,
            args,
            returns,
        }))
    }
}
