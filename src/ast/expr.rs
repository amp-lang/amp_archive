use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{ArgList, Iden, Str};

/// A function call expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Call {
    pub span: Span,
    pub callee: Box<Expr>,
    pub args: ArgList<Expr>,
}

/// An expression in Amp code.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    Iden(Iden),
    Str(Str),
    Call(Call),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> Span {
        match self {
            Expr::Iden(iden) => iden.span,
            Expr::Str(str) => str.span,
            Expr::Call(call) => call.span,
        }
    }

    /// Parses an expression.
    fn parse_atom(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(res) = parser.parse::<Iden>() {
            match res {
                Ok(iden) => Some(Ok(Expr::Iden(iden))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(res) = parser.parse::<Str>() {
            match res {
                Ok(str) => Some(Ok(Expr::Str(str))),
                Err(err) => Some(Err(err)),
            }
        } else {
            None
        }
    }

    /// Parses a basic value expression, such as `1 + 1`.
    fn parse_expr(parser: &mut Parser, min_power: u8) -> Option<Result<Self, Error>> {
        let mut left = match Self::parse_atom(parser)? {
            Ok(left) => left,
            Err(err) => return Some(Err(err)),
        };

        loop {
            let op = if let Some(next) = parser.scanner_mut().peek() {
                match next {
                    Ok(token) => {
                        if token.is_operator() {
                            token
                        } else {
                            break;
                        }
                    }
                    Err(err) => return Some(Err(err)),
                }
            } else {
                break;
            };

            let (left_power, right_power) = op.binding_power();
            if left_power < min_power {
                break;
            }

            // Parse function call operator
            if op == Token::LParen {
                let args = match parser
                    .parse::<ArgList<Expr>>()
                    .expect("We've already confirmed there's an argument list.")
                {
                    Ok(args) => args,
                    Err(err) => return Some(Err(err)),
                };

                left = Expr::Call(Call {
                    span: Span::new(parser.scanner().file_id(), left.span().start, args.span.end),
                    callee: Box::new(left),
                    args,
                });
            } else {
                parser.scanner_mut().next();
                let _rhs = Expr::parse_expr(parser, right_power)?;
                todo!() // currently unreachable
            }
        }

        Some(Ok(left))
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let value = match Self::parse_expr(parser, 0)? {
            Ok(value) => value,
            Err(err) => return Some(Err(err)),
        };

        // // parse semicolon
        // match parser.scanner_mut().next() {
        //     Some(res) => match res {
        //         Ok(token) => {
        //             dbg!(token);
        //             if token != Token::Semi {
        //                 return Some(Err(Error::ExpectedSemicolon(parser.scanner().span())));
        //             };
        //         }
        //         Err(err) => return Some(Err(err)),
        //     },
        //     None => {
        //         return Some(Err(Error::ExpectedSemicolon(parser.scanner().span())));
        //     }
        // }

        Some(Ok(value))
    }
}
