use crate::{
    error::Error,
    parser::{Parse, Parser},
    scanner::Token,
    span::Span,
};

use super::{ArgList, Block, Bool, Iden, Int, Str, Type};

/// A function call expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Call {
    pub span: Span,
    pub callee: Box<Expr>,
    pub args: ArgList<Expr>,
}

/// A return statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Return {
    pub span: Span,
    pub value: Option<Box<Expr>>,
}

impl Parse for Return {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::KReturn {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }
        let start_pos = parser.scanner().span().start;

        let value = if let Some(res) = parser.parse::<Expr>() {
            match res {
                Ok(expr) => Some(Box::new(expr)),
                Err(err) => return Some(Err(err)),
            }
        } else {
            None
        };

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start_pos,
                parser.scanner().span().end,
            ),
            value,
        }))
    }
}

/// A variable declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Var {
    pub span: Span,
    pub name: Iden,
    pub ty: Option<Type>,
    pub value: Option<Box<Expr>>,
}

impl Parse for Var {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token != Token::KVar {
                    return None;
                }

                parser.scanner_mut().next();
            }
            Err(err) => return Some(Err(err)),
        }

        let started = parser.scanner().span();

        let name = if let Some(res) = parser.parse::<Iden>() {
            match res {
                Ok(iden) => iden,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedVariableName {
                started,
                offending: parser.scanner().span(),
            }));
        };

        let ty = if let Some(Ok(Token::Colon)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();
            let type_start = parser.scanner().span();

            if let Some(res) = parser.parse::<Type>() {
                match res {
                    Ok(ty) => Some(ty),
                    Err(err) => return Some(Err(err)),
                }
            } else {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedVariableType {
                    started: type_start,
                    offending: parser.scanner().span(),
                }));
            }
        } else {
            None
        };

        let value = if let Some(Ok(Token::Eq)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();
            let value_start = parser.scanner().span();

            if let Some(res) = parser.parse::<Expr>() {
                match res {
                    Ok(expr) => Some(Box::new(expr)),
                    Err(err) => return Some(Err(err)),
                }
            } else {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedVariableValue {
                    started: value_start,
                    offending: parser.scanner().span(),
                }));
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
            name,
            ty,
            value,
        }))
    }
}

/// The operator used in a binary operation.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BinaryOp {
    /// `=`
    Eq,

    /// `.`
    Dot,

    /// `*`
    Mul,

    /// `+`
    Add,

    /// `/`
    Div,

    /// `-`
    Sub,

    /// `%`
    Mod,

    /// `==`
    LogEq,

    /// `!=`
    LogNe,

    /// `<`
    Lt,

    /// `<=`
    LtEq,

    /// `>`
    Gt,

    /// `>=`
    GtEq,
}

impl BinaryOp {
    /// Creates a binary operator from a token.
    pub fn from_token(token: Token) -> Self {
        match token {
            Token::Eq => Self::Eq,
            Token::Dot => Self::Dot,
            Token::Star => Self::Mul,
            Token::Plus => Self::Add,
            Token::Slash => Self::Div,
            Token::Minus => Self::Sub,
            Token::Percent => Self::Mod,
            Token::EqEq => Self::LogEq,
            Token::BangEq => Self::LogNe,
            Token::LtEq => Self::LtEq,
            Token::Lt => Self::Lt,
            Token::GtEq => Self::GtEq,
            Token::Gt => Self::Gt,
            _ => unreachable!("invalid token for binary operator"),
        }
    }
}

/// A binary operation.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Binary {
    pub span: Span,
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

/// A unary operator.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum UnaryOp {
    /// `*`
    Deref,

    /// `~`
    Tilde,

    /// `~const`
    ConstRef,

    /// `~mut`
    MutRef,
}

impl UnaryOp {
    /// Creates a binary operator from a token.
    pub fn from_token(token: Token) -> Self {
        match token {
            Token::Star => Self::Deref,
            Token::Tilde => Self::Tilde,
            _ => unreachable!("invalid token for binary operator"),
        }
    }
}

/// A unary expression.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Unary {
    pub span: Span,
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

/// A struct constructor field.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ConstructorField {
    pub span: Span,
    pub name: Iden,
    pub value: Box<Expr>,
}

impl Parse for ConstructorField {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let name = match parser.parse::<Iden>()? {
            Ok(iden) => iden,
            Err(err) => return Some(Err(err)),
        };

        let started = parser.scanner().span();

        if let Some(Ok(Token::Eq)) = parser.scanner_mut().next() {
        } else {
            return Some(Err(Error::ExpectedEq(parser.scanner().span())));
        }

        let value = if let Some(res) = parser.parse::<Expr>() {
            match res {
                Ok(expr) => Box::new(expr),
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedFieldValue(parser.scanner().span())));
        };

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                started.start,
                parser.scanner().span().end,
            ),
            name,
            value,
        }))
    }
}

/// The struct constructor operator.
///
/// ```amp
/// var my_struct = MyStruct .{
///    field = 1,
/// };
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Constructor {
    pub span: Span,

    /// The path to the struct type.
    pub ty: Box<Expr>,

    /// The fields in the struct.
    pub fields: Vec<ConstructorField>,
}

/// A `while` statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct While {
    pub span: Span,
    pub cond: Option<Box<Expr>>,
    pub body: Block,
}

impl Parse for While {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Ok(Token::KWhile) = parser.scanner_mut().peek()? {
            parser.scanner_mut().next();
        } else {
            return None;
        }
        let start = parser.scanner().span();

        let cond = if let Some(value) = Expr::parse(parser) {
            match value {
                Ok(expr) => Some(Box::new(expr)),
                Err(err) => return Some(Err(err)),
            }
        } else {
            None
        };

        let body = if let Some(block) = Block::parse(parser) {
            match block {
                Ok(block) => block,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedBlock(parser.scanner().span())));
        };

        Some(Ok(Self {
            span: Span::new(parser.scanner().file_id(), start.start, body.span.end),
            cond,
            body,
        }))
    }
}

/// An expression in Amp code.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    Bool(Bool),
    Iden(Iden),
    Int(Int),
    Str(Str),
    Call(Call),
    Return(Return),
    Var(Var),
    Binary(Binary),
    Unary(Unary),
    Constructor(Constructor),
    While(While),
}

impl Expr {
    /// Returns the span of the expression.
    pub fn span(&self) -> Span {
        match self {
            Expr::Bool(bool) => bool.span,
            Expr::Iden(iden) => iden.span,
            Expr::Int(int) => int.span,
            Expr::Str(str) => str.span,
            Expr::Call(call) => call.span,
            Expr::Return(return_) => return_.span,
            Expr::Var(var) => var.span,
            Expr::Binary(binary) => binary.span,
            Expr::Unary(unary) => unary.span,
            Expr::Constructor(constructor) => constructor.span,
            Expr::While(while_) => while_.span,
        }
    }

    /// Returns `true` if the expression must be followed by a semicolon.
    pub fn requires_semi(&self) -> bool {
        match self {
            Expr::While(_) => false,
            _ => true,
        }
    }

    /// Parses an expression.
    fn parse_atom(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(res) = parser.parse::<Bool>() {
            match res {
                Ok(bool) => Some(Ok(Expr::Bool(bool))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(res) = parser.parse::<Iden>() {
            match res {
                Ok(iden) => Some(Ok(Expr::Iden(iden))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(res) = parser.parse::<Str>() {
            match res {
                Ok(str) => Some(Ok(Expr::Str(str))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(res) = parser.parse::<Int>() {
            match res {
                Ok(int) => Some(Ok(Expr::Int(int))),
                Err(err) => Some(Err(err)),
            }
        } else {
            None
        }
    }

    /// Parses a unary operation.
    fn parse_unary(parser: &mut Parser, min_power: u8) -> Option<Result<Self, Error>> {
        let token = match parser.scanner_mut().peek()? {
            Ok(token) => {
                if token.is_unary_operator() {
                    parser.scanner_mut().next();
                    token
                } else {
                    return Self::parse_atom(parser);
                }
            }
            Err(err) => return Some(Err(err)),
        };
        let started = parser.scanner().span();

        let mut op = UnaryOp::from_token(token);

        if token == Token::Tilde {
            // check for const/mut
            match parser.scanner_mut().peek() {
                Some(Ok(Token::KConst)) => {
                    parser.scanner_mut().next();
                    op = UnaryOp::ConstRef;
                }
                Some(Ok(Token::KMut)) => {
                    parser.scanner_mut().next();
                    op = UnaryOp::MutRef;
                }
                _ => {}
            }
        }

        let power = token.prefix_binding_power();
        if power < min_power {
            return Some(if let Some(value) = Self::parse_atom(parser) {
                value
            } else {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
            });
        }

        let expr = if let Some(value) = Self::parse_expr(parser, power) {
            match value {
                Ok(expr) => expr,
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
        };

        Some(Ok(Expr::Unary(Unary {
            span: Span::new(
                parser.scanner().file_id(),
                started.start,
                parser.scanner().span().end,
            ),
            op,
            expr: Box::new(expr),
        })))
    }

    /// Parses a basic value expression, such as `1 + 1`.
    fn parse_expr(parser: &mut Parser, min_power: u8) -> Option<Result<Self, Error>> {
        let mut left = match Self::parse_unary(parser, min_power)? {
            Ok(left) => left,
            Err(err) => return Some(Err(err)),
        };

        loop {
            let op = if let Some(next) = parser.scanner_mut().peek() {
                match next {
                    Ok(token) => {
                        if token.is_binary_operator() {
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
            } else if op == Token::Constructor {
                parser.scanner_mut().next();
                let starts = parser.scanner().span();

                let fields = match parser
                    .parse::<Vec<ConstructorField>>()
                    .expect("cannot return nothing")
                {
                    Ok(args) => args,
                    Err(err) => return Some(Err(err)),
                };

                // check for closing brace
                if let Some(Ok(Token::RBrace)) = parser.scanner_mut().next() {
                } else {
                    return Some(Err(Error::ExpectedClosingBrace {
                        starts,
                        offending: parser.scanner().span(),
                    }));
                }

                left = Expr::Constructor(Constructor {
                    span: Span::new(
                        parser.scanner().file_id(),
                        left.span().start,
                        parser.scanner().span().end,
                    ),
                    ty: Box::new(left),
                    fields,
                });
            } else {
                parser.scanner_mut().next();
                let rhs = if let Some(expr) = Expr::parse_expr(parser, right_power) {
                    match expr {
                        Ok(expr) => expr,
                        Err(err) => return Some(Err(err)),
                    }
                } else {
                    parser.scanner_mut().next();
                    return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
                };

                left = Expr::Binary(Binary {
                    span: Span::new(
                        parser.scanner().file_id(),
                        left.span().start,
                        rhs.span().end,
                    ),
                    op: BinaryOp::from_token(op),
                    left: Box::new(left),
                    right: Box::new(rhs),
                });
            }
        }

        Some(Ok(left))
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        // todo: move statements to a [Statement] enum
        if let Some(res) = parser.parse::<Return>() {
            match res {
                Ok(return_) => Some(Ok(Expr::Return(return_))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(res) = parser.parse::<Var>() {
            match res {
                Ok(var) => Some(Ok(Expr::Var(var))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(res) = parser.parse::<While>() {
            match res {
                Ok(while_) => Some(Ok(Expr::While(while_))),
                Err(err) => Some(Err(err)),
            }
        } else {
            Self::parse_expr(parser, 0)
        }
    }
}
