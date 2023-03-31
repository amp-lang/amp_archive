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

    /// `<<`
    Shl,

    /// `>>`
    Shr,

    /// `&`
    BitAnd,
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
            Token::LtLt => Self::Shl,
            Token::GtGt => Self::Shr,
            Token::And => Self::BitAnd,
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

/// An `else if` branch to an `if` statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ElseIf {
    pub span: Span,
    pub cond: Box<Expr>,
    pub body: Block,
}

impl Parse for ElseIf {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let scanner_span = parser.scanner().span();

        if parser.scanner_mut().peek()? == Ok(Token::KElse)
            && parser.scanner_mut().peek_nth(1)? == Ok(Token::KIf)
        {
            parser.scanner_mut().nth(1);
        } else {
            return None;
        }

        let cond = if let Some(value) = Expr::parse(parser) {
            match value {
                Ok(expr) => Box::new(expr),
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
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
            span: Span::new(
                parser.scanner().file_id(),
                scanner_span.start,
                body.span.end,
            ),
            cond,
            body,
        }))
    }
}

/// An `else` branch to an `if` statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Else {
    pub span: Span,
    pub body: Block,
}

impl Parse for Else {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        let scanner_span = parser.scanner().span();

        if parser.scanner_mut().peek()? == Ok(Token::KElse) {
            parser.scanner_mut().next();
        } else {
            return None;
        }

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
            span: Span::new(
                parser.scanner().file_id(),
                scanner_span.start,
                body.span.end,
            ),
            body,
        }))
    }
}

/// A branch of an [If] statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum IfBranch {
    ElseIf(ElseIf),
    Else(Else),
}

impl Parse for IfBranch {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Some(value) = ElseIf::parse(parser) {
            match value {
                Ok(expr) => Some(Ok(Self::ElseIf(expr))),
                Err(err) => Some(Err(err)),
            }
        } else if let Some(value) = Else::parse(parser) {
            match value {
                Ok(expr) => Some(Ok(Self::Else(expr))),
                Err(err) => Some(Err(err)),
            }
        } else {
            None
        }
    }
}

/// An `if` statement.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct If {
    pub span: Span,
    pub cond: Box<Expr>,
    pub body: Block,
    pub branches: Vec<IfBranch>,
}

impl Parse for If {
    fn parse(parser: &mut Parser) -> Option<Result<Self, Error>> {
        if let Ok(Token::KIf) = parser.scanner_mut().peek()? {
            parser.scanner_mut().next();
        } else {
            return None;
        }

        let start = parser.scanner().span();

        let cond = if let Some(value) = Expr::parse(parser) {
            match value {
                Ok(expr) => Box::new(expr),
                Err(err) => return Some(Err(err)),
            }
        } else {
            parser.scanner_mut().next();
            return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
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

        let branches = {
            let mut branches = Vec::new();

            while let Some(item) = IfBranch::parse(parser) {
                match item {
                    Ok(branch) => branches.push(branch),
                    Err(err) => return Some(Err(err)),
                }
            }

            branches
        };

        Some(Ok(Self {
            span: Span::new(
                parser.scanner().file_id(),
                start.start,
                branches
                    .last()
                    .map_or(body.span.end, |branch| match branch {
                        IfBranch::ElseIf(else_if) => else_if.span.end,
                        IfBranch::Else(else_) => else_.span.end,
                    }),
            ),
            cond,
            body,
            branches,
        }))
    }
}

/// An `as` conversion.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct As {
    pub span: Span,
    pub expr: Box<Expr>,
    pub ty: Type,
}

/// An index operation.
///
/// ```amp
/// my_array[0]
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Idx {
    pub span: Span,
    pub expr: Box<Expr>,
    pub index: Box<Expr>,

    /// The `..to` part of the index, if any.
    pub to: Option<Box<Expr>>,
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
    If(If),
    As(As),
    Idx(Idx),
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
            Expr::If(if_) => if_.span,
            Expr::As(as_) => as_.span,
            Expr::Idx(idx) => idx.span,
        }
    }

    /// Returns `true` if the expression must be followed by a semicolon.
    pub fn requires_semi(&self) -> bool {
        match self {
            Expr::While(_) => false,
            Expr::If(_) => false,
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
        } else if let Some(Ok(Token::LParen)) = parser.scanner_mut().peek() {
            parser.scanner_mut().next();

            let expr = if let Some(value) = Expr::parse(parser) {
                match value {
                    Ok(expr) => expr,
                    Err(err) => return Some(Err(err)),
                }
            } else {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
            };

            if let Some(Ok(Token::RParen)) = parser.scanner_mut().peek() {
                parser.scanner_mut().next();
            } else {
                parser.scanner_mut().next();
                return Some(Err(Error::ExpectedClosingParen(parser.scanner().span())));
            }

            Some(Ok(expr))
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
            } else if op == Token::KAs {
                parser.scanner_mut().next();
                let starts = parser.scanner().span();

                let ty = match parser.parse::<Type>() {
                    Some(Ok(ty)) => ty,
                    Some(Err(err)) => return Some(Err(err)),
                    None => return Some(Err(Error::ExpectedAsType(starts))),
                };

                left = Expr::As(As {
                    span: Span::new(
                        parser.scanner().file_id(),
                        left.span().start,
                        parser.scanner().span().end,
                    ),
                    expr: Box::new(left),
                    ty,
                });
            } else if op == Token::LBrack {
                parser.scanner_mut().next();
                let started = parser.scanner().span();

                // Parse array indexing or subslicing
                let index = match parser.parse::<Expr>() {
                    Some(Ok(index)) => index,
                    Some(Err(err)) => return Some(Err(err)),
                    None => {
                        return Some(Err(Error::ExpectedExpression(parser.scanner().span())));
                    }
                };

                // Check for `..` operator
                let to = if let Some(Ok(Token::DotDot)) = parser.scanner_mut().peek() {
                    parser.scanner_mut().next();

                    match parser.parse::<Expr>() {
                        Some(Ok(to)) => Some(to),
                        Some(Err(err)) => return Some(Err(err)),
                        None => {
                            return Some(Err(Error::ExpectedExpression(parser.scanner().span())))
                        }
                    }
                } else {
                    None
                };

                // check for closing brace
                if let Some(Ok(Token::RBrack)) = parser.scanner_mut().next() {
                } else {
                    return Some(Err(Error::ExpectedClosingBrack {
                        started,
                        offending: parser.scanner().span(),
                    }));
                }

                left = Expr::Idx(Idx {
                    span: Span::new(
                        parser.scanner().file_id(),
                        left.span().start,
                        parser.scanner().span().end,
                    ),
                    expr: Box::new(left),
                    index: Box::new(index),
                    to: to.map(Box::new),
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
        } else if let Some(res) = parser.parse::<If>() {
            match res {
                Ok(if_) => Some(Ok(Expr::If(if_))),
                Err(err) => Some(Err(err)),
            }
        } else {
            Self::parse_expr(parser, 0)
        }
    }
}
