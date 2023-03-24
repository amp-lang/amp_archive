use crate::{
    ast,
    error::Error,
    span::{Span, Spanned},
};

use super::{
    decl::Modifier,
    scope::Scope,
    stmnt::Block,
    types::Type,
    var::{Var, Vars},
    Typechecker,
};

/// A unique identifier representing a function.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncId(pub usize);

/// An argument in a function declaration.
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct FuncArg {
    pub name: String,
    pub ty: Type,
}

/// The signature of a function declaration in Amp.
#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Signature {
    /// The arguments of the function.
    pub args: Vec<Spanned<FuncArg>>,

    /// The return type of the function, if any.
    pub returns: Option<Type>,
}

impl Signature {
    pub fn name(&self, checker: &Typechecker) -> String {
        format!(
            "func({}){}",
            self.args
                .iter()
                .map(|arg| arg.value.ty.name(checker))
                .collect::<Vec<_>>()
                .join(", "),
            self.returns
                .as_ref()
                .map(|ty| format!(" -> {}", ty.name(checker)))
                .unwrap_or("".to_string())
        )
    }

    /// Checks the type signature of a function declaration.
    pub fn check(scope: &mut Scope, decl: &ast::Func) -> Result<Self, Error> {
        let mut args = Vec::new();

        for arg in &decl.args.args {
            let ty = Type::check(scope, &arg.ty)?;
            args.push(Spanned::new(
                arg.span,
                FuncArg {
                    name: arg.name.value.clone(),
                    ty,
                },
            ));
        }

        let returns = match &decl.returns {
            Some(ty) => Some(Type::check(scope, ty)?),
            None => None,
        };

        Ok(Self { args, returns })
    }
}

/// A function declaration in Amp.
#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    /// The type signature of the function.
    pub signature: Signature,

    /// A list of modifiers for the function.
    pub modifiers: Vec<Modifier>,

    /// The name of the function.
    ///
    /// TODO: replace with some sort of namespace path type.
    pub name: Spanned<String>,

    /// The span of the declaration, from the `fn` keyword to the end of the return type, if any.
    pub span: Span,

    /// The definition of the [Func].
    pub func_impl: Option<FuncImpl>,
}

/// The implementation of a function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FuncImpl {
    pub block: Block,
    pub vars: Vars,
}

/// Declares the name of a function.
pub fn check_func_decl(
    checker: &mut Typechecker,
    scope: &mut Scope,
    decl: &ast::Func,
) -> Result<(), Error> {
    let signature = Signature::check(scope, decl)?;
    // TODO: implement function block

    let decl = Func {
        signature,
        modifiers: decl.modifiers.iter().map(|m| Modifier::check(m)).collect(),
        name: Spanned::new(decl.name.span, decl.name.value.clone()),
        span: Span::new(
            decl.span.file_id,
            decl.span.start,
            decl.returns
                .as_ref()
                .map_or(decl.args.span.end, |ty| ty.span().end),
        ),
        func_impl: None,
    };

    checker.declare_func(decl, scope)?;

    Ok(())
}

/// Checks a function declaration.
pub fn check_func_def(
    checker: &mut Typechecker,
    scope: &mut Scope,
    ast: &ast::Func,
) -> Result<(), Error> {
    if let Some(ast_block) = &ast.block {
        let item = scope
            .resolve_func(&ast.name.value)
            .expect("Typechecker confirms this function exists");
        let func = &checker.funcs[item.0 as usize];
        let mut vars = Vars::new();

        for (idx, arg) in func.signature.args.iter().enumerate() {
            let var = vars.declare_var(Var::new_argument(
                arg.span,
                arg.value.name.clone(),
                arg.value.ty.clone(),
                idx,
            ));
            scope.define_var(arg.value.name.clone(), var);
        }

        let block = Block::check(checker, scope, &mut vars, func, ast_block)?;

        checker.funcs[item.0 as usize].func_impl = Some(FuncImpl { block, vars });
    }

    Ok(())
}
