use crate::{
    ast,
    error::Error,
    span::{Span, Spanned},
};

use super::{
    decl::Modifier,
    path::Path,
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

    /// Whether the function has variadic arguments after the last argument in the argument list.
    pub variadic: bool,
}

impl Signature {
    /// Renders the type signature as a human-readable type string.
    pub fn name(&self, checker: &Typechecker) -> String {
        format!(
            "func({}{}){}",
            self.args
                .iter()
                .map(|arg| arg.value.ty.name(checker))
                .collect::<Vec<_>>()
                .join(", "),
            if self.variadic { ", ..." } else { "" },
            self.returns
                .as_ref()
                .map(|ty| format!(" -> {}", ty.name(checker)))
                .unwrap_or("".to_string())
        )
    }

    /// Checks the type signature of a function declaration.
    pub fn check(scope: &mut Scope, decl: &ast::Func) -> Result<Self, Error> {
        let mut args = Vec::new();
        let mut variadic = false;

        let mut decl_args = decl.args.args.iter();
        while let Some(arg) = decl_args.next() {
            match arg {
                ast::FuncArgOrVariadic::FuncArg(arg) => {
                    let ty = Type::check(scope, &arg.ty)?;
                    args.push(Spanned::new(
                        arg.span,
                        FuncArg {
                            name: arg.name.value.clone(),
                            ty,
                        },
                    ));
                }
                ast::FuncArgOrVariadic::Variadic(_) => {
                    variadic = true;

                    if let Some(bad_arg) = decl_args.next() {
                        return Err(Error::ArgCannotFollowVariadic(bad_arg.span()));
                    }
                }
            }
        }

        let returns = match &decl.returns {
            Some(ty) => Some(Type::check(scope, ty)?),
            None => None,
        };

        Ok(Self {
            args,
            returns,
            variadic,
        })
    }
}

/// A function declaration in Amp.
#[derive(Clone, Debug, PartialEq)]
pub struct Func {
    /// The type signature of the function.
    pub signature: Signature,

    /// A list of modifiers for the function.
    pub modifiers: Vec<Modifier>,

    /// The name that the function should be linked with, if any.
    pub extern_name: Option<String>,

    /// The name of the function.
    ///
    /// TODO: replace with some sort of namespace path type.
    pub name: Spanned<Path>,

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
) -> Result<FuncId, Error> {
    let signature = Signature::check(scope, decl)?;

    if signature.variadic && decl.block != None {
        return Err(Error::NonExternVariadic(decl.span));
    }

    let decl = Func {
        signature,
        modifiers: decl.modifiers.iter().map(|m| Modifier::check(m)).collect(),
        extern_name: decl.extern_name.as_ref().map(|s| s.value.clone()),
        name: Spanned::new(decl.name.span, Path::check(&decl.name)),
        span: Span::new(
            decl.span.file_id,
            decl.span.start,
            decl.returns
                .as_ref()
                .map_or(decl.args.span.end, |ty| ty.span().end),
        ),
        func_impl: None,
    };

    checker.declare_func(decl, scope)
}

/// Checks a function declaration.
pub fn check_func_def(
    checker: &mut Typechecker,
    scope: &mut Scope,
    ast: &ast::Func,
) -> Result<(), Error> {
    if let Some(ast_block) = &ast.block {
        let item = scope
            .resolve_func(&Path::check(&ast.name))
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
