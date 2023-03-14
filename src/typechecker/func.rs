use crate::{
    ast,
    error::Error,
    span::{Span, Spanned},
};

use super::{module::Module, stmnt::Block, symbol::Symbol, types::Type, Typechecker};

/// An argument in a function declaration.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncArg {
    pub name: String,
    pub ty: Type,
}

/// The signature of a function declaration in Amp.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncSignature {
    /// The arguments of the function.
    pub args: Vec<FuncArg>,

    /// The return type of the function, if any.
    pub returns: Option<Type>,
}

impl FuncSignature {
    pub fn name(&self) -> String {
        format!(
            "func({}){}",
            self.args
                .iter()
                .map(|arg| arg.ty.name())
                .collect::<Vec<_>>()
                .join(", "),
            self.returns
                .as_ref()
                .map(|ty| format!(" -> {}", ty.name()))
                .unwrap_or("void".to_string())
        )
    }

    /// Checks the type signature of a function declaration.
    pub fn check(module: &mut Module, decl: &ast::Func) -> Result<Self, Error> {
        let mut args = Vec::new();

        for arg in &decl.args.args {
            let ty = Type::check(module, &arg.ty)?;
            args.push(FuncArg {
                name: arg.name.value.clone(),
                ty,
            });
        }

        let returns = match &decl.returns {
            Some(ty) => Some(Type::check(module, ty)?),
            None => None,
        };

        Ok(Self { args, returns })
    }
}

/// A function declaration in Amp.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncDecl {
    /// The type signature of the function.
    pub signature: FuncSignature,

    /// The name of the function.
    ///
    /// TODO: replace with some sort of namespace path type.
    pub name: Spanned<String>,

    /// The span of the declaration, from the `fn` keyword to the end of the return type, if any.
    pub decl_span: Span,
}

/// The definition of a function in Amp.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FuncDef {
    pub decl: FuncDecl,
    pub block: Block,
    // TODO: implement function block
}

/// Declares the name of a function.
pub fn check_func_decl(
    checker: &mut Typechecker,
    module: &mut Module,
    decl: &ast::Func,
) -> Result<(), Error> {
    let signature = FuncSignature::check(module, decl)?;
    // TODO: implement function block

    let decl = FuncDecl {
        signature,
        name: Spanned::new(decl.name.span, decl.name.value.clone()),
        decl_span: Span::new(
            decl.span.file_id,
            decl.span.start,
            decl.returns
                .as_ref()
                .map_or(decl.args.span.end, |ty| ty.span().end),
        ),
    };

    checker.declare_func(decl, module)?;

    Ok(())
}

/// Checks a function declaration.
pub fn check_func_def(
    checker: &mut Typechecker,
    module: &mut Module,
    def: &ast::Func,
) -> Result<(), Error> {
    if let Some(block) = &def.block {
        let block = Block::check(checker, module, block)?;

        let item = module
            .resolve_symbol(&def.name.value)
            .expect("Typechecker confirms this function exists");

        let func = match &checker.symbols[item.0 as usize] {
            Symbol::FuncDecl(func) => func,
            _ => unreachable!(),
        };

        checker.symbols[item.0 as usize] = Symbol::FuncDef(FuncDef {
            decl: func.clone(),
            block,
        });

        // let mut typechecker = Typechecker::new(module);
        // typechecker.check_block(block)?;
    }

    Ok(())
}
