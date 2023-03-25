use crate::{ast, error::Error};

/// A sequence of identifiers separated by `.`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Path {
    pub items: Vec<String>,
}

impl Path {
    /// Creates a new [Path] with just a short name.
    pub fn new(short_name: String) -> Self {
        Self {
            items: vec![short_name],
        }
    }

    /// Converts an [ast::Path] to a [Path].
    pub fn check(item: &ast::Path) -> Self {
        Self {
            items: item
                .items
                .iter()
                .map(|item| item.value.to_string())
                .collect(),
        }
    }

    fn check_sub_path_expr(item: &ast::Expr, segments: &mut Vec<String>) -> Result<(), Error> {
        match item {
            ast::Expr::Iden(name) => segments.insert(0, name.value.to_string()),
            ast::Expr::Binary(ast::Binary {
                op: ast::BinaryOp::Dot,
                left,
                right,
                ..
            }) => {
                Self::check_sub_path_expr(right, segments)?;
                Self::check_sub_path_expr(left, segments)?;
            }
            _ => return Err(Error::InvalidPath(item.span())),
        }

        Ok(())
    }

    pub fn check_path_expr(item: &ast::Expr) -> Result<Self, Error> {
        let mut segments = Vec::new();
        Self::check_sub_path_expr(item, &mut segments)?;
        Ok(Self { items: segments })
    }

    /// Returns the "short name" of the path, which is the last identifier in the past, omitting
    /// the namespace.
    #[inline]
    pub fn short_name(&self) -> &str {
        self.items.last().unwrap()
    }

    /// Returns the namespace of the [Path], if any.
    pub fn namespace(&self) -> Option<&str> {
        if self.items.len() > 1 {
            Some(&self.items[0])
        } else {
            None
        }
    }
}

impl ToString for Path {
    fn to_string(&self) -> String {
        self.items.join(".")
    }
}
