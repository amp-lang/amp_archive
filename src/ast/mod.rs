//! The abstract syntax trees for Amp.

mod arg_list;
mod basic;
mod decl;
mod expr;
mod func;
mod types;

pub use arg_list::*;
pub use basic::*;
pub use decl::*;
pub use expr::*;
pub use func::*;
pub use types::*;
