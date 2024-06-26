use featherparse::Token;

pub mod ast;
pub mod cst;

pub struct ParseError {
    pub next_token: Token,
}

pub use self::ast::{Ast, AstNode, generate_ast};
