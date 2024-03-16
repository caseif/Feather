use serde::Serialize;

use crate::parser::ParseError;
use crate::parser::cst::{Cst, generate_cst};
use crate::tokenizer::TokenList;

#[derive(Debug, Serialize)]
pub struct Ast {
    //TODO
}

fn convert_to_ast(cst: Cst) -> Result<Ast, ParseError> {
    //TODO
    return Ok(Ast {});
}

pub fn generate_ast(tokens: TokenList) -> Result<Ast, ParseError> {
    let cst = generate_cst(tokens)?;
    let ast = convert_to_ast(cst);
    return ast;
}
