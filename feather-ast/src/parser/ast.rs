use std::collections::{HashMap, VecDeque};
use serde::Serialize;

use crate::parser::ParseError;
use crate::parser::cst::{Cst, CstNode, generate_cst};
use crate::tokenizer::TokenList;

#[derive(Debug, Serialize)]
pub enum AstNodeType {
    Program,
    Block,
    Expression,
    ImportStatement,
    ConstDecl,
    VarDecl,
    ClassDef,
    FnDef,
    ReturnStatement,
    IfElseChain,
    IfBlock,
    ElseIfBlock,
    ElseBlock,
    Assignment,
}

#[derive(Debug, Serialize)]
pub struct AstNode {
    node_type: AstNodeType,
    val: Option<String>,
    children: Vec<AstNode>,
}

#[derive(Debug, Serialize)]
pub struct Ast {
    root: AstNode,
}

type TreePath = Vec<usize>;

fn flatten_cst<'a>(cst: &'a Cst) -> Vec<(&'a CstNode, TreePath)> {
    /*let mut flat_cst: Vec<(&'a CstNode, TreePath)> = cst.root.children.iter().enumerate()
            .map(|(i, child)| (child, vec![i]))
            .collect();*/
    let mut flat_cst: Vec<(&'a CstNode, TreePath)> = vec![(&cst.root, vec![])];

    let mut traversal_stack: VecDeque<usize> = VecDeque::from([0]);
    while let Some(cur_index) = traversal_stack.pop_front() {
        assert!(cur_index < flat_cst.len());
        let (cur_node, cur_path) = flat_cst[cur_index].clone();

        match cur_node {
            CstNode::Expression(expr) => {
                let mut cursor = flat_cst.len();
                for (i, child) in expr.children.iter().enumerate().rev() {
                    let child_path = [&cur_path[..], &[i]].concat();
                    flat_cst.push((&child, child_path.clone()));

                    traversal_stack.push_back(cursor);
                    cursor += 1;
                }
            }
            CstNode::Token(_) => {}
        }
    }

    flat_cst.reverse();

    return flat_cst;
}

fn process_cst_node(cst_node: &CstNode, children: Vec<AstNode>) -> Option<AstNode> {
    if let CstNode::Expression(expr) = cst_node {
        if expr.type_id == "Program" {
            return Some(AstNode { node_type: AstNodeType::Program, val: None, children });
        }
    }
    return None;
}

fn convert_to_ast(cst: &Cst) -> Result<Ast, ParseError> {
    let flat_cst = flatten_cst(&cst);

    let mut child_map: HashMap<TreePath, Vec<AstNode>> = HashMap::new();

    for (node, path) in &flat_cst {
        let processed_children = child_map.remove(path).unwrap_or(vec![]);

        if let Some(ast_node) = process_cst_node(node, processed_children) {
            if path.len() == 0 {
                return Ok(Ast { root: ast_node });
            }

            let parent_path = path[0..(path.len() - 1)].into_iter().cloned().collect();
            child_map.entry(parent_path).or_insert(vec![]).push(ast_node);
        }
    }

    panic!("Flat tree did not contain root node!");
}

pub fn generate_ast(tokens: TokenList) -> Result<Ast, ParseError> {
    let cst = generate_cst(tokens)?;
    let ast = convert_to_ast(&cst);
    return ast;
}
