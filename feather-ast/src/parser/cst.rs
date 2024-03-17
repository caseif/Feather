use feather_lrgen::gen_parse_tables;
use featherparse::*;
use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};

use crate::parser::ParseError;

lazy_static! {
    static ref PARSE_TABLES: (HashMap<ActionTableKey, LRAction>, HashMap<GotoTableKey, usize>) = gen_parse_tables!();
}

pub fn generate_cst(tokens: TokenList) -> Result<Cst, ParseError> {
    let mut stack = Vec::<(usize, CstNode)>::new();

    let mut state: usize = 0;
    let mut i = 0;
    while i < tokens.tokens.len() {
        let token = &tokens.tokens[i];

        let key = ActionTableKey { state, token: token.type_id.clone() };
        if !PARSE_TABLES.0.contains_key(&key) {
            //println!("Failed in state {} on token {}", state, token.type_id);
            return Err(ParseError { next_token: token.clone() });
        }

        match &PARSE_TABLES.0[&key] {
            LRAction::Shift(next_state) => {
                stack.push((state, CstNode::Token(token.clone())));
                state = *next_state;
                i += 1;
            }
            LRAction::Reduce(prod) => {
                let mut popped_states = VecDeque::<(usize, CstNode)>::new();

                for symbol in prod.symbols.iter().rev() {
                    let (_, top) = stack.last().unwrap();
                    match symbol {
                        Symbol::Token(prod_token) => {
                            if let CstNode::Token(stack_token) = &top {
                                if &stack_token.type_id == prod_token {
                                    popped_states.push_front(stack.pop().unwrap());
                                }
                            }
                        }
                        Symbol::Expression(prod_expr) => {
                            if let CstNode::Expression(stack_expr) = &top {
                                if &stack_expr.type_id == prod_expr {
                                    popped_states.push_front(stack.pop().unwrap());
                                }
                            }
                        }
                        _ => {}
                    }
                }

                let new_node = CstNode::Expression(Expression {
                    type_id: prod.name.clone(),
                    children: popped_states.iter().map(|(_, node)| node).cloned().collect(),
                });
                let (new_state, _) = popped_states[0];
                stack.push((new_state, new_node));

                let goto_state = PARSE_TABLES.1[&GotoTableKey { state: new_state, symbol: prod.name.clone() }];
                state = goto_state;
            },
            LRAction::Accept() => {
                break;
            },
        }
    }

    let (_, root_node) = stack.pop().expect("Failed to pop final state");
    return Ok(Cst { root: root_node });
}
