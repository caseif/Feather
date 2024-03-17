use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use serde::Serialize;
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::parser::ParseError;
use featherparse::*;
use lazy_static::lazy_static;

const MAGIC_EPSILON: &str = "Epsilon";
const MAGIC_START_PROD: &str = "^";
const MAGIC_PROGRAM_PROD: &str = "Program";
const MAGIC_EOF_TOKEN: &str = "$";

const ACTION_TABLE_JSON: &str = include_str!(concat!(env!("OUT_DIR"), "/action_table.json"));
const GOTO_TABLE_JSON: &str = include_str!(concat!(env!("OUT_DIR"), "/goto_table.json"));

lazy_static! {
    static ref ACTION_TABLE: HashMap<ActionTableKey, LRAction> = load_action_table();
    static ref GOTO_TABLE: HashMap<GotoTableKey, usize> = load_goto_table();
}

fn load_action_table() -> HashMap<ActionTableKey, LRAction> {
    let mut table: HashMap<ActionTableKey, LRAction> = HashMap::new();

    let map = serde_json::from_str::<HashMap<String, Vec<SerializableActionEntry>>>(ACTION_TABLE_JSON)
            .expect("Failed to deserialize action table JSON");
    for (token, entries) in map {
        for entry in entries {
            let action = if let Some(new_state) = entry.shift_state {
                LRAction::Shift(new_state)
            } else if let Some(prod) = entry.reduce_prod {
                LRAction::Reduce(prod)
            } else {
                LRAction::Accept()
            };
            table.insert(ActionTableKey { state: entry.state, token: token.clone() }, action);
        }
    }

    return table;
}

fn load_goto_table() -> HashMap<GotoTableKey, usize> {
    let mut table: HashMap<GotoTableKey, usize> = HashMap::new();

    let map = serde_json::from_str::<HashMap<String, Vec<SerializableGotoEntry>>>(GOTO_TABLE_JSON)
            .expect("Failed to deserialize action table JSON");
    for (symbol, entries) in map {
        for entry in entries {
            table.insert(GotoTableKey { state: entry.cur_state, symbol: symbol.clone() }, entry.goto_state);
        }
    }

    return table;
}

pub fn generate_cst(tokens: TokenList) -> Result<Cst, ParseError> {
    let mut stack = Vec::<(usize, CstNode)>::new();

    let mut state: usize = 0;
    let mut i = 0;
    while i < tokens.tokens.len() {
        let token = &tokens.tokens[i];

        let key = ActionTableKey { state, token: token.type_id.clone() };
        if !ACTION_TABLE.contains_key(&key) {
            //println!("Failed in state {} on token {}", state, token.type_id);
            return Err(ParseError { next_token: token.clone() });
        }

        match &ACTION_TABLE[&key] {
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

                let goto_state = GOTO_TABLE[&GotoTableKey { state: new_state, symbol: prod.name.clone() }];
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
