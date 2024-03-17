use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use crate::tokenizer::{get_all_token_defs, Token, TokenList};

const MAGIC_EPSILON: &str = "Epsilon";
const MAGIC_START_PROD: &str = "^";
const MAGIC_PROGRAM_PROD: &str = "Program";
const MAGIC_EOF_TOKEN: &str = "$";

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub enum Symbol {
    Epsilon(),
    Token(String),
    Expression(String),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Serialize, Deserialize)]
pub struct Production {
    pub name: String,
    pub symbols: Vec<Symbol>,
}

type ProductionSet = Vec<Production>;

#[derive(Clone, Debug, Serialize)]
pub struct Expression {
    pub type_id: String,
    pub children: Vec<CstNode>,
}

#[derive(Clone, Debug, Serialize)]
pub enum CstNode {
    Token(Token),
    Expression(Expression),
}

type CstPath = Vec<usize>;

impl Expression {
    fn get_child(&self, path: &CstPath) -> Result<&CstNode, InvalidCstPathError> {
        if path.is_empty() {
            return Err(InvalidCstPathError {});
        }

        let cur_index = path[0];
        if cur_index >= self.children.len() {
            return Err(InvalidCstPathError {});
        }

        let child = &self.children[cur_index];
        return if path.len() == 1 {
            Ok(&child)
        } else {
            match child {
                CstNode::Expression(expr) => expr.get_child(&path[1..].to_vec()),
                CstNode::Token(_) => Err(InvalidCstPathError {}),
            }
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Cst {
    pub root: CstNode,
}

impl Cst {
    fn add_child(&mut self, parent_path: &CstPath, node: CstNode) -> Result<CstPath, InvalidCstPathError> {
        let mut subpath = parent_path.as_slice();
        let mut cur_child = match &mut self.root {
            CstNode::Expression(expr) => expr,
            _ => panic!("Root node is not an expression"),
        };
        while !subpath.is_empty() {
            let cur_index = subpath[0];

            if cur_index >= cur_child.children.len() {
                return Err(InvalidCstPathError {});
            }

            cur_child = match &mut cur_child.children[subpath[0]] {
                CstNode::Token(_) => {
                    return Err(InvalidCstPathError {});
                }
                CstNode::Expression(expr) => { expr }
            };
            subpath = &subpath[1..];
        }

        let (child_type, child_id) = match &node {
            CstNode::Token(t) => { ("token", &t.type_id) }
            CstNode::Expression(e) => { ("expr", &e.type_id) }
        };

        let mut child_path = parent_path.clone();
        child_path.push(cur_child.children.len());

        cur_child.children.push(node);

        Ok(child_path)
    }

    fn remove_child(&mut self, path: &CstPath) -> Result<(), InvalidCstPathError> {
        if path.is_empty() {
            return Err(InvalidCstPathError {});
        }

        let mut subpath = path.as_slice();
        let mut cur_child = match &mut self.root {
            CstNode::Expression(expr) => expr,
            _ => panic!("Root node is not an expression"),
        };
        while subpath.len() > 1 {
            let cur_index = subpath[0];
            if cur_index >= cur_child.children.len() {
                return Err(InvalidCstPathError {});
            }

            cur_child = match &mut cur_child.children[subpath[0]] {
                CstNode::Token(_) => {
                    return Err(InvalidCstPathError {});
                }
                CstNode::Expression(expr) => expr,
            };
            subpath = &subpath[1..];
        }

        let leaf_index = subpath[0];
        if cur_child.children.len() != leaf_index + 1 {
            return Err(InvalidCstPathError {});
        }

        cur_child.children.pop();
        Ok(())
    }
}

#[derive(Debug)]
struct InvalidCstPathError {
}

impl Display for InvalidCstPathError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid AST path")
    }
}

impl Error for InvalidCstPathError {
}

lazy_static! {
    static ref ID_REGEX: Regex = Regex::new("^[A-Za-z0-9_]+$").unwrap();
    #[derive(Debug)]
    static ref GRAMMAR: IndexMap<String, ProductionSet> = parse_bnf(include_str!["../res/grammar.txt"])
            .expect("Failed to parse grammar");
}

fn parse_bnf_production(name: &String, prod: &str) -> Result<Production, String> {
    let mut prod_symbols = Vec::<Symbol>::new();

    for symbol_str in prod.trim().split(' ').map(|p| p.trim()) {
        let symbol = if &symbol_str == &MAGIC_EPSILON {
            Symbol::Epsilon()
        } else if symbol_str.starts_with('(') && symbol_str.ends_with(')') {
            let symbol_id = &symbol_str[1..(symbol_str.len() - 1)];
            if !ID_REGEX.is_match(symbol_id) {
                return Err(format!("Invalid token identifier '{symbol_id}'"));
            }
            Symbol::Token(symbol_id.to_string())
        } else {
            if !ID_REGEX.is_match(symbol_str) {
                return Err(format!("Invalid production identifier '{symbol_str}'"));
            }
            Symbol::Expression(symbol_str.to_string())
        };

        prod_symbols.push(symbol);
    }

    return Ok(Production { name: name.to_string(), symbols: prod_symbols });
}

fn parse_bnf(content: &str) -> Result<IndexMap<String, ProductionSet>, String> {
    let mut prod_sets = IndexMap::<String, ProductionSet>::new();

    let mut cur_set: Option<(String, ProductionSet)> = None;
    let mut line_num = 0;
    for line in content.split('\n') {
        line_num += 1;

        if line.is_empty() {
            if let Some((prev_prod_id, prev_prod)) = cur_set {
                prod_sets.insert(prev_prod_id, prev_prod);
                cur_set = None;
            }
            continue;
        } else if line.contains(":=") {
            if let Some((prev_prod_id, prev_prod)) = cur_set {
                prod_sets.insert(prev_prod_id, prev_prod);
            }

            let spl = line.split_once(":=").unwrap();
            let (id, prod) = (spl.0.trim(), spl.1.trim());
            cur_set = Some((id.to_string(), vec![parse_bnf_production(&id.to_string(), prod)?]));
        } else if line.trim().starts_with("|") {
            if let Some((prod_name, prod_set)) = &mut cur_set {
                let next_prod = line.trim()[1..].trim();
                prod_set.push(parse_bnf_production(prod_name, next_prod)?);
            } else {
                return Err(format!("Encountered unexpected production continuation at line {line_num}"))
            }
        } else {
            return Err(format!("Encountered unexpected content at line {line_num}"));
        }
    }

    return Ok(prod_sets);
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct LR1Item {
    prod: Production,
    pos: usize,
    lookahead: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum LRAction {
    Shift(usize),
    Reduce(Production),
    Accept(),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Hash)]
pub struct ActionTableKey {
    pub state: usize,
    pub token: String,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Hash)]
pub struct GotoTableKey {
    pub state: usize,
    pub symbol: String,
}

#[derive(Serialize, Deserialize)]
pub struct SerializableActionEntry {
    pub state: usize,
    pub shift_state: Option<usize>,
    pub reduce_prod: Option<Production>,
}

#[derive(Serialize, Deserialize)]
pub struct SerializableGotoEntry {
    pub cur_state: usize,
    pub goto_state: usize,
}

pub type SerializableActionTable = HashMap<String, SerializableActionEntry>;

pub type SerializableGotoTable = HashMap<String, SerializableGotoEntry>;

fn compute_first_sets() -> HashMap<String, BTreeSet<String>> {
    let mut first_sets = HashMap::<String, BTreeSet<String>>::new();

    loop {
        let mut did_modify = false;

        for (nt_name, nt) in GRAMMAR.iter() {
            _ = first_sets.entry(nt_name.to_string()).or_insert(BTreeSet::new());

            for prod in nt {
                for i in 0..prod.symbols.len() {
                    let symbol = &prod.symbols[i];

                    match symbol {
                        Symbol::Token(token) => {
                            let cur_set = first_sets.get_mut(nt_name).expect("Failed to get first set");
                            did_modify |= cur_set.insert(token.clone());
                            // don't consider any further symbols
                            break;
                        },
                        Symbol::Expression(expr) => {
                            if expr == nt_name {
                                break;
                            }

                            if let Some(expr_set) = first_sets.get(expr).cloned() {
                                let cur_set = first_sets.get_mut(nt_name).expect("Failed to get first set");

                                if cur_set.intersection(&expr_set).count() < expr_set.len() {
                                    cur_set.extend(expr_set);
                                    did_modify = true;
                                }
                            }

                            let prods = GRAMMAR.get(expr);
                            if prods.is_none() {
                                panic!("Encountered unknown grammar symbol {} in production for {}", expr, prod.name);
                            }
                            if prods.unwrap().iter().any(|prod| prod.symbols[0] == Symbol::Epsilon()) {
                                if i == prod.symbols.len() - 1 {
                                    let cur_set = first_sets.get_mut(nt_name).expect("Failed to get first set");
                                    cur_set.insert(MAGIC_EPSILON.to_string());
                                }

                                // go to the next symbol since the current one can be empty
                                continue;
                            }

                            // don't consider any further symbols
                            break;
                        },
                        Symbol::Epsilon() => {
                            let cur_set = first_sets.get_mut(nt_name).expect("Failed to get first set");
                            cur_set.insert(MAGIC_EPSILON.to_string());
                        },
                    }
                }
            }
        }

        if !did_modify {
            break;
        }
    }

    return first_sets;
}

fn compute_follow_sets(first_sets: &HashMap<String, BTreeSet<String>>) -> HashMap<String, BTreeSet<String>> {
    let mut follow_sets = HashMap::<String, BTreeSet<String>>::new();

    follow_sets.insert(MAGIC_START_PROD.to_string(), std::iter::once(MAGIC_EOF_TOKEN.to_string()).collect());

    loop {
        let mut did_modify = false;

        for prod in GRAMMAR.values().flatten()
                .chain(std::iter::once(&Production {
                    name: MAGIC_START_PROD.to_string(),
                    symbols: vec![Symbol::Expression(MAGIC_PROGRAM_PROD.to_string())]
                })) {
            for i in 0..prod.symbols.len() {
                match &prod.symbols[i] {
                    Symbol::Expression(expr) => {
                        _ = follow_sets.entry(expr.clone()).or_insert(BTreeSet::new());

                        if i == prod.symbols.len() - 1 {
                            if let Some(to_add) = follow_sets.get(&prod.name).cloned() {
                                if follow_sets[expr].intersection(&to_add).count() < to_add.len() {
                                    follow_sets.get_mut(expr).expect("Failed to get follow set").extend(to_add);
                                    did_modify = true;
                                }
                            }
                        } else {
                            match &prod.symbols[i + 1] {
                                Symbol::Token(token) => {
                                    did_modify |= follow_sets.get_mut(expr).expect("Failed to get follow set")
                                            .insert(token.clone());
                                },
                                Symbol::Expression(next_expr) => {
                                    _ = follow_sets.entry(expr.clone()).or_insert(BTreeSet::new());

                                    if let Some(mut to_add) = first_sets.get(next_expr).cloned() {
                                        to_add.remove(MAGIC_EPSILON);
                                        if follow_sets[expr].intersection(&to_add).count() < to_add.len() {
                                            follow_sets.get_mut(expr).expect("Failed to get follow set")
                                                    .extend(to_add);
                                            did_modify = true;
                                        }
                                    }

                                    if prod.symbols.len() > 1 && i == prod.symbols.len() - 2
                                            && first_sets.get(next_expr)
                                            .map(|fs| fs.contains(MAGIC_EPSILON)).unwrap_or(false) {
                                        if let Some(to_add) = follow_sets.get(&prod.name).cloned() {
                                            if follow_sets[expr].intersection(&to_add).count() < to_add.len() {
                                                follow_sets.get_mut(expr).expect("Failed to get follow set")
                                                        .extend(to_add);
                                                did_modify = true;
                                            }
                                        }
                                    }
                                },
                                Symbol::Epsilon() => {},
                            }
                        }
                    },
                    _ => (),
                }
            }
        }

        if !did_modify {
            break;
        }
    }

    return follow_sets;
}

#[allow(unused)]

fn compute_lookahead_tokens(prod: &Production, pos: usize, cur_lookahead: &String,
    first_sets: &HashMap<String, BTreeSet<String>>) -> BTreeSet<String> {
    let mut cur_lookaheads = BTreeSet::<String>::new();

    let mut can_be_empty = false;

    let is_at_last_symbol = pos == prod.symbols.len() - 1;
    let is_at_end = pos >= prod.symbols.len();

    // this check isn't strictly necessary since the for loop wouldn't
    // do any iterations if this weren't true, but it's included for
    // code clarity
    if !is_at_end {
        can_be_empty = true;
        for i in pos..prod.symbols.len() {
            match &prod.symbols[i] {
                Symbol::Expression(expr) => {
                    let mut la = first_sets.get(expr).cloned()
                            .unwrap_or(BTreeSet::<String>::new());
                    let has_epsilon = la.contains(MAGIC_EPSILON);
                    if has_epsilon {
                        //la.extend(follow_sets[expr].iter().cloned());
                        //la.insert(item.lookahead.clone());
                        la.remove(MAGIC_EPSILON);
                    }

                    cur_lookaheads.append(&mut la);

                    if !has_epsilon {
                        can_be_empty = false;
                        break;
                    }
                }
                Symbol::Token(token) => {
                    cur_lookaheads.insert(token.clone());
                    // token can't be empty by definition,
                    // so no need to keep going through
                    can_be_empty = false;
                    break;
                }
                _ => (),
            }
        }
    }

    if can_be_empty {
        cur_lookaheads.insert(MAGIC_EPSILON.to_string());
    }

    if is_at_end || can_be_empty {
        cur_lookaheads.insert(cur_lookahead.clone());
    }

    return cur_lookaheads;
}

#[allow(unused)]
fn print_configurating_set(state: usize, set: &BTreeSet<LR1Item>) {
    print!("{:04}:   ", state);
    for (j, item) in set.iter().enumerate() {
        if j != 0 {
            print!("        ");
        }
        print!("{} -> ", item.prod.name);
        for (k, symbol) in item.prod.symbols.iter().enumerate() {
            if item.pos == k {
                print!(".");
            }
            match &symbol {
                Symbol::Expression(expr) => print!("{expr}"),
                Symbol::Token(token) => print!("({token})"),
                Symbol::Epsilon() => print!("ε"),
            };
            if k != item.prod.symbols.len() - 1 {
                print!(" ");
            }
        }
        if item.pos == item.prod.symbols.len() {
            print!(".");
        }
        print!(" | {:?}", item.lookahead);
        println!();
    }
}


#[allow(unused)]
fn print_action_table(table: &HashMap<ActionTableKey, LRAction>, config_sets: &Vec<BTreeSet<LR1Item>>,
    prods: &IndexSet<Production>) {
    for i in 0..config_sets.len() {
        print!("{},", i);
        println!("{}", get_all_token_defs().iter()
                .map(|t| match table.get(&ActionTableKey { state: i, token: t.clone() }) {
                    Some(LRAction::Shift(next_state)) => format!("s{}", next_state),
                    Some(LRAction::Reduce(prod)) => format!("r({:?})", prod),
                    Some(LRAction::Accept()) => "acc".to_string(),
                    None => "".to_string(),
                }).join(","));
    }
}

#[allow(unused)]
fn print_goto_table(table: &HashMap<GotoTableKey, usize>, config_sets: &Vec<BTreeSet<LR1Item>>,
    prods: &IndexSet<Production>) {
    for i in 0..config_sets.len() {
        print!("{},", i);
        println!("{}", prods.iter().map(|p| &p.name).unique()
                .map(|p| match table.get(&GotoTableKey { state: i, symbol: p.clone() }) {
                    Some(next_state) => format!("{}", next_state),
                    None => "".to_string(),
                }).join(","));
    }
}

pub fn build_parsing_tables() -> (HashMap<ActionTableKey, LRAction>, HashMap<GotoTableKey, usize>) {
    let prods: IndexSet<Production> = std::iter::once(Production {
        name: MAGIC_START_PROD.to_string(),
        symbols: vec![Symbol::Expression(MAGIC_PROGRAM_PROD.to_string())]
    }).chain(GRAMMAR.values().flatten().cloned()).collect();

    let first_sets = compute_first_sets();
    let follow_sets = compute_follow_sets(&first_sets);

    let mut config_sets = Vec::<BTreeSet<LR1Item>>::new();

    let mut item_map = HashMap::<LR1Item, usize>::new();

    let mut transitions = Vec::<HashMap<Symbol, usize>>::new();

    let mut action_table = HashMap::<ActionTableKey, LRAction>::new();
    let mut goto_table = HashMap::<GotoTableKey, usize>::new();

    let mut kernels: Vec<(Vec<LR1Item>, Option<usize>, Option<Symbol>)> = vec![(vec![LR1Item {
        prod: prods.first().expect("Failed to get start symbol").clone(),
        pos: 0,
        lookahead: "$".to_string(),
    }], None, None)];
    let mut seen_kernels: HashMap<Vec<LR1Item>, usize> = HashMap::new();

    let mut cur_state = 0;
    while let Some((kernel, incoming_state, incoming_symbol)) = kernels.pop() { // lmao

        if let Some(kernel_state) = seen_kernels.get(&kernel) {
            if let Some(prev_state) = incoming_state {
                if let Some(existing) = transitions[prev_state].get(incoming_symbol.as_ref().unwrap())
                        .filter(|val| *val != kernel_state) {
                    panic!("State/symbol pair ({}, {:?}) has two successor states ({}, {})",
                        prev_state, incoming_symbol.unwrap(), existing, kernel_state);
                }
                transitions[prev_state].insert(incoming_symbol.unwrap(), *kernel_state);
            }
            continue;
        }

        let mut cur_set = BTreeSet::<LR1Item>::new();

        let mut item_stack: Vec<LR1Item> = kernel.iter().cloned().collect();

        // perform closure

        while let Some(item) = item_stack.pop() {
            let mut inserted = false;
            if cur_set.insert(item.clone()) {
                inserted = true;
            }
            if !inserted {
                continue;
            }

            if item.pos == item.prod.symbols.len() || item.prod.symbols[item.pos] == Symbol::Epsilon() {
                continue;
            }

            let cur_lookaheads = compute_lookahead_tokens(&item.prod, item.pos + 1, &item.lookahead,
                &first_sets);

            match &item.prod.symbols[item.pos] {
                Symbol::Expression(expr) => {
                    let prods = GRAMMAR.get(expr);
                    if prods.is_none() {
                        panic!("Encountered unknown grammar symbol {} in production for {}", expr, item.prod.name);
                    }
                    for prod in prods.unwrap() {
                        item_stack.extend(cur_lookaheads.iter().filter(|la| *la != MAGIC_EPSILON).map(|la| LR1Item {
                            prod: prod.clone(),
                            pos: 0,
                            lookahead: la.clone(),
                        }));
                    }

                    if prods.unwrap().iter().any(|prod| prod.symbols[0] == Symbol::Epsilon()) {
                        item_stack.push(LR1Item {
                            prod: item.prod.clone(),
                            pos: item.pos + 1,
                            lookahead: item.lookahead.clone(),
                        });
                    }
                }
                _ => {}
            }
        }

        //print_configurating_set(cur_state, &cur_set);

        let existing_set = config_sets.binary_search(&cur_set);
        if !existing_set.is_ok() {
            transitions.push(HashMap::new());
        }

        let dest_state = existing_set.unwrap_or(cur_state);
        if let Some(prev_state) = incoming_state {
            let prev_symbol = incoming_symbol
                    .expect("incoming_state and incoming_symbol must both be Some or None");
            if let Some(existing) = transitions[prev_state].get(&prev_symbol)
                    .filter(|val| **val != dest_state) {
                panic!("State/symbol pair ({}, {:?}) has two successor states ({}, {})",
                    prev_state, prev_symbol, existing, dest_state);
            }
            //println!("Adding state transition ({}, {:?}) -> {}", prev_state, prev_symbol, dest_state);
            transitions[prev_state].insert(prev_symbol, dest_state);
        }

        if existing_set.is_ok() {
            continue;
        }

        seen_kernels.insert(kernel.clone(), cur_state);

        // create initial successor set

        let next = cur_set.iter()
                .filter(|item| item.pos != item.prod.symbols.len() && item.prod.symbols[item.pos] != Symbol::Epsilon())
                .map(|item| LR1Item { prod: item.prod.clone(), pos: item.pos + 1, lookahead: item.lookahead.clone() })
                .into_group_map_by(|item| item.prod.symbols[item.pos - 1].clone())
                .into_iter()
                .map(|(key, items)| (key.clone(), items));
        for (next_symbol, next_kernel) in next {
            kernels.push((next_kernel, Some(cur_state), Some(next_symbol)));
        }

        item_map.extend(cur_set.iter().map(|item| (item.clone(), cur_state)));

        config_sets.push(cur_set);

        cur_state += 1;
    }

    // compute action and goto tables

    for (state, set) in config_sets.iter().enumerate() {
        for item in set {
            if item.pos == item.prod.symbols.len() {
                let key = ActionTableKey { state, token: item.lookahead.clone() };

                if item.prod.name == "^" && item.lookahead == "$" {
                    action_table.insert(key, LRAction::Accept());
                } else {
                    let prod_index = prods.get_index_of(&item.prod)
                            .expect("Failed to get index for production");
                    if let Some(existing) = action_table.get(&key) {
                        match existing {
                            LRAction::Reduce(existing_prod) => {
                                if existing_prod != &item.prod {
                                    panic!("Encountered reduce-reduce conflict between symbols \
                                    {} and {} on token {} in state {} (existing: {:?}, current: {:?})",
                                        item.prod.name, existing_prod.name, key.token, state,
                                        existing_prod, item.prod);
                                }
                            }
                            LRAction::Shift(_) => {
                                panic!("Encountered shift-reduce conflict between symbol {} and token {} in state {} \
                                (production: {:?})",
                                    item.prod.name, key.token, state, item.prod);
                            }
                            _ => {
                                panic!("Encountered unexpected conflict while resolving action table");
                            }
                        }
                    }
                    action_table.insert(key, LRAction::Reduce(item.prod.clone()));
                }
            } else {
                let transition_symbol = &item.prod.symbols[item.pos];
                if let Some(&succ_state) = transitions[state].get(transition_symbol) {
                    match transition_symbol {
                        Symbol::Expression(expr) => {
                            let key = GotoTableKey { state, symbol: expr.clone() };
                            if goto_table.get(&key).filter(|dst| **dst != succ_state).is_some() {
                                panic!("Attempted to insert goto key {:?} multiple times", key);
                            }
                            goto_table.insert(key, succ_state);
                        }
                        Symbol::Token(token) => {
                            action_table.insert(ActionTableKey { state, token: token.clone() },
                                LRAction::Shift(succ_state));
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    //print_action_table(&action_table, &config_sets, &prods);
    //print_goto_table(&goto_table, &config_sets, &prods);

    return (action_table, goto_table);
}
