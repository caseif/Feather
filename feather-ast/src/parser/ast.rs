use featherparse::{Cst, CstNode, TokenList};
use lazy_static::lazy_static;
use serde::Serialize;
use std::collections::{HashMap, VecDeque};
use std::str::FromStr;
use strum_macros::{Display, EnumString};

use crate::parser::ParseError;
use crate::parser::cst::generate_cst;

#[derive(Clone, Debug, Display, Hash, PartialEq, Serialize, EnumString)]
pub enum AstNodeType {
    // used for operator tokens
    Unknown,
    // symbols directly from tokens
    TypeInt8,
    TypeInt16,
    TypeInt32,
    TypeInt64,
    TypeUint8,
    TypeUint16,
    TypeUint32,
    TypeUint64,
    TypeFloat32,
    TypeFloat64,
    TypeBool,
    TypeChar,
    TypeString,
    TypeComplex,
    TypeArray,
    TypeTuple,
    LiteralString,
    LiteralInteger,
    LiteralHex,
    LiteralDecimal,
    LiteralBoolean,
    Nil,
    Identifier,
    // symbols from the formal grammar
    Tuple,
    InitList,
    TypeAnnotation,
    DeclVar,
    DeclConst,
    AccessStatic,
    AccessInstanced,
    AccessIndexed,
    AccessSliced,
    RangeExclusive,
    RangeInclusive,
    SliceExclusive,
    SliceInclusive,
    FnParamList,
    FnInvocation,
    Expression,
    Assignment,
    Block,
    IfBlock,
    ElseIfBlock,
    ElseBlock,
    IfChain,
    WhileLoop,
    ForLoop,
    ContinueStatement,
    BreakStatement,
    ImportStatement,
    FnParamDefs,
    ReturnType,
    FunctionSig,
    FunctionDef,
    ClassFields,
    ClassFieldDef,
    ClassFunctions,
    ClassDef,
    ReturnStatement,
    Annotation,
    Program,
    // virtual symbols not present in the CST
    OpNot,
    OpNegate,
    OpMultiply,
    OpDivide,
    OpAdd,
    OpSubtract,
    OpModulo,
    CmpLess,
    CmpLessEq,
    CmpGreater,
    CmpGreaterEq,
    CmpEquals,
    CmpNotEquals,
    OpAnd,
    OpOr,
}

#[derive(Clone, Debug, Serialize)]
pub struct AstNode {
    pub ty: AstNodeType,
    pub val: Option<String>,
    pub children: Vec<AstNode>,
}

#[derive(Debug, Serialize)]
pub struct Ast {
    pub root: AstNode,
}

enum UniversalRule {
    // Remove the node's subtree entirely.
    Prune,
    // Replace the node with its children, effectively pulling them up one level.
    Flatten,
    // Leave the node in place but replace its value with that of its (only) child.
    PullUpChildValue,
    // Flatten if only 1 child, otherwise rename based on type of nth child and
    // remove the child (see OPERATOR_RULES for specific rules).
    PullUpOperator,
    // Everything else is implicitly left in place as-is.
}

lazy_static! {
    static ref EXPR_RULES: HashMap<&'static str, UniversalRule> = HashMap::from([
        ("TypeInt", UniversalRule::Flatten),
        ("TypeUint", UniversalRule::Flatten),
        ("TypeFloat", UniversalRule::Flatten),
        ("TypeBuiltIn", UniversalRule::Flatten),
        ("Type", UniversalRule::Flatten),
        ("Literal", UniversalRule::Flatten),
        ("ArrayModNaught", UniversalRule::Flatten),
        ("ArraySelector", UniversalRule::Flatten),
        ("TupleTypes", UniversalRule::Flatten),
        ("Parenthetical", UniversalRule::Flatten),
        ("TupleElements", UniversalRule::Flatten),
        ("TupleElsPrime", UniversalRule::Flatten),
        ("InitListEntries", UniversalRule::Flatten),
        ("Lval", UniversalRule::Flatten),
        ("RangeBound", UniversalRule::Flatten),
        ("Range", UniversalRule::Flatten),
        ("SliceSelector", UniversalRule::Flatten),
        ("Slice", UniversalRule::Flatten),
        ("Invokable", UniversalRule::Flatten),
        ("FnParamListPrime", UniversalRule::Flatten),
        ("IfChainPrime", UniversalRule::Flatten),
        ("ForParam", UniversalRule::Flatten),
        ("FnPmDefsPrime", UniversalRule::Flatten),
        ("ClassFldsPrime", UniversalRule::Flatten),
        ("ClassFnsPrime", UniversalRule::Flatten),
        ("AnnotationBody", UniversalRule::Flatten),
        ("Annotatable", UniversalRule::Flatten),
        ("StatementNaught", UniversalRule::Flatten),
        ("StatementList", UniversalRule::Flatten),
        ("Primary", UniversalRule::Flatten),
        ("OpUnary", UniversalRule::Flatten),
        ("OpNumMult", UniversalRule::Flatten),
        ("OpNumAdd", UniversalRule::Flatten),
        ("CmpBoolRel", UniversalRule::Flatten),
        ("CmpBoolEq", UniversalRule::Flatten),
        ("OpBoolAnd", UniversalRule::Flatten),
        ("OpBoolOr", UniversalRule::Flatten),
        ("Statement", UniversalRule::Flatten),
        ("TypeComplex", UniversalRule::PullUpChildValue),
        ("ExprOpUnary", UniversalRule::PullUpOperator),
        ("ExprOpNumMult", UniversalRule::PullUpOperator),
        ("ExprOpNumAdd", UniversalRule::PullUpOperator),
        ("ExprCmpBoolRel", UniversalRule::PullUpOperator),
        ("ExprCmpBoolEq", UniversalRule::PullUpOperator),
        ("ExprOpBoolAnd", UniversalRule::PullUpOperator),
        ("ExprOpBoolOr", UniversalRule::PullUpOperator),
        ("OptNewline", UniversalRule::Prune),
        ("StatementEnd", UniversalRule::Prune),
    ]);

    // Rules for operator expressions specifically. The value is the index of
    // the child to operate on alongside mappings from original child names to
    // new parent names.
    static ref OPERATOR_RULES: HashMap<&'static str, (usize, HashMap<&'static str, AstNodeType>)> = HashMap::from([
        ("ExprOpUnary", (0, HashMap::from([
            ("Not", AstNodeType::OpNot), // invert boolean value
            ("Hyphen", AstNodeType::OpNegate), // negate numeric value
        ]))),
        ("ExprOpNumMult", (1, HashMap::from([
            ("Asterisk", AstNodeType::OpMultiply),
            ("ForwardSlash", AstNodeType::OpDivide),
        ]))),
        ("ExprOpNumAdd", (1, HashMap::from([
            ("Plus", AstNodeType::OpAdd),
            ("Hyphen", AstNodeType::OpSubtract),
            ("Percent", AstNodeType::OpModulo),
        ]))),
        ("ExprCmpBoolRel", (1, HashMap::from([
            ("LessThan", AstNodeType::CmpLess),
            ("LessEqual", AstNodeType::CmpLessEq),
            ("GreaterThan", AstNodeType::CmpGreater),
            ("GreaterEqual", AstNodeType::CmpGreaterEq),
        ]))),
        ("ExprCmpBoolEq", (1, HashMap::from([
            ("Equals", AstNodeType::CmpEquals),
            ("NotEquals", AstNodeType::CmpNotEquals),
        ]))),
        ("ExprOpBoolAnd", (1, HashMap::from([
            ("And", AstNodeType::OpAnd),
        ]))),
        ("ExprOpBoolOr", (1, HashMap::from([
            ("Or", AstNodeType::OpOr),
        ]))),
    ]);

    // Tokens to explicitly preserve while processing the AST. All other nodes
    // will be implicitly pruned, unless they contain a value in which case they
    // will be preserved.
    static ref PRESERVE_TOKENS: Vec<&'static str> = Vec::from([
        "TypeInt8",
        "TypeInt16",
        "TypeInt32",
        "TypeInt64",
        "TypeUnt8",
        "TypeUnt16",
        "TypeUnt32",
        "TypeUnt64",
        "TypeFloat8",
        "TypeFloat16",
        "TypeFloat32",
        "TypeFloat64",
        "TypeBool",
        "TypeChar",
        "TypeString",
        "LiteralString",
        "LiteralInteger",
        "LiteralHex",
        "LiteralDecimal",
        "LiteralBoolean",
        "Nil",
    ]);

    // Operator tokens which are used only in resolving their parent
    // expressions. They will not be pruned, but their type ID will be moved to
    // the value field and they will be given a generic "Operator" type ID.
    // They will be pruned while processing their respective parent nodes.
    static ref OPERATOR_TOKENS: Vec<&'static str> = Vec::from([
        "Not",
        "Plus",
        "Hyphen",
        "Percent",
        "Asterisk",
        "ForwardSlash",
        "LessThan",
        "LessEqual",
        "GreaterThan",
        "GreaterEqual",
        "Equals",
        "NotEquals",
        "And",
        "Or",
    ]);
}

type TreePath = Vec<usize>;

fn flatten_cst<'a>(cst: &'a Cst) -> Vec<(&'a CstNode, TreePath)> {
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

fn process_cst_node(cst_node: &CstNode, children: Vec<AstNode>) -> Vec<AstNode> {
    match cst_node {
        CstNode::Expression(expr) => {
            if let Some(rule) = EXPR_RULES.get(expr.type_id.as_str()) {
                match rule {
                    UniversalRule::Prune => {
                        return vec![];
                    }
                    UniversalRule::Flatten => {
                        return children;
                    }
                    UniversalRule::PullUpChildValue => {
                        if children.len() != 1 {
                            panic!("Expression must have exactly 1 child to pull value up from");
                        }

                        return vec![AstNode {
                            ty: AstNodeType::from_str(expr.type_id.as_str()).unwrap_or_else(|_| panic!(
                                "Expression type string '{}' should have corresponding enum variant", expr.type_id)),
                            val: children[0].val.clone(),
                            children: vec![]
                        }];
                    }
                    UniversalRule::PullUpOperator => {
                        if children.len() == 1 {
                            return children;
                        } else {
                            let (child_index, mappings) = OPERATOR_RULES.get(expr.type_id.as_str())
                                    .unwrap_or_else(|| panic!(
                                        "Operator expression '{}' should have corresponding operator rule",
                                        expr.type_id));
                            if *child_index >= children.len() {
                                panic!("Operator expression '{}' does not have enough children", expr.type_id);
                            }
                            let op_token = children[*child_index].val.as_ref().unwrap();
                            let mapped_type = mappings.get(op_token.as_str()).unwrap_or_else(|| panic!(
                                "Operator token '{}' should have corresponding name mapping", op_token));
                            return vec![AstNode {
                                ty: mapped_type.clone(),
                                val: None,
                                children: [&children[0..*child_index], &children[(*child_index + 1)..]]
                                        .concat().clone(),
                            }];
                        }
                    }
                }
            }

            return vec![AstNode {
                ty: AstNodeType::from_str(expr.type_id.as_str()).unwrap_or_else(|_| panic!(
                    "Expression type ID '{}' should have corresponding enum variant", expr.type_id)),
                val: None,
                children
            }];
        }
        CstNode::Token(token) => {
            if PRESERVE_TOKENS.contains(&token.type_id.as_str()) || token.value.is_some() {
                return vec![AstNode {
                    ty: AstNodeType::from_str(token.type_id.as_str()).unwrap_or_else(|_| panic!(
                        "Token type '{}' with value should have corresponding enum variant", token.type_id)),
                    val: token.value.clone(),
                    children: vec![],
                }];
            } else if OPERATOR_TOKENS.contains(&token.type_id.as_str()) {
                return vec![AstNode {
                    ty: AstNodeType::Unknown,
                    val: Some(token.type_id.clone()),
                    children: vec![],
                }];
            } else {
                return vec![];
            }
        }
    }
}

fn convert_to_ast(cst: &Cst) -> Result<Ast, ParseError> {
    let flat_cst = flatten_cst(&cst);

    let mut child_map: HashMap<TreePath, Vec<AstNode>> = HashMap::new();

    for (node, path) in &flat_cst {
        let processed_children = child_map.remove(path).unwrap_or(vec![]);

        let mut new_nodes = process_cst_node(node, processed_children);

        if path.len() == 0 {
            return Ok(Ast { root: new_nodes[0].clone() });
        }

        let parent_path = path[0..(path.len() - 1)].into_iter().cloned().collect();
        child_map.entry(parent_path).or_insert(vec![]).append(&mut new_nodes);
    }

    panic!("Flat tree did not contain root node!");
}

pub fn generate_ast(tokens: TokenList) -> Result<Ast, ParseError> {
    let cst = generate_cst(tokens)?;
    let ast = convert_to_ast(&cst);
    return ast;
}
