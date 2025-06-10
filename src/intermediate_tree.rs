//! # Intermediate tree module
//!
//! This module contains all required types and functions to generate an
//! abstract tree representing an intermediate step between parsing trees and
//! their bytecode representations. Such a tree represents the semantics of a
//! program in an arborescent way while being low enough to easily compile to
//! LuaJIT bytecode.
//!
//! There a several transformations done while compiling to an intermediate
//! tree:
//!   * Declarations and initializations are split
//!   * Variable reading kinds are precised (param access, local access, ...)
//!   * Function expressions are removed, all lambdas are node forward
//!     declared.

use std::{
    cell::{OnceCell, RefCell},
    collections::HashSet,
    fmt::{Debug, Display},
    hash::Hash,
    rc::{Rc, Weak},
};

use crate::sources::SourceSection;

pub mod lkql_lowering;

const INDENT_STR: &str = "|  ";
const EMPTY_STR: &str = "[EMPTY]";

/// This structure represents a program in the intermediate format. This should
/// be the result of lowering a top-level node.
#[derive(Debug)]
pub struct Program {
    pub main_function: Rc<RefCell<Function>>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.main_function
                .borrow()
                .pretty_print(0, Some("MainFunction"))
        )
    }
}

/// This structure represents a function in the intermediate format. This
/// structure contains all its children function in order to map easily to the
/// LuaJIT bytecode prototypes standard.
#[derive(Debug)]
pub struct Function {
    /// Source location that was used to create this function object.
    pub origin_location: SourceSection,

    /// Function object parenting this one.
    pub parent_function: OnceCell<Weak<RefCell<Function>>>,

    /// List of children function.
    pub children_function: Vec<Rc<RefCell<Function>>>,

    /// A set of symbols ([`String`]) that are locally reachable in the
    /// function.
    pub local_symbols: HashSet<String>,

    /// Function parameters, each one being optionally associated to a default
    /// value.
    pub params: Vec<(Identifier, Option<Node>)>,

    /// The body of the function, representing its semantics.
    pub body: Vec<Node>,
}

impl Function {
    fn pretty_print(&self, indent_level: usize, fun_name: Option<&str>) -> String {
        // Create the current level indentation string and the initial result
        let indent = INDENT_STR.repeat(indent_level);
        let indent_further = INDENT_STR.repeat(indent_level + 1);
        let mut res =
            format!("{indent}<{} {}>", fun_name.unwrap_or("Function"), self.origin_location);

        // Add parameters in the pretty print image
        res.push_str(&format!("\n{indent}|params:"));
        if self.params.is_empty() {
            res.push_str(&format!("\n{indent_further}{EMPTY_STR}"));
        } else {
            for (name, default) in &self.params {
                res.push_str(&format!(
                    "\n{indent_further}{name}:{}",
                    default
                        .as_ref()
                        .map_or(String::from(" <no_default>"), |d| format!(
                            "\n{}",
                            d.pretty_print(indent_level + 2)
                        ))
                ));
            }
        }

        // Add the function body to the result
        res.push_str(&format!("\n{indent}|body:"));
        if self.body.is_empty() {
            res.push_str(&format!("\n{indent_further}{EMPTY_STR}"));
        } else {
            for node in &self.body {
                res.push('\n');
                res.push_str(&node.pretty_print(indent_level + 1));
            }
        }

        // Then, add the children functions
        res.push_str(&format!("\n{indent}|children:"));
        if self.children_function.is_empty() {
            res.push_str(&format!("\n{indent_further}{EMPTY_STR}"));
        } else {
            for child in &self.children_function {
                res.push('\n');
                res.push_str(&child.borrow().pretty_print(indent_level + 1, None));
            }
        }

        // Finally return the result
        res
    }
}

/// This structure represents a node of the intermediate tree. The structure of
/// the tree is made to easily represents the program semantics while being
/// convenient to analyze and compile to LuaJIT bytecode.
#[derive(Debug)]
pub struct Node {
    pub origin_location: SourceSection,
    pub variant: NodeVariant,
}

/// This enumeration represents the variant part of an [`Node`].
#[derive(Debug)]
pub enum NodeVariant {
    // --- Function call
    FunCall {
        callee: Box<Node>,
        positional_args: Vec<Node>,
        named_args: Vec<(Identifier, Node)>,
    },

    // --- Composite expressions
    DottedExpr {
        prefix: Box<Node>,
        suffix: Identifier,
        is_safe: bool,
    },
    IndexExpr {
        indexed_val: Box<Node>,
        index: Box<Node>,
        is_safe: bool,
    },
    IfExpr {
        condition: Box<Node>,
        consequence: Box<Node>,
        alternative: Option<Box<Node>>,
    },
    BlockExpr {
        local_symbols: HashSet<String>,
        body: Vec<Node>,
        val: Box<Node>,
    },

    // --- Lazy sequence creation
    LazySeqExpr {
        source_seq: Box<Node>,
        next_fun: Box<Node>,
    },

    // --- Operations
    BinOp {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
    UnOp {
        operator: Operator,
        operand: Box<Node>,
    },

    // --- Symbol accesses
    InitLocal {
        symbol: Identifier,
        val: Box<Node>,
    },
    ReadLocal(Identifier),

    // --- Children function access
    ChildFunRef(usize),

    // --- Literals
    NullLiteral,
    UnitLiteral,
    BoolLiteral(bool),
    IntLiteral(String),
    StringLiteral(String),
    TupleLiteral(Vec<Node>),
    ListLiteral(Vec<Node>),
    ObjectLiteral(Vec<(Identifier, Node)>),
}

impl Node {
    fn pretty_print(&self, indent_level: usize) -> String {
        let child_lvl = indent_level + 1;
        // Match the node variant to extract information to display
        let (name, children): (&str, Vec<(&str, String)>) = match &self.variant {
            NodeVariant::FunCall { callee, positional_args, named_args } => (
                "FunCall",
                vec![
                    ("callee", callee.pretty_print(child_lvl)),
                    ("positional_args", Self::pretty_print_vec(positional_args, child_lvl)),
                    ("named_args", Self::pretty_print_labeled_vec(named_args, child_lvl)),
                ],
            ),
            NodeVariant::DottedExpr { prefix, suffix, is_safe } => (
                "DottedExpr",
                vec![
                    ("is_safe", is_safe.to_string()),
                    ("prefix", prefix.pretty_print(child_lvl)),
                    ("suffix", format!("\"{}\"", suffix.text)),
                ],
            ),
            NodeVariant::IndexExpr { indexed_val, index, is_safe } => (
                "IndexExpr",
                vec![
                    ("is_safe", is_safe.to_string()),
                    ("indexed_val", indexed_val.pretty_print(child_lvl)),
                    ("index", index.pretty_print(child_lvl)),
                ],
            ),
            NodeVariant::IfExpr { condition, consequence, alternative } => (
                "IfExpr",
                vec![
                    ("condition", condition.pretty_print(child_lvl)),
                    ("consequence", consequence.pretty_print(child_lvl)),
                    ("alternative", Self::pretty_print_option(alternative, child_lvl)),
                ],
            ),
            NodeVariant::BlockExpr { local_symbols, body, val } => (
                "BlockExpr",
                vec![
                    ("body", Self::pretty_print_vec(body, child_lvl)),
                    ("val", val.pretty_print(child_lvl)),
                ],
            ),
            NodeVariant::LazySeqExpr { source_seq, next_fun } => (
                "LazySeqExpr",
                vec![
                    ("source_seq", source_seq.pretty_print(child_lvl)),
                    ("next_fun", next_fun.pretty_print(child_lvl)),
                ],
            ),
            NodeVariant::BinOp { left, operator, right } => (
                "BinOp",
                vec![
                    ("left", left.pretty_print(child_lvl)),
                    ("operator", operator.to_string()),
                    ("right", right.pretty_print(child_lvl)),
                ],
            ),
            NodeVariant::UnOp { operator, operand } => (
                "BinOp",
                vec![
                    ("operator", operator.to_string()),
                    ("operand", operand.pretty_print(child_lvl)),
                ],
            ),
            NodeVariant::InitLocal { symbol, val } => (
                "InitLocal",
                vec![
                    ("symbol", format!("\"{}\"", symbol.text)),
                    ("val", val.pretty_print(child_lvl)),
                ],
            ),
            NodeVariant::ReadLocal(symbol) => {
                ("ReadLocal", vec![("symbol", format!("\"{}\"", symbol.text))])
            }
            NodeVariant::ChildFunRef(index) => ("ChildFunRef", vec![("index", index.to_string())]),
            NodeVariant::NullLiteral => ("NullLiteral", vec![]),
            NodeVariant::UnitLiteral => ("UnitLiteral", vec![]),
            NodeVariant::BoolLiteral(value) => ("BoolLiteral", vec![("value", value.to_string())]),
            NodeVariant::IntLiteral(value) => {
                ("IntLiteral", vec![("value", format!("\"{value}\""))])
            }
            NodeVariant::StringLiteral(value) => {
                ("StringLiteral", vec![("value", format!("{:?}", value))])
            }
            NodeVariant::TupleLiteral(items) => {
                ("TupleLiteral", vec![("items", Self::pretty_print_vec(items, child_lvl))])
            }
            NodeVariant::ListLiteral(items) => {
                ("ListLiteral", vec![("items", Self::pretty_print_vec(items, child_lvl))])
            }
            NodeVariant::ObjectLiteral(items) => (
                "ObjectLiteral",
                vec![("items", Self::pretty_print_labeled_vec(items, child_lvl))],
            ),
        };

        // Format the gathered information
        let indent = INDENT_STR.repeat(indent_level);
        let mut res = format!("{indent}<{name}>");
        if !children.is_empty() {
            let children_string = children
                .iter()
                .map(|(name, image)| {
                    let final_image = if image.contains('\n') {
                        format!("\n{image}")
                    } else {
                        format!(" {}", image.trim_start_matches(INDENT_STR))
                    };
                    format!("{indent}|{name}:{final_image}")
                })
                .collect::<Vec<String>>()
                .join("\n");
            res.push_str(":\n");
            res.push_str(&children_string);
        }

        // Finally return the result
        res
    }

    /// Util internal function used to easily get the pretty representation of
    /// an optional node.
    fn pretty_print_option(opt_node: &Option<Box<Node>>, indent_level: usize) -> String {
        opt_node
            .as_ref()
            .map_or(String::from("None"), |n| n.pretty_print(indent_level))
    }

    /// Util internal function used to ease the pretty print for a vector of
    /// nodes.
    fn pretty_print_vec(nodes: &Vec<Node>, indent_level: usize) -> String {
        Self::vec_pretty_print_helper(
            &nodes
                .iter()
                .enumerate()
                .map(|(i, n)| (i.to_string(), n))
                .collect(),
            indent_level,
        )
    }

    fn pretty_print_labeled_vec(nodes: &Vec<(Identifier, Node)>, indent_level: usize) -> String {
        Self::vec_pretty_print_helper(
            &nodes
                .iter()
                .map(|(id, n)| (format!("\"{}\"", id.text.clone()), n))
                .collect(),
            indent_level,
        )
    }

    fn vec_pretty_print_helper(
        labeled_nodes: &Vec<(String, &Node)>,
        indent_level: usize,
    ) -> String {
        let indent = INDENT_STR.repeat(indent_level);
        if labeled_nodes.is_empty() {
            String::from(EMPTY_STR)
        } else {
            labeled_nodes
                .iter()
                .map(|(label, node)| {
                    format!("{indent}|[{label}]:\n{}", node.pretty_print(indent_level + 1))
                })
                .collect::<Vec<String>>()
                .join("\n")
        }
    }
}

/// This structure represents an operator in the intermediate tree. It wraps
/// the [`OperatorVariant`] enumeration, adding meta-information on it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Operator {
    pub origin_location: SourceSection,
    pub variant: OperatorVariant,
}

/// This enumeration represents possible operators in the intermediate tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatorVariant {
    Plus,
    Minus,
    Multiply,
    Divide,
    Concat,
    Or,
    And,
    Not,
    In,
    Equals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

/// This structure represents a textual information in the intermediate tree,
/// with additional data.
#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    pub origin_location: SourceSection,
    pub text: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Hash for Identifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.text.hash(state);
    }
}

impl PartialEq for Identifier {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}
