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
    cell::RefCell,
    fmt::{Debug, Display},
    hash::Hash,
    rc::{Rc, Weak},
};

use crate::sources::SourceSection;

pub mod constant_eval;
pub mod lkql_lowering;

const INDENT_STR: &str = "|  ";
const EMPTY_STR: &str = "[EMPTY]";

/// This type represents an intermediate execution unit. This is a part of
/// code that can be executed such as functions or modules. This type is used
/// to map parsing structures to the [`crate::bytecode::Prototype`] concept.
pub struct ExecutionUnit {
    /// Source location that was used to create this function object.
    origin_location: SourceSection,

    /// Execution unit owning (parenting) this one.
    parent_unit: Option<Weak<RefCell<ExecutionUnit>>>,

    /// List of children execution units.
    children_units: Vec<Rc<RefCell<ExecutionUnit>>>,

    /// Variant part, containing specific data.
    variant: ExecutionUnitVariant,
}

pub enum ExecutionUnitVariant {
    Module {
        /// The name of the module.
        name: String,

        /// All symbols declared in this module, those are used to fill the
        /// result table and compute the frame size.
        symbols: Vec<Identifier>,

        /// All elements of the module (declarations and expressions).
        elements: Vec<Node>,
    },
    Function {
        /// Function parameters, each one being optionally associated to a
        /// default value.
        params: Vec<(Identifier, Option<Node>)>,

        /// The body of the function, representing its semantics.
        body: Node,
    },
}

impl Display for ExecutionUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print(0))
    }
}

impl ExecutionUnit {
    // --- Pretty printing

    fn pretty_print(&self, indent_level: usize) -> String {
        // Start by getting images of the specific children
        let child_level = indent_level + 1;
        let (name, mut pretty_children) = match &self.variant {
            ExecutionUnitVariant::Module { name, symbols, elements } => (
                "Module",
                vec![
                    ("name", format!("\"{name}\"")),
                    (
                        "symbols",
                        format!("{:?}", symbols.iter().map(|i| &i.text).collect::<Vec<_>>()),
                    ),
                    ("elements", Node::pretty_print_vec(&elements, child_level)),
                ],
            ),
            ExecutionUnitVariant::Function { params, body } => (
                "Function",
                vec![
                    (
                        "params",
                        pretty_print_vec_helper(
                            &params
                                .iter()
                                .map(|(id, v)| {
                                    (
                                        format!("\"{}\"", id.text),
                                        v.as_ref().map_or(String::from("None"), |n| {
                                            n.pretty_print(child_level + 1)
                                        }),
                                    )
                                })
                                .collect(),
                            child_level,
                        ),
                    ),
                    ("body", body.pretty_print(child_level)),
                ],
            ),
        };

        // Then append image of generic children
        pretty_children.append(&mut vec![(
            "children_units",
            pretty_print_vec_helper(
                &self
                    .children_units
                    .iter()
                    .enumerate()
                    .map(|(i, c)| (i.to_string(), c.borrow().pretty_print(child_level)))
                    .collect(),
                indent_level,
            ),
        )]);

        // Finally pretty print all elements of the execution unit
        pretty_print_node_helper(&(name, pretty_children), indent_level)
    }
}

/// This structure represents a node of the intermediate tree. The structure of
/// the tree is made to easily represents the program semantics while being
/// convenient to analyze and compile to LuaJIT bytecode.
#[derive(Debug, PartialEq)]
pub struct Node {
    pub origin_location: SourceSection,
    pub variant: NodeVariant,
}

/// This enumeration represents the variant part of an [`Node`].
#[derive(Debug, PartialEq)]
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
    InClause {
        value: Box<Node>,
        collection: Box<Node>,
    },
    IfExpr {
        condition: Box<Node>,
        consequence: Box<Node>,
        alternative: Box<Node>,
    },
    BlockExpr {
        local_symbols: Vec<Identifier>,
        body: Vec<Node>,
        val: Box<Node>,
    },

    // --- Lazy sequence creation
    LazySeqExpr {
        source_seq: Box<Node>,
        next_fun: Box<Node>,
    },

    // --- Binary operations
    ArithBinOp {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
    LogicBinOp {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
    CompBinOp {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
    MiscBinOp {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },

    // --- Unary operations
    ArithUnOp {
        operator: Operator,
        operand: Box<Node>,
    },
    LogicUnOp {
        operator: Operator,
        operand: Box<Node>,
    },

    // --- Symbol accesses
    /// Standard local symbol initialization
    InitLocal {
        symbol: Identifier,
        val: Box<Node>,
    },
    /// Recursive local symbol initialization, meaning that the symbol is
    /// accessible from the value initializing it (used for function-like
    /// constructs).
    InitRecLocal {
        symbol: Identifier,
        val: Box<Node>,
    },
    ReadSymbol(Identifier),

    // --- Children function access
    ChildFunRef(u16),

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
    // --- Pretty printing

    /// Get a pretty tree representation of this node.
    fn pretty_print(&self, indent_level: usize) -> String {
        let child_level = indent_level + 1;
        // Match the node variant to extract information to display
        let pretty_node: (&str, Vec<(&str, String)>) = match &self.variant {
            NodeVariant::FunCall { callee, positional_args, named_args } => (
                "FunCall",
                vec![
                    ("callee", callee.pretty_print(child_level)),
                    ("positional_args", Self::pretty_print_vec(positional_args, child_level)),
                    ("named_args", Self::pretty_print_labeled_vec(named_args, child_level)),
                ],
            ),
            NodeVariant::DottedExpr { prefix, suffix, is_safe } => (
                "DottedExpr",
                vec![
                    ("is_safe", is_safe.to_string()),
                    ("prefix", prefix.pretty_print(child_level)),
                    ("suffix", format!("\"{}\"", suffix.text)),
                ],
            ),
            NodeVariant::IndexExpr { indexed_val, index, is_safe } => (
                "IndexExpr",
                vec![
                    ("is_safe", is_safe.to_string()),
                    ("indexed_val", indexed_val.pretty_print(child_level)),
                    ("index", index.pretty_print(child_level)),
                ],
            ),
            NodeVariant::InClause { value, collection } => (
                "InClause",
                vec![
                    ("value", value.pretty_print(child_level)),
                    ("collection", collection.pretty_print(child_level)),
                ],
            ),
            NodeVariant::IfExpr { condition, consequence, alternative } => (
                "IfExpr",
                vec![
                    ("condition", condition.pretty_print(child_level)),
                    ("consequence", consequence.pretty_print(child_level)),
                    ("alternative", alternative.pretty_print(child_level)),
                ],
            ),
            NodeVariant::BlockExpr { local_symbols, body, val } => (
                "BlockExpr",
                vec![
                    (
                        "locals",
                        format!("{:?}", local_symbols.iter().map(|i| &i.text).collect::<Vec<_>>()),
                    ),
                    ("body", Self::pretty_print_vec(body, child_level)),
                    ("val", val.pretty_print(child_level)),
                ],
            ),
            NodeVariant::LazySeqExpr { source_seq, next_fun } => (
                "LazySeqExpr",
                vec![
                    ("source_seq", source_seq.pretty_print(child_level)),
                    ("next_fun", next_fun.pretty_print(child_level)),
                ],
            ),
            NodeVariant::ArithBinOp { left, operator, right }
            | NodeVariant::LogicBinOp { left, operator, right }
            | NodeVariant::CompBinOp { left, operator, right }
            | NodeVariant::MiscBinOp { left, operator, right } => (
                match &self.variant {
                    NodeVariant::ArithBinOp { .. } => "ArithBinOp",
                    NodeVariant::LogicBinOp { .. } => "LogicBinOp",
                    NodeVariant::CompBinOp { .. } => "CompBinOp",
                    NodeVariant::MiscBinOp { .. } => "MiscBinOp",
                    _ => unreachable!(),
                },
                vec![
                    ("left", left.pretty_print(child_level)),
                    ("operator", operator.to_string()),
                    ("right", right.pretty_print(child_level)),
                ],
            ),
            NodeVariant::ArithUnOp { operator, operand }
            | NodeVariant::LogicUnOp { operator, operand } => (
                match &self.variant {
                    NodeVariant::ArithUnOp { .. } => "ArithUnOp",
                    NodeVariant::LogicUnOp { .. } => "LogicUnOp",
                    _ => unreachable!(),
                },
                vec![
                    ("operator", operator.to_string()),
                    ("operand", operand.pretty_print(child_level)),
                ],
            ),
            NodeVariant::InitLocal { symbol, val } | NodeVariant::InitRecLocal { symbol, val } => (
                match &self.variant {
                    NodeVariant::InitLocal { .. } => "InitLocal",
                    NodeVariant::InitRecLocal { .. } => "InitRecLocal",
                    _ => unreachable!(),
                },
                vec![
                    ("symbol", format!("\"{}\"", symbol.text)),
                    ("val", val.pretty_print(child_level)),
                ],
            ),
            NodeVariant::ReadSymbol(symbol) => {
                ("ReadSymbol", vec![("symbol", format!("\"{}\"", symbol.text))])
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
                ("TupleLiteral", vec![("items", Self::pretty_print_vec(items, child_level))])
            }
            NodeVariant::ListLiteral(items) => {
                ("ListLiteral", vec![("items", Self::pretty_print_vec(items, child_level))])
            }
            NodeVariant::ObjectLiteral(items) => (
                "ObjectLiteral",
                vec![("items", Self::pretty_print_labeled_vec(items, child_level))],
            ),
        };

        // Format the gathered information
        pretty_print_node_helper(&pretty_node, indent_level)
    }

    /// Internal util function used to ease the pretty print of a vector of
    /// nodes.
    fn pretty_print_vec(nodes: &Vec<Node>, indent_level: usize) -> String {
        pretty_print_vec_helper(
            &nodes
                .iter()
                .enumerate()
                .map(|(i, n)| (i.to_string(), n.pretty_print(indent_level + 1)))
                .collect(),
            indent_level,
        )
    }

    /// Internal util function used to ease the pretty print of a vector of
    /// labeled nodes.
    fn pretty_print_labeled_vec(nodes: &Vec<(Identifier, Node)>, indent_level: usize) -> String {
        pretty_print_vec_helper(
            &nodes
                .iter()
                .map(|(id, n)| {
                    (format!("\"{}\"", id.text.clone()), n.pretty_print(indent_level + 1))
                })
                .collect(),
            indent_level,
        )
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

/// Helper function to get the image of a node in a pretty-tree representation.
fn pretty_print_node_helper(
    pretty_node: &(&str, Vec<(&str, String)>),
    indent_level: usize,
) -> String {
    let indent = INDENT_STR.repeat(indent_level);
    let (node_name, node_children) = pretty_node;
    let mut res = format!("{indent}<{node_name}>");
    if !node_children.is_empty() {
        let children_string = node_children
            .iter()
            .map(|(name, image)| {
                let final_image = if image.contains('\n') {
                    format!("\n{image}")
                } else {
                    format!(" {}", image.trim_start_matches(INDENT_STR))
                };
                format!("{indent}|{name}:{final_image}")
            })
            .collect::<Vec<_>>()
            .join("\n");
        res.push_str(":\n");
        res.push_str(&children_string);
    }
    res
}

/// Internal util function factorizing the node vector printing process.
fn pretty_print_vec_helper(labeled_values: &Vec<(String, String)>, indent_level: usize) -> String {
    let indent = INDENT_STR.repeat(indent_level);
    if labeled_values.is_empty() {
        String::from(EMPTY_STR)
    } else {
        labeled_values
            .iter()
            .map(|(label, value)| {
                let final_image = if value.contains('\n') {
                    format!("\n{value}")
                } else {
                    format!(" {}", value.trim_start_matches(INDENT_STR))
                };
                format!("{indent}|[{label}]:{final_image}")
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
