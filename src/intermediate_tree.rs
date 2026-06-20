//! # Intermediate tree module
//!
//! This module contains all required types and functions to generate an
//! abstract tree representing an intermediate step between parsing trees and
//! their bytecode representations. Such a tree represents the semantics of a
//! program in an arborescent way while being low enough to easily compile to
//! LuaJIT bytecode.

use crate::{
    builtins::{traits::BuiltinTrait, types::BuiltinType},
    errors::ErrorTemplate,
    sources::SourceSection,
};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    path::PathBuf,
};

pub mod compilation;
pub mod constant_eval;
pub mod typing;

const INDENT_STR: &str = "|  ";
const EMPTY_STR: &str = "[EMPTY]";

/// This type represents an intermediate execution unit. This is a part of
/// code that can be executed such as functions or modules. This type is used
/// to map parsing structures to the [`crate::bytecode::Prototype`] concept.
pub struct ExecutionUnit {
    /// Source location that was used to create this function object.
    pub origin_location: SourceSection,

    /// Name of the execution unit.
    pub name: String,

    /// List of children execution units.
    pub children_units: Vec<ExecutionUnit>,

    /// Variant part, containing specific data.
    pub variant: ExecutionUnitVariant,
}

pub enum ExecutionUnitVariant {
    /// This variant represents an LKQL module. This execution unit perform all
    /// semantics of its elements and return a table associating each symbol of
    /// the module to its value.
    Module {
        /// All symbols declared in this module, those are used to fill the
        /// result table and compute the frame size.
        symbols: Vec<Identifier>,

        /// All elements of the module (declarations and expressions).
        elements: Vec<Node>,
    },

    /// This variant represents a function callable by the user directly in the
    /// language. This callable value implies some runtime checks and a
    /// specific calling convention. See [`Node::compile_as_value`] for more
    /// information.
    Function {
        /// Function parameters, each one being optionally associated to a
        /// default value.
        params: Vec<(Identifier, Option<Node>)>,

        /// The body of the function, representing its semantics.
        body: Node,
    },

    /// This variant is like [`ExecutionUnitVariant::Function`] but without any
    /// additional semantics than getting called and returning a result. It
    /// doesn't perform any runtime checks.
    RawCallable {
        /// Parameters of the execution unit, there is no possibility to
        /// provide default value for them.
        params: Vec<Identifier>,

        /// The body of the callable, result of its execution.
        body: Node,
    },
}

impl Display for ExecutionUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pretty_print(0))
    }
}

impl ExecutionUnit {
    /// Create a new execution unit object.
    pub fn new(
        origin_location: SourceSection,
        name: String,
        children_units: Vec<ExecutionUnit>,
        variant: ExecutionUnitVariant,
    ) -> Self {
        Self { origin_location, name, children_units, variant }
    }

    // --- Pretty printing

    fn pretty_print(&self, indent_level: usize) -> String {
        // Get images of the specific children
        let child_level = indent_level + 1;
        let (name, mut pretty_children) = match &self.variant {
            ExecutionUnitVariant::Module { symbols, elements } => (
                format!("Module \"{}\"", self.name),
                vec![
                    (
                        "symbols",
                        format!("{:?}", symbols.iter().map(|i| &i.text).collect::<Vec<_>>()),
                    ),
                    ("elements", Node::pretty_print_vec(elements, child_level)),
                ],
            ),
            ExecutionUnitVariant::Function { params, body } => (
                format!("Function \"{}\"", self.name),
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
                                .collect::<Vec<_>>(),
                            child_level,
                        ),
                    ),
                    ("body", body.pretty_print(child_level)),
                ],
            ),
            ExecutionUnitVariant::RawCallable { params, body } => (
                format!("RawCallable \"{}\"", self.name),
                vec![
                    (
                        "params",
                        format!("{:?}", params.iter().map(|i| &i.text).collect::<Vec<_>>()),
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
                    .map(|(i, c)| (i.to_string(), c.pretty_print(child_level)))
                    .collect::<Vec<_>>(),
                indent_level,
            ),
        )]);

        // Finally pretty print all elements of the execution unit
        pretty_print_node_helper(&(&name, &pretty_children), indent_level)
    }
}

/// This structure represents a node of the intermediate tree. The structure of
/// the tree is made to easily represents the program semantics while being
/// convenient to analyze and compile to LuaJIT bytecode.
#[derive(Debug, Clone)]
pub struct Node {
    pub origin_location: SourceSection,
    pub variant: NodeVariant,
}

/// This enumeration represents the variant part of an [`Node`].
#[derive(Debug, Clone)]
pub enum NodeVariant {
    // --- Call expressions
    FunCall {
        callee: Box<Node>,
        positional_args: Vec<Node>,
        named_args: Vec<(Identifier, Node)>,
    },

    // --- Composite expressions
    DottedExpr {
        prefix: Box<Node>,
        suffix: Identifier,
    },
    LengthExpr(Box<Node>),
    IndexExpr {
        indexed_val: Box<Node>,
        index: Box<Node>,
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
        body: Vec<Node>,
        val: Box<Node>,
    },

    // --- List comprehension
    LazyComprehension {
        source_iterables: Vec<Node>,
        body_index: u16,
    },

    // --- Binary operations
    ArithBinOp {
        left: Box<Node>,
        operator: ArithOperator,
        right: Box<Node>,
    },
    LogicBinOp {
        left: Box<Node>,
        operator: LogicOperator,
        right: Box<Node>,
    },
    CompBinOp {
        left: Box<Node>,
        operator: CompOperator,
        right: Box<Node>,
    },
    MiscBinOp {
        left: Box<Node>,
        operator: MiscOperator,
        right: Box<Node>,
    },

    // --- Unary operations
    ArithUnOp {
        operator: ArithOperator,
        operand: Box<Node>,
    },
    LogicUnOp {
        operator: LogicOperator,
        operand: Box<Node>,
    },

    // --- Lexical scope introduction
    InLexicalScope {
        local_symbols: Vec<Identifier>,
        expr: Box<Node>,
    },

    // --- Symbol introduction
    InitLocal {
        symbol: Identifier,
        val: Box<Node>,
    },
    InitLocalFun(u16),
    ImportModule {
        name: Identifier,
        file: PathBuf,
    },

    // --- Symbol access
    ReadSymbol(Identifier),
    ReadChildUnit(u16),

    // --- Let expression
    /// This node should be used to introduce an identified value in the tree.
    /// The provided value is going to be evaluated once an may be referenced
    /// elsewhere in the sub-tree with the [`Self::Read`] node variant.
    Let {
        id: usize,
        value: Box<Node>,
        r#in: Box<Node>,
    },
    /// Read the value identified by the provided [`usize`].
    Read(usize),

    // --- Type checkers
    InstanceOf {
        expression: Box<Node>,
        expected_type_tag: i32,
    },
    RequireType {
        expression: Box<Node>,
        expected_type: &'static BuiltinType,
    },
    RequireTrait {
        expression: Box<Node>,
        required_trait: &'static BuiltinTrait,
    },

    // --- Error emission
    RuntimeError {
        error_template: &'static ErrorTemplate,
        message_args: Vec<Node>,
    },

    // --- Literals
    NilLiteral,
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
    /// Create a new intermediate node object.
    pub fn new(origin_location: SourceSection, variant: NodeVariant) -> Self {
        Self { origin_location, variant }
    }

    /// Create a new node, with the same location as this one, with the
    /// provided variant.
    pub fn related_node(&self, variant: NodeVariant) -> Self {
        Self::new(self.origin_location, variant)
    }

    // --- Pretty printing

    /// Get a pretty tree representation of this node.
    fn pretty_print(&self, indent_level: usize) -> String {
        let child_level = indent_level + 1;
        // Match the node variant to extract information to display
        let pretty_node: (&str, &[(&str, String)]) = match &self.variant {
            NodeVariant::FunCall { callee, positional_args, named_args } => (
                "FunCall",
                &[
                    ("callee", callee.pretty_print(child_level)),
                    ("positional_args", Self::pretty_print_vec(positional_args, child_level)),
                    ("named_args", Self::pretty_print_labeled_vec(named_args, child_level)),
                ],
            ),
            NodeVariant::DottedExpr { prefix, suffix } => (
                "DottedExpr",
                &[
                    ("prefix", prefix.pretty_print(child_level)),
                    ("suffix", format!("\"{}\"", suffix.text)),
                ],
            ),
            NodeVariant::LengthExpr(value) => {
                ("LengthExpr", &[("value", value.pretty_print(child_level))])
            }
            NodeVariant::IndexExpr { indexed_val, index } => (
                "IndexExpr",
                &[
                    ("indexed_val", indexed_val.pretty_print(child_level)),
                    ("index", index.pretty_print(child_level)),
                ],
            ),
            NodeVariant::InClause { value, collection } => (
                "InClause",
                &[
                    ("value", value.pretty_print(child_level)),
                    ("collection", collection.pretty_print(child_level)),
                ],
            ),
            NodeVariant::IfExpr { condition, consequence, alternative } => (
                "IfExpr",
                &[
                    ("condition", condition.pretty_print(child_level)),
                    ("consequence", consequence.pretty_print(child_level)),
                    ("alternative", alternative.pretty_print(child_level)),
                ],
            ),
            NodeVariant::BlockExpr { body, val } => (
                "BlockExpr",
                &[
                    ("body", Self::pretty_print_vec(body, child_level)),
                    ("val", val.pretty_print(child_level)),
                ],
            ),
            NodeVariant::LazyComprehension { source_iterables, body_index } => (
                "LazyComprehension",
                &[
                    ("source_iterables", Self::pretty_print_vec(source_iterables, child_level)),
                    ("body_index", body_index.to_string()),
                ],
            ),
            NodeVariant::ArithBinOp { left, operator, right } => (
                "ArithBinOp",
                &[
                    ("left", left.pretty_print(child_level)),
                    ("operator", operator.to_string()),
                    ("right", right.pretty_print(child_level)),
                ],
            ),
            NodeVariant::LogicBinOp { left, operator, right } => (
                "LogicBinOp",
                &[
                    ("left", left.pretty_print(child_level)),
                    ("operator", operator.to_string()),
                    ("right", right.pretty_print(child_level)),
                ],
            ),
            NodeVariant::CompBinOp { left, operator, right } => (
                "CompBinOp",
                &[
                    ("left", left.pretty_print(child_level)),
                    ("operator", operator.to_string()),
                    ("right", right.pretty_print(child_level)),
                ],
            ),
            NodeVariant::MiscBinOp { left, operator, right } => (
                "MiscBinOp",
                &[
                    ("left", left.pretty_print(child_level)),
                    ("operator", operator.to_string()),
                    ("right", right.pretty_print(child_level)),
                ],
            ),
            NodeVariant::ArithUnOp { operator, operand } => (
                "ArithUnOp",
                &[
                    ("operator", operator.to_string()),
                    ("operand", operand.pretty_print(child_level)),
                ],
            ),
            NodeVariant::LogicUnOp { operator, operand } => (
                "LogicUnOp",
                &[
                    ("operator", operator.to_string()),
                    ("operand", operand.pretty_print(child_level)),
                ],
            ),
            NodeVariant::InLexicalScope { local_symbols, expr } => (
                "InLexicalScope",
                &[
                    (
                        "symbols",
                        format!("{:?}", local_symbols.iter().map(|i| &i.text).collect::<Vec<_>>()),
                    ),
                    ("expr", expr.pretty_print(child_level)),
                ],
            ),
            NodeVariant::InitLocal { symbol, val } => (
                "InitLocal",
                &[
                    ("symbol", format!("\"{}\"", symbol.text)),
                    ("val", val.pretty_print(child_level)),
                ],
            ),
            NodeVariant::InitLocalFun(child_index) => {
                ("InitLocalFun", &[("child_index", child_index.to_string())])
            }
            NodeVariant::ImportModule { name, file } => (
                "ImportModule",
                &[
                    ("name", format!("\"{}\"", name.text)),
                    ("file", format!("\"{}\"", file.as_os_str().to_string_lossy())),
                ],
            ),
            NodeVariant::ReadSymbol(symbol) => {
                ("ReadSymbol", &[("symbol", format!("\"{}\"", symbol.text))])
            }
            NodeVariant::ReadChildUnit(child_index) => {
                ("ReadChildUnit", &[("child_index", child_index.to_string())])
            }
            NodeVariant::Let { id, value, r#in } => (
                "Let",
                &[
                    ("id", format!("\"{id}\"")),
                    ("value", value.pretty_print(child_level)),
                    ("in", r#in.pretty_print(child_level)),
                ],
            ),
            NodeVariant::Read(id) => ("Read", &[("id", format!("\"{id}\""))]),
            NodeVariant::InstanceOf { expression, expected_type_tag } => (
                "InstanceOf",
                &[
                    ("expression", expression.pretty_print(child_level)),
                    ("expected_type", expected_type_tag.to_string()),
                ],
            ),
            NodeVariant::RequireType { expression, expected_type } => (
                "RequireType",
                &[
                    ("expression", expression.pretty_print(child_level)),
                    ("expected_type", expected_type.display_name().to_string()),
                ],
            ),
            NodeVariant::RequireTrait { expression, required_trait } => (
                "RequireTrait",
                &[
                    ("expression", expression.pretty_print(child_level)),
                    ("required_trait", required_trait.name.to_string()),
                ],
            ),
            NodeVariant::RuntimeError { error_template, message_args } => (
                "RuntimeError",
                &[
                    ("error_template", format!("\"{}\"", error_template.title)),
                    ("message_args", format!("{:?}", message_args)),
                ],
            ),
            NodeVariant::NilLiteral => ("NilLiteral", &[]),
            NodeVariant::NullLiteral => ("NullLiteral", &[]),
            NodeVariant::UnitLiteral => ("UnitLiteral", &[]),
            NodeVariant::BoolLiteral(value) => ("BoolLiteral", &[("value", value.to_string())]),
            NodeVariant::IntLiteral(value) => ("IntLiteral", &[("value", format!("\"{value}\""))]),
            NodeVariant::StringLiteral(value) => {
                ("StringLiteral", &[("value", format!("{:?}", value))])
            }
            NodeVariant::TupleLiteral(items) => {
                ("TupleLiteral", &[("items", Self::pretty_print_vec(items, child_level))])
            }
            NodeVariant::ListLiteral(items) => {
                ("ListLiteral", &[("items", Self::pretty_print_vec(items, child_level))])
            }
            NodeVariant::ObjectLiteral(items) => (
                "ObjectLiteral",
                &[("items", Self::pretty_print_labeled_vec(items, child_level))],
            ),
        };

        // Format the gathered information
        pretty_print_node_helper(&pretty_node, indent_level)
    }

    /// Internal util function used to ease the pretty print of a vector of
    /// nodes.
    fn pretty_print_vec(nodes: &[Node], indent_level: usize) -> String {
        pretty_print_vec_helper(
            &nodes
                .iter()
                .enumerate()
                .map(|(i, n)| (i.to_string(), n.pretty_print(indent_level + 1)))
                .collect::<Vec<_>>(),
            indent_level,
        )
    }

    /// Internal util function used to ease the pretty print of a vector of
    /// labeled nodes.
    fn pretty_print_labeled_vec(nodes: &[(Identifier, Node)], indent_level: usize) -> String {
        pretty_print_vec_helper(
            &nodes
                .iter()
                .map(|(id, n)| {
                    (format!("\"{}\"", id.text.clone()), n.pretty_print(indent_level + 1))
                })
                .collect::<Vec<_>>(),
            indent_level,
        )
    }
}

/// This type represents an arithmetic operator in the intermediate tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArithOperator {
    pub origin_location: SourceSection,
    pub variant: ArithOperatorVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOperatorVariant {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl Display for ArithOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

impl ArithOperator {
    /// Create a new arithmetic operator object.
    pub fn new(origin_location: SourceSection, variant: ArithOperatorVariant) -> Self {
        Self { origin_location, variant }
    }
}

/// This type represents a logic operator in the intermediate tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LogicOperator {
    pub origin_location: SourceSection,
    pub variant: LogicOperatorVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicOperatorVariant {
    Or,
    And,
    Not,
}

impl Display for LogicOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

impl LogicOperator {
    /// Create a new logic operator object.
    pub fn new(origin_location: SourceSection, variant: LogicOperatorVariant) -> Self {
        Self { origin_location, variant }
    }
}

/// This type represents a comparison operator in the intermediate tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompOperator {
    pub origin_location: SourceSection,
    pub variant: CompOperatorVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompOperatorVariant {
    Equals,
    NotEquals,
    Greater,
    GreaterOrEquals,
    Less,
    LessOrEquals,
}

impl Display for CompOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

impl CompOperator {
    /// Create a new comparison operator object.
    pub fn new(origin_location: SourceSection, variant: CompOperatorVariant) -> Self {
        Self { origin_location, variant }
    }
}

/// This type represents all miscellaneous operator in the intermediate tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MiscOperator {
    pub origin_location: SourceSection,
    pub variant: MiscOperatorVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiscOperatorVariant {
    Concat,
}

impl Display for MiscOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.variant.fmt(f)
    }
}

impl MiscOperator {
    /// Create a new miscellaneous operator object.
    pub fn new(origin_location: SourceSection, variant: MiscOperatorVariant) -> Self {
        Self { origin_location, variant }
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

impl Identifier {
    /// Create a new identifier object.
    pub fn new(origin_location: SourceSection, text: String) -> Self {
        Self { origin_location, text }
    }
}

/// Helper function to get the image of a node in a pretty-tree representation.
fn pretty_print_node_helper(
    pretty_node: &(&str, &[(&str, String)]),
    indent_level: usize,
) -> String {
    let indent = INDENT_STR.repeat(indent_level);
    let (node_name, node_children) = pretty_node;
    let mut res = format!("{indent}<{node_name}>");
    if !node_children.is_empty() {
        let children_string = node_children
            .iter()
            .map(|(name, image)| {
                let trimmed_image = image.trim_start_matches(INDENT_STR);
                let final_image = if image.contains('\n') || trimmed_image.starts_with('|') {
                    format!("\n{image}")
                } else {
                    format!(" {trimmed_image}")
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
fn pretty_print_vec_helper(labeled_values: &[(String, String)], indent_level: usize) -> String {
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
