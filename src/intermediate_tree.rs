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
    hash::Hash,
    rc::{Rc, Weak},
};

use crate::sources::SourceSection;

/// This structure represents a program in the intermediate format. This should
/// be the result of lowering a top-level node.
#[derive(Debug)]
pub struct Program {
    pub main_function: Rc<RefCell<Function>>,
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

/// This structure represents a textual information in the intermediate tree,
/// with additional data.
#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    pub origin_location: SourceSection,
    pub text: String,
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
