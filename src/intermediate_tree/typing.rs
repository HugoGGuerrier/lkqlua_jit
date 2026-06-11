//! # Intermediate tree typing module
//!
//! This module tries to provide minimal typing support for intermediate tree
//! nodes. It can be used by lowering phase to optimize emitted nodes and avoid
//! useless runtime type checks.

use crate::{
    builtins::types::{BuiltinType, bool, function, int, list, obj, str, stream, tuple, unit},
    intermediate_tree::{Node, NodeVariant},
};

impl Node {
    /// Try to deduct the type of the expression represented by the node.
    /// Returns [`None`] if this isn't possible to determine it.
    pub fn expr_type(&self) -> Option<&BuiltinType> {
        match &self.variant {
            // --- Composite expression
            NodeVariant::InClause { .. } => Some(&bool::TYPE),
            NodeVariant::BlockExpr { val, .. } => val.expr_type(),
            NodeVariant::LazyComprehension { .. } => Some(&stream::TYPE),

            // --- Binary operations
            NodeVariant::LogicBinOp { .. } => Some(&bool::TYPE),
            NodeVariant::ArithBinOp { .. } => Some(&int::TYPE),
            NodeVariant::CompBinOp { .. } => Some(&bool::TYPE),

            // --- Unary operations
            NodeVariant::LogicUnOp { .. } => Some(&bool::TYPE),
            NodeVariant::ArithUnOp { .. } => Some(&int::TYPE),

            // --- Symbol introductions
            NodeVariant::InitLocal { .. } => Some(&unit::TYPE),
            NodeVariant::InitLocalFun { .. } => Some(&unit::TYPE),

            // --- Recursive nodes
            NodeVariant::InLexicalScope { expr, .. } => expr.expr_type(),
            NodeVariant::Let { r#in, .. } => r#in.expr_type(),

            // --- Type checking
            NodeVariant::InstanceOf { .. } => Some(&bool::TYPE),

            // --- Literals
            NodeVariant::UnitLiteral => Some(&unit::TYPE),
            NodeVariant::BoolLiteral(_) => Some(&bool::TYPE),
            NodeVariant::IntLiteral(_) => Some(&int::TYPE),
            NodeVariant::StringLiteral(_) => Some(&str::TYPE),
            NodeVariant::TupleLiteral(_) => Some(&tuple::TYPE),
            NodeVariant::ListLiteral(_) => Some(&list::TYPE),
            NodeVariant::ObjectLiteral(_) => Some(&obj::TYPE),
            NodeVariant::ReadChildUnit(_) => Some(&function::TYPE),

            // --- Default case, no type can be deducted
            _ => None,
        }
    }
}

#[allow(unused_imports)]
mod test {
    use crate::{
        builtins::types::{bool, function, int, list, obj, str, stream, tuple, unit},
        intermediate_tree::{
            ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, Identifier,
            LogicOperator, LogicOperatorVariant, Node, NodeVariant,
        },
        sources::{Location, SourceSection},
    };

    fn _dummy_loc() -> SourceSection {
        SourceSection::new(0, Location::new(0, 0), Location::new(0, 0))
    }

    // --- Node creation helpers

    fn _node(variant: NodeVariant) -> Node {
        Node::new(_dummy_loc(), variant)
    }

    fn _bool_node() -> Box<Node> {
        Box::new(_node(NodeVariant::BoolLiteral(true)))
    }

    fn _int_node() -> Box<Node> {
        Box::new(_node(NodeVariant::IntLiteral(String::from("42"))))
    }

    fn _read_node() -> Box<Node> {
        Box::new(_node(NodeVariant::ReadSymbol(_id())))
    }

    fn _id() -> Identifier {
        Identifier::new(_dummy_loc(), String::from("x"))
    }

    fn _dummy() -> Box<Node> {
        Box::new(_node(NodeVariant::NilLiteral))
    }

    // --- Testing functions

    #[test]
    fn test_typing() {
        // In clause
        let mut intermediate_tree =
            _node(NodeVariant::InClause { value: _dummy(), collection: _dummy() });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));

        // Block expr
        intermediate_tree = _node(NodeVariant::BlockExpr { body: vec![], val: _bool_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::BlockExpr { body: vec![], val: _int_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::BlockExpr { body: vec![], val: _read_node() });
        assert_eq!(intermediate_tree.expr_type(), None);

        // Lazy comprehension
        intermediate_tree =
            _node(NodeVariant::LazyComprehension { source_iterables: vec![], body_index: 0 });
        assert_eq!(intermediate_tree.expr_type(), Some(&stream::TYPE));

        // Binary operations
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _dummy(),
            operator: LogicOperator::new(_dummy_loc(), LogicOperatorVariant::And),
            right: _dummy(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _dummy(),
            operator: ArithOperator::new(_dummy_loc(), ArithOperatorVariant::Plus),
            right: _dummy(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _dummy(),
            operator: CompOperator::new(_dummy_loc(), CompOperatorVariant::Equals),
            right: _dummy(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));

        // Unary operations
        intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: LogicOperator::new(_dummy_loc(), LogicOperatorVariant::Not),
            operand: _dummy(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: ArithOperator::new(_dummy_loc(), ArithOperatorVariant::Plus),
            operand: _dummy(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));

        // Symbol introduction
        intermediate_tree = _node(NodeVariant::InitLocal { symbol: _id(), val: _dummy() });
        assert_eq!(intermediate_tree.expr_type(), Some(&unit::TYPE));
        intermediate_tree = _node(NodeVariant::InitLocalFun(0));
        assert_eq!(intermediate_tree.expr_type(), Some(&unit::TYPE));

        // Recursive nodes
        intermediate_tree =
            _node(NodeVariant::InLexicalScope { local_symbols: vec![], expr: _bool_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree =
            _node(NodeVariant::InLexicalScope { local_symbols: vec![], expr: _int_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree =
            _node(NodeVariant::InLexicalScope { local_symbols: vec![], expr: _read_node() });
        assert_eq!(intermediate_tree.expr_type(), None);
        intermediate_tree = _node(NodeVariant::Let { id: 0, value: _dummy(), r#in: _bool_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::Let { id: 0, value: _dummy(), r#in: _int_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::Let { id: 0, value: _dummy(), r#in: _read_node() });
        assert_eq!(intermediate_tree.expr_type(), None);

        // Type checking
        intermediate_tree =
            _node(NodeVariant::InstanceOf { expression: _dummy(), expected_type_tag: 0 });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));

        // Literals
        intermediate_tree = _node(NodeVariant::UnitLiteral);
        assert_eq!(intermediate_tree.expr_type(), Some(&unit::TYPE));
        intermediate_tree = _node(NodeVariant::BoolLiteral(false));
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::IntLiteral(String::from("1")));
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::StringLiteral(String::from("1")));
        assert_eq!(intermediate_tree.expr_type(), Some(&str::TYPE));
        intermediate_tree = _node(NodeVariant::TupleLiteral(vec![]));
        assert_eq!(intermediate_tree.expr_type(), Some(&tuple::TYPE));
        intermediate_tree = _node(NodeVariant::ListLiteral(vec![]));
        assert_eq!(intermediate_tree.expr_type(), Some(&list::TYPE));
        intermediate_tree = _node(NodeVariant::ObjectLiteral(vec![]));
        assert_eq!(intermediate_tree.expr_type(), Some(&obj::TYPE));
        intermediate_tree = _node(NodeVariant::ReadChildUnit(0));
        assert_eq!(intermediate_tree.expr_type(), Some(&function::TYPE));
    }
}
