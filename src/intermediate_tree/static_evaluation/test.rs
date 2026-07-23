//! # Static evaluation testing
//!
//! This module contains all unit tests for static evaluation processes.

use crate::{
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, Identifier,
        LogicOperator, LogicOperatorVariant, MiscOperator, MiscOperatorVariant, Node, NodeVariant,
        static_evaluation::{ConstantValue, ConstantValueVariant},
    },
    sources::{Location, SourceSection},
};
use regex::Regex;

fn _dummy_loc() -> SourceSection {
    SourceSection::new(0, Location::new(0, 0), Location::new(0, 0))
}

// --- Node creation helpers

fn _node(variant: NodeVariant) -> Box<Node> {
    Box::new(Node::new(_dummy_loc(), variant))
}

fn _id(text: &str) -> Identifier {
    Identifier::new(_dummy_loc(), String::from(text))
}

fn _nil_node() -> Box<Node> {
    _node(NodeVariant::NilLiteral)
}

fn _bool_node(value: bool) -> Box<Node> {
    _node(NodeVariant::BoolLiteral(value))
}

fn _int_node(value: isize) -> Box<Node> {
    _node(NodeVariant::IntLiteral(value.to_string()))
}

fn _big_int_node(value: &str) -> Box<Node> {
    _node(NodeVariant::IntLiteral(String::from(value)))
}

fn _str_node(value: &str) -> Box<Node> {
    _node(NodeVariant::StringLiteral(String::from(value)))
}

fn _pattern_node(regex: &str) -> Box<Node> {
    _node(NodeVariant::PatternLiteral(Regex::new(regex).unwrap()))
}

fn _read_symbol_node() -> Box<Node> {
    _node(NodeVariant::ReadSymbol(_id("whatever")))
}

fn _read_node(id: usize) -> Box<Node> {
    _node(NodeVariant::Read(id))
}

fn _if_node(condition: Box<Node>, consequence: Box<Node>, alternative: Box<Node>) -> Box<Node> {
    _node(NodeVariant::IfExpr { condition, consequence, alternative })
}

fn _arith_op(variant: ArithOperatorVariant) -> ArithOperator {
    ArithOperator::new(_dummy_loc(), variant)
}

fn _logic_op(variant: LogicOperatorVariant) -> LogicOperator {
    LogicOperator::new(_dummy_loc(), variant)
}

fn _comp_op(variant: CompOperatorVariant) -> CompOperator {
    CompOperator::new(_dummy_loc(), variant)
}

fn _misc_op(variant: MiscOperatorVariant) -> MiscOperator {
    MiscOperator::new(_dummy_loc(), variant)
}

// --- Constant creation helpers

fn _nil_cst() -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Nil }
}

fn _null_cst() -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Null }
}

fn _unit_cst() -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Unit }
}

fn _bool_cst(value: bool) -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Bool(value) }
}

fn _int_cst(value: &str) -> ConstantValue {
    ConstantValue {
        origin_location: _dummy_loc(),
        variant: ConstantValueVariant::Int(value.parse().unwrap()),
    }
}

fn _str_cst(value: &str) -> ConstantValue {
    ConstantValue {
        origin_location: _dummy_loc(),
        variant: ConstantValueVariant::String(String::from(value)),
    }
}

fn _pattern_cst(regex: &str) -> ConstantValue {
    ConstantValue {
        origin_location: _dummy_loc(),
        variant: ConstantValueVariant::Pattern(Regex::new(regex).unwrap()),
    }
}

fn _tuple_cst(value: Vec<ConstantValue>) -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Tuple(value) }
}

fn _list_cst(value: Vec<ConstantValue>) -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::List(value) }
}

fn _obj_cst(value: Vec<(String, ConstantValue)>) -> ConstantValue {
    ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Object(value) }
}

#[cfg(test)]
mod typing_tests {
    use super::*;
    use crate::builtins::{traits::iterable, types::*};

    #[test]
    fn test_in_clause() {
        let intermediate_tree =
            _node(NodeVariant::InClause { value: _nil_node(), collection: _nil_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
    }

    #[test]
    fn test_block_expr() {
        let mut intermediate_tree =
            _node(NodeVariant::BlockExpr { body: vec![], val: _bool_node(true) });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::BlockExpr { body: vec![], val: _int_node(2) });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree =
            _node(NodeVariant::BlockExpr { body: vec![], val: _read_symbol_node() });
        assert_eq!(intermediate_tree.expr_type(), None);
    }

    #[test]
    fn test_lazy_comprehension() {
        let intermediate_tree =
            _node(NodeVariant::LazyComprehension { source_iterables: vec![], body_index: 0 });
        assert_eq!(intermediate_tree.expr_type(), Some(&stream::TYPE));
    }

    #[test]
    fn test_binary_operations() {
        let mut intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _nil_node(),
            operator: LogicOperator::new(_dummy_loc(), LogicOperatorVariant::And),
            right: _nil_node(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _nil_node(),
            operator: ArithOperator::new(_dummy_loc(), ArithOperatorVariant::Plus),
            right: _nil_node(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _nil_node(),
            operator: CompOperator::new(_dummy_loc(), CompOperatorVariant::Equals),
            right: _nil_node(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
    }

    #[test]
    fn test_unary_operations() {
        let mut intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: LogicOperator::new(_dummy_loc(), LogicOperatorVariant::Not),
            operand: _nil_node(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: ArithOperator::new(_dummy_loc(), ArithOperatorVariant::Plus),
            operand: _nil_node(),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
    }

    #[test]
    fn test_symbol_introductions() {
        let mut intermediate_tree =
            _node(NodeVariant::InitLocal { symbol: _id("x"), val: _nil_node() });
        assert_eq!(intermediate_tree.expr_type(), Some(&unit::TYPE));
        intermediate_tree = _node(NodeVariant::InitLocalFun(0));
        assert_eq!(intermediate_tree.expr_type(), Some(&unit::TYPE));
    }

    #[test]
    fn test_lexical_scope() {
        let mut intermediate_tree =
            _node(NodeVariant::InLexicalScope { local_symbols: vec![], expr: _bool_node(true) });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree =
            _node(NodeVariant::InLexicalScope { local_symbols: vec![], expr: _int_node(1) });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree =
            _node(NodeVariant::InLexicalScope { local_symbols: vec![], expr: _read_symbol_node() });
        assert_eq!(intermediate_tree.expr_type(), None);
        intermediate_tree = _node(NodeVariant::OutsideLexicalScope(_bool_node(true)));
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::OutsideLexicalScope(_int_node(1)));
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::OutsideLexicalScope(_read_symbol_node()));
        assert_eq!(intermediate_tree.expr_type(), None);
    }

    #[test]
    fn test_let_in() {
        let mut intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _nil_node(), r#in: _bool_node(true) });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _nil_node(), r#in: _int_node(1) });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _nil_node(), r#in: _read_symbol_node() });
        assert_eq!(intermediate_tree.expr_type(), None);
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _bool_node(true), r#in: _read_node(0) });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _int_node(1), r#in: _read_node(0) });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _node(NodeVariant::Let {
            id: 42,
            value: _int_node(1),
            r#in: _node(NodeVariant::Let { id: 1, value: _bool_node(true), r#in: _read_node(42) }),
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _int_node(1), r#in: _read_node(1) });
        assert_eq!(intermediate_tree.expr_type(), None);
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _read_symbol_node(), r#in: _read_node(0) });
        assert_eq!(intermediate_tree.expr_type(), None);
    }

    #[test]
    fn test_type_checks() {
        let mut intermediate_tree =
            _node(NodeVariant::InstanceOf { expression: _nil_node(), expected_type_tag: 0 });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _node(NodeVariant::HasTrait {
            expression: _nil_node(),
            expected_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
    }

    #[test]
    fn test_literals() {
        let mut intermediate_tree = _node(NodeVariant::UnitLiteral);
        assert_eq!(intermediate_tree.expr_type(), Some(&unit::TYPE));
        intermediate_tree = _bool_node(false);
        assert_eq!(intermediate_tree.expr_type(), Some(&bool::TYPE));
        intermediate_tree = _int_node(1);
        assert_eq!(intermediate_tree.expr_type(), Some(&int::TYPE));
        intermediate_tree = _str_node("1");
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

#[cfg(test)]
mod constant_evaluation_tests {
    use super::*;
    use crate::builtins::{traits::iterable, types::bool};

    #[test]
    fn test_literals() {
        // Test nil literal
        let mut intermediate_tree = _nil_node();
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_nil_cst()));

        // Test null literal
        intermediate_tree = _node(NodeVariant::NullLiteral);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_null_cst()));

        // Test unit literal
        intermediate_tree = _node(NodeVariant::UnitLiteral);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_unit_cst()));

        // Test boolean literals
        intermediate_tree = _bool_node(false);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _bool_node(true);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test integer literals
        intermediate_tree = _int_node(0);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("0")));
        intermediate_tree = _int_node(42);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _int_node(-42);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-42")));
        intermediate_tree =
            _big_int_node("100000000000000000000000000000000000000000000000000000000000000000");
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_int_cst(
                "100000000000000000000000000000000000000000000000000000000000000000"
            ))
        );

        // Test string literals
        intermediate_tree = _str_node("");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("")));
        intermediate_tree = _str_node("Hello!");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("Hello!")));

        // Test pattern literals
        intermediate_tree = _pattern_node("");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_pattern_cst("")));
        intermediate_tree = _pattern_node("my_regex");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_pattern_cst("my_regex")));

        // Test tuple literals
        intermediate_tree = _node(NodeVariant::TupleLiteral(vec![
            *_node(NodeVariant::UnitLiteral),
            *_bool_node(true),
            *_node(NodeVariant::TupleLiteral(vec![*_int_node(42)])),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_tuple_cst(vec![
                _unit_cst(),
                _bool_cst(true),
                _tuple_cst(vec![_int_cst("42")])
            ]))
        );
        intermediate_tree = _node(NodeVariant::TupleLiteral(vec![
            *_node(NodeVariant::UnitLiteral),
            *_read_symbol_node(),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test list literals
        intermediate_tree = _node(NodeVariant::ListLiteral(vec![
            *_node(NodeVariant::UnitLiteral),
            *_bool_node(true),
            *_node(NodeVariant::ListLiteral(vec![*_int_node(42)])),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_list_cst(vec![
                _unit_cst(),
                _bool_cst(true),
                _list_cst(vec![_int_cst("42")])
            ]))
        );
        intermediate_tree = _node(NodeVariant::ListLiteral(vec![
            *_node(NodeVariant::UnitLiteral),
            *_read_symbol_node(),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test object literals
        intermediate_tree = _node(NodeVariant::ObjectLiteral(vec![
            (_id("a"), *_node(NodeVariant::UnitLiteral)),
            (_id("b"), *_bool_node(true)),
            (
                _id("c"),
                *_node(NodeVariant::ObjectLiteral(vec![(_id("inner"), *_int_node(42))])),
            ),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_obj_cst(vec![
                (String::from("a"), _unit_cst()),
                (String::from("b"), _bool_cst(true)),
                (String::from("c"), _obj_cst(vec![(String::from("inner"), _int_cst("42"))]))
            ]))
        );
        intermediate_tree = _node(NodeVariant::ObjectLiteral(vec![
            (_id("a"), *_node(NodeVariant::UnitLiteral)),
            (_id("a"), *_read_symbol_node()),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_arithmetic_binary_operations() {
        // Test additions
        let mut intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(40),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(42),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: _int_node(-2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("40")));

        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(2),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: _int_node(-5),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-3")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(10),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: _node(NodeVariant::ArithBinOp {
                left: _int_node(6),
                operator: _arith_op(ArithOperatorVariant::Plus),
                right: _int_node(10),
            }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("26")));

        // Test subtractions
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(40),
            operator: _arith_op(ArithOperatorVariant::Minus),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("38")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(42),
            operator: _arith_op(ArithOperatorVariant::Minus),
            right: _int_node(-2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("44")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(10),
            operator: _arith_op(ArithOperatorVariant::Minus),
            right: _node(NodeVariant::ArithBinOp {
                left: _int_node(6),
                operator: _arith_op(ArithOperatorVariant::Minus),
                right: _int_node(10),
            }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("14")));

        // Test multiplications
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(40),
            operator: _arith_op(ArithOperatorVariant::Multiply),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("80")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(42),
            operator: _arith_op(ArithOperatorVariant::Multiply),
            right: _int_node(-2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-84")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(10),
            operator: _arith_op(ArithOperatorVariant::Multiply),
            right: _node(NodeVariant::ArithBinOp {
                left: _int_node(6),
                operator: _arith_op(ArithOperatorVariant::Multiply),
                right: _int_node(10),
            }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("600")));

        // Test divisions
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(40),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("20")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(42),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: _int_node(-2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-21")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(10),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: _node(NodeVariant::ArithBinOp {
                left: _int_node(16),
                operator: _arith_op(ArithOperatorVariant::Divide),
                right: _int_node(4),
            }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));

        // Test an invalid evaluation
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(40),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: _str_node("not an int"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: _int_node(40),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: _int_node(0),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_concatenation_operations() {
        // Test string concatenation
        let mut intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _str_node("hello"),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _str_node(" world"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _str_node("hello"),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _str_node(""),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _str_node("hello"),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _node(NodeVariant::MiscBinOp {
                left: _str_node(" "),
                operator: _misc_op(MiscOperatorVariant::Concat),
                right: _str_node("world"),
            }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));

        // Test list concatenation
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _node(NodeVariant::ListLiteral(vec![*_int_node(1), *_int_node(2)])),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _node(NodeVariant::ListLiteral(vec![*_int_node(3), *_int_node(4)])),
        });
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_list_cst(
                vec![_int_cst("1"), _int_cst("2"), _int_cst("3"), _int_cst("4"),]
            ))
        );
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _str_node("hello"),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _str_node(""),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _str_node("hello"),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _node(NodeVariant::MiscBinOp {
                left: _str_node(" "),
                operator: _misc_op(MiscOperatorVariant::Concat),
                right: _str_node("world"),
            }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));

        // Test invalid concatenation
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: _str_node("hello"),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: _int_node(40),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_binary_operations() {
        // Test the logical and
        let mut intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _bool_node(false),
            operator: _logic_op(LogicOperatorVariant::And),
            right: _bool_node(true),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _bool_node(true),
            operator: _logic_op(LogicOperatorVariant::And),
            right: _bool_node(true),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test the logical or
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _bool_node(false),
            operator: _logic_op(LogicOperatorVariant::Or),
            right: _bool_node(true),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _bool_node(false),
            operator: _logic_op(LogicOperatorVariant::Or),
            right: _bool_node(false),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test invalid logical operation
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: _int_node(1),
            operator: _logic_op(LogicOperatorVariant::Or),
            right: _bool_node(false),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_in_clause() {
        // Test valid "in" clauses
        let mut intermediate_tree = _node(NodeVariant::InClause {
            value: _int_node(2),
            collection: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::InClause {
            value: _int_node(4),
            collection: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test an invalid "in" clause
        intermediate_tree =
            _node(NodeVariant::InClause { value: _int_node(2), collection: _str_node("123") });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_equality_operations() {
        // Test equality operations
        let mut intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("hello"),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: _str_node("hello"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("hello"),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: _str_node("world"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(1),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: _str_node("1"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
                *_int_node(4),
            ])),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test inequality operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("hello"),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: _str_node("hello"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("hello"),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: _str_node("world"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(1),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: _str_node("1"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
                *_int_node(4),
            ])),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
    }

    #[test]
    fn test_comparison_operations() {
        // Test "greater than" operations
        let mut intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: _int_node(1),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: _int_node(3),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: _str_node("a"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: _str_node("b"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: _str_node("c"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test "greater or equals" operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: _int_node(1),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: _int_node(3),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: _str_node("a"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: _str_node("b"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: _str_node("c"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test "less than" operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Less),
            right: _int_node(1),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Less),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::Less),
            right: _int_node(3),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::Less),
            right: _str_node("a"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::Less),
            right: _str_node("b"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::Less),
            right: _str_node("c"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test "less or equals" operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: _int_node(1),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _int_node(2),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: _int_node(3),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: _str_node("a"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: _str_node("b"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: _str_node("b"),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: _str_node("c"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
    }

    #[test]
    fn test_arithmetic_unary_operation() {
        let mut intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: _arith_op(ArithOperatorVariant::Minus),
            operand: _int_node(42),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-42")));
        intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: _arith_op(ArithOperatorVariant::Minus),
            operand: _int_node(-5),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("5")));
        intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: _arith_op(ArithOperatorVariant::Minus),
            operand: _str_node("hello"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_unary_operation() {
        let mut intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: _logic_op(LogicOperatorVariant::Not),
            operand: _bool_node(false),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: _logic_op(LogicOperatorVariant::Not),
            operand: _bool_node(true),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: _logic_op(LogicOperatorVariant::Not),
            operand: _int_node(42),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_dotted_access() {
        // Test dot access on object literals
        let mut intermediate_tree = _node(NodeVariant::DottedExpr {
            prefix: _node(NodeVariant::ObjectLiteral(vec![(_id("a"), *_int_node(42))])),
            suffix: _id("a"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _node(NodeVariant::DottedExpr {
            prefix: _node(NodeVariant::ObjectLiteral(vec![(_id("a"), *_int_node(42))])),
            suffix: _id("b"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_indexing() {
        // Test indexing a list
        let mut intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
            index: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: _node(NodeVariant::ListLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
            index: _int_node(4),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test indexing a tuple
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: _node(NodeVariant::TupleLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
            index: _int_node(2),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: _node(NodeVariant::TupleLiteral(vec![
                *_int_node(1),
                *_int_node(2),
                *_int_node(3),
            ])),
            index: _int_node(4),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test safe indexing
        intermediate_tree =
            _node(NodeVariant::IndexExpr { indexed_val: _str_node("hello"), index: _int_node(2) });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_if_expr() {
        let mut intermediate_tree = _if_node(_bool_node(true), _int_node(42), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _if_node(_bool_node(false), _int_node(42), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _if_node(_read_symbol_node(), _int_node(42), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _if_node(_bool_node(true), _read_symbol_node(), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _if_node(_bool_node(false), _int_node(42), _read_symbol_node());
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_let_in() {
        let mut intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _int_node(42), r#in: _read_node(0) });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _bool_node(true), r#in: _read_node(0) });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::Let {
            id: 0,
            value: _str_node("hello"),
            r#in: _node(NodeVariant::Let { id: 1, value: _int_node(42), r#in: _read_node(0) }),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _int_node(42), r#in: _read_node(1) });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree =
            _node(NodeVariant::Let { id: 0, value: _read_symbol_node(), r#in: _read_node(0) });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_instance_of() {
        let mut intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: _bool_node(false),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: _str_node("hello"),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: _node(NodeVariant::CompBinOp {
                left: _read_symbol_node(),
                operator: _comp_op(CompOperatorVariant::Equals),
                right: _read_symbol_node(),
            }),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: _read_symbol_node(),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_has_trait() {
        let mut intermediate_tree = _node(NodeVariant::HasTrait {
            expression: _node(NodeVariant::ListLiteral(vec![*_int_node(2)])),
            expected_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::HasTrait {
            expression: _str_node("hello"),
            expected_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        let mut intermediate_tree = _node(NodeVariant::HasTrait {
            expression: _node(NodeVariant::ListLiteral(vec![*_read_symbol_node()])),
            expected_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::HasTrait {
            expression: _read_symbol_node(),
            expected_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }
}
