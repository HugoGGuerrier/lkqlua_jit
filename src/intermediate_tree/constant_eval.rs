//! # Constant evaluation module
//!
//! This module contains all required entities to evaluate an [`Node`] as a
//! constant value.

use num_bigint::BigInt;

use crate::{
    bytecode::{ComplexConstant, NumericConstant, TableConstantElement},
    intermediate_tree::{Node, NodeVariant, OperatorVariant},
};

impl Node {
    /// Try to evaluate this node as a constant value, returning it if this is
    /// feasible. Otherwise this function returns [`None`].
    pub fn eval_as_constant(&self) -> Option<ConstantValue> {
        match &self.variant {
            // --- Literals
            NodeVariant::NullLiteral => Some(ConstantValue::Null),
            NodeVariant::UnitLiteral => Some(ConstantValue::Unit),
            NodeVariant::BoolLiteral(b) => Some(ConstantValue::Bool(*b)),
            NodeVariant::IntLiteral(i) => Some(ConstantValue::Int(i.parse::<BigInt>().unwrap())),
            NodeVariant::StringLiteral(s) => Some(ConstantValue::String(s.clone())),
            NodeVariant::TupleLiteral(nodes) | NodeVariant::ListLiteral(nodes) => {
                let constants = nodes
                    .iter()
                    .filter_map(|n| n.eval_as_constant())
                    .collect::<Vec<_>>();
                if constants.len() != nodes.len() {
                    None
                } else {
                    if matches!(self.variant, NodeVariant::TupleLiteral(_)) {
                        Some(ConstantValue::Tuple(constants))
                    } else {
                        Some(ConstantValue::List(constants))
                    }
                }
            }
            NodeVariant::ObjectLiteral(items) => {
                let constant_items = items
                    .iter()
                    .filter_map(|(n, v)| v.eval_as_constant().map(|c| (n.text.clone(), c)))
                    .collect::<Vec<_>>();
                if constant_items.len() != items.len() {
                    None
                } else {
                    Some(ConstantValue::Object(constant_items))
                }
            }

            // --- Binary operations
            NodeVariant::ArithBinOp { left, operator, right } => {
                match (left.eval_as_constant(), right.eval_as_constant()) {
                    (Some(ConstantValue::Int(ref li)), Some(ConstantValue::Int(ref ri))) => {
                        Some(ConstantValue::Int(match operator.variant {
                            OperatorVariant::Plus => li + ri,
                            OperatorVariant::Minus => li - ri,
                            OperatorVariant::Multiply => li * ri,
                            OperatorVariant::Divide => li / ri,
                            _ => unreachable!(),
                        }))
                    }
                    _ => None,
                }
            }
            NodeVariant::LogicBinOp { left, operator, right } => {
                match (left.eval_as_constant(), right.eval_as_constant()) {
                    (Some(ConstantValue::Bool(lb)), Some(ConstantValue::Bool(rb))) => {
                        Some(ConstantValue::Bool(match operator.variant {
                            OperatorVariant::Or => lb || rb,
                            OperatorVariant::And => lb && rb,
                            _ => unreachable!(),
                        }))
                    }
                    _ => None,
                }
            }
            NodeVariant::CompBinOp { left, operator, right } => {
                match (left.eval_as_constant(), right.eval_as_constant()) {
                    (Some(constant_left), Some(constant_right)) => match operator.variant {
                        OperatorVariant::Equals => {
                            Some(ConstantValue::Bool(constant_left == constant_right))
                        }
                        OperatorVariant::NotEquals => {
                            Some(ConstantValue::Bool(constant_left != constant_right))
                        }
                        OperatorVariant::Greater => compare_ints_or_strings(
                            constant_left,
                            constant_right,
                            BigInt::gt,
                            String::gt,
                        ),
                        OperatorVariant::GreaterOrEquals => compare_ints_or_strings(
                            constant_left,
                            constant_right,
                            BigInt::ge,
                            String::ge,
                        ),
                        OperatorVariant::Less => compare_ints_or_strings(
                            constant_left,
                            constant_right,
                            BigInt::lt,
                            String::lt,
                        ),
                        OperatorVariant::LessOrEquals => compare_ints_or_strings(
                            constant_left,
                            constant_right,
                            BigInt::le,
                            String::le,
                        ),
                        _ => unreachable!(),
                    },
                    _ => None,
                }
            }
            NodeVariant::MiscBinOp { left, operator, right } => {
                match (left.eval_as_constant(), right.eval_as_constant()) {
                    (Some(constant_left), Some(constant_right)) => match operator.variant {
                        OperatorVariant::Concat => match (constant_left, constant_right) {
                            (ConstantValue::String(ls), ConstantValue::String(rs)) => {
                                Some(ConstantValue::String(format!("{ls}{rs}")))
                            }
                            (ConstantValue::List(ll), ConstantValue::List(rl)) => {
                                let mut constant_result = Vec::with_capacity(ll.len() + rl.len());
                                ll.iter().for_each(|e| constant_result.push(e.clone()));
                                rl.iter().for_each(|e| constant_result.push(e.clone()));
                                Some(ConstantValue::List(constant_result))
                            }
                            _ => None,
                        },
                        _ => unreachable!(),
                    },
                    _ => None,
                }
            }

            // --- Unary operations
            NodeVariant::ArithUnOp { operator, operand } => {
                if let Some(ConstantValue::Int(ref i)) = operand.eval_as_constant() {
                    Some(match &operator.variant {
                        OperatorVariant::Plus => ConstantValue::Int(i.clone()),
                        OperatorVariant::Minus => ConstantValue::Int(-i),
                        _ => unreachable!(),
                    })
                } else {
                    None
                }
            }
            NodeVariant::LogicUnOp { operator, operand } => {
                if let Some(ConstantValue::Bool(ref b)) = operand.eval_as_constant() {
                    Some(match &operator.variant {
                        OperatorVariant::Not => ConstantValue::Bool(!b),
                        _ => unreachable!(),
                    })
                } else {
                    None
                }
            }

            // --- Composite expressions
            NodeVariant::DottedExpr { prefix, suffix, is_safe } => prefix
                .eval_as_constant()
                .and_then(|constant_prefix| match constant_prefix {
                    ConstantValue::Null => {
                        if *is_safe {
                            Some(ConstantValue::Null)
                        } else {
                            None
                        }
                    }
                    ConstantValue::Object(items) => items
                        .iter()
                        .find(|(s, _)| s == &suffix.text)
                        .and_then(|(_, constant_result)| Some(constant_result.clone())),
                    _ => None,
                }),
            NodeVariant::IndexExpr { indexed_val, index, is_safe } => indexed_val
                .eval_as_constant()
                .and_then(|constant_indexed_val| match constant_indexed_val {
                    ConstantValue::Null => {
                        if *is_safe {
                            Some(ConstantValue::Null)
                        } else {
                            None
                        }
                    }
                    ConstantValue::Tuple(values) | ConstantValue::List(values) => index
                        .eval_as_constant()
                        .and_then(|constant_index| match constant_index {
                            ConstantValue::Int(constant_index) => {
                                if let Ok(i) = usize::try_from(constant_index) {
                                    values.get(i - 1).map(|c| c.clone()).or(if *is_safe {
                                        Some(ConstantValue::Null)
                                    } else {
                                        None
                                    })
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }),
                    _ => None,
                }),
            NodeVariant::InClause { value, collection } => {
                match (value.eval_as_constant(), collection.eval_as_constant()) {
                    (Some(value_constant), Some(ConstantValue::List(collection_elements))) => {
                        Some(ConstantValue::Bool(
                            collection_elements
                                .into_iter()
                                .find(|e| e == &value_constant)
                                .is_some(),
                        ))
                    }
                    _ => None,
                }
            }

            // --- All other nodes cannot be evaluated as constant
            _ => None,
        }
    }
}

/// This enumeration represents the result of a node constant evaluation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValue {
    Null,
    Unit,
    Bool(bool),
    Int(BigInt),
    String(String),
    Tuple(Vec<ConstantValue>),
    List(Vec<ConstantValue>),
    Object(Vec<(String, ConstantValue)>),
}

impl ConstantValue {
    /// Get the complex constant representing this constant value if it one
    /// exists. Otherwise, this function returns [`None`].
    pub fn to_complex_constant(&self) -> Option<ComplexConstant> {
        match self {
            ConstantValue::String(value) => Some(ComplexConstant::String(value.clone())),
            ConstantValue::Tuple(constant_values) | ConstantValue::List(constant_values) => {
                let array_part = constant_values
                    .iter()
                    .filter_map(|c| c.to_table_constant_element())
                    .collect::<Vec<_>>();
                if array_part.len() == constant_values.len() {
                    Some(ComplexConstant::Table { array_part, hash_part: Vec::new() })
                } else {
                    None
                }
            }
            ConstantValue::Object(items) => {
                let hash_part = items
                    .iter()
                    .filter_map(|(name, value)| {
                        value
                            .to_table_constant_element()
                            .map(|tc| (TableConstantElement::String(name.clone()), tc))
                    })
                    .collect::<Vec<_>>();
                if hash_part.len() == items.len() {
                    Some(ComplexConstant::Table { array_part: Vec::new(), hash_part })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Get the complex constant representing this constant value if it one
    /// exists. Otherwise, this function returns [`None`].
    pub fn to_numeric_constant(&self) -> Option<NumericConstant> {
        match self {
            ConstantValue::Int(value) => {
                if value >= &BigInt::from(i32::MIN) && value <= &BigInt::from(i32::MAX) {
                    let value_le_bytes = value.to_signed_bytes_le();
                    let mut le_bytes = [0 as u8; 4];
                    for i in 0..value_le_bytes.len() {
                        le_bytes[i] = *value_le_bytes.get(i).unwrap_or(&0);
                    }
                    Some(NumericConstant::Integer(i32::from_le_bytes(le_bytes)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Get the table constant element representing this constant value if one
    /// exists. Otherwise, this function return [`None`].
    pub fn to_table_constant_element(&self) -> Option<TableConstantElement> {
        match self {
            ConstantValue::Bool(value) => {
                Some(if *value { TableConstantElement::True } else { TableConstantElement::False })
            }
            ConstantValue::Int(value) => {
                if value <= &BigInt::from(i32::MAX) {
                    let value_le_bytes = value.to_signed_bytes_le();
                    let mut le_bytes = [0 as u8; 4];
                    for i in 0..le_bytes.len() {
                        le_bytes[i] = *value_le_bytes.get(i).unwrap_or(&0);
                    }
                    Some(TableConstantElement::Integer(i32::from_le_bytes(le_bytes)))
                } else {
                    None
                }
            }
            ConstantValue::String(value) => Some(TableConstantElement::String(value.clone())),
            _ => None,
        }
    }
}

/// If both provided constants are [`ConstantValue::Int`], then call the
/// provided function on them and return the result wrapped in a constant
/// value.
fn apply_to_ints<F>(left: ConstantValue, right: ConstantValue, f: F) -> Option<ConstantValue>
where
    F: Fn(BigInt, BigInt) -> BigInt,
{
    match (left, right) {
        (ConstantValue::Int(li), ConstantValue::Int(ri)) => Some(ConstantValue::Int(f(li, ri))),
        _ => None,
    }
}

/// If both provided constants are [`ConstantValue::Bool`], then call the
/// provided function on them and return the result wrapped in a constant
/// value.
fn apply_to_bools<F>(left: ConstantValue, right: ConstantValue, f: F) -> Option<ConstantValue>
where
    F: Fn(bool, bool) -> bool,
{
    match (left, right) {
        (ConstantValue::Bool(lb), ConstantValue::Bool(rb)) => Some(ConstantValue::Bool(f(lb, rb))),
        _ => None,
    }
}

/// Compare left and right constant values with the appropriate comparison
/// function. Return the boolean result of the comparison wrapped in a constant
/// value.
fn compare_ints_or_strings<F, G>(
    left: ConstantValue,
    right: ConstantValue,
    int_comp: F,
    str_comp: G,
) -> Option<ConstantValue>
where
    F: Fn(&BigInt, &BigInt) -> bool,
    G: Fn(&String, &String) -> bool,
{
    match (&left, &right) {
        (ConstantValue::Int(li), ConstantValue::Int(ri)) => {
            Some(ConstantValue::Bool(int_comp(li, ri)))
        }
        (ConstantValue::String(ls), ConstantValue::String(rs)) => {
            Some(ConstantValue::Bool(str_comp(ls, rs)))
        }
        _ => None,
    }
}

mod tests {
    use crate::{
        intermediate_tree::{self, Identifier, Operator},
        sources::{Location, SourceSection},
    };

    use super::*;

    fn dummy_loc() -> SourceSection {
        SourceSection {
            source: String::new(),
            start: Location { line: 0, col: 0 },
            end: Location { line: 0, col: 0 },
        }
    }

    fn new_node(variant: NodeVariant) -> Node {
        Node { origin_location: dummy_loc(), variant }
    }

    fn new_op(variant: OperatorVariant) -> Operator {
        Operator { origin_location: dummy_loc(), variant }
    }

    fn new_id(text: &str) -> Identifier {
        Identifier { origin_location: dummy_loc(), text: String::from(text) }
    }

    fn bool_node(value: bool) -> Node {
        new_node(NodeVariant::BoolLiteral(value))
    }

    fn int_node(value: &str) -> Node {
        new_node(NodeVariant::IntLiteral(String::from(value)))
    }

    fn str_node(value: &str) -> Node {
        new_node(NodeVariant::StringLiteral(String::from(value)))
    }

    fn bool_cst(value: bool) -> ConstantValue {
        ConstantValue::Bool(value)
    }

    fn int_cst(value: &str) -> ConstantValue {
        ConstantValue::Int(value.parse::<BigInt>().unwrap())
    }

    fn str_cst(value: &str) -> ConstantValue {
        ConstantValue::String(String::from(value))
    }

    #[test]
    fn test_literals_constant_evaluation() {
        // Test null literal
        let mut intermediate_tree = new_node(NodeVariant::NullLiteral);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(ConstantValue::Null));

        // Test unit literal
        intermediate_tree = new_node(NodeVariant::UnitLiteral);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(ConstantValue::Unit));

        // Test boolean literals
        intermediate_tree = bool_node(false);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = bool_node(true);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));

        // Test integer literals
        intermediate_tree = int_node("0");
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(ConstantValue::Int(BigInt::ZERO))
        );
        intermediate_tree = int_node("42");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("42")));
        intermediate_tree = int_node("-42");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("-42")));
        intermediate_tree =
            int_node("100000000000000000000000000000000000000000000000000000000000000000");
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(int_cst(
                "100000000000000000000000000000000000000000000000000000000000000000"
            ))
        );

        // Test string literals
        intermediate_tree = str_node("");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("")));
        intermediate_tree = str_node("Hello!");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("Hello!")));

        // Test tuple literals
        intermediate_tree = new_node(NodeVariant::TupleLiteral(vec![
            new_node(NodeVariant::UnitLiteral),
            bool_node(true),
            new_node(NodeVariant::TupleLiteral(vec![int_node("42")])),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(ConstantValue::Tuple(vec![
                ConstantValue::Unit,
                bool_cst(true),
                ConstantValue::Tuple(vec![int_cst("42")])
            ]))
        );
        intermediate_tree = new_node(NodeVariant::TupleLiteral(vec![
            new_node(NodeVariant::UnitLiteral),
            new_node(NodeVariant::ReadSymbol(new_id("nope"))),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test list literals
        intermediate_tree = new_node(NodeVariant::ListLiteral(vec![
            new_node(NodeVariant::UnitLiteral),
            bool_node(true),
            new_node(NodeVariant::ListLiteral(vec![int_node("42")])),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(ConstantValue::List(vec![
                ConstantValue::Unit,
                ConstantValue::Bool(true),
                ConstantValue::List(vec![int_cst("42")])
            ]))
        );
        intermediate_tree = new_node(NodeVariant::ListLiteral(vec![
            new_node(NodeVariant::UnitLiteral),
            new_node(NodeVariant::ReadSymbol(new_id("nope"))),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test object literals
        intermediate_tree = new_node(NodeVariant::ObjectLiteral(vec![
            (new_id("a"), new_node(NodeVariant::UnitLiteral)),
            (new_id("b"), bool_node(true)),
            (
                new_id("c"),
                new_node(NodeVariant::ObjectLiteral(vec![(new_id("inner"), int_node("42"))])),
            ),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(ConstantValue::Object(vec![
                (String::from("a"), ConstantValue::Unit),
                (String::from("b"), ConstantValue::Bool(true)),
                (
                    String::from("c"),
                    ConstantValue::Object(vec![(String::from("inner"), int_cst("42"))])
                )
            ]))
        );
        intermediate_tree = new_node(NodeVariant::ObjectLiteral(vec![
            (new_id("a"), new_node(NodeVariant::UnitLiteral)),
            (new_id("a"), new_node(NodeVariant::ReadSymbol(new_id("nope")))),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_arithmetic_binary_operations() {
        // Test additions
        let mut intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("40")),
            operator: new_op(OperatorVariant::Plus),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("42")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("42")),
            operator: new_op(OperatorVariant::Plus),
            right: Box::new(int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("40")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("10")),
            operator: new_op(OperatorVariant::Plus),
            right: Box::new(new_node(NodeVariant::ArithBinOp {
                left: Box::new(int_node("6")),
                operator: new_op(OperatorVariant::Plus),
                right: Box::new(int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("26")));

        // Test subtractions
        let mut intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("40")),
            operator: new_op(OperatorVariant::Minus),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("38")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("42")),
            operator: new_op(OperatorVariant::Minus),
            right: Box::new(int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("44")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("10")),
            operator: new_op(OperatorVariant::Minus),
            right: Box::new(new_node(NodeVariant::ArithBinOp {
                left: Box::new(int_node("6")),
                operator: new_op(OperatorVariant::Minus),
                right: Box::new(int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("14")));

        // Test multiplications
        let mut intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("40")),
            operator: new_op(OperatorVariant::Multiply),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("80")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("42")),
            operator: new_op(OperatorVariant::Multiply),
            right: Box::new(int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("-84")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("10")),
            operator: new_op(OperatorVariant::Multiply),
            right: Box::new(new_node(NodeVariant::ArithBinOp {
                left: Box::new(int_node("6")),
                operator: new_op(OperatorVariant::Multiply),
                right: Box::new(int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("600")));

        // Test divisions
        let mut intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("40")),
            operator: new_op(OperatorVariant::Divide),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("20")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("42")),
            operator: new_op(OperatorVariant::Divide),
            right: Box::new(int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("-21")));
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("10")),
            operator: new_op(OperatorVariant::Divide),
            right: Box::new(new_node(NodeVariant::ArithBinOp {
                left: Box::new(int_node("16")),
                operator: new_op(OperatorVariant::Divide),
                right: Box::new(int_node("4")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("2")));

        // Test an invalid evaluation
        intermediate_tree = new_node(NodeVariant::ArithBinOp {
            left: Box::new(int_node("40")),
            operator: new_op(OperatorVariant::Plus),
            right: Box::new(str_node("not an int")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_concatenation_operations() {
        // Test string concatenation
        let mut intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(str_node(" world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("hello world")));
        intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(str_node("")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("hello")));
        intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(new_node(NodeVariant::MiscBinOp {
                left: Box::new(str_node(" ")),
                operator: new_op(OperatorVariant::Concat),
                right: Box::new(str_node("world")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("hello world")));

        // Test list concatenation
        intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(new_node(NodeVariant::ListLiteral(vec![int_node("1"), int_node("2")]))),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(new_node(NodeVariant::ListLiteral(vec![int_node("3"), int_node("4")]))),
        });
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(ConstantValue::List(vec![
                int_cst("1"),
                int_cst("2"),
                int_cst("3"),
                int_cst("4"),
            ]))
        );
        intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(str_node("")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("hello")));
        intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(new_node(NodeVariant::MiscBinOp {
                left: Box::new(str_node(" ")),
                operator: new_op(OperatorVariant::Concat),
                right: Box::new(str_node("world")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(str_cst("hello world")));

        // Test invalid concatenation
        intermediate_tree = new_node(NodeVariant::MiscBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Concat),
            right: Box::new(int_node("40")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_binary_operations() {
        // Test the logical and
        let mut intermediate_tree = new_node(NodeVariant::LogicBinOp {
            left: Box::new(bool_node(false)),
            operator: new_op(OperatorVariant::And),
            right: Box::new(bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::LogicBinOp {
            left: Box::new(bool_node(true)),
            operator: new_op(OperatorVariant::And),
            right: Box::new(bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));

        // Test the logical or
        intermediate_tree = new_node(NodeVariant::LogicBinOp {
            left: Box::new(bool_node(false)),
            operator: new_op(OperatorVariant::Or),
            right: Box::new(bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::LogicBinOp {
            left: Box::new(bool_node(false)),
            operator: new_op(OperatorVariant::Or),
            right: Box::new(bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        // Test invalid logical operation
        intermediate_tree = new_node(NodeVariant::LogicBinOp {
            left: Box::new(int_node("1")),
            operator: new_op(OperatorVariant::Or),
            right: Box::new(bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_in_clause() {
        // Test valid "in" clauses
        let mut intermediate_tree = new_node(NodeVariant::InClause {
            value: Box::new(int_node("2")),
            collection: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::InClause {
            value: Box::new(int_node("4")),
            collection: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        // Test an invalid "in" clause
        intermediate_tree = new_node(NodeVariant::InClause {
            value: Box::new(int_node("2")),
            collection: Box::new(str_node("123")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_equality_operations() {
        // Test equality operations
        let mut intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Equals),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Equals),
            right: Box::new(str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::Equals),
            right: Box::new(str_node("world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("1")),
            operator: new_op(OperatorVariant::Equals),
            right: Box::new(str_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
            operator: new_op(OperatorVariant::Equals),
            right: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
                int_node("4"),
            ]))),
            operator: new_op(OperatorVariant::Equals),
            right: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        // Test inequality operations
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::NotEquals),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::NotEquals),
            right: Box::new(str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("hello")),
            operator: new_op(OperatorVariant::NotEquals),
            right: Box::new(str_node("world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("1")),
            operator: new_op(OperatorVariant::NotEquals),
            right: Box::new(str_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
            operator: new_op(OperatorVariant::NotEquals),
            right: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
                int_node("4"),
            ]))),
            operator: new_op(OperatorVariant::NotEquals),
            right: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
    }

    #[test]
    fn test_comparison_operations() {
        // Test "greater than" operations
        let mut intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Greater),
            right: Box::new(int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Greater),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Greater),
            right: Box::new(int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::Greater),
            right: Box::new(str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::Greater),
            right: Box::new(str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::Greater),
            right: Box::new(str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        // Test "greater or equals" operations
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::GreaterOrEquals),
            right: Box::new(int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::GreaterOrEquals),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::GreaterOrEquals),
            right: Box::new(int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::GreaterOrEquals),
            right: Box::new(str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::GreaterOrEquals),
            right: Box::new(str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::GreaterOrEquals),
            right: Box::new(str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));

        // Test "less than" operations
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Less),
            right: Box::new(int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Less),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::Less),
            right: Box::new(int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));

        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::Less),
            right: Box::new(str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::Less),
            right: Box::new(str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::Less),
            right: Box::new(str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));

        // Test "less or equals" operations
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::LessOrEquals),
            right: Box::new(int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::LessOrEquals),
            right: Box::new(int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(int_node("2")),
            operator: new_op(OperatorVariant::LessOrEquals),
            right: Box::new(int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));

        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::LessOrEquals),
            right: Box::new(str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::LessOrEquals),
            right: Box::new(str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::CompBinOp {
            left: Box::new(str_node("b")),
            operator: new_op(OperatorVariant::LessOrEquals),
            right: Box::new(str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
    }

    #[test]
    fn test_arithmetic_unary_operation() {
        let mut intermediate_tree = new_node(NodeVariant::ArithUnOp {
            operator: new_op(OperatorVariant::Minus),
            operand: Box::new(int_node("42")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("-42")));
        intermediate_tree = new_node(NodeVariant::ArithUnOp {
            operator: new_op(OperatorVariant::Minus),
            operand: Box::new(int_node("-5")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("5")));
        intermediate_tree = new_node(NodeVariant::ArithUnOp {
            operator: new_op(OperatorVariant::Minus),
            operand: Box::new(str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_unary_operation() {
        let mut intermediate_tree = new_node(NodeVariant::LogicUnOp {
            operator: new_op(OperatorVariant::Not),
            operand: Box::new(bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(true)));
        intermediate_tree = new_node(NodeVariant::LogicUnOp {
            operator: new_op(OperatorVariant::Not),
            operand: Box::new(bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(bool_cst(false)));
        intermediate_tree = new_node(NodeVariant::LogicUnOp {
            operator: new_op(OperatorVariant::Not),
            operand: Box::new(int_node("42")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_dotted_access() {
        // Test dot access on object literals
        let mut intermediate_tree = new_node(NodeVariant::DottedExpr {
            prefix: Box::new(new_node(NodeVariant::ObjectLiteral(vec![(
                new_id("a"),
                int_node("42"),
            )]))),
            suffix: new_id("a"),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("42")));
        intermediate_tree = new_node(NodeVariant::DottedExpr {
            prefix: Box::new(new_node(NodeVariant::ObjectLiteral(vec![(
                new_id("a"),
                int_node("42"),
            )]))),
            suffix: new_id("b"),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test safe dot access
        intermediate_tree = new_node(NodeVariant::DottedExpr {
            prefix: Box::new(new_node(NodeVariant::NullLiteral)),
            suffix: new_id("a"),
            is_safe: true,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(ConstantValue::Null));
    }

    #[test]
    fn test_indexing() {
        // Test indexing a list
        let mut intermediate_tree = new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
            index: Box::new(int_node("2")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("2")));
        intermediate_tree = new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(new_node(NodeVariant::ListLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
            index: Box::new(int_node("4")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test indexing a tuple
        intermediate_tree = new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(new_node(NodeVariant::TupleLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
            index: Box::new(int_node("2")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(int_cst("2")));
        intermediate_tree = new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(new_node(NodeVariant::TupleLiteral(vec![
                int_node("1"),
                int_node("2"),
                int_node("3"),
            ]))),
            index: Box::new(int_node("4")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test safe indexing
        intermediate_tree = new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(str_node("hello")),
            index: Box::new(int_node("2")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }
}
