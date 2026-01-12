//! # Constant evaluation module
//!
//! This module contains all required entities to evaluate an [`Node`] as a
//! constant value.

use crate::{
    bytecode::{ComplexConstant, NumericConstant, TableConstantElement},
    intermediate_tree::{
        ArithOperatorVariant, CompOperatorVariant, LogicOperatorVariant, MiscOperatorVariant, Node,
        NodeVariant,
    },
    sources::SourceSection,
};
use num_bigint::BigInt;

impl Node {
    /// Try to evaluate this node as a constant value, returning it if this is
    /// feasible. Otherwise this function returns [`None`].
    pub fn eval_as_constant(&self) -> Option<ConstantValue> {
        /// Internal recursive function to evaluate a node as a constant
        /// variant. This is used to avoid useless wrapping during constant
        /// evaluation.
        fn eval_as_constant_variant(node: &Node) -> Option<ConstantValueVariant> {
            match &node.variant {
                // --- Literals
                NodeVariant::NullLiteral => Some(ConstantValueVariant::Null),
                NodeVariant::UnitLiteral => Some(ConstantValueVariant::Unit),
                NodeVariant::BoolLiteral(b) => Some(ConstantValueVariant::Bool(*b)),
                NodeVariant::IntLiteral(i) => {
                    Some(ConstantValueVariant::Int(i.parse::<BigInt>().unwrap()))
                }
                NodeVariant::StringLiteral(s) => Some(ConstantValueVariant::String(s.clone())),
                NodeVariant::TupleLiteral(nodes) | NodeVariant::ListLiteral(nodes) => {
                    let constants = nodes
                        .iter()
                        .filter_map(|n| n.eval_as_constant())
                        .collect::<Vec<_>>();
                    if constants.len() != nodes.len() {
                        None
                    } else {
                        if matches!(node.variant, NodeVariant::TupleLiteral(_)) {
                            Some(ConstantValueVariant::Tuple(constants))
                        } else {
                            Some(ConstantValueVariant::List(constants))
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
                        Some(ConstantValueVariant::Object(constant_items))
                    }
                }

                // --- Binary operations
                NodeVariant::ArithBinOp { left, operator, right } => {
                    match (eval_as_constant_variant(left), eval_as_constant_variant(right)) {
                        (
                            Some(ConstantValueVariant::Int(ref li)),
                            Some(ConstantValueVariant::Int(ref ri)),
                        ) => Some(ConstantValueVariant::Int(match operator.variant {
                            ArithOperatorVariant::Plus => li + ri,
                            ArithOperatorVariant::Minus => li - ri,
                            ArithOperatorVariant::Multiply => li * ri,
                            ArithOperatorVariant::Divide => li / ri,
                        })),
                        _ => None,
                    }
                }
                NodeVariant::LogicBinOp { left, operator, right } => {
                    match (eval_as_constant_variant(left), eval_as_constant_variant(right)) {
                        (
                            Some(ConstantValueVariant::Bool(lb)),
                            Some(ConstantValueVariant::Bool(rb)),
                        ) => Some(ConstantValueVariant::Bool(match operator.variant {
                            LogicOperatorVariant::Or => lb || rb,
                            LogicOperatorVariant::And => lb && rb,
                            LogicOperatorVariant::Not => unreachable!(),
                        })),
                        _ => None,
                    }
                }
                NodeVariant::CompBinOp { left, operator, right } => {
                    match (eval_as_constant_variant(left), eval_as_constant_variant(right)) {
                        (Some(left_variant), Some(right_variant)) => match operator.variant {
                            CompOperatorVariant::Equals => {
                                Some(ConstantValueVariant::Bool(left_variant == right_variant))
                            }
                            CompOperatorVariant::NotEquals => {
                                Some(ConstantValueVariant::Bool(left_variant != right_variant))
                            }
                            CompOperatorVariant::Greater => compare_ints_or_strings(
                                left_variant,
                                right_variant,
                                BigInt::gt,
                                String::gt,
                            ),
                            CompOperatorVariant::GreaterOrEquals => compare_ints_or_strings(
                                left_variant,
                                right_variant,
                                BigInt::ge,
                                String::ge,
                            ),
                            CompOperatorVariant::Less => compare_ints_or_strings(
                                left_variant,
                                right_variant,
                                BigInt::lt,
                                String::lt,
                            ),
                            CompOperatorVariant::LessOrEquals => compare_ints_or_strings(
                                left_variant,
                                right_variant,
                                BigInt::le,
                                String::le,
                            ),
                        },
                        _ => None,
                    }
                }
                NodeVariant::MiscBinOp { left, operator, right } => {
                    match (eval_as_constant_variant(left), eval_as_constant_variant(right)) {
                        (Some(left_variant), Some(right_variant)) => match operator.variant {
                            MiscOperatorVariant::Concat => match (left_variant, right_variant) {
                                (
                                    ConstantValueVariant::String(ls),
                                    ConstantValueVariant::String(rs),
                                ) => Some(ConstantValueVariant::String(format!("{ls}{rs}"))),
                                (
                                    ConstantValueVariant::List(ll),
                                    ConstantValueVariant::List(rl),
                                ) => {
                                    let mut constant_result =
                                        Vec::with_capacity(ll.len() + rl.len());
                                    ll.iter().for_each(|e| constant_result.push(e.clone()));
                                    rl.iter().for_each(|e| constant_result.push(e.clone()));
                                    Some(ConstantValueVariant::List(constant_result))
                                }
                                _ => None,
                            },
                        },
                        _ => None,
                    }
                }

                // --- Unary operations
                NodeVariant::ArithUnOp { operator, operand } => {
                    if let Some(ConstantValueVariant::Int(ref i)) =
                        eval_as_constant_variant(operand)
                    {
                        Some(match &operator.variant {
                            ArithOperatorVariant::Plus => ConstantValueVariant::Int(i.clone()),
                            ArithOperatorVariant::Minus => ConstantValueVariant::Int(-i),
                            _ => unreachable!(),
                        })
                    } else {
                        None
                    }
                }
                NodeVariant::LogicUnOp { operator, operand } => {
                    if let Some(ConstantValueVariant::Bool(ref b)) =
                        eval_as_constant_variant(operand)
                    {
                        Some(match &operator.variant {
                            LogicOperatorVariant::Not => ConstantValueVariant::Bool(!b),
                            _ => unreachable!(),
                        })
                    } else {
                        None
                    }
                }

                // --- Composite expressions
                NodeVariant::DottedExpr { prefix, suffix, is_safe } => eval_as_constant_variant(
                    prefix,
                )
                .and_then(|prefix_variant: ConstantValueVariant| match prefix_variant {
                    ConstantValueVariant::Null => {
                        if *is_safe {
                            Some(ConstantValueVariant::Null)
                        } else {
                            None
                        }
                    }
                    ConstantValueVariant::Object(items) => items
                        .iter()
                        .find(|(s, _)| s == &suffix.text)
                        .and_then(|(_, constant_result)| Some(constant_result.variant.clone())),
                    _ => None,
                }),
                NodeVariant::IndexExpr { indexed_val, index, is_safe } => {
                    eval_as_constant_variant(indexed_val).and_then(|indexed_val_variant| {
                        match indexed_val_variant {
                            ConstantValueVariant::Null => {
                                if *is_safe {
                                    Some(ConstantValueVariant::Null)
                                } else {
                                    None
                                }
                            }
                            ConstantValueVariant::Tuple(values)
                            | ConstantValueVariant::List(values) => eval_as_constant_variant(index)
                                .and_then(|constant_index| match constant_index {
                                    ConstantValueVariant::Int(constant_index) => {
                                        if let Ok(i) = usize::try_from(constant_index) {
                                            values.get(i - 1).map(|c| c.variant.clone()).or(
                                                if *is_safe {
                                                    Some(ConstantValueVariant::Null)
                                                } else {
                                                    None
                                                },
                                            )
                                        } else {
                                            None
                                        }
                                    }
                                    _ => None,
                                }),
                            _ => None,
                        }
                    })
                }
                NodeVariant::InClause { value, collection } => {
                    match (eval_as_constant_variant(value), eval_as_constant_variant(collection)) {
                        (
                            Some(value_constant),
                            Some(ConstantValueVariant::List(collection_elements)),
                        ) => Some(ConstantValueVariant::Bool(
                            collection_elements
                                .into_iter()
                                .find(|e| e.variant == value_constant)
                                .is_some(),
                        )),
                        _ => None,
                    }
                }

                // --- All other nodes cannot be evaluated as constant
                _ => None,
            }
        }

        // Get the constant variant from the current node and return the
        // wrapped constant value if some.
        eval_as_constant_variant(self)
            .map(|variant| ConstantValue { origin_location: self.origin_location.clone(), variant })
    }
}

/// This type represents a constant value evaluated from an intermediate tree.
#[derive(Debug, Clone)]
pub struct ConstantValue {
    pub origin_location: SourceSection,
    pub variant: ConstantValueVariant,
}

/// This enumeration represents the result of a node constant evaluation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantValueVariant {
    Null,
    Unit,
    Bool(bool),
    Int(BigInt),
    String(String),
    Tuple(Vec<ConstantValue>),
    List(Vec<ConstantValue>),
    Object(Vec<(String, ConstantValue)>),
}

impl PartialEq for ConstantValue {
    fn eq(&self, other: &Self) -> bool {
        self.variant == other.variant
    }
}

impl Eq for ConstantValue {}

impl ConstantValue {
    /// Get the complex constant representing this constant value if it one
    /// exists. Otherwise, this function returns [`None`].
    pub fn to_complex_constant(&self) -> Option<ComplexConstant> {
        match &self.variant {
            ConstantValueVariant::String(value) => Some(ComplexConstant::String(value.clone())),
            ConstantValueVariant::Tuple(constant_values)
            | ConstantValueVariant::List(constant_values) => {
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
            ConstantValueVariant::Object(items) => {
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
        match &self.variant {
            ConstantValueVariant::Int(value) => {
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
        match &self.variant {
            ConstantValueVariant::Bool(value) => {
                Some(if *value { TableConstantElement::True } else { TableConstantElement::False })
            }
            ConstantValueVariant::Int(value) => {
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
            ConstantValueVariant::String(value) => {
                Some(TableConstantElement::String(value.clone()))
            }
            _ => None,
        }
    }
}

/// Compare left and right constant values with the appropriate comparison
/// function. Return the boolean result of the comparison wrapped in a constant
/// value.
fn compare_ints_or_strings<F, G>(
    left: ConstantValueVariant,
    right: ConstantValueVariant,
    int_comp: F,
    str_comp: G,
) -> Option<ConstantValueVariant>
where
    F: Fn(&BigInt, &BigInt) -> bool,
    G: Fn(&String, &String) -> bool,
{
    match (&left, &right) {
        (ConstantValueVariant::Int(li), ConstantValueVariant::Int(ri)) => {
            Some(ConstantValueVariant::Bool(int_comp(li, ri)))
        }
        (ConstantValueVariant::String(ls), ConstantValueVariant::String(rs)) => {
            Some(ConstantValueVariant::Bool(str_comp(ls, rs)))
        }
        _ => None,
    }
}

mod tests {
    use crate::{
        intermediate_tree::{ArithOperator, CompOperator, Identifier, LogicOperator, MiscOperator},
        sources::{Location, SourceSection},
    };

    use super::*;

    fn _dummy_loc() -> SourceSection {
        SourceSection {
            source: 0,
            start: Location { line: 0, col: 0 },
            end: Location { line: 0, col: 0 },
        }
    }

    // --- Node creation helpers

    fn _new_node(variant: NodeVariant) -> Node {
        Node { origin_location: _dummy_loc(), variant }
    }

    fn _new_arith_op(variant: ArithOperatorVariant) -> ArithOperator {
        ArithOperator { origin_location: _dummy_loc(), variant }
    }

    fn _new_logic_op(variant: LogicOperatorVariant) -> LogicOperator {
        LogicOperator { origin_location: _dummy_loc(), variant }
    }

    fn _new_comp_op(variant: CompOperatorVariant) -> CompOperator {
        CompOperator { origin_location: _dummy_loc(), variant }
    }

    fn _new_misc_op(variant: MiscOperatorVariant) -> MiscOperator {
        MiscOperator { origin_location: _dummy_loc(), variant }
    }

    fn _new_id(text: &str) -> Identifier {
        Identifier { origin_location: _dummy_loc(), text: String::from(text) }
    }

    fn _bool_node(value: bool) -> Node {
        _new_node(NodeVariant::BoolLiteral(value))
    }

    fn _int_node(value: &str) -> Node {
        _new_node(NodeVariant::IntLiteral(String::from(value)))
    }

    fn _str_node(value: &str) -> Node {
        _new_node(NodeVariant::StringLiteral(String::from(value)))
    }

    // --- Constant creation helpers

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

    fn _tuple_cst(value: Vec<ConstantValue>) -> ConstantValue {
        ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::Tuple(value) }
    }

    fn _list_cst(value: Vec<ConstantValue>) -> ConstantValue {
        ConstantValue { origin_location: _dummy_loc(), variant: ConstantValueVariant::List(value) }
    }

    fn _obj_cst(value: Vec<(String, ConstantValue)>) -> ConstantValue {
        ConstantValue {
            origin_location: _dummy_loc(),
            variant: ConstantValueVariant::Object(value),
        }
    }

    #[test]
    fn test_literals_constant_evaluation() {
        // Test null literal
        let mut intermediate_tree = _new_node(NodeVariant::NullLiteral);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_null_cst()));

        // Test unit literal
        intermediate_tree = _new_node(NodeVariant::UnitLiteral);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_unit_cst()));

        // Test boolean literals
        intermediate_tree = _bool_node(false);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _bool_node(true);
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test integer literals
        intermediate_tree = _int_node("0");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("0")));
        intermediate_tree = _int_node("42");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _int_node("-42");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-42")));
        intermediate_tree =
            _int_node("100000000000000000000000000000000000000000000000000000000000000000");
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

        // Test tuple literals
        intermediate_tree = _new_node(NodeVariant::TupleLiteral(vec![
            _new_node(NodeVariant::UnitLiteral),
            _bool_node(true),
            _new_node(NodeVariant::TupleLiteral(vec![_int_node("42")])),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_tuple_cst(vec![
                _unit_cst(),
                _bool_cst(true),
                _tuple_cst(vec![_int_cst("42")])
            ]))
        );
        intermediate_tree = _new_node(NodeVariant::TupleLiteral(vec![
            _new_node(NodeVariant::UnitLiteral),
            _new_node(NodeVariant::ReadSymbol(_new_id("nope"))),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test list literals
        intermediate_tree = _new_node(NodeVariant::ListLiteral(vec![
            _new_node(NodeVariant::UnitLiteral),
            _bool_node(true),
            _new_node(NodeVariant::ListLiteral(vec![_int_node("42")])),
        ]));
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_list_cst(vec![
                _unit_cst(),
                _bool_cst(true),
                _list_cst(vec![_int_cst("42")])
            ]))
        );
        intermediate_tree = _new_node(NodeVariant::ListLiteral(vec![
            _new_node(NodeVariant::UnitLiteral),
            _new_node(NodeVariant::ReadSymbol(_new_id("nope"))),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test object literals
        intermediate_tree = _new_node(NodeVariant::ObjectLiteral(vec![
            (_new_id("a"), _new_node(NodeVariant::UnitLiteral)),
            (_new_id("b"), _bool_node(true)),
            (
                _new_id("c"),
                _new_node(NodeVariant::ObjectLiteral(vec![(_new_id("inner"), _int_node("42"))])),
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
        intermediate_tree = _new_node(NodeVariant::ObjectLiteral(vec![
            (_new_id("a"), _new_node(NodeVariant::UnitLiteral)),
            (_new_id("a"), _new_node(NodeVariant::ReadSymbol(_new_id("nope")))),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_arithmetic_binary_operations() {
        // Test additions
        let mut intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _new_arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _new_arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("40")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _new_arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_new_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("6")),
                operator: _new_arith_op(ArithOperatorVariant::Plus),
                right: Box::new(_int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("26")));

        // Test subtractions
        let mut intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _new_arith_op(ArithOperatorVariant::Minus),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("38")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _new_arith_op(ArithOperatorVariant::Minus),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("44")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _new_arith_op(ArithOperatorVariant::Minus),
            right: Box::new(_new_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("6")),
                operator: _new_arith_op(ArithOperatorVariant::Minus),
                right: Box::new(_int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("14")));

        // Test multiplications
        let mut intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _new_arith_op(ArithOperatorVariant::Multiply),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("80")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _new_arith_op(ArithOperatorVariant::Multiply),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-84")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _new_arith_op(ArithOperatorVariant::Multiply),
            right: Box::new(_new_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("6")),
                operator: _new_arith_op(ArithOperatorVariant::Multiply),
                right: Box::new(_int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("600")));

        // Test divisions
        let mut intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _new_arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("20")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _new_arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-21")));
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _new_arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_new_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("16")),
                operator: _new_arith_op(ArithOperatorVariant::Divide),
                right: Box::new(_int_node("4")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));

        // Test an invalid evaluation
        intermediate_tree = _new_node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _new_arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_str_node("not an int")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_concatenation_operations() {
        // Test string concatenation
        let mut intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_str_node(" world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));
        intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_str_node("")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_new_node(NodeVariant::MiscBinOp {
                left: Box::new(_str_node(" ")),
                operator: _new_misc_op(MiscOperatorVariant::Concat),
                right: Box::new(_str_node("world")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));

        // Test list concatenation
        intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
            ]))),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("3"),
                _int_node("4"),
            ]))),
        });
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_list_cst(
                vec![_int_cst("1"), _int_cst("2"), _int_cst("3"), _int_cst("4"),]
            ))
        );
        intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_str_node("")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_new_node(NodeVariant::MiscBinOp {
                left: Box::new(_str_node(" ")),
                operator: _new_misc_op(MiscOperatorVariant::Concat),
                right: Box::new(_str_node("world")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));

        // Test invalid concatenation
        intermediate_tree = _new_node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_int_node("40")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_binary_operations() {
        // Test the logical and
        let mut intermediate_tree = _new_node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(false)),
            operator: _new_logic_op(LogicOperatorVariant::And),
            right: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(true)),
            operator: _new_logic_op(LogicOperatorVariant::And),
            right: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test the logical or
        intermediate_tree = _new_node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(false)),
            operator: _new_logic_op(LogicOperatorVariant::Or),
            right: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(false)),
            operator: _new_logic_op(LogicOperatorVariant::Or),
            right: Box::new(_bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test invalid logical operation
        intermediate_tree = _new_node(NodeVariant::LogicBinOp {
            left: Box::new(_int_node("1")),
            operator: _new_logic_op(LogicOperatorVariant::Or),
            right: Box::new(_bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_in_clause() {
        // Test valid "in" clauses
        let mut intermediate_tree = _new_node(NodeVariant::InClause {
            value: Box::new(_int_node("2")),
            collection: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::InClause {
            value: Box::new(_int_node("4")),
            collection: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test an invalid "in" clause
        intermediate_tree = _new_node(NodeVariant::InClause {
            value: Box::new(_int_node("2")),
            collection: Box::new(_str_node("123")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_equality_operations() {
        // Test equality operations
        let mut intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Equals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_comp_op(CompOperatorVariant::Equals),
            right: Box::new(_str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_comp_op(CompOperatorVariant::Equals),
            right: Box::new(_str_node("world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("1")),
            operator: _new_comp_op(CompOperatorVariant::Equals),
            right: Box::new(_str_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            operator: _new_comp_op(CompOperatorVariant::Equals),
            right: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
                _int_node("4"),
            ]))),
            operator: _new_comp_op(CompOperatorVariant::Equals),
            right: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test inequality operations
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _new_comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_str_node("world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("1")),
            operator: _new_comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_str_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            operator: _new_comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
                _int_node("4"),
            ]))),
            operator: _new_comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
    }

    #[test]
    fn test_comparison_operations() {
        // Test "greater than" operations
        let mut intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Greater),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Greater),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Greater),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::Greater),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::Greater),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::Greater),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test "greater or equals" operations
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test "less than" operations
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Less),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Less),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::Less),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::Less),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::Less),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::Less),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test "less or equals" operations
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _new_comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _new_comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
    }

    #[test]
    fn test_arithmetic_unary_operation() {
        let mut intermediate_tree = _new_node(NodeVariant::ArithUnOp {
            operator: _new_arith_op(ArithOperatorVariant::Minus),
            operand: Box::new(_int_node("42")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-42")));
        intermediate_tree = _new_node(NodeVariant::ArithUnOp {
            operator: _new_arith_op(ArithOperatorVariant::Minus),
            operand: Box::new(_int_node("-5")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("5")));
        intermediate_tree = _new_node(NodeVariant::ArithUnOp {
            operator: _new_arith_op(ArithOperatorVariant::Minus),
            operand: Box::new(_str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_unary_operation() {
        let mut intermediate_tree = _new_node(NodeVariant::LogicUnOp {
            operator: _new_logic_op(LogicOperatorVariant::Not),
            operand: Box::new(_bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _new_node(NodeVariant::LogicUnOp {
            operator: _new_logic_op(LogicOperatorVariant::Not),
            operand: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _new_node(NodeVariant::LogicUnOp {
            operator: _new_logic_op(LogicOperatorVariant::Not),
            operand: Box::new(_int_node("42")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_dotted_access() {
        // Test dot access on object literals
        let mut intermediate_tree = _new_node(NodeVariant::DottedExpr {
            prefix: Box::new(_new_node(NodeVariant::ObjectLiteral(vec![(
                _new_id("a"),
                _int_node("42"),
            )]))),
            suffix: _new_id("a"),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _new_node(NodeVariant::DottedExpr {
            prefix: Box::new(_new_node(NodeVariant::ObjectLiteral(vec![(
                _new_id("a"),
                _int_node("42"),
            )]))),
            suffix: _new_id("b"),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test safe dot access
        intermediate_tree = _new_node(NodeVariant::DottedExpr {
            prefix: Box::new(_new_node(NodeVariant::NullLiteral)),
            suffix: _new_id("a"),
            is_safe: true,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_null_cst()));
    }

    #[test]
    fn test_indexing() {
        // Test indexing a list
        let mut intermediate_tree = _new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("2")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));
        intermediate_tree = _new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_new_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("4")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test indexing a tuple
        intermediate_tree = _new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_new_node(NodeVariant::TupleLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("2")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));
        intermediate_tree = _new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_new_node(NodeVariant::TupleLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("4")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test safe indexing
        intermediate_tree = _new_node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_str_node("hello")),
            index: Box::new(_int_node("2")),
            is_safe: false,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }
}
