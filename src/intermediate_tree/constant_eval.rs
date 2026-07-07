//! # Constant evaluation module
//!
//! This module contains all required entities to evaluate an [`Node`] as a
//! constant value.

use crate::{
    builtins::types::{BuiltinType, bool, int, list, obj, pattern, str, tuple, unit},
    bytecode::{ComplexConstant, NumericConstant, TableConstantElement},
    intermediate_tree::{
        ArithOperatorVariant, CompOperatorVariant, LogicOperatorVariant, MiscOperatorVariant, Node,
        NodeVariant,
    },
    sources::SourceSection,
};
use num_bigint::BigInt;
use regex::Regex;
use std::collections::HashMap;

impl Node {
    /// Try to evaluate this node as a constant value, returning it if this is
    /// feasible. Otherwise this function returns [`None`].
    pub fn eval_as_constant(&self) -> Option<ConstantValue> {
        /// Internal recursive function to evaluate a node as a constant
        /// variant. This is used to avoid useless wrapping during constant
        /// evaluation.
        fn eval_as_constant_variant(
            ctx: &mut EvaluationContext,
            node: &Node,
        ) -> Option<ConstantValueVariant> {
            match &node.variant {
                // --- Literals
                NodeVariant::NilLiteral => Some(ConstantValueVariant::Nil),
                NodeVariant::NullLiteral => Some(ConstantValueVariant::Null),
                NodeVariant::UnitLiteral => Some(ConstantValueVariant::Unit),
                NodeVariant::BoolLiteral(b) => Some(ConstantValueVariant::Bool(*b)),
                NodeVariant::IntLiteral(i) => {
                    Some(ConstantValueVariant::Int(i.parse::<BigInt>().unwrap()))
                }
                NodeVariant::StringLiteral(s) => Some(ConstantValueVariant::String(s.clone())),
                NodeVariant::PatternLiteral(r) => Some(ConstantValueVariant::Pattern(r.clone())),
                NodeVariant::TupleLiteral(nodes) | NodeVariant::ListLiteral(nodes) => {
                    let constants = nodes
                        .iter()
                        .filter_map(|n| inner_eval_as_constant(ctx, n))
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
                        .filter_map(|(n, v)| {
                            inner_eval_as_constant(ctx, v).map(|c| (n.text.clone(), c))
                        })
                        .collect::<Vec<_>>();
                    if constant_items.len() != items.len() {
                        None
                    } else {
                        Some(ConstantValueVariant::Object(constant_items))
                    }
                }

                // --- Binary operations
                NodeVariant::ArithBinOp { left, operator, right } => {
                    match (
                        eval_as_constant_variant(ctx, left),
                        eval_as_constant_variant(ctx, right),
                    ) {
                        (
                            Some(ConstantValueVariant::Int(ref li)),
                            Some(ConstantValueVariant::Int(ref ri)),
                        ) => {
                            let result = match operator.variant {
                                ArithOperatorVariant::Plus => Some(li + ri),
                                ArithOperatorVariant::Minus => Some(li - ri),
                                ArithOperatorVariant::Multiply => Some(li * ri),
                                ArithOperatorVariant::Divide if ri != &BigInt::ZERO => {
                                    Some(li / ri)
                                }
                                _ => None,
                            };
                            result.map(ConstantValueVariant::Int)
                        }
                        _ => None,
                    }
                }
                NodeVariant::LogicBinOp { left, operator, right } => {
                    match (
                        eval_as_constant_variant(ctx, left),
                        eval_as_constant_variant(ctx, right),
                    ) {
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
                    match (
                        eval_as_constant_variant(ctx, left),
                        eval_as_constant_variant(ctx, right),
                    ) {
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
                    match (
                        eval_as_constant_variant(ctx, left),
                        eval_as_constant_variant(ctx, right),
                    ) {
                        (
                            Some(ConstantValueVariant::String(ls)),
                            Some(ConstantValueVariant::String(rs)),
                        ) => match operator.variant {
                            MiscOperatorVariant::Concat => {
                                Some(ConstantValueVariant::String(format!("{ls}{rs}")))
                            }
                        },
                        (
                            Some(ConstantValueVariant::List(mut ll)),
                            Some(ConstantValueVariant::List(mut rl)),
                        ) => match operator.variant {
                            MiscOperatorVariant::Concat => {
                                ll.append(&mut rl);
                                Some(ConstantValueVariant::List(ll))
                            }
                        },
                        _ => None,
                    }
                }

                // --- Unary operations
                NodeVariant::ArithUnOp { operator, operand } => {
                    match eval_as_constant_variant(ctx, operand) {
                        Some(ConstantValueVariant::Int(ref i)) => Some(match &operator.variant {
                            ArithOperatorVariant::Plus => ConstantValueVariant::Int(i.clone()),
                            ArithOperatorVariant::Minus => ConstantValueVariant::Int(-i),
                            _ => unreachable!(),
                        }),
                        _ => None,
                    }
                }
                NodeVariant::LogicUnOp { operator, operand } => {
                    match eval_as_constant_variant(ctx, operand) {
                        Some(ConstantValueVariant::Bool(ref b)) => Some(match &operator.variant {
                            LogicOperatorVariant::Not => ConstantValueVariant::Bool(!b),
                            _ => unreachable!(),
                        }),
                        _ => None,
                    }
                }

                // --- Composite expressions
                NodeVariant::DottedExpr { prefix, suffix } => eval_as_constant_variant(ctx, prefix)
                    .and_then(|prefix_variant: ConstantValueVariant| match prefix_variant {
                        ConstantValueVariant::Object(items) => items
                            .iter()
                            .find(|(s, _)| s == &suffix.text)
                            .map(|(_, constant_result)| constant_result.variant.clone()),
                        _ => None,
                    }),
                NodeVariant::IndexExpr { indexed_val, index } => {
                    match (
                        eval_as_constant_variant(ctx, indexed_val),
                        eval_as_constant_variant(ctx, index),
                    ) {
                        (
                            Some(ConstantValueVariant::Tuple(values))
                            | Some(ConstantValueVariant::List(values)),
                            Some(ConstantValueVariant::Int(constant_index)),
                        ) => usize::try_from(constant_index)
                            .ok()
                            .and_then(|i| values.get(i - 1).map(|c| c.variant.clone())),
                        _ => None,
                    }
                }
                NodeVariant::InClause { value, collection } => {
                    match (
                        eval_as_constant_variant(ctx, value),
                        eval_as_constant_variant(ctx, collection),
                    ) {
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
                NodeVariant::IfExpr { condition, consequence, alternative } => {
                    match eval_as_constant_variant(ctx, condition) {
                        Some(ConstantValueVariant::Bool(cond_value)) => {
                            if cond_value {
                                eval_as_constant_variant(ctx, consequence)
                            } else {
                                eval_as_constant_variant(ctx, alternative)
                            }
                        }
                        _ => None,
                    }
                }

                // --- Let-in
                NodeVariant::Let { id, value, r#in } => {
                    if let Some(value_cst) = inner_eval_as_constant(ctx, value) {
                        ctx.let_bindings.insert(*id, value_cst);
                    }
                    eval_as_constant_variant(ctx, r#in)
                }
                NodeVariant::Read(id) => ctx.let_bindings.get(id).map(|c| c.variant.clone()),

                // --- Type checking
                NodeVariant::InstanceOf { expression, expected_type_tag } => {
                    match inner_eval_as_constant(ctx, expression) {
                        Some(cst) => cst
                            .constant_type()
                            .map(|t| ConstantValueVariant::Bool(&t.tag == expected_type_tag)),
                        _ => None,
                    }
                }

                // --- All other nodes cannot be evaluated as constant
                _ => None,
            }
        }

        /// Inner function to evaluate a node as a constant value in an
        /// evaluation context.
        fn inner_eval_as_constant(
            ctx: &mut EvaluationContext,
            node: &Node,
        ) -> Option<ConstantValue> {
            // Get the constant variant from the current node and return the
            // wrapped constant value if some.
            eval_as_constant_variant(ctx, node)
                .map(|variant| ConstantValue { origin_location: node.origin_location, variant })
        }

        // Call the inner function with an empty context
        let mut evaluation_context = EvaluationContext { let_bindings: HashMap::new() };
        inner_eval_as_constant(&mut evaluation_context, self)
    }
}

/// Context used to evaluate an expression.
struct EvaluationContext {
    let_bindings: HashMap<usize, ConstantValue>,
}

/// This type represents a constant value evaluated from an intermediate tree.
#[derive(Debug, Clone, Eq)]
pub struct ConstantValue {
    pub origin_location: SourceSection,
    pub variant: ConstantValueVariant,
}

/// This enumeration represents the result of a node constant evaluation.
#[derive(Debug, Clone)]
pub enum ConstantValueVariant {
    Nil,
    Null,
    Unit,
    Bool(bool),
    Int(BigInt),
    String(String),
    Pattern(Regex),
    Tuple(Vec<ConstantValue>),
    List(Vec<ConstantValue>),
    Object(Vec<(String, ConstantValue)>),
}

impl PartialEq for ConstantValue {
    fn eq(&self, other: &Self) -> bool {
        self.variant == other.variant
    }
}

impl PartialEq for ConstantValueVariant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) | (Self::Null, Self::Null) | (Self::Unit, Self::Unit) => true,
            (Self::Bool(l), Self::Bool(r)) => l == r,
            (Self::Int(l), Self::Int(r)) => l == r,
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Pattern(l), Self::Pattern(r)) => l.as_str() == r.as_str(),
            (Self::Tuple(l), Self::Tuple(r)) => l == r,
            (Self::List(l), Self::List(r)) => l == r,
            (Self::Object(l), Self::Object(r)) => l == r,
            _ => false,
        }
    }
}

impl Eq for ConstantValueVariant {}

impl ConstantValue {
    /// Get the string representation of this constant value.
    pub fn to_string(&self) -> String {
        fn img(value: &ConstantValue) -> String {
            match &value.variant {
                ConstantValueVariant::String(s) => format!("\"{}\"", s),
                _ => value.to_string(),
            }
        }
        match &self.variant {
            ConstantValueVariant::Nil => String::from("nil"),
            ConstantValueVariant::Null => String::from("null"),
            ConstantValueVariant::Unit => String::from("()"),
            ConstantValueVariant::Bool(b) => b.to_string(),
            ConstantValueVariant::Int(big_int) => big_int.to_str_radix(10),
            ConstantValueVariant::String(s) => s.clone(),
            ConstantValueVariant::Pattern(r) => String::from(r.as_str()),
            ConstantValueVariant::Tuple(constant_values)
            | ConstantValueVariant::List(constant_values) => {
                let (ls, rs) = match &self.variant {
                    ConstantValueVariant::Tuple(_) => ('(', ')'),
                    ConstantValueVariant::List(_) => ('[', ']'),
                    _ => unreachable!(),
                };
                format!(
                    "{ls}{}{rs}",
                    constant_values
                        .iter()
                        .map(Self::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ConstantValueVariant::Object(items) => {
                format!(
                    "{{{}}}",
                    items
                        .iter()
                        .map(|(key, val)| format!("\"{key}\": {}", img(val)))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }

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
                    let mut le_bytes = [if value < &BigInt::ZERO { 0xFF_u8 } else { 0_u8 }; 4];
                    value
                        .to_signed_bytes_le()
                        .iter()
                        .enumerate()
                        .for_each(|(i, b)| le_bytes[i] = *b);
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
                if value >= &BigInt::from(i32::MIN) && value <= &BigInt::from(i32::MAX) {
                    let mut le_bytes = [if value < &BigInt::ZERO { 0xFF_u8 } else { 0_u8 }; 4];
                    value
                        .to_signed_bytes_le()
                        .iter()
                        .enumerate()
                        .for_each(|(i, b)| le_bytes[i] = *b);
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

    /// Get the type of the constant value if possible.
    pub fn constant_type(&self) -> Option<&'static BuiltinType> {
        match self.variant {
            ConstantValueVariant::Unit => Some(&unit::TYPE),
            ConstantValueVariant::Bool(_) => Some(&bool::TYPE),
            ConstantValueVariant::Int(_) => Some(&int::TYPE),
            ConstantValueVariant::String(_) => Some(&str::TYPE),
            ConstantValueVariant::Pattern(_) => Some(&pattern::TYPE),
            ConstantValueVariant::Tuple(_) => Some(&tuple::TYPE),
            ConstantValueVariant::List(_) => Some(&list::TYPE),
            ConstantValueVariant::Object(_) => Some(&obj::TYPE),
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

#[allow(unused_imports)]
mod tests {
    use super::*;
    use crate::{
        builtins::{
            traits::{indexable, iterable},
            types::{bool, int, str},
        },
        intermediate_tree::{
            self, ArithOperator, CompOperator, Identifier, LogicOperator, MiscOperator,
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

    fn _id(text: &str) -> Identifier {
        Identifier::new(_dummy_loc(), String::from(text))
    }

    fn _bool_node(value: bool) -> Node {
        _node(NodeVariant::BoolLiteral(value))
    }

    fn _int_node(value: &str) -> Node {
        _node(NodeVariant::IntLiteral(String::from(value)))
    }

    fn _str_node(value: &str) -> Node {
        _node(NodeVariant::StringLiteral(String::from(value)))
    }

    fn _pattern_node(regex: &str) -> Node {
        _node(NodeVariant::PatternLiteral(Regex::new(regex).unwrap()))
    }

    fn _read_symbol_node(value: &str) -> Node {
        _node(NodeVariant::ReadSymbol(_id(value)))
    }

    fn _read_node(id: usize) -> Node {
        _node(NodeVariant::Read(id))
    }

    fn _if_node(cond: Node, consequence: Node, alternative: Node) -> Node {
        _node(NodeVariant::IfExpr {
            condition: Box::new(cond),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        })
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
        ConstantValue {
            origin_location: _dummy_loc(),
            variant: ConstantValueVariant::Object(value),
        }
    }

    #[test]
    fn test_literals_constant_evaluation() {
        // Test nil literal
        let mut intermediate_tree = _node(NodeVariant::NilLiteral);
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

        // Test pattern literals
        intermediate_tree = _pattern_node("");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_pattern_cst("")));
        intermediate_tree = _pattern_node("my_regex");
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_pattern_cst("my_regex")));

        // Test tuple literals
        intermediate_tree = _node(NodeVariant::TupleLiteral(vec![
            _node(NodeVariant::UnitLiteral),
            _bool_node(true),
            _node(NodeVariant::TupleLiteral(vec![_int_node("42")])),
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
            _node(NodeVariant::UnitLiteral),
            _read_symbol_node("nope"),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test list literals
        intermediate_tree = _node(NodeVariant::ListLiteral(vec![
            _node(NodeVariant::UnitLiteral),
            _bool_node(true),
            _node(NodeVariant::ListLiteral(vec![_int_node("42")])),
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
            _node(NodeVariant::UnitLiteral),
            _read_symbol_node("nope"),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test object literals
        intermediate_tree = _node(NodeVariant::ObjectLiteral(vec![
            (_id("a"), _node(NodeVariant::UnitLiteral)),
            (_id("b"), _bool_node(true)),
            (
                _id("c"),
                _node(NodeVariant::ObjectLiteral(vec![(_id("inner"), _int_node("42"))])),
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
            (_id("a"), _node(NodeVariant::UnitLiteral)),
            (_id("a"), _read_symbol_node("nope")),
        ]));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_arithmetic_binary_operations() {
        // Test additions
        let mut intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("40")));

        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("2")),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_int_node("-5")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-3")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("6")),
                operator: _arith_op(ArithOperatorVariant::Plus),
                right: Box::new(_int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("26")));

        // Test subtractions
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _arith_op(ArithOperatorVariant::Minus),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("38")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _arith_op(ArithOperatorVariant::Minus),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("44")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _arith_op(ArithOperatorVariant::Minus),
            right: Box::new(_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("6")),
                operator: _arith_op(ArithOperatorVariant::Minus),
                right: Box::new(_int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("14")));

        // Test multiplications
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _arith_op(ArithOperatorVariant::Multiply),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("80")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _arith_op(ArithOperatorVariant::Multiply),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-84")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _arith_op(ArithOperatorVariant::Multiply),
            right: Box::new(_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("6")),
                operator: _arith_op(ArithOperatorVariant::Multiply),
                right: Box::new(_int_node("10")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("600")));

        // Test divisions
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("20")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("42")),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_int_node("-2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-21")));
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("10")),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_node(NodeVariant::ArithBinOp {
                left: Box::new(_int_node("16")),
                operator: _arith_op(ArithOperatorVariant::Divide),
                right: Box::new(_int_node("4")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));

        // Test an invalid evaluation
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _arith_op(ArithOperatorVariant::Plus),
            right: Box::new(_str_node("not an int")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _node(NodeVariant::ArithBinOp {
            left: Box::new(_int_node("40")),
            operator: _arith_op(ArithOperatorVariant::Divide),
            right: Box::new(_int_node("0")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_concatenation_operations() {
        // Test string concatenation
        let mut intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_str_node(" world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_str_node("")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_node(NodeVariant::MiscBinOp {
                left: Box::new(_str_node(" ")),
                operator: _misc_op(MiscOperatorVariant::Concat),
                right: Box::new(_str_node("world")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));

        // Test list concatenation
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_node(NodeVariant::ListLiteral(vec![_int_node("1"), _int_node("2")]))),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_node(NodeVariant::ListLiteral(vec![_int_node("3"), _int_node("4")]))),
        });
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_list_cst(
                vec![_int_cst("1"), _int_cst("2"), _int_cst("3"), _int_cst("4"),]
            ))
        );
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_str_node("")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_node(NodeVariant::MiscBinOp {
                left: Box::new(_str_node(" ")),
                operator: _misc_op(MiscOperatorVariant::Concat),
                right: Box::new(_str_node("world")),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello world")));

        // Test invalid concatenation
        intermediate_tree = _node(NodeVariant::MiscBinOp {
            left: Box::new(_str_node("hello")),
            operator: _misc_op(MiscOperatorVariant::Concat),
            right: Box::new(_int_node("40")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_binary_operations() {
        // Test the logical and
        let mut intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(false)),
            operator: _logic_op(LogicOperatorVariant::And),
            right: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(true)),
            operator: _logic_op(LogicOperatorVariant::And),
            right: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test the logical or
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(false)),
            operator: _logic_op(LogicOperatorVariant::Or),
            right: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: Box::new(_bool_node(false)),
            operator: _logic_op(LogicOperatorVariant::Or),
            right: Box::new(_bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test invalid logical operation
        intermediate_tree = _node(NodeVariant::LogicBinOp {
            left: Box::new(_int_node("1")),
            operator: _logic_op(LogicOperatorVariant::Or),
            right: Box::new(_bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_in_clause() {
        // Test valid "in" clauses
        let mut intermediate_tree = _node(NodeVariant::InClause {
            value: Box::new(_int_node("2")),
            collection: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::InClause {
            value: Box::new(_int_node("4")),
            collection: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test an invalid "in" clause
        intermediate_tree = _node(NodeVariant::InClause {
            value: Box::new(_int_node("2")),
            collection: Box::new(_str_node("123")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_equality_operations() {
        // Test equality operations
        let mut intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: Box::new(_str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: Box::new(_str_node("world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("1")),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: Box::new(_str_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
                _int_node("4"),
            ]))),
            operator: _comp_op(CompOperatorVariant::Equals),
            right: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test inequality operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("hello")),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_str_node("world")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("1")),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_str_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
                _int_node("4"),
            ]))),
            operator: _comp_op(CompOperatorVariant::NotEquals),
            right: Box::new(_node(NodeVariant::ListLiteral(vec![
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
        let mut intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::Greater),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test "greater or equals" operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::GreaterOrEquals),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));

        // Test "less than" operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Less),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Less),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::Less),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::Less),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::Less),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::Less),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        // Test "less or equals" operations
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_int_node("1")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_int_node("2")),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_int_node("3")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));

        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_str_node("a")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_str_node("b")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::CompBinOp {
            left: Box::new(_str_node("b")),
            operator: _comp_op(CompOperatorVariant::LessOrEquals),
            right: Box::new(_str_node("c")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
    }

    #[test]
    fn test_arithmetic_unary_operation() {
        let mut intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: _arith_op(ArithOperatorVariant::Minus),
            operand: Box::new(_int_node("42")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("-42")));
        intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: _arith_op(ArithOperatorVariant::Minus),
            operand: Box::new(_int_node("-5")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("5")));
        intermediate_tree = _node(NodeVariant::ArithUnOp {
            operator: _arith_op(ArithOperatorVariant::Minus),
            operand: Box::new(_str_node("hello")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_logical_unary_operation() {
        let mut intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: _logic_op(LogicOperatorVariant::Not),
            operand: Box::new(_bool_node(false)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: _logic_op(LogicOperatorVariant::Not),
            operand: Box::new(_bool_node(true)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::LogicUnOp {
            operator: _logic_op(LogicOperatorVariant::Not),
            operand: Box::new(_int_node("42")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_dotted_access() {
        // Test dot access on object literals
        let mut intermediate_tree = _node(NodeVariant::DottedExpr {
            prefix: Box::new(_node(NodeVariant::ObjectLiteral(vec![(_id("a"), _int_node("42"))]))),
            suffix: _id("a"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _node(NodeVariant::DottedExpr {
            prefix: Box::new(_node(NodeVariant::ObjectLiteral(vec![(_id("a"), _int_node("42"))]))),
            suffix: _id("b"),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_indexing() {
        // Test indexing a list
        let mut intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("4")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test indexing a tuple
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_node(NodeVariant::TupleLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("2")));
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_node(NodeVariant::TupleLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            index: Box::new(_int_node("4")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);

        // Test safe indexing
        intermediate_tree = _node(NodeVariant::IndexExpr {
            indexed_val: Box::new(_str_node("hello")),
            index: Box::new(_int_node("2")),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_if_expr() {
        let mut intermediate_tree = _if_node(_bool_node(true), _int_node("42"), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _if_node(_bool_node(false), _int_node("42"), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree =
            _if_node(_read_symbol_node("nope"), _int_node("42"), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree =
            _if_node(_bool_node(true), _read_symbol_node("nope"), _str_node("hello"));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _if_node(_bool_node(false), _int_node("42"), _read_symbol_node("nope"));
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_let_in() {
        let mut intermediate_tree = _node(NodeVariant::Let {
            id: 0,
            value: Box::new(_int_node("42")),
            r#in: Box::new(_read_node(0)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_int_cst("42")));
        intermediate_tree = _node(NodeVariant::Let {
            id: 0,
            value: Box::new(_bool_node(true)),
            r#in: Box::new(_read_node(0)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::Let {
            id: 0,
            value: Box::new(_str_node("hello")),
            r#in: Box::new(_node(NodeVariant::Let {
                id: 1,
                value: Box::new(_int_node("42")),
                r#in: Box::new(_read_node(0)),
            })),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _node(NodeVariant::Let {
            id: 0,
            value: Box::new(_int_node("42")),
            r#in: Box::new(_read_node(1)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _node(NodeVariant::Let {
            id: 0,
            value: Box::new(_read_symbol_node("nope")),
            r#in: Box::new(_read_node(0)),
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_instance_of() {
        let mut intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: Box::new(_bool_node(false)),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(true)));
        intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: Box::new(_str_node("hello")),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::InstanceOf {
            expression: Box::new(_read_symbol_node("x")),
            expected_type_tag: bool::TYPE.tag,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_type_requirement() {
        let mut intermediate_tree = _node(NodeVariant::RequireType {
            expression: Box::new(_bool_node(false)),
            expected_type: &bool::TYPE,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_bool_cst(false)));
        intermediate_tree = _node(NodeVariant::RequireType {
            expression: Box::new(_str_node("hello")),
            expected_type: &str::TYPE,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), Some(_str_cst("hello")));
        intermediate_tree = _node(NodeVariant::RequireType {
            expression: Box::new(_str_node("hello")),
            expected_type: &int::TYPE,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _node(NodeVariant::RequireType {
            expression: Box::new(_read_node("x")),
            expected_type: &int::TYPE,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }

    #[test]
    fn test_trait_requirement() {
        let mut intermediate_tree = _node(NodeVariant::RequireTrait {
            expression: Box::new(_node(NodeVariant::TupleLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            required_trait: &indexable::TRAIT,
        });
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_tuple_cst(vec![_int_cst("1"), _int_cst("2"), _int_cst("3")]))
        );
        intermediate_tree = _node(NodeVariant::RequireTrait {
            expression: Box::new(_node(NodeVariant::ListLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            required_trait: &iterable::TRAIT,
        });
        assert_eq!(
            intermediate_tree.eval_as_constant(),
            Some(_list_cst(vec![_int_cst("1"), _int_cst("2"), _int_cst("3")]))
        );
        intermediate_tree = _node(NodeVariant::RequireTrait {
            expression: Box::new(_node(NodeVariant::TupleLiteral(vec![
                _int_node("1"),
                _int_node("2"),
                _int_node("3"),
            ]))),
            required_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
        intermediate_tree = _node(NodeVariant::RequireTrait {
            expression: Box::new(_read_node("x")),
            required_trait: &iterable::TRAIT,
        });
        assert_eq!(intermediate_tree.eval_as_constant(), None);
    }
}
