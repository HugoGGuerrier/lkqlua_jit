//! # Intermediate tree static evaluation module
//!
//! This module host all support and entry points to perform static evaluation
//! on an intermediate tree.

use crate::{
    builtins::types::{
        BuiltinType, bool, function, int, list, obj, pattern, str, stream, tuple, unit,
    },
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

mod test;

impl Node {
    /// Try to deduct the type of the expression represented by the node.
    /// Returns [`None`] if this isn't possible to determine it.
    pub fn expr_type(&self) -> Option<&'static BuiltinType> {
        // Create a typing context and call the inner function
        let mut typing_context = EvaluationContext { let_bindings: HashMap::new() };
        self.expr_type_with_context(&mut typing_context)
    }

    /// Inner function to get the type of an intermediate node in an evaluation
    /// context.
    fn expr_type_with_context<'a: 'b, 'b: 'a>(
        &'a self,
        ctx: &mut EvaluationContext<'b>,
    ) -> Option<&'static BuiltinType> {
        match &self.variant {
            // --- Composite expression
            NodeVariant::InClause { .. } => Some(&bool::TYPE),
            NodeVariant::BlockExpr { val, .. } => val.expr_type_with_context(ctx),
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
            NodeVariant::InLexicalScope { expr, .. } => expr.expr_type_with_context(ctx),
            NodeVariant::OutsideLexicalScope(expr) => expr.expr_type_with_context(ctx),

            // --- Let-in
            NodeVariant::Let { id, value, r#in } => {
                ctx.let_bindings.insert(*id, value);
                r#in.expr_type_with_context(ctx)
            }
            NodeVariant::Read(id) => {
                if let Some(n) = ctx.let_bindings.get(id) {
                    n.expr_type_with_context(ctx)
                } else {
                    None
                }
            }

            // --- Type checking
            NodeVariant::InstanceOf { .. } => Some(&bool::TYPE),
            NodeVariant::HasTrait { .. } => Some(&bool::TYPE),

            // --- Literals
            NodeVariant::UnitLiteral => Some(&unit::TYPE),
            NodeVariant::BoolLiteral(_) => Some(&bool::TYPE),
            NodeVariant::IntLiteral(_) => Some(&int::TYPE),
            NodeVariant::StringLiteral(_) => Some(&str::TYPE),
            NodeVariant::PatternLiteral(_) => Some(&pattern::TYPE),
            NodeVariant::TupleLiteral(_) => Some(&tuple::TYPE),
            NodeVariant::ListLiteral(_) => Some(&list::TYPE),
            NodeVariant::ObjectLiteral(_) => Some(&obj::TYPE),
            NodeVariant::ReadChildUnit(_) => Some(&function::TYPE),

            // --- Default case, no type can be deducted
            _ => None,
        }
    }

    /// Try to evaluate this node as a constant value, returning it if this is
    /// feasible. Otherwise this function returns [`None`].
    pub fn eval_as_constant(&self) -> Option<ConstantValue> {
        // Call the inner function with an empty context
        let mut evaluation_context = EvaluationContext { let_bindings: HashMap::new() };
        self.eval_as_constant_with_context(&mut evaluation_context)
    }

    fn eval_as_constant_with_context<'a: 'b, 'b: 'a>(
        &'a self,
        ctx: &mut EvaluationContext<'b>,
    ) -> Option<ConstantValue> {
        /// Internal recursive function to evaluate a node as a constant
        /// variant. This is used to avoid useless wrapping during constant
        /// evaluation.
        fn eval_as_constant_variant<'a: 'b, 'b: 'a>(
            ctx: &mut EvaluationContext<'b>,
            node: &'a Node,
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
                        .filter_map(|n| n.eval_as_constant_with_context(ctx))
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
                            v.eval_as_constant_with_context(ctx)
                                .map(|c| (n.text.clone(), c))
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
                    ctx.let_bindings.insert(*id, value);
                    eval_as_constant_variant(ctx, r#in)
                }
                NodeVariant::Read(id) => {
                    if let Some(n) = ctx.let_bindings.get(id) {
                        eval_as_constant_variant(ctx, n)
                    } else {
                        None
                    }
                }

                // --- Type checking
                NodeVariant::InstanceOf { expression, expected_type_tag } => expression
                    .expr_type_with_context(ctx)
                    .map(|t| ConstantValueVariant::Bool(&t.tag == expected_type_tag)),
                NodeVariant::HasTrait { expression, expected_trait } => {
                    expression.expr_type_with_context(ctx).map(|t| {
                        ConstantValueVariant::Bool(t.traits.iter().any(|t| t == expected_trait))
                    })
                }

                // --- All other nodes cannot be evaluated as constant
                _ => None,
            }
        }

        // Get the constant variant from the current node and return the
        // wrapped constant value if some.
        eval_as_constant_variant(ctx, self)
            .map(|variant| ConstantValue { origin_location: self.origin_location, variant })
    }
}

/// Context used to evaluate an expression.
struct EvaluationContext<'a> {
    let_bindings: HashMap<usize, &'a Node>,
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
                    let max_f64 = BigInt::from(2).pow(1024);
                    if value <= &max_f64 && value >= &(-max_f64) {
                        Some(NumericConstant::Float(value.to_string().parse().unwrap()))
                    } else {
                        None
                    }
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
