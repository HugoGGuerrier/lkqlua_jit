//! This module contains all required operations to lower an LKQL parse source
//! to an intermediate representation.

use std::{collections::HashMap, path::PathBuf};

use crate::{
    errors::{POS_AFTER_NAMED_ARGUMENT, PREVIOUS_NAMED_ARG_HINT},
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperator, LogicOperatorVariant, MiscOperator,
        MiscOperatorVariant, Node, NodeVariant,
    },
    report::{Hint, Report},
    sources::{SourceId, SourceSection},
};

use liblkqllang::{BaseFunction, LkqlNode};

impl ExecutionUnit {
    /// Lower the provided LKQL node as an intermediate [`ExecutionUnit`]. The
    /// provided node MUST be one of the following variants:
    ///   * [`LkqlNode::TopLevelList`]
    ///   * [`LkqlNode::FunDecl`]
    ///   * [`LkqlNode::AnonymousFunction`]
    ///
    /// If there is errors during the lowering of LKQL source, this function
    /// returns a [`Result::Err`] which contains all diagnostics.
    pub fn lower_lkql_node(source: SourceId, node: &LkqlNode) -> Result<Self, Report> {
        let mut lowering_context = LoweringContext::new(source);
        let res = Self::internal_lower_lkql_node(node, &mut lowering_context)?;
        if lowering_context.diagnostics.is_empty() {
            Ok(res)
        } else {
            Err(Report::Composed(lowering_context.diagnostics))
        }
    }

    /// Internal function to lower an [`LkqlNode`] to an [`ExecutionUnit`].
    fn internal_lower_lkql_node(
        node: &LkqlNode,
        ctx: &mut LoweringContext,
    ) -> Result<Self, Report> {
        // Iterate over all children execution units to lower them and to
        // associate each one to an index in the children units vector.
        // This needs to be done before node lowering.
        let mut local_units = Vec::new();
        let mut local_unit_counter = 0;
        let mut children_units = Vec::new();
        all_local_execution_units(node, &mut local_units)?;
        for unit in &local_units {
            ctx.child_index_map.insert(unit.clone(), local_unit_counter);
            local_unit_counter += 1;
            assert!(local_unit_counter < u16::MAX, "Too many children execution units");
            children_units.push(Self::internal_lower_lkql_node(&unit, ctx)?);
        }

        // Create the variant part of the result
        let (variant, name) = match &node {
            LkqlNode::TopLevelList(top_level) => {
                // Lower the top level elements
                let mut module_elements = Vec::new();
                for maybe_top_level_elem in top_level {
                    if let Some(top_level_elem) = maybe_top_level_elem? {
                        module_elements.push(Node::lower_lkql_node(&top_level_elem, ctx)?);
                    }
                }

                // Get the module name from the node unit's filename
                let unit_path = PathBuf::from(top_level.unit()?.unwrap().filename()?);

                // Create the resulting module
                (
                    ExecutionUnitVariant::Module {
                        symbols: all_local_symbols(node, ctx)?,
                        elements: module_elements,
                    },
                    unit_path.file_stem().unwrap().to_string_lossy().to_string(),
                )
            }
            LkqlNode::FunDecl(_) | LkqlNode::AnonymousFunction(_) => {
                // Get the function name, parameters and body nodes
                let (name, params_node, body_node) = match &node {
                    LkqlNode::FunDecl(fd) => match fd.f_fun_expr()? {
                        LkqlNode::NamedFunction(nf) => {
                            (fd.f_name()?.text()?, nf.f_parameters()?, nf.f_body_expr()?)
                        }
                        _ => unreachable!(),
                    },
                    LkqlNode::AnonymousFunction(af) => {
                        (ctx.get_lambda_name(&node).clone(), af.f_parameters()?, af.f_body_expr()?)
                    }
                    _ => unreachable!(),
                };

                // Then lower the function parameters
                let mut params = Vec::new();
                for maybe_param_decl in &params_node {
                    match maybe_param_decl? {
                        Some(LkqlNode::ParameterDecl(pd)) => {
                            let name = Identifier::from_lkql_node(&pd.f_param_identifier()?, ctx)?;
                            let default_expr = if let Some(n) = pd.f_default_expr()? {
                                Some(Node::lower_lkql_node(&n, ctx)?)
                            } else {
                                None
                            };
                            params.push((name, default_expr))
                        }
                        _ => unreachable!(),
                    }
                }

                // Then create the function execution unit variant
                (
                    ExecutionUnitVariant::Function {
                        params,
                        body: Node::lower_lkql_node(&body_node, ctx)?,
                    },
                    name,
                )
            }
            _ => unreachable!(),
        };

        // Finally return the new execution unit
        Ok(ExecutionUnit {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            name,
            children_units,
            variant,
        })
    }
}

impl Node {
    /// Lower an LKQL node as an intermediate node. All LKQL node kinds should
    /// be accepted by this function.
    fn lower_lkql_node(node: &LkqlNode, ctx: &mut LoweringContext) -> Result<Self, Report> {
        // Lower the node
        let variant = match node {
            // --- Declarations
            LkqlNode::ValDecl(vd) => NodeVariant::InitLocal {
                symbol: Identifier::from_lkql_node(&vd.f_identifier()?, ctx)?,
                val: Box::new(Self::lower_lkql_node(&vd.f_value()?, ctx)?),
            },
            LkqlNode::FunDecl(_) | LkqlNode::SelectorDecl(_) => {
                let id_node = match node {
                    LkqlNode::FunDecl(fd) => fd.f_name()?,
                    LkqlNode::SelectorDecl(sd) => sd.f_name()?,
                    _ => unreachable!(),
                };
                NodeVariant::InitLocalFun {
                    symbol: Identifier::from_lkql_node(&id_node, ctx)?,
                    child_index: *ctx.child_index_map.get(node).unwrap(),
                }
            }

            // --- Function call
            LkqlNode::FunCall(fc) => {
                // Create the argument vectors
                let mut positional_args: Vec<Node> = Vec::new();
                let mut named_args: Vec<(Identifier, Node)> = Vec::new();
                for maybe_arg in fc.f_arguments()?.children_iter()? {
                    if let Some(ref arg) = maybe_arg? {
                        match arg {
                            LkqlNode::ExprArg(ea) => {
                                // Ensure that no named arguments have been
                                // lowered yet.
                                if named_args.is_empty() {
                                    positional_args
                                        .push(Self::lower_lkql_node(&ea.f_value_expr()?, ctx)?);
                                } else {
                                    let (last_id, last_node) = named_args.last().unwrap();
                                    ctx.diagnostics
                                        .push(Report::from_error_template_with_hints::<&str>(
                                            &SourceSection::from_lkql_node(
                                                ctx.lowered_source,
                                                arg,
                                            )?,
                                            &POS_AFTER_NAMED_ARGUMENT,
                                            &vec![],
                                            vec![Hint::new(
                                                PREVIOUS_NAMED_ARG_HINT,
                                                &SourceSection::range(
                                                    &last_id.origin_location,
                                                    &last_node.origin_location,
                                                )?,
                                            )],
                                        ));
                                }
                            }
                            LkqlNode::NamedArg(na) => named_args.push((
                                Identifier::from_lkql_node(&na.f_arg_name()?, ctx)?,
                                Self::lower_lkql_node(&na.f_value_expr()?, ctx)?,
                            )),
                            _ => unreachable!(),
                        }
                    }
                }

                // Return the resulting node variant
                NodeVariant::FunCall {
                    callee: Box::new(Self::lower_lkql_node(&fc.f_name()?, ctx)?),
                    positional_args: positional_args,
                    named_args: named_args,
                }
            }

            // --- Dotted expression
            LkqlNode::DotAccess(_) | LkqlNode::SafeAccess(_) => {
                let (receiver, member, is_safe) = match node {
                    LkqlNode::DotAccess(da) => (da.f_receiver(), da.f_member(), false),
                    LkqlNode::SafeAccess(sa) => (sa.f_receiver(), sa.f_member(), true),
                    _ => unreachable!(),
                };
                NodeVariant::DottedExpr {
                    prefix: Box::new(Self::lower_lkql_node(&receiver?, ctx)?),
                    suffix: Identifier::from_lkql_node(&member?, ctx)?,
                    is_safe,
                }
            }

            // --- Index expression
            LkqlNode::Indexing(_) | LkqlNode::SafeIndexing(_) => {
                let (coll_expr, index, is_safe) = match node {
                    LkqlNode::Indexing(i) => (i.f_collection_expr(), i.f_index_expr(), false),
                    LkqlNode::SafeIndexing(si) => (si.f_collection_expr(), si.f_index_expr(), true),
                    _ => unreachable!(),
                };
                NodeVariant::IndexExpr {
                    indexed_val: Box::new(Self::lower_lkql_node(&coll_expr?, ctx)?),
                    index: Box::new(Self::lower_lkql_node(&index?, ctx)?),
                    is_safe,
                }
            }

            // --- In clause
            LkqlNode::InClause(ic) => NodeVariant::InClause {
                value: Box::new(Self::lower_lkql_node(&ic.f_value_expr()?, ctx)?),
                collection: Box::new(Self::lower_lkql_node(&ic.f_list_expr()?, ctx)?),
            },

            // --- If expression
            LkqlNode::CondExpr(ce) => NodeVariant::IfExpr {
                condition: Box::new(Self::lower_lkql_node(&ce.f_condition()?, ctx)?),
                consequence: Box::new(Self::lower_lkql_node(&ce.f_then_expr()?, ctx)?),
                alternative: ce
                    .f_else_expr()?
                    .map(|n| Self::lower_lkql_node(&n, ctx))
                    .transpose()?
                    .map_or(
                        Box::new(Node {
                            origin_location: SourceSection::from_lkql_node(
                                ctx.lowered_source,
                                node,
                            )?,
                            variant: NodeVariant::BoolLiteral(true),
                        }),
                        |n| Box::new(n),
                    ),
            },

            // --- Block expression
            LkqlNode::BlockExpr(be) => {
                let body_node = be.f_body()?;
                let mut body = Vec::with_capacity(body_node.children_count()?);
                for maybe_body_part_node in &body_node {
                    if let Some(ref body_part_node) = maybe_body_part_node? {
                        body.push(Self::lower_lkql_node(body_part_node, ctx)?);
                    }
                }
                NodeVariant::BlockExpr {
                    local_symbols: all_local_symbols(node, ctx)?,
                    body,
                    val: Box::new(Self::lower_lkql_node(&be.f_expr()?, ctx)?),
                }
            }
            LkqlNode::BlockBodyDecl(bbd) => return Self::lower_lkql_node(&bbd.f_decl()?, ctx),
            LkqlNode::BlockBodyExpr(bbe) => return Self::lower_lkql_node(&bbe.f_expr()?, ctx),

            // --- Binary operation
            LkqlNode::ArithBinOp(bo) => NodeVariant::ArithBinOp {
                left: Box::new(Self::lower_lkql_node(&bo.f_left()?, ctx)?),
                operator: ArithOperator::lower_lkql_node(&bo.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(&bo.f_right()?, ctx)?),
            },
            LkqlNode::RelBinOp(bo) => NodeVariant::CompBinOp {
                left: Box::new(Self::lower_lkql_node(&bo.f_left()?, ctx)?),
                operator: CompOperator::lower_lkql_node(&bo.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(&bo.f_right()?, ctx)?),
            },
            LkqlNode::BinOp(bo) => {
                let operator_node = bo.f_op()?;
                let left = Box::new(Self::lower_lkql_node(&bo.f_left()?, ctx)?);
                let right = Box::new(Self::lower_lkql_node(&bo.f_right()?, ctx)?);
                match &operator_node {
                    LkqlNode::OpAnd(_) | LkqlNode::OpOr(_) | LkqlNode::OpNot(_) => {
                        NodeVariant::LogicBinOp {
                            left,
                            operator: LogicOperator::lower_lkql_node(&operator_node, ctx)?,
                            right,
                        }
                    }
                    LkqlNode::OpConcat(_) => NodeVariant::MiscBinOp {
                        left,
                        operator: MiscOperator::lower_lkql_node(&operator_node, ctx)?,
                        right,
                    },
                    _ => unreachable!(),
                }
            }

            // --- Unary operation
            LkqlNode::UnOp(uo) => {
                let operand = Box::new(Self::lower_lkql_node(&uo.f_operand()?, ctx)?);
                let operator_node = uo.f_op()?;
                match &operator_node {
                    LkqlNode::OpPlus(_) | LkqlNode::OpMinus(_) => NodeVariant::ArithUnOp {
                        operator: ArithOperator::lower_lkql_node(&operator_node, ctx)?,
                        operand,
                    },
                    LkqlNode::OpNot(_) => NodeVariant::LogicUnOp {
                        operator: LogicOperator::lower_lkql_node(&operator_node, ctx)?,
                        operand,
                    },
                    _ => unreachable!(),
                }
            }

            // --- Literals
            LkqlNode::UnitLiteral(_) => NodeVariant::UnitLiteral,
            LkqlNode::NullLiteral(_) => NodeVariant::NullLiteral,
            LkqlNode::BoolLiteralFalse(_) => NodeVariant::BoolLiteral(false),
            LkqlNode::BoolLiteralTrue(_) => NodeVariant::BoolLiteral(true),
            LkqlNode::IntegerLiteral(_) => NodeVariant::IntLiteral(node.text()?),
            LkqlNode::StringLiteral(_) => {
                NodeVariant::StringLiteral(String::from(node.text()?.trim_matches('"')))
            }
            LkqlNode::BlockStringLiteral(bs) => {
                let mut builder = String::new();
                for maybe_str_part in &bs.f_docs()? {
                    if let Some(str_part) = maybe_str_part? {
                        let text = str_part.text()?;
                        builder.push_str(text.trim_start_matches("|\"").trim_start());
                        builder.push('\n');
                    }
                }
                NodeVariant::StringLiteral(builder)
            }
            LkqlNode::Tuple(tl) => {
                let items_node = tl.f_exprs()?;
                let mut items = Vec::with_capacity(items_node.children_count()?);
                for maybe_item_node in &items_node {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(item_node, ctx)?);
                    }
                }
                NodeVariant::TupleLiteral(items)
            }
            LkqlNode::ListLiteral(ll) => {
                let items_node = ll.f_exprs()?;
                let mut items = Vec::with_capacity(items_node.children_count()?);
                for maybe_item_node in &items_node {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(item_node, ctx)?);
                    }
                }
                NodeVariant::ListLiteral(items)
            }
            LkqlNode::ObjectLiteral(ol) => {
                let assocs_node = ol.f_assocs()?;
                let mut assocs = Vec::with_capacity(assocs_node.children_count()?);
                for maybe_assoc_node in &assocs_node {
                    if let Some(LkqlNode::ObjectAssoc(ref assoc_node)) = maybe_assoc_node? {
                        assocs.push((
                            Identifier::from_lkql_node(&assoc_node.f_name()?, ctx)?,
                            Self::lower_lkql_node(&assoc_node.f_expr()?, ctx)?,
                        ));
                    }
                }
                NodeVariant::ObjectLiteral(assocs)
            }
            LkqlNode::Identifier(_) => {
                NodeVariant::ReadSymbol(Identifier::from_lkql_node(node, ctx)?)
            }
            LkqlNode::AnonymousFunction(_) => {
                NodeVariant::LambdaFun(*ctx.child_index_map.get(node).unwrap())
            }

            // --- For now, not all node kinds are handled
            _ => panic!("{} is not handled by lowering", node.image()?),
        };

        // Finally return the resulting node
        Ok(Node {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            variant,
        })
    }
}

impl ArithOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext) -> Result<Self, Report> {
        Ok(ArithOperator {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            variant: match node {
                LkqlNode::OpPlus(_) => ArithOperatorVariant::Plus,
                LkqlNode::OpMinus(_) => ArithOperatorVariant::Minus,
                LkqlNode::OpMul(_) => ArithOperatorVariant::Multiply,
                LkqlNode::OpDiv(_) => ArithOperatorVariant::Divide,
                _ => unreachable!(),
            },
        })
    }
}

impl LogicOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext) -> Result<Self, Report> {
        Ok(LogicOperator {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            variant: match node {
                LkqlNode::OpAnd(_) => LogicOperatorVariant::And,
                LkqlNode::OpOr(_) => LogicOperatorVariant::Or,
                LkqlNode::OpNot(_) => LogicOperatorVariant::Not,
                _ => unreachable!(),
            },
        })
    }
}

impl CompOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext) -> Result<Self, Report> {
        Ok(CompOperator {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            variant: match node {
                LkqlNode::OpEq(_) => CompOperatorVariant::Equals,
                LkqlNode::OpNeq(_) => CompOperatorVariant::NotEquals,
                LkqlNode::OpLt(_) => CompOperatorVariant::Less,
                LkqlNode::OpLeq(_) => CompOperatorVariant::LessOrEquals,
                LkqlNode::OpGt(_) => CompOperatorVariant::Greater,
                LkqlNode::OpGeq(_) => CompOperatorVariant::GreaterOrEquals,
                _ => unreachable!(),
            },
        })
    }
}

impl MiscOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext) -> Result<Self, Report> {
        Ok(MiscOperator {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            variant: match node {
                LkqlNode::OpConcat(_) => MiscOperatorVariant::Concat,
                _ => unreachable!(),
            },
        })
    }
}

impl Identifier {
    /// Util function to easily create an identifier from an LKQL node.
    fn from_lkql_node(node: &LkqlNode, ctx: &LoweringContext) -> Result<Self, Report> {
        Ok(Self {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            text: node.text()?,
        })
    }
}

struct LoweringContext {
    /// The source that is currently being lowered.
    lowered_source: SourceId,

    /// Map each function declaration node to the "child index" of its produced
    /// [`Function`] object.
    child_index_map: HashMap<LkqlNode, u16>,

    /// Each lambda (anonymous) function is associated to a unique name.
    lambda_name_map: HashMap<LkqlNode, String>,
    lambda_counter: usize,

    /// The list of diagnostics emitted during the lowering.
    diagnostics: Vec<Report>,
}

impl LoweringContext {
    pub fn new(lowered_source: SourceId) -> Self {
        Self {
            lowered_source,
            child_index_map: HashMap::new(),
            lambda_name_map: HashMap::new(),
            lambda_counter: 0,
            diagnostics: Vec::new(),
        }
    }

    /// Get the name associated to the provided anonymous function node.
    fn get_lambda_name(&mut self, node: &LkqlNode) -> &String {
        assert!(matches!(node, LkqlNode::AnonymousFunction(_)));
        if !self.lambda_name_map.contains_key(node) {
            self.lambda_name_map
                .insert(node.clone(), format!("<lambda_{}>", self.lambda_counter));
            self.lambda_counter += 1;
        }
        self.lambda_name_map.get(node).unwrap()
    }
}

/// Util function to find all local declarations from the provided node.
/// Is defined as "declaration" all nodes that introduce a new symbol in the
/// lexical environment.
/// A declaration is defined as "local" if it isn't contained in a
/// function-like tree that is strictly lower than the provided node.
/// Are defined as "function-like" the following LKQL nodes:
///   * [`LkqlNode::TopLevelList`]
///   * [`LkqlNode::FunDecl`]
///   * [`LkqlNode::SelectorDecl`]
///   * [`LkqlNode::AnonymousFunction`]
///   * [`LkqlNode::ListComprehension`]
fn all_local_decls(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Report> {
    for maybe_child in node {
        if let Some(child) = maybe_child? {
            match child {
                LkqlNode::TopLevelList(_) => (),
                LkqlNode::AnonymousFunction(_) => (),
                LkqlNode::ListComprehension(_) => (),
                LkqlNode::BlockExpr(_) => (),
                LkqlNode::ValDecl(_) => {
                    all_local_decls(&child, output)?;
                    output.push(child);
                }
                LkqlNode::FunDecl(_) => output.push(child),
                LkqlNode::SelectorDecl(_) => output.push(child),
                _ => all_local_decls(&child, output)?,
            }
        }
    }
    Ok(())
}

/// Util function to get all lexical symbols that are local to the given node.
/// Local symbols are represented by identifiers with their locations being
/// the declaration location.
/// This function relies on [`all_local_decls`] to compute its result, meaning
/// that all concepts described in the latter's doc are true for this function.
fn all_local_symbols(node: &LkqlNode, ctx: &LoweringContext) -> Result<Vec<Identifier>, Report> {
    let mut local_decls = Vec::new();
    all_local_decls(node, &mut local_decls)?;
    Ok(local_decls
        .iter()
        .map(|n| -> Result<Identifier, Report> {
            let text = match n {
                LkqlNode::ValDecl(vd) => vd.f_identifier()?.text()?,
                LkqlNode::FunDecl(fd) => fd.f_name()?.text()?,
                LkqlNode::SelectorDecl(sd) => sd.f_name()?.text()?,
                _ => unreachable!(),
            };
            Ok(Identifier {
                origin_location: SourceSection::from_lkql_node(ctx.lowered_source, n)?,
                text,
            })
        })
        .collect::<Result<_, _>>()?)
}

/// Util function to find all execution units in the local environment of the
/// provided node.
/// A node is considered as a "execution units" if it can be lowered by the
/// [`ExecutionUnit::lower_lkql_node`] method.
/// The locality is different from the one defined in the [`all_local_decls`]
/// function. We explore the whole tree to found all units, stopping the
/// recursion on execution units bodies. IOW, we return all direct children
/// units.
fn all_local_execution_units(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Report> {
    for maybe_child in node {
        if let Some(child) = maybe_child? {
            match child {
                LkqlNode::TopLevelList(_) => output.push(child),
                LkqlNode::AnonymousFunction(_) => output.push(child),
                LkqlNode::ListComprehension(_) => output.push(child),
                LkqlNode::FunDecl(_) => output.push(child),
                LkqlNode::SelectorDecl(_) => output.push(child),
                _ => all_local_execution_units(&child, output)?,
            }
        }
    }
    Ok(())
}
