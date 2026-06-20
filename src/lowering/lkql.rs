//! # LKQL lowering module
//!
//! This module contains all required operations to lower an LKQL parsing tree
//! to the intermediate representation defined in the
//! [`crate::intermediate_tree`] module.

use crate::{
    ExecutionContext,
    builtins::{
        traits::{self, BuiltinTrait},
        types::{self, BuiltinType},
    },
    diagnostics::{Diagnostic, DiagnosticCollector, Hint},
    errors::{
        AMBIGUOUS_IMPORT, INDEX_OUT_OF_BOUNDS, INVALID_SELECTOR_CALL, MODULE_NOT_FOUND,
        MULTIPLE_SPLAT_PATTERNS, NULL_DOT_RECEIVER, POS_AFTER_NAMED_ARGUMENT,
        PREVIOUS_NAMED_ARG_HINT, PREVIOUS_SPLAT_PATTERN_HINT, SUBPATTERN_AFTER_SPLAT,
        UNKNOWN_NODE_TYPE,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperator, LogicOperatorVariant, MiscOperator,
        MiscOperatorVariant, Node, NodeVariant,
    },
    lowering::{LoweringContext, unescape_string},
    sources::{Location, SourceId, SourceSection},
};
use liblkqllang::{BaseFunction, LkqlNode};
use std::{
    env,
    path::{Path, PathBuf},
};

impl ExecutionUnit {
    /// Lower the provided LKQL node as an intermediate [`ExecutionUnit`]. The
    /// provided node MUST be one of the following variants:
    ///   * [`LkqlNode::TopLevelList`]
    ///   * [`LkqlNode::FunDecl`]
    ///   * [`LkqlNode::AnonymousFunction`]
    ///   * [`LkqlNode::ListComprehension`]
    ///   * [`LkqlNode::NodePatternSelector`]
    ///
    /// If there is errors during the lowering of LKQL source, this function
    /// returns a [`Result::Err`] which contains all diagnostics.
    pub fn lower_lkql_node(
        execution_context: &ExecutionContext,
        source: SourceId,
        node: &LkqlNode,
    ) -> Result<Self, DiagnosticCollector> {
        let mut lowering_context = LoweringContext::new(execution_context, source);
        match Self::internal_lower_lkql_node(node, &mut lowering_context) {
            Ok(res) => {
                if lowering_context.diagnostics.is_empty() {
                    Ok(res)
                } else {
                    Err(lowering_context.diagnostics)
                }
            }
            Err(diag) => {
                lowering_context.diagnostics.add(*diag);
                Err(lowering_context.diagnostics)
            }
        }
    }

    /// Internal function to lower an [`LkqlNode`] to an [`ExecutionUnit`].
    fn internal_lower_lkql_node(
        node: &LkqlNode,
        ctx: &mut LoweringContext<LkqlNode>,
    ) -> Result<Self, Box<Diagnostic>> {
        // Create the origin location of the execution unit
        let l = loc(ctx, node);

        // First, we get the name of the currently lowered execution unit.
        let name = match &node {
            LkqlNode::TopLevelList(top_level) => {
                let unit_path = PathBuf::from(top_level.unit()?.unwrap().filename()?);
                unit_path.file_name().unwrap().to_string_lossy().to_string()
            }
            LkqlNode::FunDecl(fun_decl) => fun_decl.f_name()?.text()?,
            LkqlNode::AnonymousFunction(_) => ctx.next_lambda_name(),
            LkqlNode::ListComprehension(_) => ctx.next_lazy_comprehension_name(),
            LkqlNode::NodePatternSelector(_) => ctx.next_selector_pattern_name(),
            _ => unreachable!(),
        };

        // Iterate over all children execution units to lower them and to
        // associate each one to an index in the children units vector.
        // This needs to be done before the lowering of the unit itself.
        let mut local_units = Vec::new();
        all_local_execution_units(node, &mut local_units)?;
        let mut children_units = Vec::new();
        for unit in &local_units {
            ctx.child_index_map
                .insert(unit.clone(), children_units.len() as u16);
            children_units.push(Self::internal_lower_lkql_node(unit, ctx)?);
            assert!(
                children_units.len() < u16::MAX as usize,
                "Too many children execution units"
            );
        }

        // Create the variant part of the result
        let variant = match &node {
            LkqlNode::TopLevelList(top_level) => {
                // Lower the top level elements
                let mut elements = Vec::new();
                for maybe_top_level_elem in top_level {
                    if let Some(top_level_elem) = maybe_top_level_elem? {
                        elements.push(Node::lower_lkql_node(ctx, &top_level_elem)?);
                    }
                }

                // Create the resulting module
                ExecutionUnitVariant::Module { symbols: all_local_symbols(node, ctx)?, elements }
            }
            LkqlNode::FunDecl(_) | LkqlNode::AnonymousFunction(_) => {
                // Get the function name, parameters and body nodes
                let (lkql_params, lkql_body) = match &node {
                    LkqlNode::FunDecl(fun_decl) => match fun_decl.f_fun_expr()? {
                        LkqlNode::NamedFunction(named_fun) => {
                            (named_fun.f_parameters()?, named_fun.f_body_expr()?)
                        }
                        _ => unreachable!(),
                    },
                    LkqlNode::AnonymousFunction(anon_fun) => {
                        (anon_fun.f_parameters()?, anon_fun.f_body_expr()?)
                    }
                    _ => unreachable!(),
                };

                // Then lower the function parameters
                let mut params = Vec::new();
                for maybe_param_decl in &lkql_params {
                    match maybe_param_decl? {
                        Some(LkqlNode::ParameterDecl(lkql_param_decl)) => {
                            let name = id(ctx, &lkql_param_decl.f_param_identifier()?);
                            let default_expr = lkql_param_decl
                                .f_default_expr()?
                                .map(|n| Node::lower_lkql_node(ctx, &n))
                                .transpose()?;
                            params.push((name, default_expr))
                        }
                        _ => unreachable!(),
                    }
                }

                // Then create the function execution unit variant
                ExecutionUnitVariant::Function {
                    params,
                    body: Node::lower_lkql_node(ctx, &lkql_body)?,
                }
            }
            LkqlNode::ListComprehension(list_comp) => {
                // Get the collection bindings in the list comprehension, those
                // are going to be the parameters of the created function.
                let params = list_comp
                    .f_generators()?
                    .into_iter()
                    .map(|n| -> Result<Identifier, Box<Diagnostic>> {
                        match n? {
                            Some(LkqlNode::ListCompAssoc(assoc)) => {
                                Ok(id(ctx, &assoc.f_binding_name()?))
                            }
                            _ => unreachable!(),
                        }
                    })
                    .collect::<Result<_, _>>()?;

                // Lower the list comprehension mapping logic
                let mapping = Node::lower_lkql_node(ctx, &list_comp.f_expr()?)?;

                // If there is a guard, wrap the mapping node inside it
                let body = match list_comp.f_guard()? {
                    Some(lkql_guard) => n(
                        l,
                        NodeVariant::IfExpr {
                            condition: Box::new(Node::lower_lkql_node(ctx, &lkql_guard)?),
                            consequence: Box::new(mapping),
                            alternative: bn(l, NodeVariant::NilLiteral),
                        },
                    ),
                    None => mapping,
                };

                // Then return the new function execution unit variant
                ExecutionUnitVariant::RawCallable { params, body }
            }
            LkqlNode::NodePatternSelector(selector_pattern) => {
                // Create a name for the "self" parameter, value input in the
                // pattern.
                let self_param_name = id_str(l, "self");
                let self_param_id = ctx.new_tmp_id();

                // Then, lower the selector pattern matching logic as a
                // function.
                ExecutionUnitVariant::Function {
                    params: vec![(self_param_name.clone(), None)],
                    body: Node::lower_lkql_pattern(
                        ctx,
                        &selector_pattern.f_pattern()?,
                        self_param_id,
                    )?
                    .with_let(self_param_id, n(l, NodeVariant::ReadSymbol(self_param_name))),
                }
            }
            _ => unreachable!(),
        };

        // Finally return the new execution unit
        Ok(ExecutionUnit::new(l, name, children_units, variant))
    }
}

impl Node {
    /// Lower an LKQL node as an intermediate node. All LKQL node kinds should
    /// be accepted by this function.
    fn lower_lkql_node(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
    ) -> Result<Self, Box<Diagnostic>> {
        // Get the location of the node
        let l = loc(ctx, node);

        // Lower the node
        let variant = match node {
            // --- Declarations
            LkqlNode::ValDecl(val_decl) => NodeVariant::InitLocal {
                symbol: id(ctx, &val_decl.f_identifier()?),
                val: Box::new(Self::lower_lkql_node(ctx, &val_decl.f_value()?)?),
            },
            LkqlNode::FunDecl(_) | LkqlNode::SelectorDecl(_) => {
                NodeVariant::InitLocalFun(*ctx.child_index_map.get(node).unwrap())
            }
            LkqlNode::Import(import) => {
                // Create a vector of directories to look in
                let mut searching_dirs: Vec<PathBuf> = Vec::new();

                // Get the parent directory of the file being lowered
                let current_file = node.unit()?.unwrap().filename()?;
                if let Some(p) = Path::new(&current_file).parent() {
                    searching_dirs.push(p.to_path_buf());
                }

                // Now get all directories in the "LKQL_PATH" environment
                // variable.
                if let Ok(lkql_path) = env::var("LKQL_PATH") {
                    env::split_paths(&lkql_path)
                        .filter(|p| p.exists() && p.is_dir())
                        .for_each(|d| searching_dirs.push(d));
                }

                // Now look for the LKQL file corresponding to the module
                let module_name = id(ctx, &import.f_name()?);
                let module_base_file = PathBuf::from(format!("{}.lkql", &module_name.text));
                let mut module_files = searching_dirs
                    .iter()
                    .filter_map(|d| {
                        let possible_module_file = d.join(&module_base_file);
                        if possible_module_file.exists() && possible_module_file.is_file() {
                            Some(possible_module_file)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                // Check that there is exactly one matching file
                let module_file = if module_files.len() == 1 {
                    module_files.remove(0)
                } else if module_files.is_empty() {
                    ctx.diagnostics.add(Diagnostic::error_from_template(
                        &l,
                        &MODULE_NOT_FOUND,
                        &[&module_name.text],
                    ));
                    PathBuf::new()
                } else {
                    ctx.diagnostics.add(Diagnostic::error_from_template(
                        &l,
                        &AMBIGUOUS_IMPORT,
                        &[&module_files
                            .iter()
                            .map(|f| f.to_string_lossy())
                            .collect::<Vec<_>>()
                            .join(" & ")],
                    ));
                    PathBuf::new()
                };

                // Finally return the created node variant
                NodeVariant::ImportModule { name: module_name, file: module_file }
            }

            // --- Function call
            LkqlNode::FunCall(fun_call) => {
                // Create the argument vectors
                let mut positional_args: Vec<Node> = Vec::new();
                let mut named_args: Vec<(Identifier, Node)> = Vec::new();
                Self::lower_lkql_arguments(
                    ctx,
                    &fun_call.f_arguments()?,
                    &mut positional_args,
                    &mut named_args,
                )?;

                // There is a special case when the callee of the function is a
                // dot access, in that case we emit a method call.
                let name = fun_call.f_name()?;
                match name {
                    LkqlNode::DotAccess(_) | LkqlNode::SafeAccess(_) => {
                        // Get the dot left part and the field name
                        let (prefix, member, is_safe) = match name {
                            LkqlNode::DotAccess(ref dot_access) => {
                                (dot_access.f_receiver()?, dot_access.f_member()?, false)
                            }
                            LkqlNode::SafeAccess(ref safe_access) => {
                                (safe_access.f_receiver()?, safe_access.f_member()?, true)
                            }
                            _ => unreachable!(),
                        };

                        // Create a new named temporary value to compute the
                        // prefix part of the dot access only once.
                        let prefix_id = ctx.new_tmp_id();
                        let prefix_ref = bn(loc(ctx, &prefix), NodeVariant::Read(prefix_id));

                        // Create a callee access node that use the prefix
                        // temporary value created before.
                        let callee = bn(
                            loc(ctx, &name),
                            lower_dot_access(
                                ctx,
                                &loc(ctx, &name),
                                prefix_ref.clone(),
                                &member,
                                is_safe,
                            ),
                        );

                        // Then create a vector argument containing the callee
                        // that is used in the case of a method call.
                        let mut method_positional_args = positional_args.clone();
                        method_positional_args.insert(0, *prefix_ref.clone());

                        // Finally create a node to check the type of the
                        // prefix and decide at runtime how to act.
                        n(
                            l,
                            NodeVariant::IfExpr {
                                condition: bn(
                                    l,
                                    NodeVariant::InstanceOf {
                                        expression: prefix_ref,
                                        expected_type_tag: types::namespace::TYPE.tag,
                                    },
                                ),
                                consequence: bn(
                                    l,
                                    NodeVariant::FunCall {
                                        callee: callee.clone(),
                                        positional_args,
                                        named_args: named_args.clone(),
                                    },
                                ),
                                alternative: bn(
                                    l,
                                    NodeVariant::FunCall {
                                        callee,
                                        positional_args: method_positional_args,
                                        named_args,
                                    },
                                ),
                            },
                        )
                        .with_let(prefix_id, Self::lower_lkql_node(ctx, &prefix)?)
                        .variant
                    }
                    _ => NodeVariant::FunCall {
                        callee: Box::new(Self::lower_lkql_node(ctx, &name)?),
                        positional_args,
                        named_args,
                    },
                }
            }

            // --- Dotted expression
            LkqlNode::DotAccess(_) | LkqlNode::SafeAccess(_) => {
                // Get prefix and member nodes
                let (lkql_prefix, lkql_member, is_safe) = match node {
                    LkqlNode::DotAccess(dot_access) => {
                        (dot_access.f_receiver()?, dot_access.f_member()?, false)
                    }
                    LkqlNode::SafeAccess(safe_access) => {
                        (safe_access.f_receiver()?, safe_access.f_member()?, true)
                    }
                    _ => unreachable!(),
                };

                // Create the identifier and access node for the prefix value
                let prefix_id = ctx.new_tmp_id();
                let prefix_ref = bn(loc(ctx, &lkql_prefix), NodeVariant::Read(prefix_id));

                // Then, return the new node
                n(l, lower_dot_access(ctx, &l, prefix_ref, &lkql_member, is_safe))
                    .with_let(prefix_id, Node::lower_lkql_node(ctx, &lkql_prefix)?)
                    .variant
            }

            // --- Index expression
            LkqlNode::Indexing(_) | LkqlNode::SafeIndexing(_) => {
                let (lkql_collection, lkql_index, is_safe) = match node {
                    LkqlNode::Indexing(indexing) => {
                        (indexing.f_collection_expr()?, indexing.f_index_expr()?, false)
                    }
                    LkqlNode::SafeIndexing(safe_indexing) => {
                        (safe_indexing.f_collection_expr()?, safe_indexing.f_index_expr()?, true)
                    }
                    _ => unreachable!(),
                };

                // Prepare the index reference and lower the index node
                let index_id = ctx.new_tmp_id();
                let index_ref = n(loc(ctx, &lkql_index), NodeVariant::Read(index_id));
                let index = Self::lower_lkql_node(ctx, &lkql_index)?
                    .with_type_requirement(&types::int::TYPE);

                // Create the node to access the indexed value
                let indexing = n(
                    l,
                    NodeVariant::IndexExpr {
                        indexed_val: Box::new(
                            Self::lower_lkql_node(ctx, &lkql_collection)?
                                .with_trait_requirement(&traits::indexable::TRAIT),
                        ),
                        index: Box::new(index_ref.clone()),
                    },
                );

                // Create the node to execute of the index is out of bounds
                let if_index_out_of_bounds = n(
                    loc(ctx, &lkql_index),
                    if is_safe {
                        NodeVariant::UnitLiteral
                    } else {
                        NodeVariant::RuntimeError {
                            error_template: &INDEX_OUT_OF_BOUNDS,
                            message_args: vec![index_ref],
                        }
                    },
                );

                // Create an identifier for the result
                let result_id = ctx.new_tmp_id();
                let result_ref = n(l, NodeVariant::Read(result_id));

                // Then, return the indexing node
                n(
                    l,
                    result_ref.clone().with_equality_check(
                        NodeVariant::NilLiteral,
                        if_index_out_of_bounds,
                        result_ref,
                    ),
                )
                .with_let(result_id, indexing)
                .with_let(index_id, index)
                .variant
            }

            // --- In clause
            LkqlNode::InClause(in_clause) => NodeVariant::InClause {
                value: Box::new(Self::lower_lkql_node(ctx, &in_clause.f_value_expr()?)?),
                collection: Box::new(
                    Self::lower_lkql_node(ctx, &in_clause.f_list_expr()?)?
                        .with_trait_requirement(&traits::iterable::TRAIT),
                ),
            },

            // --- Is clause
            LkqlNode::IsClause(is_clause) => {
                let value_id = ctx.new_tmp_id();
                Self::lower_lkql_pattern(ctx, &is_clause.f_pattern()?, value_id)?
                    .with_let(value_id, Self::lower_lkql_node(ctx, &is_clause.f_node_expr()?)?)
                    .variant
            }

            // --- If expression
            LkqlNode::CondExpr(cond_expr) => NodeVariant::IfExpr {
                condition: Box::new(
                    Self::lower_lkql_node(ctx, &cond_expr.f_condition()?)?
                        .with_type_requirement(&types::bool::TYPE),
                ),
                consequence: Box::new(Self::lower_lkql_node(ctx, &cond_expr.f_then_expr()?)?),
                alternative: cond_expr
                    .f_else_expr()?
                    .map(|n| Self::lower_lkql_node(ctx, &n))
                    .transpose()?
                    .map_or(bn(loc(ctx, node), NodeVariant::BoolLiteral(true)), Box::new),
            },

            // --- Block expression
            LkqlNode::BlockExpr(block_expr) => {
                let lkql_body = block_expr.f_body()?;
                let mut body = Vec::with_capacity(lkql_body.children_count()?);
                for maybe_body_part in &lkql_body {
                    if let Some(ref lkql_body_part) = maybe_body_part? {
                        body.push(Self::lower_lkql_node(ctx, lkql_body_part)?);
                    }
                }
                NodeVariant::BlockExpr {
                    body,
                    val: Box::new(Self::lower_lkql_node(ctx, &block_expr.f_expr()?)?),
                }
            }
            LkqlNode::BlockBodyDecl(body_decl) => {
                return Self::lower_lkql_node(ctx, &body_decl.f_decl()?);
            }
            LkqlNode::BlockBodyExpr(body_expr) => {
                return Self::lower_lkql_node(ctx, &body_expr.f_expr()?);
            }

            // --- List comprehension
            LkqlNode::ListComprehension(list_comp) => NodeVariant::LazyComprehension {
                source_iterables: list_comp
                    .f_generators()?
                    .into_iter()
                    .map(|n| -> Result<Node, Box<Diagnostic>> {
                        Ok(match n?.unwrap() {
                            LkqlNode::ListCompAssoc(assoc) => {
                                Self::lower_lkql_node(ctx, &assoc.f_coll_expr()?)?
                                    .with_trait_requirement(&traits::iterable::TRAIT)
                            }
                            _ => unreachable!(),
                        })
                    })
                    .collect::<Result<_, Box<Diagnostic>>>()?,
                body_index: *ctx.child_index_map.get(node).unwrap(),
            },

            // --- Binary operation
            LkqlNode::ArithBinOp(arith_bin_op) => NodeVariant::ArithBinOp {
                left: Box::new(Self::lower_lkql_node(ctx, &arith_bin_op.f_left()?)?),
                operator: ArithOperator::lower_lkql_node(&arith_bin_op.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(ctx, &arith_bin_op.f_right()?)?),
            },
            LkqlNode::RelBinOp(rel_bin_op) => NodeVariant::CompBinOp {
                left: Box::new(Self::lower_lkql_node(ctx, &rel_bin_op.f_left()?)?),
                operator: CompOperator::lower_lkql_node(&rel_bin_op.f_op()?, ctx),
                right: Box::new(Self::lower_lkql_node(ctx, &rel_bin_op.f_right()?)?),
            },
            LkqlNode::BinOp(bin_op) => {
                let lkql_operator = bin_op.f_op()?;
                let left = Self::lower_lkql_node(ctx, &bin_op.f_left()?)?;
                let right = Self::lower_lkql_node(ctx, &bin_op.f_right()?)?;
                match &lkql_operator {
                    LkqlNode::OpAnd(_) | LkqlNode::OpOr(_) => NodeVariant::LogicBinOp {
                        left: Box::new(left.with_type_requirement(&types::bool::TYPE)),
                        operator: LogicOperator::lower_lkql_node(&lkql_operator, ctx)?,
                        right: Box::new(right.with_type_requirement(&types::bool::TYPE)),
                    },
                    LkqlNode::OpConcat(_) => NodeVariant::MiscBinOp {
                        left: Box::new(left),
                        operator: MiscOperator::lower_lkql_node(&lkql_operator, ctx),
                        right: Box::new(right),
                    },
                    _ => unreachable!(),
                }
            }

            // --- Unary operation
            LkqlNode::UnOp(un_op) => {
                let lkql_operator = un_op.f_op()?;
                let operand = Self::lower_lkql_node(ctx, &un_op.f_operand()?)?;
                match &lkql_operator {
                    LkqlNode::OpPlus(_) | LkqlNode::OpMinus(_) => NodeVariant::ArithUnOp {
                        operator: ArithOperator::lower_lkql_node(&lkql_operator, ctx)?,
                        operand: Box::new(operand.with_type_requirement(&types::int::TYPE)),
                    },
                    LkqlNode::OpNot(_) => NodeVariant::LogicUnOp {
                        operator: LogicOperator::lower_lkql_node(&lkql_operator, ctx)?,
                        operand: Box::new(operand.with_type_requirement(&types::bool::TYPE)),
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
                NodeVariant::StringLiteral(unescape_string(node.text()?.trim_matches('"')))
            }
            LkqlNode::BlockStringLiteral(block_string) => {
                let mut builder = String::new();
                for maybe_str_part in &block_string.f_docs()? {
                    if let Some(str_part) = maybe_str_part? {
                        builder.push_str(&unescape_string(
                            str_part.text()?.trim_start_matches("|\"").trim_start(),
                        ));
                        builder.push('\n');
                    }
                }
                NodeVariant::StringLiteral(builder)
            }
            LkqlNode::Tuple(tuple) => {
                let lkql_items = tuple.f_exprs()?;
                let mut items = Vec::with_capacity(lkql_items.children_count()?);
                for maybe_item_node in &lkql_items {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(ctx, item_node)?);
                    }
                }
                NodeVariant::TupleLiteral(items)
            }
            LkqlNode::ListLiteral(list) => {
                let lkql_items = list.f_exprs()?;
                let mut items = Vec::with_capacity(lkql_items.children_count()?);
                for maybe_item_node in &lkql_items {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(ctx, item_node)?);
                    }
                }
                NodeVariant::ListLiteral(items)
            }
            LkqlNode::ObjectLiteral(object) => {
                let lkql_assocs = object.f_assocs()?;
                let mut assocs = Vec::with_capacity(lkql_assocs.children_count()?);
                for maybe_assoc_node in &lkql_assocs {
                    if let Some(LkqlNode::ObjectAssoc(ref assoc_node)) = maybe_assoc_node? {
                        assocs.push((
                            id(ctx, &assoc_node.f_name()?),
                            Self::lower_lkql_node(ctx, &assoc_node.f_expr()?)?,
                        ));
                    }
                }
                NodeVariant::ObjectLiteral(assocs)
            }
            LkqlNode::Identifier(_) => NodeVariant::ReadSymbol(id(ctx, node)),
            LkqlNode::AnonymousFunction(_) => {
                NodeVariant::ReadChildUnit(*ctx.child_index_map.get(node).unwrap())
            }

            // --- For now, not all node kinds are handled
            _ => panic!("{} is not handled by the lowering phase", node.image()?),
        };

        // Create the result node
        let lowered_node = n(l, variant);

        // Return the result node, potentially wrapped in a lexical scope
        Ok(if has_lexical_scope(node) {
            lowered_node.with_wrapper(|n| {
                Ok(NodeVariant::InLexicalScope {
                    local_symbols: all_local_symbols(node, ctx)?,
                    expr: Box::new(n),
                })
            })?
        } else {
            lowered_node
        })
    }

    /// Lower all children of the provided `args_node` as arguments and place
    /// them in provided buffers.
    /// This function ensures no positional arguments are defined after a named
    /// one.
    fn lower_lkql_arguments(
        ctx: &mut LoweringContext<LkqlNode>,
        args_node: &LkqlNode,
        positional_args: &mut Vec<Node>,
        named_args: &mut Vec<(Identifier, Node)>,
    ) -> Result<(), Box<Diagnostic>> {
        // Lower each argument in the node containing them
        for maybe_arg in args_node.children_iter()? {
            if let Some(ref arg) = maybe_arg? {
                match arg {
                    LkqlNode::ExprArg(expr_arg) => {
                        // Ensure that no named arguments have been
                        // lowered yet.
                        if named_args.is_empty() {
                            positional_args
                                .push(Self::lower_lkql_node(ctx, &expr_arg.f_value_expr()?)?);
                        } else {
                            let (last_id, last_node) = named_args.last().unwrap();
                            ctx.diagnostics.add(
                                Diagnostic::from_error_template_with_hints::<&str>(
                                    &loc(ctx, arg),
                                    &POS_AFTER_NAMED_ARGUMENT,
                                    &[],
                                    vec![Hint::new(
                                        String::from(PREVIOUS_NAMED_ARG_HINT),
                                        SourceSection::range(
                                            &last_id.origin_location,
                                            &last_node.origin_location,
                                        ),
                                    )],
                                ),
                            );
                        }
                    }
                    LkqlNode::NamedArg(named_arg) => named_args.push((
                        id(ctx, &named_arg.f_arg_name()?),
                        Self::lower_lkql_node(ctx, &named_arg.f_value_expr()?)?,
                    )),
                    _ => unreachable!(),
                }
            }
        }

        // Return the success
        Ok(())
    }

    /// Lower the provided pattern node into a intermediate tree node that
    /// expresses the matching logic.
    /// The provided `matched_value_id` should be the "let id" pointing to
    /// the value to match.
    fn lower_lkql_pattern(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
        matched_value_id: usize,
    ) -> Result<Self, Box<Diagnostic>> {
        // Special handling of the parenthesized pattern
        if let LkqlNode::ParenPattern(parent_pattern) = node {
            return Self::lower_lkql_pattern(ctx, &parent_pattern.f_pattern()?, matched_value_id);
        }

        // Get the location of the pattern node
        let l = loc(ctx, node);

        // Util function to combine a predicate vector in a "and" boolean
        // expression.
        let combine_predicates = |mut predicates: Vec<Node>| match predicates.len() {
            0 => n(l, NodeVariant::BoolLiteral(true)),
            1 => predicates.remove(0),
            _ => {
                let mut iter = predicates.into_iter().rev();
                let init = iter.next().unwrap();
                iter.fold(init, |res, next| {
                    n(
                        l,
                        NodeVariant::LogicBinOp {
                            left: Box::new(next),
                            operator: LogicOperator::new(l, LogicOperatorVariant::And),
                            right: Box::new(res),
                        },
                    )
                })
            }
        };

        // Create a node to read the matched value
        let matched_value_ref = bn(l, NodeVariant::Read(matched_value_id));

        // Lower the pattern not to an intermediate tree node variant
        let variant = match node {
            // --- Simple value patterns
            LkqlNode::UniversalPattern(_) => NodeVariant::BoolLiteral(true),
            LkqlNode::NullPattern(_)
            | LkqlNode::BoolPatternTrue(_)
            | LkqlNode::BoolPatternFalse(_)
            | LkqlNode::IntegerPattern(_) => {
                let target_literal = match node {
                    LkqlNode::NullPattern(_) => NodeVariant::NullLiteral,
                    LkqlNode::BoolPatternTrue(_) => NodeVariant::BoolLiteral(true),
                    LkqlNode::BoolPatternFalse(_) => NodeVariant::BoolLiteral(false),
                    LkqlNode::IntegerPattern(int_pattern) => {
                        NodeVariant::IntLiteral(int_pattern.text()?)
                    }
                    _ => unreachable!(),
                };
                NodeVariant::CompBinOp {
                    left: matched_value_ref,
                    operator: CompOperator::new(l, CompOperatorVariant::Equals),
                    right: bn(l, target_literal),
                }
            }
            LkqlNode::NodeKindPattern(node_kind_pattern) => {
                let node_type_name = node_kind_pattern.f_kind_name()?.text()?;
                let matched_node_type = ctx
                    .execution_context
                    .engine
                    .analysis_lib
                    .node_types
                    .get_type_by_name(&node_type_name);
                if let Some(node_type) = matched_node_type {
                    NodeVariant::InstanceOf {
                        expression: matched_value_ref,
                        expected_type_tag: node_type.tag,
                    }
                } else {
                    ctx.diagnostics.add(Diagnostic::error_from_template(
                        &l,
                        &UNKNOWN_NODE_TYPE,
                        &[&node_type_name],
                    ));
                    NodeVariant::BoolLiteral(false)
                }
            }

            // --- List pattern
            LkqlNode::ListPattern(list_pattern) => {
                // Create working variables
                let mut maybe_splat_pattern = None;

                // Start by lowering all sub-patterns
                let mut sub_patterns = Vec::new();
                for (i, sub_pattern_source) in list_pattern
                    .f_patterns()?
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, c)| c.transpose().map(|c| (i, c)))
                {
                    match sub_pattern_source? {
                        splat_pattern @ LkqlNode::SplatPattern(_) => {
                            // Emit an error if a splat pattern has already
                            // been stored.
                            if let Some(ref previous_splat_pattern) = maybe_splat_pattern {
                                ctx.diagnostics
                                    .add(Diagnostic::from_error_template_with_hints::<&str>(
                                        &loc(ctx, &splat_pattern),
                                        &MULTIPLE_SPLAT_PATTERNS,
                                        &[],
                                        vec![Hint::new(
                                            String::from(PREVIOUS_SPLAT_PATTERN_HINT),
                                            loc(ctx, previous_splat_pattern),
                                        )],
                                    ));
                            }

                            // Store the splat pattern node to handle it later
                            maybe_splat_pattern = Some(splat_pattern);
                        }
                        sub_pattern_source => {
                            // Register an error if there is a pattern after a
                            // splat one.
                            if let Some(ref splat_pattern) = maybe_splat_pattern {
                                ctx.diagnostics
                                    .add(Diagnostic::from_error_template_with_hints::<&str>(
                                        &loc(ctx, &sub_pattern_source),
                                        &SUBPATTERN_AFTER_SPLAT,
                                        &[],
                                        vec![Hint::new(
                                            String::from(PREVIOUS_SPLAT_PATTERN_HINT),
                                            loc(ctx, splat_pattern),
                                        )],
                                    ));
                            } else {
                                let elem_id = ctx.new_tmp_id();
                                let sub_pattern =
                                    Self::lower_lkql_pattern(ctx, &sub_pattern_source, elem_id)?;
                                let index_access =
                                    sub_pattern.related_node(NodeVariant::IndexExpr {
                                        indexed_val: matched_value_ref.clone(),
                                        index: Box::new(sub_pattern.related_node(
                                            NodeVariant::IntLiteral((i + 1).to_string()),
                                        )),
                                    });
                                sub_patterns.push(sub_pattern.with_let(elem_id, index_access));
                            }
                        }
                    }
                }

                // Store the count of subpatterns matching elements of the list
                let sub_pattern_count = sub_patterns.len();

                // Add type checking to subpatterns
                sub_patterns.insert(
                    0,
                    n(
                        l,
                        NodeVariant::InstanceOf {
                            expression: matched_value_ref.clone(),
                            expected_type_tag: types::list::TYPE.tag,
                        },
                    ),
                );

                // Add the list size checking to subpatterns
                sub_patterns.insert(
                    1,
                    n(
                        l,
                        NodeVariant::CompBinOp {
                            left: bn(l, NodeVariant::LengthExpr(matched_value_ref.clone())),
                            operator: CompOperator::new(
                                l,
                                if maybe_splat_pattern.is_some() {
                                    CompOperatorVariant::GreaterOrEquals
                                } else {
                                    CompOperatorVariant::Equals
                                },
                            ),
                            right: bn(l, NodeVariant::IntLiteral(sub_pattern_count.to_string())),
                        },
                    ),
                );

                // If there is a splat pattern with a binding, add its
                // corresponding initialization at the end of subpatterns
                if let Some(LkqlNode::SplatPattern(splat_pattern)) = maybe_splat_pattern
                    && let Some(binding) = splat_pattern.f_binding()?
                {
                    let l = loc(ctx, &binding);
                    let binding_id = id(ctx, &binding);
                    let sublist_access = bn(
                        l,
                        NodeVariant::DottedExpr {
                            prefix: matched_value_ref.clone(),
                            suffix: id_str(l, "sublist"),
                        },
                    );
                    let sublist_call = bn(
                        l,
                        NodeVariant::FunCall {
                            callee: sublist_access,
                            positional_args: vec![
                                *matched_value_ref.clone(),
                                n(l, NodeVariant::IntLiteral((sub_pattern_count + 1).to_string())),
                                n(l, NodeVariant::LengthExpr(matched_value_ref)),
                            ],
                            named_args: vec![],
                        },
                    );
                    let bool_lit = bn(l, NodeVariant::BoolLiteral(true));
                    sub_patterns.push(n(
                        l,
                        NodeVariant::BlockExpr {
                            body: vec![n(
                                l,
                                NodeVariant::InitLocal { symbol: binding_id, val: sublist_call },
                            )],
                            val: bool_lit,
                        },
                    ));
                }

                // Finally, combine all subpatterns in a "and" expression
                combine_predicates(sub_patterns).variant
            }

            // --- Not pattern
            LkqlNode::NotPattern(not_pattern) => NodeVariant::LogicUnOp {
                operator: LogicOperator::new(l, LogicOperatorVariant::Not),
                operand: Box::new(Self::lower_lkql_pattern(
                    ctx,
                    &not_pattern.f_pattern()?,
                    matched_value_id,
                )?),
            },

            // --- Or pattern
            LkqlNode::OrPattern(or_pattern) => NodeVariant::LogicBinOp {
                left: Box::new(Self::lower_lkql_pattern(
                    ctx,
                    &or_pattern.f_left()?,
                    matched_value_id,
                )?),
                operator: LogicOperator::new(l, LogicOperatorVariant::Or),
                right: Box::new(Self::lower_lkql_pattern(
                    ctx,
                    &or_pattern.f_right()?,
                    matched_value_id,
                )?),
            },

            // --- Complex pattern
            LkqlNode::ComplexPattern(complex_pattern) => {
                // Lower the binding as a local initialization
                let binding_in_vec = complex_pattern
                    .f_binding()?
                    .map(|b| {
                        Ok::<_, Box<Diagnostic>>(Self::new(
                            loc(ctx, &b),
                            NodeVariant::InitLocal { symbol: id(ctx, &b), val: matched_value_ref },
                        ))
                    })
                    .transpose()?
                    .into_iter()
                    .collect::<Vec<_>>();

                // Collect all checks in a vector
                let mut matching_elems = Vec::new();

                // Lower the value pattern
                if let Some(pattern) = complex_pattern.f_pattern()? {
                    matching_elems.push(Self::lower_lkql_pattern(ctx, &pattern, matched_value_id)?)
                }

                // Lower pattern details
                for maybe_detail in &complex_pattern.f_details()? {
                    if let Some(detail) = maybe_detail? {
                        matching_elems.push(Self::lower_lkql_pattern_detail(
                            ctx,
                            &detail,
                            matched_value_id,
                        )?);
                    }
                }

                // Lower the predicate
                if let Some(predicate) = complex_pattern.f_predicate()? {
                    matching_elems.push(
                        Self::lower_lkql_node(ctx, &predicate)?
                            .with_type_requirement(&types::bool::TYPE),
                    );
                }

                // Compose all checks in a sequence of binary operations
                let matching_logic_node = combine_predicates(matching_elems);

                // Finally, return the lowered complex pattern
                if binding_in_vec.is_empty() {
                    matching_logic_node.variant
                } else {
                    NodeVariant::BlockExpr {
                        body: binding_in_vec,
                        val: Box::new(matching_logic_node),
                    }
                }
            }
            _ => unreachable!(),
        };

        // Return the result node
        Ok(n(l, variant))
    }

    /// Lower the provided pattern detail node into a intermediate tree node
    /// that evaluates to a boolean.
    /// The provided `detailed_values_id` should be the "let id" pointing to
    /// the value to get the detail from.
    fn lower_lkql_pattern_detail(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
        matched_value_id: usize,
    ) -> Result<Self, Box<Diagnostic>> {
        // Get the location of the pattern detail node
        let l = loc(ctx, node);

        // Create a node to read the matched value
        let matched_value_ref = n(l, NodeVariant::Read(matched_value_id));

        // Get the intermediate node variant from the LKQL pattern detail
        let variant = match node {
            LkqlNode::NodePatternField(pattern_field) => {
                let field_id = ctx.new_tmp_id();
                Self::lower_lkql_pattern(ctx, &pattern_field.f_expected_value()?, field_id)?
                    .with_let(
                        field_id,
                        n(
                            l,
                            NodeVariant::DottedExpr {
                                prefix: Box::new(matched_value_ref),
                                suffix: id(ctx, &pattern_field.f_identifier()?),
                            },
                        ),
                    )
                    .variant
            }
            LkqlNode::NodePatternProperty(pattern_property) => {
                // Get the LKQL call node in the detail and prepare a temporary
                // value for its result.
                let call = match pattern_property.f_call()? {
                    LkqlNode::FunCall(fun_call) => fun_call,
                    _ => unreachable!(),
                };
                let prop_result_id = ctx.new_tmp_id();

                // Create argument vectors
                let mut positional_args = Vec::new();
                let mut named_args = Vec::new();
                Self::lower_lkql_arguments(
                    ctx,
                    &call.f_arguments()?,
                    &mut positional_args,
                    &mut named_args,
                )?;
                positional_args.insert(0, matched_value_ref.clone());

                // Create the node to call the property
                let property_call = n(
                    l,
                    NodeVariant::FunCall {
                        callee: bn(
                            l,
                            NodeVariant::DottedExpr {
                                prefix: Box::new(matched_value_ref),
                                suffix: id(ctx, &call.f_name()?),
                            },
                        ),
                        positional_args,
                        named_args,
                    },
                );

                // Then create the node variant to call the property and match
                // it against expected result.
                Self::lower_lkql_pattern(
                    ctx,
                    &pattern_property.f_expected_value()?,
                    prop_result_id,
                )?
                .with_let(prop_result_id, property_call)
                .variant
            }
            LkqlNode::NodePatternSelector(pattern_selector) => {
                // Get the LKQL node of the selector call
                let lkql_quantified_selector_call = match pattern_selector.f_call()? {
                    LkqlNode::SelectorCall(c) => c,
                    _ => unreachable!(),
                };

                // Create selector call argument vectors
                let mut positional_args = vec![matched_value_ref.clone()];
                let mut named_args = Vec::new();

                // The create the selector callee node
                let lkql_selector_call = lkql_quantified_selector_call.f_selector_call()?;
                let selector_callee = bn(
                    loc(ctx, &lkql_selector_call),
                    match &lkql_selector_call {
                        LkqlNode::Identifier(i) => NodeVariant::ReadSymbol(id(ctx, &i.as_node())),
                        LkqlNode::FunCall(fun_call) => {
                            Self::lower_lkql_arguments(
                                ctx,
                                &fun_call.f_arguments()?,
                                &mut positional_args,
                                &mut named_args,
                            )?;
                            Self::lower_lkql_node(ctx, &fun_call.f_name()?)?.variant
                        }
                        _ => {
                            ctx.diagnostics.add(Diagnostic::error_from_template::<&str>(
                                &l,
                                &INVALID_SELECTOR_CALL,
                                &[],
                            ));
                            NodeVariant::NilLiteral
                        }
                    },
                );

                // Create the node calling the selector value
                let selector_call = n(
                    loc(ctx, &lkql_selector_call),
                    NodeVariant::FunCall { callee: selector_callee, positional_args, named_args },
                )
                .with_trait_requirement(&traits::iterable::TRAIT);

                // Create a new named value that will contain the resulting
                // selector list.
                let selector_list_id = ctx.new_tmp_id();
                let selector_list_ref = n(l, NodeVariant::Read(selector_list_id));

                // Create the quantifier call node
                let quantifier_call = n(
                    l,
                    NodeVariant::FunCall {
                        callee: bn(
                            l,
                            NodeVariant::DottedExpr {
                                prefix: Box::new(selector_list_ref.clone()),
                                suffix: id(ctx, &lkql_quantified_selector_call.f_quantifier()?),
                            },
                        ),
                        positional_args: vec![
                            selector_list_ref,
                            n(
                                l,
                                NodeVariant::ReadChildUnit(*ctx.child_index_map.get(node).unwrap()),
                            ),
                        ],
                        named_args: vec![],
                    },
                );

                // Finally, create the quantification call node
                quantifier_call
                    .with_let(selector_list_id, selector_call)
                    .variant
            }
            _ => unreachable!(),
        };

        // Finally, create the result node
        Ok(n(l, variant))
    }

    /// Wrap the current node using the provided wrapper creation function,
    /// propagating all information in the current node to the wrapper.
    fn with_wrapper<F>(self, create_wrapper: F) -> Result<Self, Box<Diagnostic>>
    where
        F: FnOnce(Self) -> Result<NodeVariant, Box<Diagnostic>>,
    {
        Ok(n(self.origin_location, create_wrapper(self)?))
    }

    /// Wrap the node in a type requirement one.
    fn with_type_requirement(self, required_type: &'static BuiltinType) -> Self {
        self.with_wrapper(|n| {
            Ok(NodeVariant::RequireType { expression: Box::new(n), expected_type: required_type })
        })
        .unwrap()
    }

    /// Wrap the node in a trait requirement one.
    fn with_trait_requirement(self, required_trait: &'static BuiltinTrait) -> Self {
        self.with_wrapper(|n| {
            Ok(NodeVariant::RequireTrait { expression: Box::new(n), required_trait })
        })
        .unwrap()
    }

    /// Wrap this node in another to introduce a new named value in its
    /// environment.
    fn with_let(self, id: usize, value: Self) -> Self {
        self.with_wrapper(|n| {
            Ok(NodeVariant::Let { id, value: Box::new(value), r#in: Box::new(n) })
        })
        .unwrap()
    }

    /// Wrap the node inside an equality check dispatching the execution
    /// according to the result.
    fn with_equality_check(
        self,
        comparing_to: NodeVariant,
        consequence: Self,
        alternative: Self,
    ) -> NodeVariant {
        NodeVariant::IfExpr {
            condition: bn(
                self.origin_location,
                NodeVariant::CompBinOp {
                    operator: CompOperator {
                        origin_location: self.origin_location,
                        variant: CompOperatorVariant::Equals,
                    },
                    right: bn(self.origin_location, comparing_to),
                    left: Box::new(self),
                },
            ),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        }
    }
}

impl ArithOperator {
    fn lower_lkql_node(
        node: &LkqlNode,
        ctx: &LoweringContext<LkqlNode>,
    ) -> Result<Self, Box<Diagnostic>> {
        Ok(ArithOperator::new(
            loc(ctx, node),
            match node {
                LkqlNode::OpPlus(_) => ArithOperatorVariant::Plus,
                LkqlNode::OpMinus(_) => ArithOperatorVariant::Minus,
                LkqlNode::OpMul(_) => ArithOperatorVariant::Multiply,
                LkqlNode::OpDiv(_) => ArithOperatorVariant::Divide,
                _ => unreachable!(),
            },
        ))
    }
}

impl LogicOperator {
    fn lower_lkql_node(
        node: &LkqlNode,
        ctx: &LoweringContext<LkqlNode>,
    ) -> Result<Self, Box<Diagnostic>> {
        Ok(LogicOperator::new(
            loc(ctx, node),
            match node {
                LkqlNode::OpAnd(_) => LogicOperatorVariant::And,
                LkqlNode::OpOr(_) => LogicOperatorVariant::Or,
                LkqlNode::OpNot(_) => LogicOperatorVariant::Not,
                _ => unreachable!(),
            },
        ))
    }
}

impl CompOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Self {
        CompOperator::new(
            loc(ctx, node),
            match node {
                LkqlNode::OpEq(_) => CompOperatorVariant::Equals,
                LkqlNode::OpNeq(_) => CompOperatorVariant::NotEquals,
                LkqlNode::OpLt(_) => CompOperatorVariant::Less,
                LkqlNode::OpLeq(_) => CompOperatorVariant::LessOrEquals,
                LkqlNode::OpGt(_) => CompOperatorVariant::Greater,
                LkqlNode::OpGeq(_) => CompOperatorVariant::GreaterOrEquals,
                _ => unreachable!(),
            },
        )
    }
}

impl MiscOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Self {
        MiscOperator::new(
            loc(ctx, node),
            match node {
                LkqlNode::OpConcat(_) => MiscOperatorVariant::Concat,
                _ => unreachable!(),
            },
        )
    }
}

// ----- Lowering helpers -----

/// Shortcut function to create a new source section from an LKQL node.
fn loc(ctx: &LoweringContext<LkqlNode>, node: &LkqlNode) -> SourceSection {
    let sloc_range = node.sloc_range().unwrap();
    SourceSection::new(
        ctx.lowered_source,
        Location::from_lkql_location(sloc_range.start),
        Location::from_lkql_location(sloc_range.end),
    )
}

/// Shortcut function to create a new identifier from an LKQL node.
fn id(ctx: &LoweringContext<LkqlNode>, node: &LkqlNode) -> Identifier {
    Identifier::new(loc(ctx, node), node.text().unwrap())
}

/// Shortcut function to create an identifier from a string.
fn id_str(origin_location: SourceSection, text: &str) -> Identifier {
    Identifier::new(origin_location, String::from(text))
}

/// Shortcut function to create a new intermediate node.
fn n(origin_location: SourceSection, variant: NodeVariant) -> Node {
    Node::new(origin_location, variant)
}

/// Shortcut function to create a new intermediate node wrapped in a [`Box`].
fn bn(origin_location: SourceSection, variant: NodeVariant) -> Box<Node> {
    Box::new(n(origin_location, variant))
}

fn lower_dot_access(
    ctx: &LoweringContext<LkqlNode>,
    origin_location: &SourceSection,
    prefix_ref: Box<Node>,
    member: &LkqlNode,
    is_safe: bool,
) -> NodeVariant {
    // Create the node to execute if the prefix in dot
    // access is null
    let if_prefix_null = n(
        *origin_location,
        if is_safe {
            NodeVariant::NullLiteral
        } else {
            NodeVariant::RuntimeError { error_template: &NULL_DOT_RECEIVER, message_args: vec![] }
        },
    );

    // Create the member access node
    let member_access = n(
        *origin_location,
        NodeVariant::DottedExpr { prefix: prefix_ref.clone(), suffix: id(ctx, member) },
    );

    // Return the dot access wrapped in a null checking node.
    prefix_ref.with_equality_check(NodeVariant::NullLiteral, if_prefix_null, member_access)
}

/// Util function to get whether the provided LKQL parsing node introduce a
/// new lexical scope.
fn has_lexical_scope(node: &LkqlNode) -> bool {
    matches!(node, LkqlNode::BlockExpr(_) | LkqlNode::IsClause(_))
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
///   * [`LkqlNode::BlockExpr`]
///   * [`LkqlNode::IsClause`]
///   * [`LkqlNode::NodePatternSelector`]
fn all_local_decls(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Box<Diagnostic>> {
    for maybe_child in node {
        if let Some(child) = maybe_child? {
            match &child {
                // Symbol introducing nodes
                LkqlNode::ValDecl(_) => {
                    all_local_decls(&child, output)?;
                    output.push(child);
                }
                LkqlNode::FunDecl(_) => output.push(child),
                LkqlNode::SelectorDecl(_) => output.push(child),
                LkqlNode::Import(_) => output.push(child),
                LkqlNode::ComplexPattern(pattern) => {
                    if let Some(binding) = pattern.f_binding()? {
                        output.push(binding);
                    }
                    all_local_decls(&child, output)?;
                }
                LkqlNode::SplatPattern(pattern) => {
                    if let Some(binding) = pattern.f_binding()? {
                        output.push(binding);
                    }
                    all_local_decls(&child, output)?;
                }

                // Recursion bounds
                LkqlNode::TopLevelList(_)
                | LkqlNode::AnonymousFunction(_)
                | LkqlNode::ListComprehension(_)
                | LkqlNode::BlockExpr(_)
                | LkqlNode::IsClause(_)
                | LkqlNode::NodePatternSelector(_) => (),

                // Default case, explore all children
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
fn all_local_symbols(
    node: &LkqlNode,
    ctx: &LoweringContext<LkqlNode>,
) -> Result<Vec<Identifier>, Box<Diagnostic>> {
    // Declare working vectors and get all local declarations
    let mut local_decls = Vec::new();
    let mut local_symbols = Vec::new();
    all_local_decls(node, &mut local_decls)?;

    // For each declaration, create an identifier
    for decl in &local_decls {
        local_symbols.push(id(
            ctx,
            &match decl {
                LkqlNode::ValDecl(vd) => vd.f_identifier()?,
                LkqlNode::FunDecl(fd) => fd.f_name()?,
                LkqlNode::SelectorDecl(sd) => sd.f_name()?,
                LkqlNode::Import(i) => i.f_name()?,
                LkqlNode::Identifier(id) => id.as_node(),
                _ => unreachable!(),
            },
        ));
    }

    // Finally, return all symbols in the node
    Ok(local_symbols)
}

/// Util function to find all execution units in the local environment of the
/// provided node.
/// A node is considered as a "execution units" if it can be lowered by the
/// [`ExecutionUnit::lower_lkql_node`] method.
/// The locality is different from the one defined in the [`all_local_decls`]
/// function. We explore the whole tree to found all units, stopping the
/// recursion on execution units bodies. IOW, we return all direct children
/// units.
fn all_local_execution_units(
    node: &LkqlNode,
    output: &mut Vec<LkqlNode>,
) -> Result<(), Box<Diagnostic>> {
    for maybe_child in node {
        if let Some(child) = maybe_child? {
            match child {
                LkqlNode::TopLevelList(_)
                | LkqlNode::FunDecl(_)
                | LkqlNode::SelectorDecl(_)
                | LkqlNode::AnonymousFunction(_)
                | LkqlNode::ListComprehension(_)
                | LkqlNode::NodePatternSelector(_) => output.push(child),
                _ => all_local_execution_units(&child, output)?,
            }
        }
    }
    Ok(())
}
