//! # LKQL lowering module
//!
//! This module contains all required operations to lower an LKQL parsing tree
//! to the intermediate representation defined in the
//! [`crate::intermediate_tree`] module.

use crate::{
    ExecutionContext,
    builtins::{
        traits::{self, BuiltinTrait},
        types::{
            self, BuiltinType, TYPE_NAME_FIELD,
            stream::selector_list::{
                REC_RECURSE_FIELD, REC_RECURSE_UNPACK_FIELD, REC_RESULT_FIELD,
                REC_RESULT_UNPACK_FIELD,
            },
        },
    },
    diagnostics::{Diagnostic, DiagnosticCollector, Hint},
    errors::{
        AMBIGUOUS_IMPORT, ErrorTemplate, INDEX_OUT_OF_BOUNDS, INVALID_SELECTOR_CALL, MISSING_TRAIT,
        MODULE_NOT_FOUND, MULTIPLE_SPLAT_PATTERNS, NULL_DOT_RECEIVER, POS_AFTER_NAMED_ARGUMENT,
        PREVIOUS_NAMED_ARG_HINT, PREVIOUS_SPLAT_PATTERN_HINT, REGEX_SYNTAX_ERROR, REGEX_TOO_BIG,
        SUBPATTERN_AFTER_SPLAT, UNKNOWN_MEMBER, UNKNOWN_NODE_TYPE, WRONG_PARAM_TYPE, WRONG_TYPE,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperator, LogicOperatorVariant, MiscOperator,
        MiscOperatorVariant, Node, NodeVariant,
    },
    lowering::{LoweringContext, unescape_string},
    runtime::G_LKQL_IMPORT,
    sources::{Location, SourceId, SourceSection},
};
use liblkqllang::{BaseFunction, LkqlNode};
use regex::Regex;
use std::{
    collections::HashSet,
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
            LkqlNode::SelectorDecl(selector_decl) => selector_decl.f_name()?.text()?,
            LkqlNode::SelectorArmList(selector_arm_list) => {
                match selector_arm_list.parent()?.unwrap() {
                    LkqlNode::SelectorDecl(selector_decl) => {
                        format!("{}#body", selector_decl.f_name()?.text()?)
                    }
                    _ => unreachable!(),
                }
            }
            LkqlNode::AnonymousFunction(_) => ctx.next_lambda_name(),
            LkqlNode::ListComprehension(_) => ctx.next_lazy_comprehension_name(),
            LkqlNode::NodePatternSelector(_) => ctx.next_selector_pattern_name(),
            _ => unreachable!(),
        };

        // Fetch all children units in this execution unit
        let mut local_units = Vec::new();
        match node {
            LkqlNode::ListComprehension(list_comp) => {
                all_local_execution_units(&list_comp.f_expr()?, &mut local_units)?;
                if let Some(guard) = list_comp.f_guard()? {
                    all_local_execution_units(&guard, &mut local_units)?;
                }
            }
            LkqlNode::NodePatternSelector(pattern_selector) => {
                all_local_execution_units(&pattern_selector.f_pattern()?, &mut local_units)?;
            }
            _ => all_local_execution_units(node, &mut local_units)?,
        };
        // Iterate over all children execution units to lower them and to
        // associate each one to an index in the children units vector.
        // This needs to be done before the lowering of the unit itself.
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
            LkqlNode::SelectorDecl(selector_decl) => {
                /// Create a symbol reading node wrapped in a type checking one.
                fn read_int(ctx: &mut LoweringContext<LkqlNode>, id: Identifier) -> Box<Node> {
                    let id_text = id.text.clone();
                    Box::new(
                        n(id.origin_location, NodeVariant::ReadSymbol(id))
                            .with_param_type_requirement(ctx, &types::int::TYPE, id_text),
                    )
                }

                let name_location = loc(ctx, &selector_decl.f_name()?);
                let root_id = id_str(name_location, "root");
                let depth_id = id_str(name_location, "depth");
                let min_depth_id = id_str(name_location, "min_depth");
                let max_depth_id = id_str(name_location, "max_depth");
                let default_val =
                    Some(n(name_location, NodeVariant::IntLiteral(String::from("-1"))));
                ExecutionUnitVariant::Function {
                    params: vec![
                        (root_id.clone(), None),
                        (depth_id.clone(), default_val.clone()),
                        (min_depth_id.clone(), default_val.clone()),
                        (max_depth_id.clone(), default_val),
                    ],
                    body: n(
                        l,
                        NodeVariant::SelectorInstantiation {
                            root: bn(name_location, NodeVariant::ReadSymbol(root_id)),
                            depth: read_int(ctx, depth_id),
                            min_depth: read_int(ctx, min_depth_id),
                            max_depth: read_int(ctx, max_depth_id),
                            body_index: *ctx.child_index_map.get(&selector_decl.f_arms()?).unwrap(),
                        },
                    ),
                }
            }
            LkqlNode::SelectorArmList(_) => {
                let this_identifier = id_str(l, "this");
                ExecutionUnitVariant::RawCallable {
                    params: vec![this_identifier.clone()],
                    body: lower_matching_arms(
                        ctx,
                        n(l, NodeVariant::ReadSymbol(this_identifier)),
                        node,
                    )?,
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

                // Get the pattern source
                let pattern_source = selector_pattern.f_pattern()?;

                // Then, lower the selector pattern matching logic as a
                // function.
                ExecutionUnitVariant::Function {
                    params: vec![(self_param_name.clone(), None)],
                    body: n(
                        loc(ctx, &pattern_source),
                        NodeVariant::InLexicalScope {
                            local_symbols: all_local_symbols(&pattern_source, ctx)?,
                            expr: Box::new(
                                Node::lower_lkql_pattern(ctx, &pattern_source, self_param_id)?
                                    .with_let(
                                        self_param_id,
                                        n(l, NodeVariant::ReadSymbol(self_param_name)),
                                    ),
                            ),
                        },
                    ),
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

                // Create the node that will represents the value to initialize
                // the module local name.
                let module_value = if module_files.len() == 1 {
                    NodeVariant::CallExpr {
                        callee: bn(l, NodeVariant::ReadSymbol(id_str(l, G_LKQL_IMPORT))),
                        positional_args: vec![n(
                            module_name.origin_location,
                            NodeVariant::StringLiteral(String::from(
                                module_files.remove(0).to_string_lossy(),
                            )),
                        )],
                        named_args: vec![],
                    }
                } else if module_files.is_empty() {
                    ctx.diagnostics.add(Diagnostic::error_from_template(
                        &l,
                        &MODULE_NOT_FOUND,
                        &[&module_name.text],
                    ));
                    NodeVariant::UnitLiteral
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
                    NodeVariant::UnitLiteral
                };

                // Check that there is exactly one matching file, and create
                // the importation function call.
                NodeVariant::InitLocal { symbol: module_name, val: bn(l, module_value) }
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
                            lower_member_access_with_prefix_check(
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
                                    NodeVariant::CallExpr {
                                        callee: callee.clone(),
                                        positional_args,
                                        named_args: named_args.clone(),
                                    },
                                ),
                                alternative: bn(
                                    l,
                                    NodeVariant::CallExpr {
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
                    _ => NodeVariant::CallExpr {
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
                n(
                    l,
                    lower_member_access_with_prefix_check(
                        ctx,
                        &l,
                        prefix_ref,
                        &lkql_member,
                        is_safe,
                    ),
                )
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
                    .with_type_requirement(ctx, &types::int::TYPE);

                // Create the node to access the indexed value
                let indexing = n(
                    l,
                    NodeVariant::IndexExpr {
                        indexed_val: Box::new(
                            Self::lower_lkql_node(ctx, &lkql_collection)?
                                .with_trait_requirement(ctx, &traits::indexable::TRAIT),
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
                result_ref
                    .clone()
                    .with_equality_check(
                        NodeVariant::NilLiteral,
                        if_index_out_of_bounds,
                        result_ref,
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
                        .with_trait_requirement(ctx, &traits::iterable::TRAIT),
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
                        .with_type_requirement(ctx, &types::bool::TYPE),
                ),
                consequence: Box::new(Self::lower_lkql_node(ctx, &cond_expr.f_then_expr()?)?),
                alternative: cond_expr
                    .f_else_expr()?
                    .map(|n| Self::lower_lkql_node(ctx, &n))
                    .transpose()?
                    .map_or(bn(loc(ctx, node), NodeVariant::BoolLiteral(true)), Box::new),
            },

            // --- Match expression
            LkqlNode::Match(match_expr) => {
                let matched_value = Self::lower_lkql_node(ctx, &match_expr.f_matched_val()?)?;
                lower_matching_arms(ctx, matched_value, &match_expr.f_arms()?)?.variant
            }

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

            // --- Rec expression
            LkqlNode::RecExpr(rec_expr) => {
                /// Create a boolean literal from an unpack LKQL node.
                fn to_boolean_literal(
                    ctx: &mut LoweringContext<LkqlNode>,
                    unpack_node: &LkqlNode,
                ) -> Node {
                    n(
                        loc(ctx, unpack_node),
                        NodeVariant::BoolLiteral(matches!(unpack_node, LkqlNode::UnpackPresent(_))),
                    )
                }

                // Get sources for the recurse and result expression
                let recurse_expr_source = rec_expr.f_recurse_expr()?;
                let recurse_unpack_source = rec_expr.f_recurse_unpack()?;
                let result_unpack_source = rec_expr.f_result_unpack()?;
                let recurse_unpack_loc = loc(ctx, &recurse_unpack_source);
                let result_unpack_loc = loc(ctx, &result_unpack_source);

                // Create a vector that will contains the object fields and
                // place the lowered recurse expression.
                let mut object_fields = vec![
                    (
                        id_str(loc(ctx, &recurse_expr_source), REC_RECURSE_FIELD),
                        Self::lower_lkql_node(ctx, &recurse_expr_source)?,
                    ),
                    (
                        id_str(recurse_unpack_loc, REC_RECURSE_UNPACK_FIELD),
                        to_boolean_literal(ctx, &recurse_unpack_source),
                    ),
                ];

                // If there is a result expression, add it to the object fields
                if let Some(result_expr_source) = rec_expr.f_result_expr()? {
                    object_fields.append(&mut vec![
                        (
                            id_str(loc(ctx, &result_expr_source), REC_RESULT_FIELD),
                            Self::lower_lkql_node(ctx, &result_expr_source)?,
                        ),
                        (
                            id_str(result_unpack_loc, REC_RESULT_UNPACK_FIELD),
                            to_boolean_literal(ctx, &result_unpack_source),
                        ),
                    ]);
                }

                // Then return the object literal representing the "rec"
                // expression.
                NodeVariant::ObjectLiteral(object_fields)
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
                                    .with_trait_requirement(ctx, &traits::iterable::TRAIT)
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
                        left: Box::new(left.with_type_requirement(ctx, &types::bool::TYPE)),
                        operator: LogicOperator::lower_lkql_node(&lkql_operator, ctx)?,
                        right: Box::new(right.with_type_requirement(ctx, &types::bool::TYPE)),
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
                        operand: Box::new(operand.with_type_requirement(ctx, &types::int::TYPE)),
                    },
                    LkqlNode::OpNot(_) => NodeVariant::LogicUnOp {
                        operator: LogicOperator::lower_lkql_node(&lkql_operator, ctx)?,
                        operand: Box::new(operand.with_type_requirement(ctx, &types::bool::TYPE)),
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
                let raw_str = node.text()?;
                NodeVariant::StringLiteral(unescape_string(&raw_str[1..raw_str.len() - 1]))
            }
            LkqlNode::BlockStringLiteral(block_string) => {
                let mut builder = String::new();
                for maybe_str_part in &block_string.f_docs()? {
                    if let Some(str_part) = maybe_str_part? {
                        builder.push_str(&unescape_string(&str_part.text()?[3..]));
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
                    .get_node_types()
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

            // --- Regex pattern
            LkqlNode::RegexPattern(regex_pattern) => {
                // Get the regex source
                let raw_str = regex_pattern.text()?;
                let regex_source = String::from(&raw_str[1..raw_str.len() - 1]);

                // Try to compile the regex
                match Regex::new(&format!("^{regex_source}$")) {
                    Ok(regex) => {
                        // Create the node to check the value type
                        let type_check = n(
                            l,
                            NodeVariant::InstanceOf {
                                expression: matched_value_ref.clone(),
                                expected_type_tag: types::str::TYPE.tag,
                            },
                        );

                        // Create the node to check if the pattern match the value
                        let is_match = n(l, NodeVariant::PatternLiteral(regex)).with_method_call(
                            ctx,
                            types::pattern::IS_MATCH_FIELD,
                            vec![*matched_value_ref],
                        );

                        // Then return the checking combination
                        combine_predicates(vec![type_check, is_match]).variant
                    }
                    Err(error) => {
                        ctx.diagnostics.add(Diagnostic::error_from_template(
                            &l,
                            match error {
                                regex::Error::Syntax(_) => &REGEX_SYNTAX_ERROR,
                                regex::Error::CompiledTooBig(_) => &REGEX_TOO_BIG,
                                _ => unreachable!(),
                            },
                            &[regex_source],
                        ));
                        NodeVariant::BoolLiteral(false)
                    }
                }
            }

            // --- Tuple pattern
            LkqlNode::TuplePattern(tuple_pattern) => {
                // Create a vector to store all sub-patterns
                let mut sub_patterns = Vec::new();

                // Lower all sub-patterns and add them in the vector
                for (i, sub_pattern_source) in tuple_pattern
                    .f_patterns()?
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, c)| c.transpose().map(|c| (i, c)))
                {
                    let elem_id = ctx.new_tmp_id();
                    let sub_pattern = Self::lower_lkql_pattern(ctx, &sub_pattern_source?, elem_id)?;
                    let index_access = sub_pattern.related_node(NodeVariant::IndexExpr {
                        indexed_val: matched_value_ref.clone(),
                        index: bn(
                            sub_pattern.origin_location,
                            NodeVariant::IntLiteral((i + 1).to_string()),
                        ),
                    });
                    sub_patterns.push(sub_pattern.with_let(elem_id, index_access));
                }

                // Store the count of sub-patterns matching elements of the tuple
                let sub_pattern_count = sub_patterns.len();

                // Add type checking to sub-patterns
                sub_patterns.insert(
                    0,
                    n(
                        l,
                        NodeVariant::InstanceOf {
                            expression: matched_value_ref.clone(),
                            expected_type_tag: types::tuple::TYPE.tag,
                        },
                    ),
                );

                // Add the tuple size checking to sub-patterns
                sub_patterns.insert(
                    1,
                    n(
                        l,
                        NodeVariant::CompBinOp {
                            left: bn(l, NodeVariant::LengthExpr(matched_value_ref.clone())),
                            operator: CompOperator::new(l, CompOperatorVariant::Equals),
                            right: bn(l, NodeVariant::IntLiteral(sub_pattern_count.to_string())),
                        },
                    ),
                );

                // Finally, combine all sub-patterns in a "and" expression
                combine_predicates(sub_patterns).variant
            }

            // --- List pattern
            LkqlNode::ListPattern(list_pattern) => {
                // Create a variable to store the splat pattern if there is one
                // in the list pattern.
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

                // Store the count of sub-patterns matching elements of the list
                let sub_pattern_count = sub_patterns.len();

                // Add type checking to sub-patterns
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

                // Add the list size checking to sub-patterns
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
                // corresponding initialization at the end of sub-patterns
                if let Some(LkqlNode::SplatPattern(splat_pattern)) = maybe_splat_pattern
                    && let Some(binding) = splat_pattern.f_binding()?
                {
                    let l = loc(ctx, &binding);
                    let sublist_call = matched_value_ref.clone().with_method_call(
                        ctx,
                        types::list::SUBLIST_FIELD,
                        vec![
                            n(l, NodeVariant::IntLiteral((sub_pattern_count + 1).to_string())),
                            n(l, NodeVariant::LengthExpr(matched_value_ref)),
                        ],
                    );
                    sub_patterns.push(n(
                        l,
                        NodeVariant::BlockExpr {
                            body: vec![n(
                                l,
                                NodeVariant::InitLocal {
                                    symbol: id(ctx, &binding),
                                    val: Box::new(sublist_call),
                                },
                            )],
                            val: bn(l, NodeVariant::BoolLiteral(true)),
                        },
                    ));
                }

                // Finally, combine all sub-patterns in a "and" expression
                combine_predicates(sub_patterns).variant
            }

            // --- Object pattern
            LkqlNode::ObjectPattern(object_pattern) => {
                // Create working variables
                let mut maybe_splat_pattern = None;
                let mut matched_fields = HashSet::new();

                // Start by lowering all sub-patterns
                let mut sub_patterns = Vec::new();
                for sub_pattern_source in object_pattern
                    .f_patterns()?
                    .into_iter()
                    .filter_map(Result::transpose)
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
                        assoc_source => match &assoc_source {
                            LkqlNode::ObjectPatternAssoc(opa) => {
                                // Get the name of the matched field
                                let field_name = opa.f_name()?;

                                // Create the identifier to access the field
                                // value and the sub-tree to initialize it.
                                let elem_id = ctx.new_tmp_id();
                                let dot_access = n(
                                    loc(ctx, &assoc_source),
                                    NodeVariant::DottedExpr {
                                        prefix: matched_value_ref.clone(),
                                        suffix: id(ctx, &field_name),
                                    },
                                );

                                // Add the field match in sub-patterns and
                                // store the matched fields name.
                                sub_patterns.push(
                                    Self::lower_lkql_pattern(ctx, &opa.f_pattern()?, elem_id)?
                                        .with_let(elem_id, dot_access),
                                );
                                matched_fields.insert(field_name.text()?);
                            }
                            _ => unreachable!(),
                        },
                    }
                }

                // Add type checking to sub-patterns
                sub_patterns.insert(
                    0,
                    n(
                        l,
                        NodeVariant::InstanceOf {
                            expression: matched_value_ref.clone(),
                            expected_type_tag: types::obj::TYPE.tag,
                        },
                    ),
                );

                // Create the node to access the new object without matched keys
                let without_keys_call = Box::new(matched_value_ref.clone().with_method_call(
                    ctx,
                    types::obj::WITHOUT_KEYS_FIELD,
                    vec![n(
                        l,
                        NodeVariant::ListLiteral(
                            matched_fields
                                .into_iter()
                                .map(|f| n(l, NodeVariant::StringLiteral(f)))
                                .collect(),
                        ),
                    )],
                ));

                // If there is a splat pattern with a bindings, assign the
                // remaining object to it.
                if let Some(LkqlNode::SplatPattern(splat_pattern)) = maybe_splat_pattern.as_ref()
                    && let Some(binding) = splat_pattern.f_binding()?
                {
                    let l = loc(ctx, &splat_pattern.as_node());
                    sub_patterns.push(n(
                        l,
                        NodeVariant::BlockExpr {
                            body: vec![n(
                                l,
                                NodeVariant::InitLocal {
                                    symbol: id(ctx, &binding),
                                    val: without_keys_call,
                                },
                            )],
                            val: bn(l, NodeVariant::BoolLiteral(true)),
                        },
                    ));
                }
                // Otherwise, check that all keys have been matched
                else if maybe_splat_pattern.is_none() {
                    sub_patterns.push(n(
                        l,
                        NodeVariant::CompBinOp {
                            left: without_keys_call,
                            operator: CompOperator::new(l, CompOperatorVariant::Equals),
                            right: bn(l, NodeVariant::ObjectLiteral(vec![])),
                        },
                    ));
                }

                // Finally, combine all sub-patterns in a "and" expression
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
                            NodeVariant::InitLocal {
                                symbol: id(ctx, &b),
                                val: matched_value_ref.clone(),
                            },
                        ))
                    })
                    .transpose()?
                    .into_iter()
                    .collect::<Vec<_>>();

                // Collect all checks in a vector
                let mut matching_elems = Vec::new();

                // Lower the value pattern if there is on, otherwise check that
                // the value exists.
                if let Some(pattern) = complex_pattern.f_pattern()? {
                    matching_elems.push(Self::lower_lkql_pattern(ctx, &pattern, matched_value_id)?)
                } else {
                    matching_elems.push(n(
                        l,
                        NodeVariant::CompBinOp {
                            left: matched_value_ref,
                            operator: CompOperator::new(l, CompOperatorVariant::NotEquals),
                            right: bn(l, NodeVariant::NilLiteral),
                        },
                    ));
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
                            .with_type_requirement(ctx, &types::bool::TYPE),
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
                            lower_member_with_access_check(
                                ctx,
                                &l,
                                Box::new(matched_value_ref),
                                &pattern_field.f_identifier()?,
                            ),
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
                    NodeVariant::CallExpr {
                        callee: bn(
                            l,
                            lower_member_with_access_check(
                                ctx,
                                &l,
                                Box::new(matched_value_ref),
                                &call.f_name()?,
                            ),
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
                    NodeVariant::CallExpr { callee: selector_callee, positional_args, named_args },
                )
                .with_trait_requirement(ctx, &traits::iterable::TRAIT);

                // Create the quantifier call node and return it
                selector_call
                    .with_method_call(
                        ctx,
                        &lkql_quantified_selector_call.f_quantifier()?.text()?,
                        vec![n(
                            l,
                            NodeVariant::ReadChildUnit(*ctx.child_index_map.get(node).unwrap()),
                        )],
                    )
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
    fn with_type_requirement(
        self,
        ctx: &mut LoweringContext<LkqlNode>,
        required_type: &'static BuiltinType,
    ) -> Self {
        let l = self.origin_location;
        type_requirement_helper(
            ctx,
            self,
            |r| NodeVariant::InstanceOf { expression: r, expected_type_tag: required_type.tag },
            &WRONG_TYPE,
            |actual| {
                vec![
                    n(l, NodeVariant::StringLiteral(String::from(required_type.display_name()))),
                    actual,
                ]
            },
        )
    }

    fn with_param_type_requirement(
        self,
        ctx: &mut LoweringContext<LkqlNode>,
        required_type: &'static BuiltinType,
        param_name: String,
    ) -> Self {
        let l = self.origin_location;
        type_requirement_helper(
            ctx,
            self,
            |r| NodeVariant::InstanceOf { expression: r, expected_type_tag: required_type.tag },
            &WRONG_PARAM_TYPE,
            |actual| {
                vec![
                    n(l, NodeVariant::StringLiteral(String::from(required_type.display_name()))),
                    n(l, NodeVariant::StringLiteral(param_name)),
                    actual,
                ]
            },
        )
    }

    /// Wrap the node in a trait requirement one.
    fn with_trait_requirement(
        self,
        ctx: &mut LoweringContext<LkqlNode>,
        required_trait: &'static BuiltinTrait,
    ) -> Self {
        let l = self.origin_location;
        type_requirement_helper(
            ctx,
            self,
            |r| NodeVariant::HasTrait { expression: r, expected_trait: required_trait },
            &MISSING_TRAIT,
            |actual| {
                vec![
                    n(l, NodeVariant::StringLiteral(String::from(required_trait.name))),
                    actual,
                ]
            },
        )
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
    ) -> Node {
        n(
            self.origin_location,
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
            },
        )
    }

    /// Wrap the provided node in a call to method named `method_name` on the
    /// value represented by this node with provided arguments.
    fn with_method_call(
        self,
        ctx: &mut LoweringContext<LkqlNode>,
        method_name: &str,
        mut positional_args: Vec<Node>,
    ) -> Self {
        // Get the location of the node
        let l = self.origin_location;

        // Create or get temporary valid id and reference for the dispatching
        // parameter.
        let (dispatching_arg_id, dispatching_arg_ref) = match &self.variant {
            NodeVariant::Read(id) => (*id, self.clone()),
            _ => {
                let id = ctx.new_tmp_id();
                (id, n(l, NodeVariant::Read(id)))
            }
        };

        // Create the method calling node
        positional_args.insert(0, dispatching_arg_ref.clone());
        let res = n(
            l,
            NodeVariant::CallExpr {
                callee: bn(
                    l,
                    NodeVariant::DottedExpr {
                        prefix: Box::new(dispatching_arg_ref),
                        suffix: id_str(l, method_name),
                    },
                ),
                positional_args,
                named_args: vec![],
            },
        );

        // If required, wraps the result node in a let-in one
        if matches!(&self.variant, NodeVariant::Read(_)) {
            res
        } else {
            res.with_let(dispatching_arg_id, self)
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

/// Lower access to `member` on `prefix` and wrap the result in a runtime check
/// to ensure the access succeeded.
fn lower_member_with_access_check(
    ctx: &mut LoweringContext<LkqlNode>,
    origin_location: &SourceSection,
    prefix: Box<Node>,
    member: &LkqlNode,
) -> NodeVariant {
    // Make a alias to the origin location
    let l = *origin_location;

    // Get the suffix and create an identifier from it
    let suffix = id(ctx, member);
    let suffix_loc = suffix.origin_location;
    let suffix_text = suffix.text.clone();

    // Create the member access node with a check to ensure it succeeded
    let result_id = ctx.new_tmp_id();
    let result_ref = n(l, NodeVariant::Read(result_id));
    NodeVariant::Let {
        id: result_id,
        value: bn(l, NodeVariant::DottedExpr { prefix, suffix }),
        r#in: Box::new(result_ref.clone().with_equality_check(
            NodeVariant::NilLiteral,
            n(
                suffix_loc,
                NodeVariant::RuntimeError {
                    error_template: &UNKNOWN_MEMBER,
                    message_args: vec![n(l, NodeVariant::StringLiteral(suffix_text))],
                },
            ),
            result_ref,
        )),
    }
}

/// Lower access of `member` on the `prefix_ref` node with a wrapper that
/// checks whether the prefix is null.
///
/// According to `is_safe`, raise an error if the prefix is null, or return
/// null.
fn lower_member_access_with_prefix_check(
    ctx: &mut LoweringContext<LkqlNode>,
    origin_location: &SourceSection,
    prefix_ref: Box<Node>,
    member: &LkqlNode,
    is_safe: bool,
) -> NodeVariant {
    // Make a alias to the origin location
    let l = *origin_location;

    // Create the node to execute if the prefix in dot
    // access is null
    let if_prefix_null = n(
        l,
        if is_safe {
            NodeVariant::NullLiteral
        } else {
            NodeVariant::RuntimeError { error_template: &NULL_DOT_RECEIVER, message_args: vec![] }
        },
    );

    // Return the dot access wrapped in a null checking node.
    prefix_ref
        .clone()
        .with_equality_check(
            NodeVariant::NullLiteral,
            if_prefix_null,
            n(l, lower_member_with_access_check(ctx, &l, prefix_ref, member)),
        )
        .variant
}

/// Lower the provided arm list as an conditional expression matching the
/// provided `matched_value` and returning the value of the arm that succeeds
/// to match.
fn lower_matching_arms(
    ctx: &mut LoweringContext<LkqlNode>,
    matched_value: Node,
    arm_list: &LkqlNode,
) -> Result<Node, Box<Diagnostic>> {
    // Create an identifier for the value to match
    let matched_value_id = ctx.new_tmp_id();

    // Create a vector with all arm information structured in tuples:
    // - The first element is the list of symbols in the arm
    // - The second element is the pattern to match
    // - The third one is the resulting expression
    let mut arm_sources = Vec::new();
    for arm_source in arm_list {
        match &arm_source?.unwrap() {
            a @ LkqlNode::MatchArm(arm) => {
                arm_sources.push((all_local_symbols(a, ctx)?, arm.f_pattern()?, arm.f_expr()?))
            }
            a @ LkqlNode::SelectorArm(arm) => {
                arm_sources.push((all_local_symbols(a, ctx)?, arm.f_pattern()?, arm.f_expr()?))
            }
            _ => unreachable!(),
        }
    }

    // Now lower arm sources and collect the result in a vector
    let mut arms = Vec::new();
    for (locals, pattern, expr) in arm_sources {
        arms.push((
            locals,
            Node::lower_lkql_pattern(ctx, &pattern, matched_value_id)?,
            Node::lower_lkql_node(ctx, &expr)?,
        ));
    }

    // Combine all match arms in a conditional expression
    Ok(arms
        .into_iter()
        .rev()
        .fold(
            n(loc(ctx, arm_list), NodeVariant::UnitLiteral),
            |alt, (local_symbols, pattern, expr)| {
                let l = SourceSection::range(&pattern.origin_location, &expr.origin_location);
                n(
                    l,
                    NodeVariant::InLexicalScope {
                        local_symbols,
                        expr: bn(
                            l,
                            NodeVariant::IfExpr {
                                condition: Box::new(pattern),
                                consequence: Box::new(expr),
                                alternative: bn(
                                    alt.origin_location,
                                    NodeVariant::OutsideLexicalScope(Box::new(alt)),
                                ),
                            },
                        ),
                    },
                )
            },
        )
        .with_let(matched_value_id, matched_value))
}

/// Util function to emit a type checking node that raise an error in case of
/// failure.
fn type_requirement_helper<F: Fn(Box<Node>) -> NodeVariant, G: FnOnce(Node) -> Vec<Node>>(
    ctx: &mut LoweringContext<LkqlNode>,
    node: Node,
    condition_creator: F,
    error_template: &'static ErrorTemplate,
    error_args_creator: G,
) -> Node {
    // Create working variables
    let l = node.origin_location;
    let checked_value_id = ctx.new_tmp_id();
    let checked_value_ref = bn(l, NodeVariant::Read(checked_value_id));

    // Create the list of error template arguments
    let message_args = error_args_creator(n(
        l,
        NodeVariant::DottedExpr {
            prefix: checked_value_ref.clone(),
            suffix: id_str(l, TYPE_NAME_FIELD),
        },
    ));

    // Create the node that check whether the value has the require trait
    let check_trait = bn(
        l,
        NodeVariant::IfExpr {
            condition: bn(l, condition_creator(checked_value_ref.clone())),
            consequence: checked_value_ref,
            alternative: bn(l, NodeVariant::RuntimeError { error_template, message_args }),
        },
    );

    // Then return the node that ensure the value has the required trait
    check_trait.with_let(checked_value_id, node)
}

/// Util function to get whether the provided LKQL parsing node introduce a
/// new lexical scope.
fn has_lexical_scope(node: &LkqlNode) -> bool {
    matches!(node, LkqlNode::BlockExpr(_) | LkqlNode::IsClause(_))
}

/// Util function to find all local declarations from the provided node.
/// Is defined as "declaration" all nodes that introduce a new symbol in the
/// lexical environment.
/// A declaration is defined as "local" if it isn't contained in the same
/// scope as the current node.
/// Are defined as "scope introducing" the following LKQL nodes:
///   * [`LkqlNode::TopLevelList`]
///   * [`LkqlNode::FunDecl`]
///   * [`LkqlNode::SelectorDecl`]
///   * [`LkqlNode::SelectorArmList`]
///   * [`LkqlNode::AnonymousFunction`]
///   * [`LkqlNode::ListComprehension`]
///   * [`LkqlNode::BlockExpr`]
///   * [`LkqlNode::IsClause`]
///   * [`LkqlNode::MatchArm`]
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
                LkqlNode::Identifier(_) => {
                    match child.parent()? {
                        Some(LkqlNode::ComplexPattern(complex_pattern)) => {
                            if complex_pattern.f_binding()?.as_ref() == Some(&child) {
                                output.push(child);
                            }
                        }
                        Some(LkqlNode::SplatPattern(_)) => output.push(child),
                        _ => (),
                    };
                }

                // Recursion bounds
                LkqlNode::TopLevelList(_)
                | LkqlNode::SelectorArmList(_)
                | LkqlNode::AnonymousFunction(_)
                | LkqlNode::ListComprehension(_)
                | LkqlNode::BlockExpr(_)
                | LkqlNode::IsClause(_)
                | LkqlNode::MatchArm(_)
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
            match &child {
                LkqlNode::TopLevelList(_)
                | LkqlNode::FunDecl(_)
                | LkqlNode::SelectorDecl(_)
                | LkqlNode::SelectorArmList(_)
                | LkqlNode::AnonymousFunction(_) => output.push(child),
                LkqlNode::ListComprehension(list_comp) => {
                    // For list comprehensions, recurse on generators that
                    // belongs to the current local scope.
                    all_local_execution_units(&list_comp.f_generators()?, output)?;
                    output.push(child);
                }
                LkqlNode::NodePatternSelector(pattern_selector) => {
                    // For node pattern selector, recurse on the call that
                    // belongs to the current local scope.
                    all_local_execution_units(&pattern_selector.f_call()?, output)?;
                    output.push(child);
                }
                _ => all_local_execution_units(&child, output)?,
            }
        }
    }
    Ok(())
}
