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
        AMBIGUOUS_IMPORT, INVALID_SELECTOR_CALL, MODULE_NOT_FOUND, MULTIPLE_SPLAT_PATTERNS,
        POS_AFTER_NAMED_ARGUMENT, PREVIOUS_NAMED_ARG_HINT, PREVIOUS_SPLAT_PATTERN_HINT,
        SUBPATTERN_AFTER_SPLAT, UNKNOWN_NODE_TYPE,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperator, LogicOperatorVariant, MiscOperator,
        MiscOperatorVariant, Node, NodeVariant,
    },
    lowering::{LoweringContext, unescape_string},
    sources::{Location, SourceId, SourceSection},
};
use liblkqllang::{BaseFunction, Exception, LkqlNode, SplatPattern};
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
                lowering_context.diagnostics.add(diag);
                Err(lowering_context.diagnostics)
            }
        }
    }

    /// Internal function to lower an [`LkqlNode`] to an [`ExecutionUnit`].
    fn internal_lower_lkql_node(
        node: &LkqlNode,
        ctx: &mut LoweringContext<LkqlNode>,
    ) -> Result<Self, Diagnostic> {
        // Create the origin location of the execution unit
        let origin_location = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

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
            children_units.push(Self::internal_lower_lkql_node(&unit, ctx)?);
            assert!(
                children_units.len() < u16::MAX as usize,
                "Too many children execution units"
            );
        }

        // Create the variant part of the result
        let variant = match &node {
            LkqlNode::TopLevelList(top_level) => {
                // Lower the top level elements
                let mut module_elements = Vec::new();
                for maybe_top_level_elem in top_level {
                    if let Some(top_level_elem) = maybe_top_level_elem? {
                        module_elements.push(Node::lower_lkql_node(ctx, &top_level_elem)?);
                    }
                }

                // Create the resulting module
                ExecutionUnitVariant::Module {
                    symbols: all_local_symbols(node, ctx)?,
                    elements: module_elements,
                }
            }
            LkqlNode::FunDecl(_) | LkqlNode::AnonymousFunction(_) => {
                // Get the function name, parameters and body nodes
                let (params_node, body_node) = match &node {
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
                for maybe_param_decl in &params_node {
                    match maybe_param_decl? {
                        Some(LkqlNode::ParameterDecl(param_decl)) => {
                            let name =
                                Identifier::from_lkql_node(&param_decl.f_param_identifier()?, ctx)?;
                            let default_expr = param_decl
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
                    body: Node::lower_lkql_node(ctx, &body_node)?,
                }
            }
            LkqlNode::ListComprehension(list_comp) => {
                // Get the collection bindings in the list comprehension, those
                // are going to be the parameters of the created function
                let collection_bindings: Result<Vec<Identifier>, Diagnostic> = list_comp
                    .f_generators()?
                    .into_iter()
                    .map(|n| -> Result<Identifier, Diagnostic> {
                        match n? {
                            Some(LkqlNode::ListCompAssoc(assoc)) => {
                                Ok(Identifier::from_lkql_node(&assoc.f_binding_name()?, ctx)?)
                            }
                            _ => unreachable!(),
                        }
                    })
                    .collect();

                // Create the intermediate node representing the body of the
                // function.
                let body = match list_comp.f_guard()? {
                    Some(guard) => Node::new(
                        origin_location.clone(),
                        NodeVariant::IfExpr {
                            condition: Box::new(Node::lower_lkql_node(ctx, &guard)?),
                            consequence: Box::new(Node::lower_lkql_node(
                                ctx,
                                &list_comp.f_expr()?,
                            )?),
                            alternative: Box::new(Node::new(
                                origin_location.clone(),
                                NodeVariant::NilLiteral,
                            )),
                        },
                    ),
                    None => Node::lower_lkql_node(ctx, &list_comp.f_expr()?)?,
                };

                // Then return the new function execution unit variant
                ExecutionUnitVariant::RawCallable { params: collection_bindings?, body }
            }
            LkqlNode::NodePatternSelector(selector_pattern) => {
                let node_param_name =
                    Identifier::new(origin_location.clone(), String::from("node"));
                let node_param_id = ctx.new_tmp_id();
                ExecutionUnitVariant::Function {
                    params: vec![(node_param_name.clone(), None)],
                    body: Node::new(
                        origin_location.clone(),
                        NodeVariant::Let {
                            id: node_param_id,
                            value: Box::new(Node::new(
                                origin_location.clone(),
                                NodeVariant::ReadSymbol(node_param_name),
                            )),
                            r#in: Box::new(Node::lower_lkql_pattern(
                                ctx,
                                &selector_pattern.f_pattern()?,
                                node_param_id,
                            )?),
                        },
                    ),
                }
            }
            _ => unreachable!(),
        };

        // Finally return the new execution unit
        Ok(ExecutionUnit::new(origin_location, name, children_units, variant))
    }
}

impl Node {
    /// Lower an LKQL node as an intermediate node. All LKQL node kinds should
    /// be accepted by this function.
    fn lower_lkql_node(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
    ) -> Result<Self, Diagnostic> {
        // Get the location of the node
        let origin_location = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

        // Util function used to create a node related to the one currently
        // lowered. The result node is wrapped in a Box and has the same origin
        // location os the current one.
        let related_node =
            |variant: NodeVariant| Box::new(Node::new(origin_location.clone(), variant));

        // Lower the node
        let variant = match node {
            // --- Declarations
            LkqlNode::ValDecl(val_decl) => NodeVariant::InitLocal {
                symbol: Identifier::from_lkql_node(&val_decl.f_identifier()?, ctx)?,
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
                let module_name_node = import.f_name()?;
                let module_name = module_name_node.text()?;
                let module_file_name = format!("{module_name}.lkql");
                let module_files = searching_dirs
                    .iter()
                    .filter_map(|d| {
                        let possible_module_file = d.join(Path::new(&module_file_name));
                        if possible_module_file.exists() && possible_module_file.is_file() {
                            Some(possible_module_file)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                // Check that there is exactly one matching file
                let module_file = match &module_files[..] {
                    [f] => f,
                    [] => {
                        ctx.diagnostics.add(Diagnostic::error_from_template(
                            &origin_location,
                            &MODULE_NOT_FOUND,
                            &[module_name],
                        ));
                        &PathBuf::new()
                    }
                    x => {
                        ctx.diagnostics.add(Diagnostic::error_from_template(
                            &origin_location,
                            &AMBIGUOUS_IMPORT,
                            &[x.iter()
                                .map(|f| f.to_string_lossy())
                                .collect::<Vec<_>>()
                                .join(" & ")],
                        ));
                        &PathBuf::new()
                    }
                };

                // Finally return the created node variant
                NodeVariant::ImportModule {
                    name: Identifier::from_lkql_node(&module_name_node, ctx)?,
                    file: module_file.clone(),
                }
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
                        let (receiver, member, is_safe) = match name {
                            LkqlNode::DotAccess(dot_access) => {
                                (dot_access.f_receiver(), dot_access.f_member(), false)
                            }
                            LkqlNode::SafeAccess(safe_access) => {
                                (safe_access.f_receiver(), safe_access.f_member(), true)
                            }
                            _ => unreachable!(),
                        };

                        // Create a new named temporary value to compute the
                        // prefix part of the dot access only once.
                        let prefix_id = ctx.new_tmp_id();
                        let prefix_ref =
                            Node::new(origin_location.clone(), NodeVariant::Read(prefix_id));

                        // Create a callee access node that use the prefix
                        // temporary value created before.
                        let callee = related_node(NodeVariant::DottedExpr {
                            prefix: Box::new(prefix_ref.clone()),
                            suffix: Identifier::from_lkql_node(&member?, ctx)?,
                            is_safe,
                        });

                        // Then create a vector argument containing the callee
                        // that is used in the case of a method call.
                        let mut method_positional_args = positional_args.clone();
                        method_positional_args.insert(0, prefix_ref.clone());

                        // Finally create a node to check the type of the
                        // prefix and decide at runtime how to act.
                        NodeVariant::Let {
                            id: prefix_id,
                            value: Box::new(Self::lower_lkql_node(ctx, &receiver?)?),
                            r#in: related_node(NodeVariant::IfExpr {
                                condition: related_node(NodeVariant::InstanceOf {
                                    expression: Box::new(prefix_ref),
                                    expected_type_tag: types::namespace::TYPE.tag,
                                }),
                                consequence: related_node(NodeVariant::FunCall {
                                    callee: callee.clone(),
                                    positional_args: positional_args,
                                    named_args: named_args.clone(),
                                }),
                                alternative: related_node(NodeVariant::FunCall {
                                    callee: callee,
                                    positional_args: method_positional_args,
                                    named_args,
                                }),
                            }),
                        }
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
                let (receiver, member, is_safe) = match node {
                    LkqlNode::DotAccess(dot_access) => {
                        (dot_access.f_receiver(), dot_access.f_member(), false)
                    }
                    LkqlNode::SafeAccess(safe_access) => {
                        (safe_access.f_receiver(), safe_access.f_member(), true)
                    }
                    _ => unreachable!(),
                };
                NodeVariant::DottedExpr {
                    prefix: Box::new(Self::lower_lkql_node(ctx, &receiver?)?),
                    suffix: Identifier::from_lkql_node(&member?, ctx)?,
                    is_safe,
                }
            }

            // --- Index expression
            LkqlNode::Indexing(_) | LkqlNode::SafeIndexing(_) => {
                let (coll_expr, index, is_safe) = match node {
                    LkqlNode::Indexing(indexing) => {
                        (indexing.f_collection_expr()?, indexing.f_index_expr()?, false)
                    }
                    LkqlNode::SafeIndexing(safe_indexing) => {
                        (safe_indexing.f_collection_expr()?, safe_indexing.f_index_expr()?, true)
                    }
                    _ => unreachable!(),
                };
                NodeVariant::IndexExpr {
                    indexed_val: Box::new(
                        Self::lower_lkql_node(ctx, &coll_expr)?
                            .with_trait_requirement(&traits::indexable::TRAIT)?,
                    ),
                    index: Box::new(
                        Self::lower_lkql_node(ctx, &index)?
                            .with_type_requirement(&types::int::TYPE)?,
                    ),
                    is_safe,
                }
            }

            // --- In clause
            LkqlNode::InClause(in_clause) => NodeVariant::InClause {
                value: Box::new(Self::lower_lkql_node(ctx, &in_clause.f_value_expr()?)?),
                collection: Box::new(
                    Self::lower_lkql_node(ctx, &in_clause.f_list_expr()?)?
                        .with_trait_requirement(&traits::iterable::TRAIT)?,
                ),
            },

            // --- Is clause
            LkqlNode::IsClause(is_clause) => {
                let value_id = ctx.new_tmp_id();
                NodeVariant::Let {
                    id: value_id,
                    value: Box::new(Self::lower_lkql_node(ctx, &is_clause.f_node_expr()?)?),
                    r#in: Box::new(Self::lower_lkql_pattern(
                        ctx,
                        &is_clause.f_pattern()?,
                        value_id,
                    )?),
                }
            }

            // --- If expression
            LkqlNode::CondExpr(cond_expr) => NodeVariant::IfExpr {
                condition: Box::new(
                    Self::lower_lkql_node(ctx, &cond_expr.f_condition()?)?
                        .with_type_requirement(&types::bool::TYPE)?,
                ),
                consequence: Box::new(Self::lower_lkql_node(ctx, &cond_expr.f_then_expr()?)?),
                alternative: cond_expr
                    .f_else_expr()?
                    .map(|n| Self::lower_lkql_node(ctx, &n))
                    .transpose()?
                    .map_or(
                        Box::new(Node::new(
                            SourceSection::from_lkql_node(ctx.lowered_source, node)?,
                            NodeVariant::BoolLiteral(true),
                        )),
                        |n| Box::new(n),
                    ),
            },

            // --- Block expression
            LkqlNode::BlockExpr(block_expr) => {
                let body_node = block_expr.f_body()?;
                let mut body = Vec::with_capacity(body_node.children_count()?);
                for maybe_body_part_node in &body_node {
                    if let Some(ref body_part_node) = maybe_body_part_node? {
                        body.push(Self::lower_lkql_node(ctx, body_part_node)?);
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
                    .map(|n| -> Result<Option<LkqlNode>, Exception> {
                        Ok(match n? {
                            Some(LkqlNode::ListCompAssoc(assoc)) => Some(assoc.f_coll_expr()?),
                            None => None,
                            _ => unreachable!(),
                        })
                    })
                    .map(|n| {
                        let expr = Self::lower_lkql_node(ctx, &n?.unwrap())?;
                        expr.with_trait_requirement(&traits::iterable::TRAIT)
                    })
                    .collect::<Result<_, Diagnostic>>()?,
                body_index: *ctx.child_index_map.get(&node).unwrap(),
            },

            // --- Binary operation
            LkqlNode::ArithBinOp(arith_bin_op) => NodeVariant::ArithBinOp {
                left: Box::new(Self::lower_lkql_node(ctx, &arith_bin_op.f_left()?)?),
                operator: ArithOperator::lower_lkql_node(&arith_bin_op.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(ctx, &arith_bin_op.f_right()?)?),
            },
            LkqlNode::RelBinOp(rel_bin_op) => NodeVariant::CompBinOp {
                left: Box::new(Self::lower_lkql_node(ctx, &rel_bin_op.f_left()?)?),
                operator: CompOperator::lower_lkql_node(&rel_bin_op.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(ctx, &rel_bin_op.f_right()?)?),
            },
            LkqlNode::BinOp(bin_op) => {
                let operator_node = bin_op.f_op()?;
                let left = Self::lower_lkql_node(ctx, &bin_op.f_left()?)?;
                let right = Self::lower_lkql_node(ctx, &bin_op.f_right()?)?;
                match &operator_node {
                    LkqlNode::OpAnd(_) | LkqlNode::OpOr(_) => NodeVariant::LogicBinOp {
                        left: Box::new(left.with_type_requirement(&types::bool::TYPE)?),
                        operator: LogicOperator::lower_lkql_node(&operator_node, ctx)?,
                        right: Box::new(right.with_type_requirement(&types::bool::TYPE)?),
                    },
                    LkqlNode::OpConcat(_) => NodeVariant::MiscBinOp {
                        left: Box::new(left),
                        operator: MiscOperator::lower_lkql_node(&operator_node, ctx)?,
                        right: Box::new(right),
                    },
                    _ => unreachable!(),
                }
            }

            // --- Unary operation
            LkqlNode::UnOp(un_op) => {
                let operand = Self::lower_lkql_node(ctx, &un_op.f_operand()?)?;
                let operator_node = un_op.f_op()?;
                match &operator_node {
                    LkqlNode::OpPlus(_) | LkqlNode::OpMinus(_) => NodeVariant::ArithUnOp {
                        operator: ArithOperator::lower_lkql_node(&operator_node, ctx)?,
                        operand: Box::new(operand.with_type_requirement(&types::int::TYPE)?),
                    },
                    LkqlNode::OpNot(_) => NodeVariant::LogicUnOp {
                        operator: LogicOperator::lower_lkql_node(&operator_node, ctx)?,
                        operand: Box::new(operand.with_type_requirement(&types::bool::TYPE)?),
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
                        let text = str_part.text()?;
                        builder.push_str(&unescape_string(
                            text.trim_start_matches("|\"").trim_start(),
                        ));
                        builder.push('\n');
                    }
                }
                NodeVariant::StringLiteral(builder)
            }
            LkqlNode::Tuple(tuple) => {
                let items_node = tuple.f_exprs()?;
                let mut items = Vec::with_capacity(items_node.children_count()?);
                for maybe_item_node in &items_node {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(ctx, item_node)?);
                    }
                }
                NodeVariant::TupleLiteral(items)
            }
            LkqlNode::ListLiteral(list) => {
                let items_node = list.f_exprs()?;
                let mut items = Vec::with_capacity(items_node.children_count()?);
                for maybe_item_node in &items_node {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(ctx, item_node)?);
                    }
                }
                NodeVariant::ListLiteral(items)
            }
            LkqlNode::ObjectLiteral(object) => {
                let assocs_node = object.f_assocs()?;
                let mut assocs = Vec::with_capacity(assocs_node.children_count()?);
                for maybe_assoc_node in &assocs_node {
                    if let Some(LkqlNode::ObjectAssoc(ref assoc_node)) = maybe_assoc_node? {
                        assocs.push((
                            Identifier::from_lkql_node(&assoc_node.f_name()?, ctx)?,
                            Self::lower_lkql_node(ctx, &assoc_node.f_expr()?)?,
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
            _ => panic!("{} is not handled by the lowering phase", node.image()?),
        };

        // Create the result node
        let lowered_node = Node::new(origin_location, variant);

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
    ) -> Result<(), Diagnostic> {
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
                                    &SourceSection::from_lkql_node(ctx.lowered_source, arg)?,
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
                        Identifier::from_lkql_node(&named_arg.f_arg_name()?, ctx)?,
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
    ) -> Result<Self, Diagnostic> {
        // Special handling of the parenthesized pattern
        if let LkqlNode::ParenPattern(parent_pattern) = node {
            return Self::lower_lkql_pattern(ctx, &parent_pattern.f_pattern()?, matched_value_id);
        }

        // Get the location of the pattern node
        let origin_location = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

        // Util function used to create a node related to the one currently
        // lowered. The result node is wrapped in a Box and has the same origin
        // location os the current one.
        let related_node = |variant: NodeVariant| Node::new(origin_location.clone(), variant);

        // Util function to combine a predicate vector in a "and" boolean
        // expression.
        let combine_predicates = |mut predicates: Vec<Node>| match predicates.len() {
            0 => related_node(NodeVariant::BoolLiteral(true)),
            1 => predicates.remove(0),
            _ => {
                let mut iter = predicates.into_iter().rev();
                let init = iter.next().unwrap();
                iter.fold(init, |res, next| {
                    related_node(NodeVariant::LogicBinOp {
                        left: Box::new(next),
                        operator: LogicOperator::new(
                            origin_location.clone(),
                            LogicOperatorVariant::And,
                        ),
                        right: Box::new(res),
                    })
                })
            }
        };

        // Create a node to read the matched value
        let read_value = Box::new(related_node(NodeVariant::Read(matched_value_id)));

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
                    left: read_value,
                    operator: CompOperator::new(
                        origin_location.clone(),
                        CompOperatorVariant::Equals,
                    ),
                    right: Box::new(related_node(target_literal)),
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
                        expression: read_value,
                        expected_type_tag: node_type.tag,
                    }
                } else {
                    ctx.diagnostics.add(Diagnostic::error_from_template(
                        &origin_location,
                        &UNKNOWN_NODE_TYPE,
                        &[&node_type_name],
                    ));
                    NodeVariant::BoolLiteral(false)
                }
            }

            // --- List pattern
            LkqlNode::ListPattern(list_pattern) => {
                // Create working variables
                let mut maybe_splat_pattern: Option<SplatPattern> = None;
                let sub_patterns_source = list_pattern.f_patterns()?;

                // Start by lowering all sub-patterns
                let mut sub_patterns = Vec::new();
                for (i, maybe_sub_pattern_source) in sub_patterns_source
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, c)| c.transpose().map(|c| (i + 1, c)))
                {
                    match maybe_sub_pattern_source? {
                        LkqlNode::SplatPattern(splat_pattern) => {
                            // Emit an error if a splat pattern has already
                            // been stored.
                            if let Some(old_splat_pattern) = maybe_splat_pattern {
                                ctx.diagnostics
                                    .add(Diagnostic::from_error_template_with_hints::<&str>(
                                        &SourceSection::from_lkql_node(
                                            ctx.lowered_source,
                                            &splat_pattern.as_node(),
                                        )?,
                                        &MULTIPLE_SPLAT_PATTERNS,
                                        &[],
                                        vec![Hint::new(
                                            String::from(PREVIOUS_SPLAT_PATTERN_HINT),
                                            SourceSection::from_lkql_node(
                                                ctx.lowered_source,
                                                &old_splat_pattern.as_node(),
                                            )?,
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
                                        &SourceSection::from_lkql_node(
                                            ctx.lowered_source,
                                            &sub_pattern_source,
                                        )?,
                                        &SUBPATTERN_AFTER_SPLAT,
                                        &[],
                                        vec![Hint::new(
                                            String::from(PREVIOUS_SPLAT_PATTERN_HINT),
                                            SourceSection::from_lkql_node(
                                                ctx.lowered_source,
                                                &splat_pattern.as_node(),
                                            )?,
                                        )],
                                    ));
                            } else {
                                let elem_id = ctx.new_tmp_id();
                                let sub_pattern =
                                    Self::lower_lkql_pattern(ctx, &sub_pattern_source, elem_id)?;
                                let index_access =
                                    sub_pattern.related_node(NodeVariant::IndexExpr {
                                        indexed_val: read_value.clone(),
                                        index: Box::new(
                                            sub_pattern.related_node(NodeVariant::IntLiteral(
                                                i.to_string(),
                                            )),
                                        ),
                                        is_safe: false,
                                    });
                                sub_patterns.push(sub_pattern.with_wrapper(|n| {
                                    Ok(NodeVariant::Let {
                                        id: elem_id,
                                        value: Box::new(index_access),
                                        r#in: Box::new(n),
                                    })
                                })?);
                            }
                        }
                    }
                }

                // Store the count of subpatterns matching elements of the list
                let sub_pattern_count = sub_patterns.len();

                // Add type checking to subpatterns
                sub_patterns.insert(
                    0,
                    related_node(NodeVariant::InstanceOf {
                        expression: read_value.clone(),
                        expected_type_tag: types::list::TYPE.tag,
                    }),
                );

                // Add the list size checking to subpatterns
                sub_patterns.insert(
                    1,
                    related_node(NodeVariant::CompBinOp {
                        left: Box::new(related_node(NodeVariant::DottedExpr {
                            prefix: read_value.clone(),
                            suffix: Identifier::new(
                                origin_location.clone(),
                                String::from("length"),
                            ),
                            is_safe: false,
                        })),
                        operator: CompOperator::new(
                            origin_location.clone(),
                            if maybe_splat_pattern.is_some() {
                                CompOperatorVariant::GreaterOrEquals
                            } else {
                                CompOperatorVariant::Equals
                            },
                        ),
                        right: Box::new(related_node(NodeVariant::IntLiteral(
                            sub_pattern_count.to_string(),
                        ))),
                    }),
                );

                // If there is a splat pattern with a binding, add its
                // corresponding initialization at the end of subpatterns
                if let Some(splat_pattern) = maybe_splat_pattern {
                    if let Some(binding) = splat_pattern.f_binding()? {
                        let loc = SourceSection::from_lkql_node(ctx.lowered_source, &binding)?;
                        let id = Identifier::from_lkql_node(&binding, ctx)?;
                        let sublist_access = id.related_node(NodeVariant::DottedExpr {
                            prefix: read_value.clone(),
                            suffix: Identifier::new(loc.clone(), String::from("sublist")),
                            is_safe: false,
                        });
                        let length_access = id.related_node(NodeVariant::DottedExpr {
                            prefix: read_value.clone(),
                            suffix: Identifier::new(loc.clone(), String::from("length")),
                            is_safe: false,
                        });
                        let sublist_call = Box::new(id.related_node(NodeVariant::FunCall {
                            callee: Box::new(sublist_access),
                            positional_args: vec![
                                *read_value,
                                id.related_node(NodeVariant::IntLiteral(
                                    (sub_pattern_count + 1).to_string(),
                                )),
                                length_access,
                            ],
                            named_args: vec![],
                        }));
                        let bool_lit = id.related_node(NodeVariant::BoolLiteral(true));
                        sub_patterns.push(Node::new(
                            loc.clone(),
                            NodeVariant::BlockExpr {
                                body: vec![Node::new(
                                    loc.clone(),
                                    NodeVariant::InitLocal { symbol: id, val: sublist_call },
                                )],
                                val: Box::new(bool_lit),
                            },
                        ));
                    }
                }

                // Finally, combine all subpatterns in a "and" expression
                combine_predicates(sub_patterns).variant
            }

            // --- Not pattern
            LkqlNode::NotPattern(not_pattern) => NodeVariant::LogicUnOp {
                operator: LogicOperator::new(origin_location.clone(), LogicOperatorVariant::Not),
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
                operator: LogicOperator::new(origin_location.clone(), LogicOperatorVariant::Or),
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
                        Ok::<Self, Diagnostic>(Self::new(
                            SourceSection::from_lkql_node(ctx.lowered_source, &b)?,
                            NodeVariant::InitLocal {
                                symbol: Identifier::from_lkql_node(&b, ctx)?,
                                val: read_value,
                            },
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
                            .with_type_requirement(&types::bool::TYPE)?,
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
        Ok(Node::new(origin_location, variant))
    }

    /// Lower the provided pattern detail node into a intermediate tree node
    /// that evaluates to a boolean.
    /// The provided `detailed_values_id` should be the "let id" pointing to
    /// the value to get the detail from.
    fn lower_lkql_pattern_detail(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
        detailed_values_id: usize,
    ) -> Result<Self, Diagnostic> {
        // Get the location of the pattern detail node
        let origin_location = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

        // Util function used to create a node related to the one currently
        // lowered. The result node is wrapped in a Box and has the same origin
        // location os the current one.
        let related_node =
            |variant: NodeVariant| Box::new(Node::new(origin_location.clone(), variant));

        // Create a node to read the matched value
        let detailed_value_ref =
            Node::new(origin_location.clone(), NodeVariant::Read(detailed_values_id));

        // Get the intermediate node variant from the LKQL pattern detail
        let variant = match node {
            LkqlNode::NodePatternField(pattern_field) => {
                let field_id = ctx.new_tmp_id();
                NodeVariant::Let {
                    id: field_id,
                    value: related_node(NodeVariant::DottedExpr {
                        prefix: Box::new(detailed_value_ref),
                        suffix: Identifier::from_lkql_node(&pattern_field.f_identifier()?, ctx)?,
                        is_safe: false,
                    }),
                    r#in: Box::new(Self::lower_lkql_pattern(
                        ctx,
                        &pattern_field.f_expected_value()?,
                        field_id,
                    )?),
                }
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
                positional_args.insert(0, detailed_value_ref.clone());

                // Then create the node variant to call the property and match
                // it against expected result.
                NodeVariant::Let {
                    id: prop_result_id,
                    value: related_node(NodeVariant::FunCall {
                        callee: related_node(NodeVariant::DottedExpr {
                            prefix: Box::new(detailed_value_ref),
                            suffix: Identifier::from_lkql_node(&call.f_name()?, ctx)?,
                            is_safe: false,
                        }),
                        positional_args,
                        named_args,
                    }),
                    r#in: Box::new(Self::lower_lkql_pattern(
                        ctx,
                        &pattern_property.f_expected_value()?,
                        prop_result_id,
                    )?),
                }
            }
            LkqlNode::NodePatternSelector(pattern_selector) => {
                // Get the LKQL node of the selector call
                let selector_call = match pattern_selector.f_call()? {
                    LkqlNode::SelectorCall(selector_call) => selector_call,
                    _ => unreachable!(),
                };

                // Create selector call argument vectors
                let mut positional_args = vec![detailed_value_ref.clone()];
                let mut named_args = Vec::new();

                // The create the selector callee node
                let callee_lkql_node = selector_call.f_selector_call()?;
                let callee_variant = match &callee_lkql_node {
                    LkqlNode::Identifier(id) => {
                        NodeVariant::ReadSymbol(Identifier::from_lkql_node(&id.as_node(), ctx)?)
                    }
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
                            &origin_location,
                            &INVALID_SELECTOR_CALL,
                            &[],
                        ));
                        NodeVariant::NilLiteral
                    }
                };
                let callee = Box::new(Node::new(
                    SourceSection::from_lkql_node(ctx.lowered_source, &callee_lkql_node)?,
                    callee_variant,
                ));

                // Create the sub-pattern lambda access
                let subpattern_lambda = Node::new(
                    origin_location.clone(),
                    NodeVariant::LambdaFun(*ctx.child_index_map.get(node).unwrap()),
                );

                // Finally, create the quantification call node
                let selector_list_id = ctx.new_tmp_id();
                let selector_list_ref =
                    Node::new(origin_location.clone(), NodeVariant::Read(selector_list_id));
                NodeVariant::Let {
                    id: selector_list_id,
                    value: Box::new(
                        Node::new(
                            SourceSection::from_lkql_node(
                                ctx.lowered_source,
                                &selector_call.f_selector_call()?,
                            )?,
                            NodeVariant::FunCall { callee, positional_args, named_args },
                        )
                        .with_trait_requirement(&traits::iterable::TRAIT)?,
                    ),
                    r#in: related_node(NodeVariant::FunCall {
                        callee: related_node(NodeVariant::DottedExpr {
                            prefix: Box::new(selector_list_ref.clone()),
                            suffix: Identifier::from_lkql_node(
                                &selector_call.f_quantifier()?,
                                ctx,
                            )?,
                            is_safe: false,
                        }),
                        positional_args: vec![selector_list_ref, subpattern_lambda],
                        named_args: vec![],
                    }),
                }
            }
            _ => unreachable!(),
        };

        // Finally, create the result node
        Ok(Node::new(origin_location, variant))
    }

    /// Wrap the current node using the provided wrapper creation function,
    /// propagating all information in the current node to the wrapper.
    fn with_wrapper<F>(self, create_wrapper: F) -> Result<Self, Diagnostic>
    where
        F: FnOnce(Self) -> Result<NodeVariant, Diagnostic>,
    {
        Ok(Node::new(self.origin_location.clone(), create_wrapper(self)?))
    }

    /// Wrap the node in a type requirement one.
    fn with_type_requirement(
        self,
        required_type: &'static BuiltinType,
    ) -> Result<Self, Diagnostic> {
        self.with_wrapper(|n| {
            Ok(NodeVariant::RequireType { expression: Box::new(n), expected_type: required_type })
        })
    }

    /// Wrap the node in a trait requirement one.
    fn with_trait_requirement(
        self,
        required_trait: &'static BuiltinTrait,
    ) -> Result<Self, Diagnostic> {
        self.with_wrapper(|n| {
            Ok(NodeVariant::RequireTrait {
                expression: Box::new(n),
                required_trait: required_trait,
            })
        })
    }
}

impl ArithOperator {
    fn lower_lkql_node(
        node: &LkqlNode,
        ctx: &LoweringContext<LkqlNode>,
    ) -> Result<Self, Diagnostic> {
        Ok(ArithOperator::new(
            SourceSection::from_lkql_node(ctx.lowered_source, node)?,
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
    ) -> Result<Self, Diagnostic> {
        Ok(LogicOperator::new(
            SourceSection::from_lkql_node(ctx.lowered_source, node)?,
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
    fn lower_lkql_node(
        node: &LkqlNode,
        ctx: &LoweringContext<LkqlNode>,
    ) -> Result<Self, Diagnostic> {
        Ok(CompOperator::new(
            SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            match node {
                LkqlNode::OpEq(_) => CompOperatorVariant::Equals,
                LkqlNode::OpNeq(_) => CompOperatorVariant::NotEquals,
                LkqlNode::OpLt(_) => CompOperatorVariant::Less,
                LkqlNode::OpLeq(_) => CompOperatorVariant::LessOrEquals,
                LkqlNode::OpGt(_) => CompOperatorVariant::Greater,
                LkqlNode::OpGeq(_) => CompOperatorVariant::GreaterOrEquals,
                _ => unreachable!(),
            },
        ))
    }
}

impl MiscOperator {
    fn lower_lkql_node(
        node: &LkqlNode,
        ctx: &LoweringContext<LkqlNode>,
    ) -> Result<Self, Diagnostic> {
        Ok(MiscOperator::new(
            SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            match node {
                LkqlNode::OpConcat(_) => MiscOperatorVariant::Concat,
                _ => unreachable!(),
            },
        ))
    }
}

impl Identifier {
    /// Util function to easily create an identifier from an LKQL node.
    fn from_lkql_node(
        node: &LkqlNode,
        ctx: &LoweringContext<LkqlNode>,
    ) -> Result<Self, Diagnostic> {
        Ok(Self::new(
            SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            node.text()?,
        ))
    }
}

impl SourceSection {
    /// Create a new section corresponding to the provided node location range
    /// in the provided source.
    fn from_lkql_node(source: SourceId, node: &LkqlNode) -> Result<Self, Diagnostic> {
        let sloc_range = node.sloc_range()?;
        Ok(Self::new(
            source,
            Location::from_lkql_location(sloc_range.start),
            Location::from_lkql_location(sloc_range.end),
        ))
    }
}

/// Util function to get whether the provided LKQL parsing node introduce a
/// new lexical scope.
fn has_lexical_scope(node: &LkqlNode) -> bool {
    match node {
        LkqlNode::BlockExpr(_) | LkqlNode::IsClause(_) => true,
        _ => false,
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
///   * [`LkqlNode::BlockExpr`]
///   * [`LkqlNode::IsClause`]
///   * [`LkqlNode::NodePatternSelector`]
fn all_local_decls(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Diagnostic> {
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
) -> Result<Vec<Identifier>, Diagnostic> {
    // Declare working vectors and get all local declarations
    let mut local_decls = Vec::new();
    let mut local_symbols = Vec::new();
    all_local_decls(node, &mut local_decls)?;

    // For each declaration, create an identifier
    for decl in &local_decls {
        local_symbols.push(Identifier::new(
            SourceSection::from_lkql_node(ctx.lowered_source, decl)?,
            match decl {
                LkqlNode::ValDecl(vd) => vd.f_identifier()?.text()?,
                LkqlNode::FunDecl(fd) => fd.f_name()?.text()?,
                LkqlNode::SelectorDecl(sd) => sd.f_name()?.text()?,
                LkqlNode::Import(i) => i.f_name()?.text()?,
                LkqlNode::Identifier(id) => id.text()?,
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
) -> Result<(), Diagnostic> {
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
