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
    errors::{
        AMBIGUOUS_IMPORT, MODULE_NOT_FOUND, POS_AFTER_NAMED_ARGUMENT, PREVIOUS_NAMED_ARG_HINT,
        UNKNOWN_NODE_TYPE,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperator, LogicOperatorVariant, MiscOperator,
        MiscOperatorVariant, Node, NodeVariant,
    },
    lowering::{LoweringContext, unescape_string},
    report::{Hint, Report},
    sources::{Location, SourceId, SourceSection},
};
use liblkqllang::{BaseFunction, Exception, LkqlNode};
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
    ///
    /// If there is errors during the lowering of LKQL source, this function
    /// returns a [`Result::Err`] which contains all diagnostics.
    pub fn lower_lkql_node(
        execution_context: &ExecutionContext,
        source: SourceId,
        node: &LkqlNode,
    ) -> Result<Self, Report> {
        let mut lowering_context = LoweringContext::new(execution_context, source);
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
        ctx: &mut LoweringContext<LkqlNode>,
    ) -> Result<Self, Report> {
        // First, we get the name of the currently lowered execution unit.
        let name = match &node {
            LkqlNode::TopLevelList(top_level) => {
                let unit_path = PathBuf::from(top_level.unit()?.unwrap().filename()?);
                unit_path.file_name().unwrap().to_string_lossy().to_string()
            }
            LkqlNode::FunDecl(fun_decl) => fun_decl.f_name()?.text()?,
            LkqlNode::AnonymousFunction(_) => ctx.next_lambda_name(),
            LkqlNode::ListComprehension(_) => ctx.next_lazy_comprehension_name(),
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
                let collection_bindings: Result<Vec<Identifier>, Report> = list_comp
                    .f_generators()?
                    .into_iter()
                    .map(|n| -> Result<Identifier, Report> {
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
                let current_loc = SourceSection::from_lkql_node(ctx.lowered_source, node)?;
                let body = match list_comp.f_guard()? {
                    Some(guard) => Node {
                        origin_location: current_loc.clone(),
                        variant: NodeVariant::IfExpr {
                            condition: Box::new(Node::lower_lkql_node(ctx, &guard)?),
                            consequence: Box::new(Node::lower_lkql_node(
                                ctx,
                                &list_comp.f_expr()?,
                            )?),
                            alternative: Box::new(Node {
                                origin_location: current_loc,
                                variant: NodeVariant::NilLiteral,
                            }),
                        },
                    },
                    None => Node::lower_lkql_node(ctx, &list_comp.f_expr()?)?,
                };

                // Then return the new function execution unit variant
                ExecutionUnitVariant::RawCallable { params: collection_bindings?, body }
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
    fn lower_lkql_node(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
    ) -> Result<Self, Report> {
        // Get the location of the node
        let origin_location = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

        // Util function used to create a node related to the one currently
        // lowered. The result node is wrapped in a Box and has the same origin
        // location os the current one.
        let related_node = |variant: NodeVariant| {
            Box::new(Node { origin_location: origin_location.clone(), variant })
        };

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
                        ctx.diagnostics.push(Report::from_error_template(
                            &origin_location,
                            &MODULE_NOT_FOUND,
                            &vec![module_name],
                        ));
                        &PathBuf::new()
                    }
                    x => {
                        ctx.diagnostics.push(Report::from_error_template(
                            &origin_location,
                            &AMBIGUOUS_IMPORT,
                            &vec![
                                x.iter()
                                    .map(|f| f.to_string_lossy())
                                    .collect::<Vec<_>>()
                                    .join(" & "),
                            ],
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
                for maybe_arg in fun_call.f_arguments()?.children_iter()? {
                    if let Some(ref arg) = maybe_arg? {
                        match arg {
                            LkqlNode::ExprArg(expr_arg) => {
                                // Ensure that no named arguments have been
                                // lowered yet.
                                if named_args.is_empty() {
                                    positional_args.push(Self::lower_lkql_node(
                                        ctx,
                                        &expr_arg.f_value_expr()?,
                                    )?);
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
                                            vec![Hint {
                                                message: String::from(PREVIOUS_NAMED_ARG_HINT),
                                                location: SourceSection::range(
                                                    &last_id.origin_location,
                                                    &last_node.origin_location,
                                                ),
                                            }],
                                        ));
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

                        // Lower receiver and field name
                        let prefix = Box::new(Self::lower_lkql_node(ctx, &receiver?)?);
                        let suffix = Identifier::from_lkql_node(&member?, ctx)?;

                        // Create a new named temporary value to compute the
                        // prefix part of the dot access only once.
                        let prefix_id = ctx.new_tmp_id();
                        let prefix_ref = related_node(NodeVariant::Read(prefix_id));

                        // Create a new node to test the type of the dot access
                        // prefix.
                        let type_condition = related_node(NodeVariant::IfExpr {
                            condition: related_node(NodeVariant::InstanceOf {
                                expression: prefix_ref.clone(),
                                expected_type_tag: types::namespace::TYPE.tag,
                            }),
                            consequence: related_node(NodeVariant::FunCall {
                                callee: related_node(NodeVariant::DottedExpr {
                                    prefix: prefix_ref.clone(),
                                    suffix: suffix.clone(),
                                    is_safe,
                                }),
                                positional_args: positional_args.clone(),
                                named_args: named_args.clone(),
                            }),
                            alternative: related_node(NodeVariant::MethodCall {
                                prefix: prefix_ref,
                                method_name: suffix,
                                is_safe,
                                positional_args,
                                named_args,
                            }),
                        });
                        NodeVariant::Let { id: prefix_id, value: prefix, r#in: type_condition }
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
                    .collect::<Result<_, Report>>()?,
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
                let left = Box::new(Self::lower_lkql_node(ctx, &bin_op.f_left()?)?);
                let right = Box::new(Self::lower_lkql_node(ctx, &bin_op.f_right()?)?);
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
            LkqlNode::UnOp(un_op) => {
                let operand = Box::new(Self::lower_lkql_node(ctx, &un_op.f_operand()?)?);
                let operator_node = un_op.f_op()?;
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
        let lowered_node = Node { origin_location, variant };

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

    /// Lower the provided pattern node into a intermediate tree node that
    /// expresses the matching logic.
    /// The provided `matched_value_ref` should be the "let id" pointing to
    /// the value to match.
    fn lower_lkql_pattern(
        ctx: &mut LoweringContext<LkqlNode>,
        node: &LkqlNode,
        matched_value_ref: usize,
    ) -> Result<Self, Report> {
        // Special handling of the parenthesized pattern
        if let LkqlNode::ParenPattern(parent_pattern) = node {
            return Self::lower_lkql_pattern(ctx, &parent_pattern.f_pattern()?, matched_value_ref);
        }

        // Get the location of the pattern node
        let loc = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

        // Util function used to create a node related to the one currently
        // lowered. The result node is wrapped in a Box and has the same origin
        // location os the current one.
        let related_node =
            |variant: NodeVariant| Box::new(Node { origin_location: loc.clone(), variant });

        // Create a node to read the matched value
        let read_value = related_node(NodeVariant::Read(matched_value_ref));

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
                    operator: CompOperator {
                        origin_location: loc.clone(),
                        variant: CompOperatorVariant::Equals,
                    },
                    right: related_node(target_literal),
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
                    ctx.diagnostics.push(Report::from_error_template(
                        &loc,
                        &UNKNOWN_NODE_TYPE,
                        &vec![&node_type_name],
                    ));
                    NodeVariant::BoolLiteral(false)
                }
            }

            // --- Not pattern
            LkqlNode::NotPattern(not_pattern) => NodeVariant::LogicUnOp {
                operator: LogicOperator {
                    origin_location: loc.clone(),
                    variant: LogicOperatorVariant::Not,
                },
                operand: Box::new(Self::lower_lkql_pattern(
                    ctx,
                    &not_pattern.f_pattern()?,
                    matched_value_ref,
                )?),
            },

            // --- Or pattern
            LkqlNode::OrPattern(or_pattern) => NodeVariant::LogicBinOp {
                left: Box::new(Self::lower_lkql_pattern(
                    ctx,
                    &or_pattern.f_left()?,
                    matched_value_ref,
                )?),
                operator: LogicOperator {
                    origin_location: loc.clone(),
                    variant: LogicOperatorVariant::Or,
                },
                right: Box::new(Self::lower_lkql_pattern(
                    ctx,
                    &or_pattern.f_right()?,
                    matched_value_ref,
                )?),
            },

            // --- Complex pattern
            LkqlNode::ComplexPattern(complex_pattern) => {
                // Lower the binding as a local initialization
                let binding_in_vec = complex_pattern
                    .f_binding()?
                    .map(|b| {
                        Ok::<Self, Report>(Self {
                            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, &b)?,
                            variant: NodeVariant::InitLocal {
                                symbol: Identifier::from_lkql_node(&b, ctx)?,
                                val: read_value,
                            },
                        })
                    })
                    .transpose()?
                    .into_iter()
                    .collect::<Vec<_>>();

                // Collect all checks in a vector
                let mut matching_elems = Vec::new();

                // Lower the value pattern
                if let Some(pattern) = complex_pattern.f_pattern()? {
                    matching_elems.push(Self::lower_lkql_pattern(ctx, &pattern, matched_value_ref)?)
                }

                // Lower the predicate
                if let Some(predicate) = complex_pattern.f_predicate()? {
                    matching_elems.push(Self::lower_lkql_node(ctx, &predicate)?);
                }

                // Compose all checks in a sequence of binary operations
                let matching_logic_node = match matching_elems.len() {
                    0 => related_node(NodeVariant::BoolLiteral(true)),
                    1 => Box::new(matching_elems.remove(0)),
                    _ => {
                        let mut iter = matching_elems.into_iter().rev();
                        let init = Box::new(iter.next().unwrap());
                        iter.fold(init, |res, next| {
                            related_node(NodeVariant::LogicBinOp {
                                left: Box::new(next),
                                operator: LogicOperator {
                                    origin_location: loc.clone(),
                                    variant: LogicOperatorVariant::And,
                                },
                                right: res,
                            })
                        })
                    }
                };

                // Finally, return the lowered complex pattern
                if binding_in_vec.is_empty() {
                    matching_logic_node.variant
                } else {
                    NodeVariant::BlockExpr { body: binding_in_vec, val: matching_logic_node }
                }
            }
            _ => unreachable!(),
        };

        // Create the result node
        let lowered_node = Node { origin_location: loc, variant };

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

    /// Wrap the current node using the provided wrapper creation function,
    /// propagating all information in the current node to the wrapper.
    fn with_wrapper<F>(self, create_wrapper: F) -> Result<Self, Report>
    where
        F: Fn(Self) -> Result<NodeVariant, Report>,
    {
        Ok(Node {
            origin_location: self.origin_location.clone(),
            variant: create_wrapper(self)?,
        })
    }

    /// Wrap the node in a type requirement one.
    fn with_type_requirement(self, required_type: &'static BuiltinType) -> Result<Self, Report> {
        self.with_wrapper(|n| {
            Ok(NodeVariant::RequireType { expression: Box::new(n), expected_type: required_type })
        })
    }

    /// Wrap the node in a trait requirement one.
    fn with_trait_requirement(self, required_trait: &'static BuiltinTrait) -> Result<Self, Report> {
        self.with_wrapper(|n| {
            Ok(NodeVariant::RequireTrait {
                expression: Box::new(n),
                required_trait: required_trait,
            })
        })
    }
}

impl ArithOperator {
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Result<Self, Report> {
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
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Result<Self, Report> {
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
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Result<Self, Report> {
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
    fn lower_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Result<Self, Report> {
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
    fn from_lkql_node(node: &LkqlNode, ctx: &LoweringContext<LkqlNode>) -> Result<Self, Report> {
        Ok(Self {
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, node)?,
            text: node.text()?,
        })
    }
}

impl SourceSection {
    /// Create a new section corresponding to the provided node location range
    /// in the provided source.
    fn from_lkql_node(source: SourceId, node: &LkqlNode) -> Result<Self, Report> {
        let sloc_range = node.sloc_range()?;
        Ok(Self {
            source,
            start: Location::from_lkql_location(sloc_range.start),
            end: Location::from_lkql_location(sloc_range.end),
        })
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
fn all_local_decls(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Report> {
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
                    all_local_decls(&child, output)?;
                    if let Some(binding) = pattern.f_binding()? {
                        output.push(binding);
                    }
                }

                // Recursion bounds
                LkqlNode::TopLevelList(_)
                | LkqlNode::AnonymousFunction(_)
                | LkqlNode::ListComprehension(_)
                | LkqlNode::BlockExpr(_)
                | LkqlNode::IsClause(_) => (),

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
) -> Result<Vec<Identifier>, Report> {
    // Declare working vectors and get all local declarations
    let mut local_decls = Vec::new();
    let mut local_symbols = Vec::new();
    all_local_decls(node, &mut local_decls)?;

    // For each declaration, create an identifier
    for decl in &local_decls {
        local_symbols.push(Identifier {
            text: match decl {
                LkqlNode::ValDecl(vd) => vd.f_identifier()?.text()?,
                LkqlNode::FunDecl(fd) => fd.f_name()?.text()?,
                LkqlNode::SelectorDecl(sd) => sd.f_name()?.text()?,
                LkqlNode::Import(i) => i.f_name()?.text()?,
                LkqlNode::Identifier(id) => id.text()?,
                _ => unreachable!(),
            },
            origin_location: SourceSection::from_lkql_node(ctx.lowered_source, decl)?,
        });
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
fn all_local_execution_units(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Report> {
    for maybe_child in node {
        if let Some(child) = maybe_child? {
            match child {
                LkqlNode::TopLevelList(_)
                | LkqlNode::FunDecl(_)
                | LkqlNode::SelectorDecl(_)
                | LkqlNode::AnonymousFunction(_)
                | LkqlNode::ListComprehension(_) => output.push(child),
                _ => all_local_execution_units(&child, output)?,
            }
        }
    }
    Ok(())
}
