//! This module contains all required operations to lower an LKQL parse source
//! to an intermediate representation.

use crate::{
    builtins::{
        traits,
        types::{self},
    },
    errors::{
        AMBIGUOUS_IMPORT, MODULE_NOT_FOUND, POS_AFTER_NAMED_ARGUMENT, PREVIOUS_NAMED_ARG_HINT,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperator, LogicOperatorVariant, MiscOperator,
        MiscOperatorVariant, Node, NodeVariant,
    },
    report::{Hint, Report},
    sources::{Location, SourceId, SourceSection},
};
use liblkqllang::{BaseFunction, Exception, LkqlNode};
use std::{
    collections::HashMap,
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
        // First, we get the name of the currently lowered execution unit.
        let name = match &node {
            LkqlNode::TopLevelList(top_level) => {
                let unit_path = PathBuf::from(top_level.unit()?.unwrap().filename()?);
                unit_path.file_stem().unwrap().to_string_lossy().to_string()
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
                        module_elements.push(Node::lower_lkql_node(&top_level_elem, ctx)?);
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
                                .map(|n| Node::lower_lkql_node(&n, ctx))
                                .transpose()?;
                            params.push((name, default_expr))
                        }
                        _ => unreachable!(),
                    }
                }

                // Then create the function execution unit variant
                ExecutionUnitVariant::Function {
                    params,
                    body: Node::lower_lkql_node(&body_node, ctx)?,
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
                            condition: Box::new(Node::lower_lkql_node(&guard, ctx)?),
                            consequence: Box::new(Node::lower_lkql_node(
                                &list_comp.f_expr()?,
                                ctx,
                            )?),
                            alternative: Box::new(Node {
                                origin_location: current_loc,
                                variant: NodeVariant::NilLiteral,
                            }),
                        },
                    },
                    None => Node::lower_lkql_node(&list_comp.f_expr()?, ctx)?,
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
    fn lower_lkql_node(node: &LkqlNode, ctx: &mut LoweringContext) -> Result<Self, Report> {
        // Get the location of the node
        let origin_location = SourceSection::from_lkql_node(ctx.lowered_source, node)?;

        // Lower the node
        let variant = match node {
            // --- Declarations
            LkqlNode::ValDecl(val_decl) => NodeVariant::InitLocal {
                symbol: Identifier::from_lkql_node(&val_decl.f_identifier()?, ctx)?,
                val: Box::new(Self::lower_lkql_node(&val_decl.f_value()?, ctx)?),
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
                                        &expr_arg.f_value_expr()?,
                                        ctx,
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
                            LkqlNode::NamedArg(named_arg) => named_args.push((
                                Identifier::from_lkql_node(&named_arg.f_arg_name()?, ctx)?,
                                Self::lower_lkql_node(&named_arg.f_value_expr()?, ctx)?,
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
                        let (receiver, member, is_safe) = match name {
                            LkqlNode::DotAccess(dot_access) => {
                                (dot_access.f_receiver(), dot_access.f_member(), false)
                            }
                            LkqlNode::SafeAccess(safe_access) => {
                                (safe_access.f_receiver(), safe_access.f_member(), true)
                            }
                            _ => unreachable!(),
                        };
                        NodeVariant::MethodCall {
                            prefix: Box::new(Self::lower_lkql_node(&receiver?, ctx)?),
                            method_name: Identifier::from_lkql_node(&member?, ctx)?,
                            is_safe,
                            positional_args,
                            named_args,
                        }
                    }
                    _ => NodeVariant::FunCall {
                        callee: Box::new(Self::lower_lkql_node(&name, ctx)?),
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
                    prefix: Box::new(Self::lower_lkql_node(&receiver?, ctx)?),
                    suffix: Identifier::from_lkql_node(&member?, ctx)?,
                    is_safe,
                }
            }

            // --- Index expression
            LkqlNode::Indexing(_) | LkqlNode::SafeIndexing(_) => {
                let (coll_expr, index, is_safe) = match node {
                    LkqlNode::Indexing(indexing) => {
                        (indexing.f_collection_expr(), indexing.f_index_expr(), false)
                    }
                    LkqlNode::SafeIndexing(safe_indexing) => {
                        (safe_indexing.f_collection_expr(), safe_indexing.f_index_expr(), true)
                    }
                    _ => unreachable!(),
                };
                NodeVariant::IndexExpr {
                    indexed_val: Box::new(Self::lower_lkql_node(&coll_expr?, ctx)?.with_wrapper(
                        |indexed_val| NodeVariant::CheckTrait {
                            expression: Box::new(indexed_val),
                            required_trait: &traits::indexable::TRAIT,
                        },
                    )),
                    index: Box::new(Self::lower_lkql_node(&index?, ctx)?.with_wrapper(|i| {
                        NodeVariant::CheckType {
                            expression: Box::new(i),
                            expected_type: &types::int::TYPE,
                        }
                    })),
                    is_safe,
                }
            }

            // --- In clause
            LkqlNode::InClause(in_clause) => NodeVariant::InClause {
                value: Box::new(Self::lower_lkql_node(&in_clause.f_value_expr()?, ctx)?),
                collection: Box::new(
                    Self::lower_lkql_node(&in_clause.f_list_expr()?, ctx)?.with_wrapper(|n| {
                        NodeVariant::CheckTrait {
                            expression: Box::new(n),
                            required_trait: &traits::iterable::TRAIT,
                        }
                    }),
                ),
            },

            // --- If expression
            LkqlNode::CondExpr(cond_expr) => NodeVariant::IfExpr {
                condition: Box::new(Self::lower_lkql_node(&cond_expr.f_condition()?, ctx)?),
                consequence: Box::new(Self::lower_lkql_node(&cond_expr.f_then_expr()?, ctx)?),
                alternative: cond_expr
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
            LkqlNode::BlockExpr(block_expr) => {
                let body_node = block_expr.f_body()?;
                let mut body = Vec::with_capacity(body_node.children_count()?);
                for maybe_body_part_node in &body_node {
                    if let Some(ref body_part_node) = maybe_body_part_node? {
                        body.push(Self::lower_lkql_node(body_part_node, ctx)?);
                    }
                }
                NodeVariant::BlockExpr {
                    local_symbols: all_local_symbols(node, ctx)?,
                    body,
                    val: Box::new(Self::lower_lkql_node(&block_expr.f_expr()?, ctx)?),
                }
            }
            LkqlNode::BlockBodyDecl(body_decl) => {
                return Self::lower_lkql_node(&body_decl.f_decl()?, ctx);
            }
            LkqlNode::BlockBodyExpr(body_expr) => {
                return Self::lower_lkql_node(&body_expr.f_expr()?, ctx);
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
                    .map(|n| Self::lower_lkql_node(&n?.unwrap(), ctx))
                    .map(|n| {
                        Ok(n?.with_wrapper(|coll_expr| NodeVariant::CheckTrait {
                            expression: Box::new(coll_expr),
                            required_trait: &traits::iterable::TRAIT,
                        }))
                    })
                    .collect::<Result<_, Report>>()?,
                body_index: *ctx.child_index_map.get(&node).unwrap(),
            },

            // --- Binary operation
            LkqlNode::ArithBinOp(arith_bin_op) => NodeVariant::ArithBinOp {
                left: Box::new(Self::lower_lkql_node(&arith_bin_op.f_left()?, ctx)?),
                operator: ArithOperator::lower_lkql_node(&arith_bin_op.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(&arith_bin_op.f_right()?, ctx)?),
            },
            LkqlNode::RelBinOp(rel_bin_op) => NodeVariant::CompBinOp {
                left: Box::new(Self::lower_lkql_node(&rel_bin_op.f_left()?, ctx)?),
                operator: CompOperator::lower_lkql_node(&rel_bin_op.f_op()?, ctx)?,
                right: Box::new(Self::lower_lkql_node(&rel_bin_op.f_right()?, ctx)?),
            },
            LkqlNode::BinOp(bin_op) => {
                let operator_node = bin_op.f_op()?;
                let left = Box::new(Self::lower_lkql_node(&bin_op.f_left()?, ctx)?);
                let right = Box::new(Self::lower_lkql_node(&bin_op.f_right()?, ctx)?);
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
                let operand = Box::new(Self::lower_lkql_node(&un_op.f_operand()?, ctx)?);
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
                NodeVariant::StringLiteral(String::from(node.text()?.trim_matches('"')))
            }
            LkqlNode::BlockStringLiteral(block_string) => {
                let mut builder = String::new();
                for maybe_str_part in &block_string.f_docs()? {
                    if let Some(str_part) = maybe_str_part? {
                        let text = str_part.text()?;
                        builder.push_str(text.trim_start_matches("|\"").trim_start());
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
                        items.push(Self::lower_lkql_node(item_node, ctx)?);
                    }
                }
                NodeVariant::TupleLiteral(items)
            }
            LkqlNode::ListLiteral(list) => {
                let items_node = list.f_exprs()?;
                let mut items = Vec::with_capacity(items_node.children_count()?);
                for maybe_item_node in &items_node {
                    if let Some(ref item_node) = maybe_item_node? {
                        items.push(Self::lower_lkql_node(item_node, ctx)?);
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
            _ => panic!("{} is not handled by the lowering phase", node.image()?),
        };

        // Finally return the resulting node
        Ok(Node { origin_location, variant })
    }

    /// Wrap the current node using the provided wrapper creation function,
    /// propagating all information in the current node to the wrapper.
    fn with_wrapper<F>(self, create_wrapper: F) -> Self
    where
        F: Fn(Self) -> NodeVariant,
    {
        Node {
            origin_location: self.origin_location.clone(),
            variant: create_wrapper(self),
        }
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

struct LoweringContext {
    /// The source that is currently being lowered.
    lowered_source: SourceId,

    /// Map each function declaration node to the "child index" of its produced
    /// [`Function`] object.
    child_index_map: HashMap<LkqlNode, u16>,

    /// Counter of encountered lambdas, used for naming them.
    lambda_counter: usize,

    /// Counter of encountered list comprehension, used for naming their
    /// execution units.
    lazy_comprehension_counter: usize,

    /// The list of diagnostics emitted during the lowering.
    diagnostics: Vec<Report>,
}

impl LoweringContext {
    pub fn new(lowered_source: SourceId) -> Self {
        Self {
            lowered_source,
            child_index_map: HashMap::new(),
            lambda_counter: 0,
            lazy_comprehension_counter: 0,
            diagnostics: Vec::new(),
        }
    }

    /// Get the next available lambda name, incrementing the counter.
    fn next_lambda_name(&mut self) -> String {
        let res = format!("<lambda_{}>", self.lambda_counter);
        self.lambda_counter += 1;
        res
    }

    /// Get the next available list comprehension name, incrementing the
    /// counter.
    fn next_lazy_comprehension_name(&mut self) -> String {
        let res = format!("<lazy_comprehension_{}>", self.lazy_comprehension_counter);
        self.lazy_comprehension_counter += 1;
        res
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
            match child {
                LkqlNode::TopLevelList(_) => (),
                LkqlNode::ValDecl(_) => {
                    all_local_decls(&child, output)?;
                    output.push(child);
                }
                LkqlNode::FunDecl(_) => output.push(child),
                LkqlNode::SelectorDecl(_) => output.push(child),
                LkqlNode::Import(_) => output.push(child),
                LkqlNode::AnonymousFunction(_)
                | LkqlNode::ListComprehension(_)
                | LkqlNode::BlockExpr(_) => (),
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
                LkqlNode::Import(i) => i.f_name()?.text()?,
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
