//! This module contains all required operations to lower an LKQL parse source
//! to an intermediate representation.

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak},
};

use crate::{
    Report,
    intermediate_tree::{
        Function, Identifier, Node, NodeVariant, Operator, OperatorVariant, Program,
    },
    sources::SourceSection,
};

use liblkqllang::{BaseFunction, LkqlNode};

impl Program {
    /// Lower the provided LKQL node as an intermediate [`Program`]. The
    /// provided node MUST be one of the following variants:
    ///   * [`LkqlNode::TopLevelList`]
    ///
    /// If there is errors in the lowered LKQL source, this function returns a
    /// [`Result::Err`] which contains all diagnostics.
    pub fn lower_lkql_node(node: &LkqlNode) -> Result<Self, Report> {
        match node {
            LkqlNode::TopLevelList(_) => {
                let mut ctx = LoweringContext::new();
                let main_function = Function::lower_lkql_node(node, &mut ctx)?;
                if ctx.diagnostics.is_empty() {
                    Ok(Program { main_function })
                } else {
                    Err(Report::Composed(ctx.diagnostics))
                }
            }
            _ => unreachable!(),
        }
    }
}

impl Function {
    /// Lower the provided LKQL node as an intermediate [`Function`]. The
    /// provided node MUST be one of the following variants:
    ///   * [`LkqlNode::TopLevelList`]
    ///   * [`LkqlNode::FunDecl`]
    ///   * [`LkqlNode::AnonymousFunction`]
    fn lower_lkql_node(
        node: &LkqlNode,
        ctx: &mut LoweringContext,
    ) -> Result<Rc<RefCell<Self>>, Report> {
        // Create a location object for the current function
        let function_location: SourceSection = SourceSection::from_lkql_node(node)?;

        // From all declarations, extract all local symbols, synthesizing names
        // for lambdas.
        let local_symbols = all_local_symbols(node)?;

        // Create the result object with empty fields
        let res = Rc::new(RefCell::new(Function {
            origin_location: function_location.clone(),
            parent_function: ctx.current_function.clone(),
            children_functions: Vec::new(),
            local_symbols,
            params: Vec::new(),
            body: Vec::new(),
        }));

        {
            // Borrow the lowered function as mutable to fill it
            let mut lowered_function = res.borrow_mut();

            // Lower the children functions
            let mut child_function_nodes = Vec::new();
            all_local_functions(node, &mut child_function_nodes)?;

            ctx.current_function = Some(Rc::downgrade(&res));
            for child_function_node in child_function_nodes {
                ctx.child_index_map
                    .insert(child_function_node.clone(), lowered_function.children_functions.len());
                lowered_function
                    .children_functions
                    .push(Self::lower_lkql_node(&child_function_node, ctx)?);
            }
            ctx.current_function = lowered_function.parent_function.clone();

            // Lower the function parameters and body
            match node {
                LkqlNode::TopLevelList(tl) => {
                    for stmt in tl {
                        if let Some(n) = stmt? {
                            lowered_function.body.push(Node::lower_lkql_node(&n, ctx)?);
                        }
                    }
                }
                LkqlNode::FunDecl(fd) => match fd.f_fun_expr()? {
                    LkqlNode::NamedFunction(nf) => {
                        Self::lower_parameters(
                            &nf.f_parameters()?,
                            ctx,
                            &mut lowered_function.params,
                        )?;
                        lowered_function
                            .body
                            .push(Node::lower_lkql_node(&nf.f_body_expr()?, ctx)?);
                    }
                    _ => unreachable!(),
                },
                LkqlNode::AnonymousFunction(af) => {
                    Self::lower_parameters(&af.f_parameters()?, ctx, &mut lowered_function.params)?;
                    lowered_function
                        .body
                        .push(Node::lower_lkql_node(&af.f_body_expr()?, ctx)?);
                }
                _ => unreachable!(),
            };
        }

        // Finally, return the lowered function
        Ok(res)
    }

    /// Util function to lower a node list of parameter declarations an place
    /// each result in an output vector.
    fn lower_parameters(
        parameters: &LkqlNode,
        ctx: &mut LoweringContext,
        output_vec: &mut Vec<(Identifier, Option<Node>)>,
    ) -> Result<(), Report> {
        for maybe_param_decl in parameters {
            match maybe_param_decl? {
                Some(LkqlNode::ParameterDecl(pd)) => {
                    let name = Identifier::from_node(&pd.f_param_identifier()?)?;
                    let default_expr = if let Some(n) = pd.f_default_expr()? {
                        Some(Node::lower_lkql_node(&n, ctx)?)
                    } else {
                        None
                    };
                    output_vec.push((name, default_expr))
                }
                _ => unreachable!(),
            }
        }
        Ok(())
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
                symbol: Identifier::from_node(&vd.f_identifier()?)?,
                val: Box::new(Self::lower_lkql_node(&vd.f_value()?, ctx)?),
            },
            LkqlNode::FunDecl(_) | LkqlNode::SelectorDecl(_) => {
                let (id_node, val_node) = match node {
                    LkqlNode::FunDecl(fd) => (fd.f_name()?, fd.f_fun_expr()?),
                    LkqlNode::SelectorDecl(sd) => (sd.f_name()?, node.clone()),
                    _ => unreachable!(),
                };
                NodeVariant::InitLocal {
                    symbol: Identifier::from_node(&id_node)?,
                    val: Box::new(Node {
                        origin_location: SourceSection::from_lkql_node(&val_node)?,
                        variant: NodeVariant::ChildFunRef(*ctx.child_index_map.get(node).unwrap()),
                    }),
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
                                    ctx.diagnostics.push(Report::pos_arg_after_named(
                                        SourceSection::from_lkql_node(arg)?,
                                        SourceSection::range(
                                            &last_id.origin_location,
                                            &last_node.origin_location,
                                        )?,
                                    ));
                                }
                            }
                            LkqlNode::NamedArg(na) => named_args.push((
                                Identifier::from_node(&na.f_arg_name()?)?,
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
                    suffix: Identifier::from_node(&member?)?,
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

            // --- If expression
            LkqlNode::CondExpr(ce) => NodeVariant::IfExpr {
                condition: Box::new(Self::lower_lkql_node(&ce.f_condition()?, ctx)?),
                consequence: Box::new(Self::lower_lkql_node(&ce.f_then_expr()?, ctx)?),
                alternative: ce
                    .f_else_expr()?
                    .map(|n| Self::lower_lkql_node(&n, ctx))
                    .transpose()?
                    .map(|n| Box::new(n)),
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
                    local_symbols: all_local_symbols(node)?,
                    body,
                    val: Box::new(Self::lower_lkql_node(&be.f_expr()?, ctx)?),
                }
            }
            LkqlNode::BlockBodyDecl(bbd) => return Self::lower_lkql_node(&bbd.f_decl()?, ctx),
            LkqlNode::BlockBodyExpr(bbe) => return Self::lower_lkql_node(&bbe.f_expr()?, ctx),

            // --- Binary operation
            LkqlNode::BinOp(_) | LkqlNode::RelBinOp(_) | LkqlNode::ArithBinOp(_) => {
                let (left, op, right) = match node {
                    LkqlNode::BinOp(bo) => (bo.f_left(), bo.f_op(), bo.f_right()),
                    LkqlNode::RelBinOp(bo) => (bo.f_left(), bo.f_op(), bo.f_right()),
                    LkqlNode::ArithBinOp(bo) => (bo.f_left(), bo.f_op(), bo.f_right()),
                    _ => unreachable!(),
                };
                NodeVariant::BinOp {
                    left: Box::new(Self::lower_lkql_node(&left?, ctx)?),
                    operator: Operator::lower_lkql_node(&op?)?,
                    right: Box::new(Self::lower_lkql_node(&right?, ctx)?),
                }
            }

            // --- Unary operation
            LkqlNode::UnOp(uo) => NodeVariant::UnOp {
                operator: Operator::lower_lkql_node(&uo.f_op()?)?,
                operand: Box::new(Self::lower_lkql_node(&uo.f_operand()?, ctx)?),
            },

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
                            Identifier::from_node(&assoc_node.f_name()?)?,
                            Self::lower_lkql_node(&assoc_node.f_expr()?, ctx)?,
                        ));
                    }
                }
                NodeVariant::ObjectLiteral(assocs)
            }
            LkqlNode::Identifier(_) => NodeVariant::ReadLocal(Identifier::from_node(node)?),
            LkqlNode::AnonymousFunction(_) => {
                NodeVariant::ChildFunRef(*ctx.child_index_map.get(node).unwrap())
            }

            // --- For now, not all node kinds are handled
            _ => panic!("{} is not handled by lowering", node.image()?),
        };

        // Finally return the resulting node
        Ok(Node { origin_location: SourceSection::from_lkql_node(node)?, variant })
    }
}

impl Operator {
    /// Lower the provided LKQL node as an intermediate [`Operator`]. The
    /// provided node MUST be one of the following variants:
    ///   * [`LkqlNode::OpPlus`]
    ///   * [`LkqlNode::OpMinus`]
    ///   * [`LkqlNode::OpMul`]
    ///   * [`LkqlNode::OpDiv`]
    ///   * [`LkqlNode::OpAnd`]
    ///   * [`LkqlNode::OpOr`]
    ///   * [`LkqlNode::OpEq`]
    ///   * [`LkqlNode::OpNeq`]
    ///   * [`LkqlNode::OpConcat`]
    ///   * [`LkqlNode::OpLt`]
    ///   * [`LkqlNode::OpLeq`]
    ///   * [`LkqlNode::OpGt`]
    ///   * [`LkqlNode::OpGeq`]
    ///   * [`LkqlNode::OpNot`]
    fn lower_lkql_node(node: &LkqlNode) -> Result<Self, Report> {
        let variant = match node {
            LkqlNode::OpPlus(_) => OperatorVariant::Plus,
            LkqlNode::OpMinus(_) => OperatorVariant::Minus,
            LkqlNode::OpMul(_) => OperatorVariant::Multiply,
            LkqlNode::OpDiv(_) => OperatorVariant::Divide,
            LkqlNode::OpAnd(_) => OperatorVariant::And,
            LkqlNode::OpOr(_) => OperatorVariant::Or,
            LkqlNode::OpConcat(_) => OperatorVariant::Concat,
            LkqlNode::OpEq(_) => OperatorVariant::Equals,
            LkqlNode::OpNeq(_) => OperatorVariant::NotEquals,
            LkqlNode::OpLt(_) => OperatorVariant::Less,
            LkqlNode::OpLeq(_) => OperatorVariant::LessOrEquals,
            LkqlNode::OpGt(_) => OperatorVariant::Greater,
            LkqlNode::OpGeq(_) => OperatorVariant::GreaterOrEquals,
            LkqlNode::OpNot(_) => OperatorVariant::Not,
            _ => unreachable!(),
        };

        Ok(Operator { origin_location: SourceSection::from_lkql_node(node)?, variant })
    }
}

impl Identifier {
    /// Util function to easily create an identifier from an LKQL node.
    fn from_node(node: &LkqlNode) -> Result<Self, Report> {
        Ok(Self { origin_location: SourceSection::from_lkql_node(node)?, text: node.text()? })
    }
}

struct LoweringContext {
    /// Map each function declaration node to the "child index" of its produced
    /// [`Function`] object.
    child_index_map: HashMap<LkqlNode, usize>,

    /// The function that is currently lowered.
    current_function: Option<Weak<RefCell<Function>>>,

    /// The list of diagnostics emitted during the lowering.
    diagnostics: Vec<Report>,
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            child_index_map: HashMap::new(),
            current_function: None,
            diagnostics: Vec::new(),
        }
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

/// Util function to get all lexical symbols which are local to the given node.
/// This function relies on [`all_local_decls`] to compute its result, meaning
/// that all concepts described in the latter's doc is true for this function.
fn all_local_symbols(node: &LkqlNode) -> Result<HashSet<String>, Report> {
    let mut local_decls = Vec::new();
    all_local_decls(node, &mut local_decls)?;
    Ok(local_decls
        .iter()
        .map(|n| -> Result<String, Report> {
            match n {
                LkqlNode::ValDecl(vd) => Ok(vd.f_identifier()?.text()?),
                LkqlNode::FunDecl(fd) => Ok(fd.f_name()?.text()?),
                LkqlNode::SelectorDecl(sd) => Ok(sd.f_name()?.text()?),
                _ => unreachable!(),
            }
        })
        .collect::<Result<HashSet<String>, Report>>()?)
}

/// Util function to find all functions in the local environment of the
/// provided node.
/// A node is considered as a "function" if it can be lowered by the
/// [`Function::lower_lkql_node`] method.
/// The locality is defined the same way as it is defined in the
/// [`all_local_decls`] function.
fn all_local_functions(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Report> {
    for maybe_child in node {
        if let Some(child) = maybe_child? {
            match child {
                LkqlNode::TopLevelList(_) => (),
                LkqlNode::AnonymousFunction(_) => output.push(child),
                LkqlNode::ListComprehension(_) => output.push(child),
                LkqlNode::FunDecl(_) => output.push(child),
                LkqlNode::SelectorDecl(_) => output.push(child),
                _ => all_local_functions(&child, output)?,
            }
        }
    }
    Ok(())
}
