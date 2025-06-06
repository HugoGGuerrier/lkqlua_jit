//! This module contains all required operations to lower an LKQL parse source
//! to an intermediate representation.

use std::{
    cell::{OnceCell, RefCell},
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::{
    Error,
    intermediate_tree::{Function, Identifier, Node, NodeVariant, Program},
    sources::{SourceRepository, SourceSection},
};

use liblkqllang::{BaseFunction, LkqlNode};

impl Program {
    /// Lower the provided LKQL node as an intermediate [`Program`]. The
    /// provided node MUST be one of the following variants:
    ///   * [`LkqlNode::TopLevelList`]
    pub fn lower_lkql_node(node: &LkqlNode, source_repo: &SourceRepository) -> Result<Self, Error> {
        match node {
            LkqlNode::TopLevelList(_) => {
                let mut ctx = LoweringContext::new(source_repo);
                let main_function = Function::lower_lkql_node(node, &mut ctx)?;
                Ok(Program { main_function })
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
    ) -> Result<Rc<RefCell<Self>>, Error> {
        // Create a location object for the current function
        let function_location: SourceSection = ctx.source_repo.section_from_lkql_node(node)?;

        // From all declarations, extract all local symbols, synthesizing names
        // for lambdas.
        let local_symbols = all_local_symbols(node)?;

        // Create the result object with empty fields
        let mut lowered_function: Function = Function {
            origin_location: function_location.clone(),
            parent_function: OnceCell::new(),
            children_function: Vec::new(),
            local_symbols,
            params: Vec::new(),
            body: Vec::new(),
        };

        // Lower the children functions
        let mut child_function_nodes = Vec::new();
        all_local_functions(node, &mut child_function_nodes)?;

        for child_function_node in child_function_nodes {
            ctx.child_index_map
                .insert(child_function_node.clone(), lowered_function.children_function.len());
            lowered_function
                .children_function
                .push(Self::lower_lkql_node(&child_function_node, ctx)?);
        }

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
                    Self::lower_parameters(&nf.f_parameters()?, ctx, &mut lowered_function.params)?;
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

        // Finally, create the result reference and set it as parent into all
        // children.
        let res = Rc::new(RefCell::new(lowered_function));
        for child in res.borrow().children_function.iter() {
            child
                .borrow_mut()
                .parent_function
                .set(Rc::downgrade(&res))
                .map_err(|_| Error::Located {
                    location: function_location.clone(),
                    message: format!("{:?} parent is already set", child),
                })?
        }
        Ok(res)
    }

    /// Util function to lower a node list of parameter declarations an place
    /// each result in an output vector.
    fn lower_parameters(
        parameters: &LkqlNode,
        ctx: &mut LoweringContext,
        output_vec: &mut Vec<(Identifier, Option<Node>)>,
    ) -> Result<(), Error> {
        for maybe_param_decl in parameters {
            match maybe_param_decl? {
                Some(LkqlNode::ParameterDecl(pd)) => {
                    let name = Identifier::from_node(&pd.f_param_identifier()?, ctx)?;
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
    fn lower_lkql_node(node: &LkqlNode, ctx: &mut LoweringContext) -> Result<Self, Error> {
        // Create a location object for the current node
        let node_location = ctx.source_repo.section_from_lkql_node(node)?;

        // Lower the node
        let variant = match node {
            // --- Declarations
            LkqlNode::ValDecl(vd) => NodeVariant::InitLocal {
                symbol: Identifier::from_node(&vd.f_identifier()?, ctx)?,
                val: Box::new(Self::lower_lkql_node(&vd.f_value()?, ctx)?),
            },
            LkqlNode::FunDecl(_) | LkqlNode::SelectorDecl(_) => {
                let (id_node, val_node) = match node {
                    LkqlNode::FunDecl(fd) => (fd.f_name()?, fd.f_fun_expr()?),
                    LkqlNode::SelectorDecl(sd) => (sd.f_name()?, node.clone()),
                    _ => unreachable!(),
                };
                NodeVariant::InitLocal {
                    symbol: Identifier::from_node(&id_node, ctx)?,
                    val: Box::new(Node {
                        origin_location: ctx.source_repo.section_from_lkql_node(&val_node)?,
                        variant: NodeVariant::ChildFunRef(*ctx.child_index_map.get(node).unwrap()),
                    }),
                }
            }

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

            // --- Binary operations
            LkqlNode::BinOp(_) | LkqlNode::RelBinOp(_) | LkqlNode::ArithBinOp(_) => {
                let (left, op, right) = match node {
                    LkqlNode::BinOp(bo) => (bo.f_left()?, bo.f_op()?, bo.f_right()?),
                    _ => unreachable!(),
                };
                NodeVariant::BinOp {
                    left: Box::new(Self::lower_lkql_node(&left, ctx)?),
                    operator: todo!(),
                    right: Box::new(Self::lower_lkql_node(&right, ctx)?),
                }
            }

            // --- Unary operations

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
                            Identifier::from_node(&assoc_node.f_name()?, ctx)?,
                            Self::lower_lkql_node(&assoc_node.f_expr()?, ctx)?,
                        ));
                    }
                }
                NodeVariant::ObjectLiteral(assocs)
            }
            LkqlNode::Identifier(_) => NodeVariant::ReadLocal(Identifier::from_node(node, ctx)?),
            LkqlNode::AnonymousFunction(_) => {
                NodeVariant::ChildFunRef(*ctx.child_index_map.get(node).unwrap())
            }

            // For now, not all node kinds are handled
            _ => panic!("{} is not handled by lowering", node.image()?),
        };

        // Finally return the resulting node
        Ok(Node { origin_location: node_location, variant })
    }
}

impl Identifier {
    /// Util function to easily create an identifier from an LKQL node.
    fn from_node(node: &LkqlNode, ctx: &LoweringContext) -> Result<Self, Error> {
        Ok(Self {
            origin_location: ctx.source_repo.section_from_lkql_node(node)?,
            text: node.text()?,
        })
    }
}

struct LoweringContext<'a> {
    /// Associated source repository.
    source_repo: &'a SourceRepository,

    /// Save child index for each function declaration node
    child_index_map: HashMap<LkqlNode, usize>,
}

impl<'a> LoweringContext<'a> {
    pub fn new(source_repo: &'a SourceRepository) -> Self {
        Self { source_repo: source_repo, child_index_map: HashMap::new() }
    }
}

/// Util function to find all local declarations from the provided node.
/// Is defined as "declaration" all nodes that introduce a new symbol in the
/// lexical environment.
/// A declaration is defined as "local" if it isn't contained in a function-like
/// tree that is strictly lower than the provided node.
/// Are defined as "function-like" the following LKQL nodes:
///   * [`LkqlNode::TopLevelList`]
///   * [`LkqlNode::FunDecl`]
///   * [`LkqlNode::SelectorDecl`]
///   * [`LkqlNode::AnonymousFunction`]
///   * [`LkqlNode::ListComprehension`]
fn all_local_decls(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Error> {
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
fn all_local_symbols(node: &LkqlNode) -> Result<HashSet<String>, Error> {
    let mut local_decls = Vec::new();
    all_local_decls(node, &mut local_decls)?;
    Ok(local_decls
        .iter()
        .map(|n| -> Result<String, Error> {
            match n {
                LkqlNode::ValDecl(vd) => Ok(vd.f_identifier()?.text()?),
                LkqlNode::FunDecl(fd) => Ok(fd.f_name()?.text()?),
                LkqlNode::SelectorDecl(sd) => Ok(sd.f_name()?.text()?),
                _ => unreachable!(),
            }
        })
        .collect::<Result<HashSet<String>, Error>>()?)
}

/// Util function to find all functions in the local environment of the
/// provided node.
/// A node is considered as a "function" if it can be lowered by the
/// [`Function::lower_lkql_node`] method.
/// The locality is defined the same way as it is defined in the
/// [`all_local_decls`] function.
fn all_local_functions(node: &LkqlNode, output: &mut Vec<LkqlNode>) -> Result<(), Error> {
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
