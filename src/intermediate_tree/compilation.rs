//! # Intermediate tree compilation module
//!
//! This module contains all required operations to compile any intermediate
//! tree into a [`crate::bytecode::BytecodeBuffer`].

use crate::{
    builtins::{
        UNIT_SINGLETON_GLOBAL_NAME, get_builtin_bindings,
        traits::BuiltinTrait,
        types::{
            self, BuiltinType, TYPE_NAME_FIELD, TYPE_TAG_FIELD, TypeImplementation,
            stream::lazy_comprehension,
        },
    },
    bytecode::{
        self, BytecodeUnit, ComplexConstant, JUMP_BIASING, NumericConstant, PRIM_FALSE, PRIM_NIL,
        PRIM_TRUE, Prototype, TableConstantElement, VariableData,
        extended_bytecode::{
            ExtendedInstruction, ExtendedInstructionBuffer, ExtendedInstructionVariant, Label,
        },
        op_codes::*,
    },
    error_templates::{
        DUPLICATED_SYMBOL, ErrorTemplate, INDEX_OUT_OF_BOUNDS, MISSING_TRAIT, NO_VALUE_FOR_PARAM,
        POS_AND_NAMED_VALUE_FOR_PARAM, PREVIOUS_SYMBOL_HINT, UNKNOWN_MEMBER, UNKNOWN_SYMBOL,
        WRONG_TYPE,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperatorVariant, MiscOperatorVariant, Node,
        NodeVariant,
        compilation::frame::{
            BindingData, ClosingKind, Frame, FrameVariant, SlotRange, UpValueTarget,
        },
        constant_eval::{ConstantValue, ConstantValueVariant},
    },
    report::{Hint, Report},
    runtime::{DynamicError, DynamicErrorArg, RuntimeData},
    sources::{SourceRepository, SourceSection},
};
use num_bigint::BigInt;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    mem,
    rc::Rc,
    u8, usize,
};

pub mod frame;

// ----- Compilation processes -----

impl ExecutionUnit {
    /// Compile this execution unit as a LuaJIT bytecode buffer. The result of
    /// this function can be used to execute the semantics described by the
    /// execution unit with the LuaJIT engine.
    pub fn compile(
        &self,
        source_repo: &SourceRepository,
    ) -> Result<(BytecodeUnit, RuntimeData), Report> {
        // Open the initial compilation context and create the prototypes vector
        let mut compile_context = CompilationContext::new(self);

        // Compile the current execution unit
        self.internal_compile(&mut compile_context);

        // Then return the success result
        if compile_context.diagnostics.is_empty() {
            Ok((
                BytecodeUnit {
                    prototypes: compile_context.prototypes,
                    source_name: source_repo
                        .get_source_by_id(self.origin_location.source)
                        .unwrap()
                        .name
                        .clone(),
                },
                compile_context.runtime_data,
            ))
        } else {
            Err(Report::Composed(compile_context.diagnostics))
        }
    }

    /// Open a new frame in the compilation context and compile this execution
    /// unit using the [`Self::internal_compile`] method.
    fn open_frame_and_compile<'a, 'b>(&'a self, ctx: &mut CompilationContext<'b>)
    where
        'a: 'b,
    {
        // Open a new semantic frame for this function
        let previous_frame = ctx.frame.clone();
        ctx.frame = Rc::new(RefCell::new(Frame::new(Some(previous_frame.clone()))));

        // Compile the execution unit
        self.internal_compile(ctx);

        // Restore the parent frame
        ctx.frame = previous_frame;
    }

    /// Compile the execution unit and all its children (direct and indirect
    /// ones), and place the result in the provided  `output` buffer.
    fn internal_compile<'a, 'b>(&'a self, ctx: &mut CompilationContext<'b>)
    where
        'a: 'b,
    {
        // Save the previous execution unit and its data and update the
        // compilation context
        let previous_unit = ctx.unit;
        ctx.unit = self;
        let previous_unit_data = mem::replace(&mut ctx.unit_data, ExecUnitCompilationData::new());

        // Save the previous instruction buffer and create a new one for this
        // unit.
        let previous_instructions =
            mem::replace(&mut ctx.instructions, ExtendedInstructionBuffer::new());

        // Create compilation working values
        let mut arg_count = 0;
        let mut is_variadic = false;

        // Compile the execution unit
        match &self.variant {
            ExecutionUnitVariant::Module { symbols, elements, .. } => {
                // Set the emitted prototype as variadic
                is_variadic = true;

                // Add module symbols to the current frame
                ctx.declare_locals(symbols);

                // Compile module elements
                for elem in elements {
                    let elem_access = elem.compile_as_access(ctx, None);
                    elem_access.release(ctx);
                }

                // Reserve a slot for the module table
                let result_tmp = ctx.frame.borrow_mut().get_tmp();

                // Create a source section at the end of the module
                let end_source_section = SourceSection {
                    source: self.origin_location.source.clone(),
                    start: self.origin_location.end,
                    end: self.origin_location.end,
                };

                // Emit instructions to create the module table
                emit_new_table(ctx, &end_source_section, result_tmp, 0, symbols.len() as u32);
                for symbol in symbols {
                    let local_slot = ctx.frame.borrow().get_local(&symbol.text).unwrap();
                    emit_table_member_write(
                        ctx,
                        Some(&end_source_section),
                        local_slot.slot,
                        result_tmp,
                        &symbol.text,
                    );
                }

                // Emit local values closing
                emit_closing_instruction(ctx);

                // Emit returning of the module table
                ctx.instructions
                    .ad(&end_source_section, RET1, result_tmp, 2);
                ctx.frame.borrow_mut().release_slot(result_tmp);
            }
            ExecutionUnitVariant::Function { params, body } => {
                // Set the argument count
                arg_count = params.len() + 1;

                // Reserve the first slot of the frame, by convention, this
                // slot is used to provide named arguments.
                let named_args_slot = ctx.frame.borrow_mut().get_tmp();

                // Get the function parameters identifiers
                let param_identifiers = params.iter().map(|(s, _)| s.clone()).collect::<Vec<_>>();

                // Add function parameters to the current frame
                ctx.declare_locals(&param_identifiers);

                // Emit instructions to ensure all parameters have a valid and
                // unique value.
                for (param_id, maybe_default_value) in params {
                    let param_slot = ctx.frame.borrow().get_local(&param_id.text).unwrap();

                    // Create working labels
                    let no_value_label = ctx.instructions.new_label();
                    let test_both_label = ctx.instructions.new_label();
                    let next_label = ctx.instructions.new_label();

                    // Test if the parameter has a positional value
                    ctx.instructions.ad_no_loc(ISNEP, param_slot.slot, PRIM_NIL);
                    ctx.goto(test_both_label);

                    // If there is no positional value, start by checking if
                    // there is a named value for the parameter.
                    ctx.instructions.ad_no_loc(ISEQP, named_args_slot, PRIM_NIL);
                    ctx.goto(no_value_label);
                    emit_table_member_read(
                        ctx,
                        None,
                        param_slot.slot,
                        named_args_slot,
                        &param_id.text,
                    );
                    ctx.instructions.ad_no_loc(ISNEP, param_slot.slot, PRIM_NIL);
                    ctx.goto(next_label);

                    // If parameter has no value, emit an error
                    ctx.instructions.label(no_value_label);
                    if let Some(default_value) = maybe_default_value {
                        default_value.compile_as_value(ctx, param_slot.slot);
                    } else {
                        emit_runtime_error(
                            ctx,
                            None,
                            &NO_VALUE_FOR_PARAM,
                            &vec![DynamicErrorArg::Static(param_id.text.clone())],
                        );
                    }

                    // Test if the parameter have both positional and named
                    // values
                    ctx.instructions.label(test_both_label);
                    ctx.instructions.ad_no_loc(ISEQP, named_args_slot, PRIM_NIL);
                    ctx.goto(next_label);
                    emit_table_member_read(
                        ctx,
                        None,
                        param_slot.slot,
                        named_args_slot,
                        &param_id.text,
                    );
                    ctx.instructions.ad_no_loc(ISEQP, param_slot.slot, PRIM_NIL);
                    ctx.goto(next_label);
                    emit_runtime_error(
                        ctx,
                        None,
                        &POS_AND_NAMED_VALUE_FOR_PARAM,
                        &vec![DynamicErrorArg::Static(param_id.text.clone())],
                    );

                    // Label the next instruction
                    ctx.instructions.label(next_label);

                    // Finally, set the parameter slot as initialized
                    ctx.frame
                        .borrow_mut()
                        .init_local(&param_id.text, next_label);
                }

                // The compile the body as a returning node
                body.compile_as_function_body(ctx);
            }
            ExecutionUnitVariant::RawCallable { params, body } => {
                // Store the argument count
                arg_count = params.len();

                // Declare all parameters as locals and initialize them
                ctx.declare_locals(&params);
                let birth_label = ctx.instructions.new_label();
                for param in params {
                    ctx.frame.borrow_mut().init_local(&param.text, birth_label);
                }

                // Emit the birth label for parameters
                ctx.instructions.label(birth_label);

                // Finally compile the body of the function
                body.compile_as_function_body(ctx);
            }
        };

        // Release locals of the execution unit
        let death_label = ctx.instructions.new_label();
        ctx.release_locals(death_label);
        ctx.instructions.label(death_label);

        // Emit additional instructions to initialize unsafely closed bindings,
        // they are inserted before all other instructions.
        for binding in ctx.frame.borrow().bindings.values() {
            if binding.closing_kind == ClosingKind::Unsafe {
                ctx.instructions.insert_instruction(
                    0,
                    ExtendedInstruction {
                        origin_location: None,
                        variant: ExtendedInstructionVariant::AD {
                            op_code: KPRI,
                            a: binding.slot,
                            d: PRIM_NIL,
                        },
                    },
                );
            }
        }

        // Perform post compilation assertions
        assert!(arg_count <= u8::MAX as usize, "Too many arguments for prototype");

        // Restore the previous execution unit and get data collected during
        // compilation.
        ctx.unit = previous_unit;
        let mut data = mem::replace(&mut ctx.unit_data, previous_unit_data);

        // Restore the previous instruction buffer
        let instructions = mem::replace(&mut ctx.instructions, previous_instructions);

        // Tell the parent execution unit it has a child
        ctx.unit_data.has_child = true;

        // Now we collect variable information to create debug data
        let label_map = instructions.label_map();
        let mut variable_data = data
            .dead_bindings
            .iter()
            .map(|(n, b)| VariableData {
                name: n.clone(),
                birth_instruction: *label_map.get(&b.birth_label).expect("Unknown label"),
                death_instruction: *label_map.get(&b.death_label).expect("Unknown label"),
            })
            .collect::<Vec<_>>();

        // We sort variable data by birth label
        variable_data.sort_by(|lvd, rvd| lvd.birth_instruction.cmp(&rvd.birth_instruction));

        // Now we get instructions and their locations
        let (instructions, instruction_locations) =
            instructions.as_instructions_and_locations(&self.origin_location);

        // We add all these information in the runtime required data
        let proto_id = ctx
            .runtime_data
            .add_prototype_data(self.origin_location.source, instruction_locations);
        data.constants
            .numeric_constants
            .push(NumericConstant::Integer(proto_id as i32));

        // Create the bytecode prototype and add it to the current context
        // result.
        match &ctx.frame.borrow().variant {
            FrameVariant::Semantic { maximum_size, up_values, .. } => {
                // Sort up-values from their index
                let mut sorted_up_values = up_values.into_iter().collect::<Vec<_>>();
                sorted_up_values.sort_by(|(_, uv_1), (_, uv_2)| uv_1.index.cmp(&uv_2.index));

                // Get the prototype first line
                let first_line = self.origin_location.start.line;

                // Then push the new prototype in the result
                ctx.prototypes.push(Prototype {
                    has_child: data.has_child,
                    is_variadic,
                    has_ffi: true,
                    arg_count: arg_count as u8,
                    frame_size: *maximum_size,
                    instructions,
                    up_values: sorted_up_values
                        .into_iter()
                        .map(|(name, uv)| match uv.target {
                            UpValueTarget::ParentSlot(s) => {
                                bytecode::UpValue::parent_local(name, s as u16)
                            }
                            UpValueTarget::ParentUpValue(u) => {
                                bytecode::UpValue::parent_up_value(name, u as u16)
                            }
                        })
                        .collect(),
                    complex_consts: data.constants.complex_constants,
                    numeric_consts: data.constants.numeric_constants,
                    first_line,
                    line_count: self.origin_location.end.line - first_line,
                    variable_data,
                })
            }
            _ => unreachable!(),
        };
    }
}

impl Node {
    /// Compile the node as a value creating unit, meaning that the result of
    /// this node execution is placed into the `target_slot` during the
    /// execution.
    /// Instructions produced by this compilation are placed in the `output`
    /// vector.
    fn compile_as_value(&self, ctx: &mut CompilationContext, result_slot: u8) {
        // If the node can be evaluated as a constant value that can be
        // directly compiled, just return now.
        if let Some(constant) = self.eval_as_constant() {
            if constant.try_to_compile(ctx, result_slot) {
                return;
            }
        }

        // If we reach here the node can't be evaluated as a constant, so we
        // have to compile it manually
        match &self.variant {
            // --- Call expressions
            NodeVariant::FunCall { .. } | NodeVariant::MethodCall { .. } => {
                // Prepare the call by getting working slots
                let call_slots = match &self.variant {
                    NodeVariant::FunCall { callee, positional_args, named_args } => {
                        Self::prepare_function_call(
                            ctx,
                            &self.origin_location,
                            callee,
                            positional_args,
                            named_args,
                        )
                    }
                    NodeVariant::MethodCall {
                        prefix,
                        method_name,
                        is_safe,
                        positional_args,
                        named_args,
                    } => Self::prepare_method_call(
                        ctx,
                        &self.origin_location,
                        prefix,
                        method_name,
                        *is_safe,
                        positional_args,
                        named_args,
                    ),
                    _ => unreachable!(),
                };

                // Finally emit a call instruction and place the result in the required slot
                ctx.instructions.abc(
                    &self.origin_location,
                    CALL,
                    call_slots.first,
                    2,
                    call_slots.count() - 1,
                );
                ctx.instructions.ad(
                    &self.origin_location,
                    MOV,
                    result_slot,
                    call_slots.first as u16,
                );
                ctx.frame.borrow_mut().release_slots(call_slots);
            }

            // --- Composite expressions
            NodeVariant::DottedExpr { prefix, suffix, is_safe } => {
                let prefix_access = prefix.compile_as_access(ctx, Some(result_slot));
                Self::compile_dot_access(
                    ctx,
                    result_slot,
                    &self.origin_location,
                    prefix_access.slot(),
                    suffix,
                    *is_safe,
                );
                prefix_access.release(ctx);
            }
            NodeVariant::IndexExpr { indexed_val, index, is_safe } => {
                // Get the access to the indexed value and to the index
                let indexed_value_access = indexed_val.compile_as_access(ctx, Some(result_slot));
                let index_access = index.compile_as_access(ctx, None);

                // Emit code to the the value at the specified index
                ctx.instructions.abc(
                    &self.origin_location,
                    TGETV,
                    result_slot,
                    indexed_value_access.slot(),
                    index_access.slot(),
                );
                indexed_value_access.release(ctx);

                // Emit post access check
                let next_label = ctx.instructions.new_label();
                ctx.instructions
                    .ad(&self.origin_location, ISNEP, result_slot, PRIM_NIL);
                ctx.goto(next_label);
                if *is_safe {
                    emit_global_read(
                        ctx,
                        Some(&self.origin_location),
                        result_slot,
                        UNIT_SINGLETON_GLOBAL_NAME,
                    );
                } else {
                    emit_runtime_error(
                        ctx,
                        Some(&index.origin_location),
                        &INDEX_OUT_OF_BOUNDS,
                        &vec![DynamicErrorArg::LocalValue(index_access.slot())],
                    );
                }

                // Release index value access accesses
                index_access.release(ctx);

                // Label the next instruction
                ctx.instructions.label(next_label);
            }
            NodeVariant::InClause { .. } => todo!(),
            NodeVariant::IfExpr { condition, consequence, alternative } => {
                // Create required labels
                let alternative_label = ctx.instructions.new_label();
                let next_label = ctx.instructions.new_label();

                // Compile the condition as a branching node
                condition.compile_as_branching(ctx, alternative_label);

                // Then, compile the consequence, that is going to be executed
                // if the condition is true
                consequence.compile_as_value(ctx, result_slot);
                ctx.goto(next_label);

                // Finally, compile the alternative with the appropriate label
                ctx.instructions.label(alternative_label);
                alternative.compile_as_value(ctx, result_slot);
                ctx.instructions.label(next_label);
            }
            NodeVariant::BlockExpr { local_symbols, body, val } => {
                // Compile the block body
                let parent_frame = Self::compile_block_body(ctx, result_slot, local_symbols, body);

                // Then compile the expression representing the value of the
                // block.
                val.compile_as_value(ctx, result_slot);

                // Release the block local bindings
                let death_label = ctx.instructions.new_label();
                ctx.release_locals(death_label);
                ctx.instructions.label(death_label);

                // Finally, restore the previous frame as the current one
                ctx.frame = parent_frame;
            }

            // --- Lazy comprehension
            NodeVariant::LazyComprehension { source_iterables, body_index } => {
                // First load the empty table that is going to be the result
                emit_new_table(ctx, &self.origin_location, result_slot, 0, 7);

                // Then compile all source iterables, placing them in a table
                // in the same order.
                let collections_table_slot = ctx.frame.borrow_mut().get_tmp();
                let collection_slot = ctx.frame.borrow_mut().get_tmp();
                emit_new_table(
                    ctx,
                    &self.origin_location,
                    collections_table_slot,
                    source_iterables.len() as u16,
                    0,
                );
                for (i, collection) in source_iterables.iter().enumerate() {
                    let collection_access =
                        collection.compile_as_access(ctx, Some(collection_slot));
                    emit_table_index_write(
                        ctx,
                        &self.origin_location,
                        collection_access.slot(),
                        collections_table_slot,
                        i + 1,
                    );
                    collection_access.release(ctx);
                }

                // Place the table with all source collections in the resulting
                // value.
                emit_table_member_write(
                    ctx,
                    Some(&self.origin_location),
                    collections_table_slot,
                    result_slot,
                    lazy_comprehension::COLLECTIONS_FIELD,
                );
                ctx.frame.borrow_mut().release_slot(collection_slot);
                ctx.frame.borrow_mut().release_slot(collections_table_slot);

                // Get the child execution unit representing the comprehension
                // body and place it in the result.
                let body_name = &ctx.unit.children_units[*body_index as usize].name;
                ctx.frame
                    .borrow_mut()
                    .bind_local(body_name, &self.origin_location);
                Self::compile_child_unit(ctx, &self.origin_location, *body_index as usize);
                let body_binding = ctx.frame.borrow().get_local(body_name).unwrap();
                emit_table_member_write(
                    ctx,
                    Some(&self.origin_location),
                    body_binding.slot,
                    result_slot,
                    lazy_comprehension::BODY_FIELD,
                );

                // Finally set the meta-table of the resulting value
                emit_set_metatable(
                    ctx,
                    Some(&self.origin_location),
                    result_slot,
                    &types::stream::lazy_comprehension::SPECIALIZATION,
                );
            }

            // --- Binary operations
            NodeVariant::ArithBinOp { left, operator, right } => {
                // Try to compile the operation with a constant-operand shape
                let mut already_compiled = false;
                if let Some(left_numeric_constant) = left
                    .eval_as_constant()
                    .and_then(|c| c.to_numeric_constant())
                {
                    already_compiled = compile_arith_bin_op_with_constant_operand(
                        ctx,
                        result_slot,
                        self,
                        operator,
                        right,
                        left_numeric_constant,
                        true,
                    );
                } else if let Some(right_numeric_constant) = right
                    .eval_as_constant()
                    .and_then(|c| c.to_numeric_constant())
                {
                    already_compiled = compile_arith_bin_op_with_constant_operand(
                        ctx,
                        result_slot,
                        self,
                        operator,
                        left,
                        right_numeric_constant,
                        false,
                    )
                }

                // If the node hasn't been compile yet, fallback on emitting
                // the fully slot-based variant.
                if !already_compiled {
                    let left_access = left.compile_as_access(ctx, Some(result_slot));
                    let right_access = right.compile_as_access(ctx, None);
                    let op = match operator.variant {
                        ArithOperatorVariant::Plus => ADDVV,
                        ArithOperatorVariant::Minus => SUBVV,
                        ArithOperatorVariant::Multiply => MULVV,
                        ArithOperatorVariant::Divide => DIVVV,
                    };
                    ctx.instructions.abc(
                        &self.origin_location,
                        op,
                        result_slot,
                        left_access.slot(),
                        right_access.slot(),
                    );
                    left_access.release(ctx);
                    right_access.release(ctx);
                }
            }
            NodeVariant::LogicBinOp { .. } | NodeVariant::CompBinOp { .. } => {
                // Create required labels
                let if_false_label = ctx.instructions.new_label();
                let next_label = ctx.instructions.new_label();

                // Compile the current node as branching to emit short
                // circuiting instructions
                self.compile_as_branching(ctx, if_false_label);

                // Emit the code to set the result to "true"
                ctx.instructions
                    .ad(&self.origin_location, KPRI, result_slot, PRIM_TRUE);
                ctx.goto(next_label);

                // Emit the code to set the result to "false"
                ctx.instructions.label(if_false_label);
                ctx.instructions
                    .ad(&self.origin_location, KPRI, result_slot, PRIM_FALSE);

                // Label the next instruction as such
                ctx.instructions.label(next_label);
            }
            NodeVariant::MiscBinOp { left, operator, right } => {
                let (left_access, right_access) = match operator.variant {
                    // The concatenation operation requires that values are
                    // stored in contiguous slots.
                    MiscOperatorVariant::Concat => {
                        let operand_slots = ctx.frame.borrow_mut().reserve_contiguous_slots(2);
                        left.compile_as_value(ctx, operand_slots.first);
                        right.compile_as_value(ctx, operand_slots.last);
                        (
                            ValueAccess::OwnedTmp(operand_slots.first),
                            ValueAccess::OwnedTmp(operand_slots.last),
                        )
                    }
                };
                let op = match operator.variant {
                    MiscOperatorVariant::Concat => CAT,
                };
                ctx.instructions.abc(
                    &self.origin_location,
                    op,
                    result_slot,
                    left_access.slot(),
                    right_access.slot(),
                );
                left_access.release(ctx);
                right_access.release(ctx);
            }

            // --- Unary operations
            NodeVariant::ArithUnOp { operator, operand } => {
                let operand_access = operand.compile_as_access(ctx, Some(result_slot));
                let op = match operator.variant {
                    ArithOperatorVariant::Plus => MOV,
                    ArithOperatorVariant::Minus => UNM,
                    _ => unreachable!(),
                };
                ctx.instructions.ad(
                    &self.origin_location,
                    op,
                    result_slot,
                    operand_access.slot() as u16,
                );
                operand_access.release(ctx);
            }
            NodeVariant::LogicUnOp { operator, operand } => {
                let operand_access = operand.compile_as_access(ctx, Some(result_slot));
                let op = match operator.variant {
                    LogicOperatorVariant::Not => NOT,
                    _ => unreachable!(),
                };
                ctx.instructions.ad(
                    &self.origin_location,
                    op,
                    result_slot,
                    operand_access.slot() as u16,
                );
                operand_access.release(ctx);
            }

            // --- Symbol accesses
            NodeVariant::InitLocal { symbol, val } => {
                // Create the birth label of the local variable
                let birth_label = ctx.instructions.new_label();

                // Then compile the initialization value and place it into the
                // reserved slot.
                let binding_slot = ctx.frame.borrow().get_local(&symbol.text).unwrap();
                val.compile_as_value(ctx, binding_slot.slot);

                // Then label the next instruction as the birthing one and flag
                // the slot as initialized in the current frame.
                ctx.instructions.label(birth_label);
                ctx.frame.borrow_mut().init_local(&symbol.text, birth_label);
            }
            NodeVariant::InitLocalFun(child_index) => {
                Self::compile_child_unit(ctx, &self.origin_location, *child_index as usize);
            }
            NodeVariant::ReadSymbol(identifier) => {
                // First try getting the symbol in the local frame
                let maybe_local_binding = ctx.frame.borrow().get_local(&identifier.text);
                if let Some(BindingData { slot, is_init: true, .. }) = maybe_local_binding {
                    ctx.instructions
                        .ad(&self.origin_location, MOV, result_slot, slot as u16);
                } else {
                    // Then try to look in the up-values
                    let maybe_up_value = ctx.frame.borrow_mut().get_up_value(&identifier.text);
                    if let Some(up_value) = maybe_up_value {
                        ctx.instructions.ad(
                            &self.origin_location,
                            UGET,
                            result_slot,
                            up_value.index as u16,
                        );
                        if !up_value.is_safe {
                            let next_label = ctx.instructions.new_label();
                            ctx.instructions.ad(
                                &self.origin_location,
                                ISNEP,
                                result_slot,
                                PRIM_NIL,
                            );
                            ctx.goto(next_label);
                            emit_runtime_error(
                                ctx,
                                Some(&identifier.origin_location),
                                &UNKNOWN_SYMBOL,
                                &vec![DynamicErrorArg::Static(identifier.text.clone())],
                            );
                            ctx.instructions.label(next_label);
                        }
                    } else {
                        // Finally, if a built-in is named like the accessed
                        // symbol, get it in the global table.
                        if ctx.builtins.contains(identifier.text.as_str()) {
                            emit_global_read(
                                ctx,
                                Some(&self.origin_location),
                                result_slot,
                                &identifier.text,
                            );
                        }
                        // If all previous step failed, the symbol doesn't
                        // exists, so we emit an error about it.
                        else {
                            ctx.diagnostics.push(Report::from_error_template(
                                &self.origin_location,
                                &UNKNOWN_SYMBOL,
                                &vec![&identifier.text],
                            ));
                        }
                    }
                }
            }

            // --- Lambda function access
            NodeVariant::LambdaFun(child_index) => {
                // Add the lambda symbol in the frame locals
                let lambda_name = &ctx.unit.children_units[*child_index as usize].name;
                ctx.frame
                    .borrow_mut()
                    .bind_local(lambda_name, &self.origin_location);

                // Compile the child unit
                Self::compile_child_unit(ctx, &self.origin_location, *child_index as usize);

                // Finally move the lambda value in the result slot
                let lambda_binding = ctx.frame.borrow().get_local(lambda_name).unwrap();
                ctx.instructions.ad(
                    &self.origin_location,
                    MOV,
                    result_slot,
                    lambda_binding.slot as u16,
                );
            }

            // --- Type checkers
            NodeVariant::CheckType { expression, expected_type } => {
                // Compile the expression in the result slot
                expression.compile_as_value(ctx, result_slot);

                // Then emit type checking instructions
                emit_type_check(ctx, Some(&self.origin_location), result_slot, expected_type);
            }
            NodeVariant::CheckTrait { expression, required_trait } => {
                // Compile the expression in the result slot
                expression.compile_as_value(ctx, result_slot);

                // Then emit trait checking instructions
                emit_trait_check(ctx, Some(&self.origin_location), result_slot, required_trait);
            }

            // --- Non-trivial literals
            NodeVariant::TupleLiteral(elements) | NodeVariant::ListLiteral(elements) => {
                // Compile nodes inside the tuple literals and place them in a table
                Self::compile_table(ctx, result_slot, &self.origin_location, elements, &Vec::new());

                // Then set the meta-table of this new table
                let type_impl = match &self.variant {
                    NodeVariant::TupleLiteral(_) => &types::tuple::IMPLEMENTATION,
                    NodeVariant::ListLiteral(_) => &types::list::IMPLEMENTATION,
                    _ => unreachable!(),
                };
                emit_set_metatable(ctx, Some(&self.origin_location), result_slot, type_impl);
            }
            NodeVariant::ObjectLiteral(items) => {
                Self::compile_table(ctx, result_slot, &self.origin_location, &Vec::new(), items);
                emit_set_metatable(
                    ctx,
                    Some(&self.origin_location),
                    result_slot,
                    &types::obj::IMPLEMENTATION,
                );
            }

            // --- Nil literal needs a special compilation process
            NodeVariant::NilLiteral => {
                ctx.instructions
                    .ad(&self.origin_location, KPRI, result_slot, PRIM_NIL);
            }

            // --- All trivial literal nodes should've been compiled by now
            NodeVariant::NullLiteral
            | NodeVariant::UnitLiteral
            | NodeVariant::BoolLiteral(_)
            | NodeVariant::IntLiteral(_)
            | NodeVariant::StringLiteral(_) => unreachable!(),
        }

        /// Try compiling the provided arithmetic operation in a constant
        /// optimized way, returning whether it is successful.
        fn compile_arith_bin_op_with_constant_operand(
            ctx: &mut CompilationContext,
            result_slot: u8,
            operation: &Node,
            operator: &ArithOperator,
            variable_operand: &Box<Node>,
            constant_operand: NumericConstant,
            is_constant_left: bool,
        ) -> bool {
            // Get the operation code regarding the arithmetic operator
            let op = match operator.variant {
                ArithOperatorVariant::Plus => ADDVN,
                ArithOperatorVariant::Minus => {
                    if is_constant_left {
                        SUBNV
                    } else {
                        SUBVN
                    }
                }
                ArithOperatorVariant::Multiply => MULVN,
                ArithOperatorVariant::Divide => {
                    if is_constant_left {
                        DIVNV
                    } else {
                        DIVVN
                    }
                }
            };

            // Try to get the numeric constant index as an u8
            if let Some(numeric_cst) = ctx
                .unit_data
                .constants
                .try_from_numeric_constant_as_u8(constant_operand)
            {
                // Emit the code the perform the arithmetic operation with the
                // constant operand.
                let operand_access = variable_operand.compile_as_access(ctx, Some(result_slot));
                ctx.instructions.abc(
                    &operation.origin_location,
                    op,
                    result_slot,
                    operand_access.slot(),
                    numeric_cst as u8,
                );
                operand_access.release(ctx);

                // Return the success
                true
            } else {
                // The compilation failed
                false
            }
        }
    }

    /// Compile the node as a value, but instead of placing it in a result slot
    /// like the [`Self::compile_as_value`] function does, this function
    /// returns a value representing an access to the node's result.
    /// If the compiled node represent an access to an already stored value,
    /// the result allow direct access to it, avoiding a useless copy.
    /// Otherwise, this function reserve a temporary slot (or use the already
    /// reserved provided one), and use the [`Self::compile_as_value`]
    /// function to place the node result in it.
    fn compile_as_access(
        &self,
        ctx: &mut CompilationContext,
        already_reserved_slot: Option<u8>,
    ) -> ValueAccess {
        fn fallback(
            ctx: &mut CompilationContext,
            already_reserved_slot: Option<u8>,
            node: &Node,
        ) -> ValueAccess {
            let result_slot = if already_reserved_slot.is_some() {
                already_reserved_slot.unwrap()
            } else {
                ctx.frame.borrow_mut().get_tmp()
            };
            node.compile_as_value(ctx, result_slot);
            if already_reserved_slot.is_some() {
                ValueAccess::BorrowedTmp(result_slot)
            } else {
                ValueAccess::OwnedTmp(result_slot)
            }
        }

        match &self.variant {
            NodeVariant::FunCall { .. } | NodeVariant::MethodCall { .. } => {
                // Prepare the call by getting slots used for it
                let call_slots = match &self.variant {
                    NodeVariant::FunCall { callee, positional_args, named_args } => {
                        Self::prepare_function_call(
                            ctx,
                            &self.origin_location,
                            callee,
                            positional_args,
                            named_args,
                        )
                    }
                    NodeVariant::MethodCall {
                        prefix,
                        method_name,
                        is_safe,
                        positional_args,
                        named_args,
                    } => Self::prepare_method_call(
                        ctx,
                        &self.origin_location,
                        prefix,
                        method_name,
                        *is_safe,
                        positional_args,
                        named_args,
                    ),
                    _ => unreachable!(),
                };

                // Emit the call instruction
                ctx.instructions.abc(
                    &self.origin_location,
                    CALL,
                    call_slots.first,
                    2,
                    call_slots.count() - 1,
                );

                // Then free all slots except the first one that contains the result
                ctx.frame
                    .borrow_mut()
                    .release_slots(call_slots.sub_range(Some(1), None));
                ValueAccess::OwnedTmp(call_slots.first)
            }

            NodeVariant::ReadSymbol(identifier) => {
                let maybe_local_binding = ctx.frame.borrow().get_local(&identifier.text);
                if let Some(BindingData { slot, is_init: true, .. }) = maybe_local_binding {
                    ValueAccess::Direct(slot)
                } else {
                    fallback(ctx, already_reserved_slot, self)
                }
            }

            NodeVariant::CheckType { expression, expected_type } => {
                let res = expression.compile_as_access(ctx, already_reserved_slot);
                emit_type_check(ctx, Some(&self.origin_location), res.slot(), expected_type);
                res
            }

            NodeVariant::CheckTrait { expression, required_trait } => {
                let res = expression.compile_as_access(ctx, already_reserved_slot);
                emit_trait_check(ctx, Some(&self.origin_location), res.slot(), required_trait);
                res
            }
            _ => fallback(ctx, already_reserved_slot, self),
        }
    }

    /// Compile the node as a branching one. It means that the node value is
    /// going to be tested at runtime, and if it is falsy, execution is going
    /// to jump to the provided `if_false_label` label.
    /// This function also optimize boolean evaluation of logic and equality
    /// operations.
    fn compile_as_branching(&self, ctx: &mut CompilationContext, if_false_label: Label) {
        // Use the `internal_compile` function to compile the node as a
        // branching one.
        let if_true_label = ctx.instructions.new_label();
        internal_compile(self, ctx, if_true_label, if_false_label, BranchingKind::IfFalse);
        ctx.instructions.label(if_true_label);

        /// Internal branching compilation function, this is used to hide
        /// branching optimization to other compiler parts.
        fn internal_compile(
            node: &Node,
            ctx: &mut CompilationContext,
            if_true_label: Label,
            if_false_label: Label,
            branching_kind: BranchingKind,
        ) {
            match &node.variant {
                NodeVariant::LogicBinOp { left, operator, right } => {
                    // Create a label in case left operand is also a short
                    // circuit operation.
                    let eval_right_part_label = ctx.instructions.new_label();

                    // First, evaluate the left operand as a branching
                    match operator.variant {
                        LogicOperatorVariant::Or => {
                            internal_compile(
                                left,
                                ctx,
                                if_true_label,
                                eval_right_part_label,
                                BranchingKind::IfTrue,
                            );
                        }
                        LogicOperatorVariant::And => {
                            internal_compile(
                                left,
                                ctx,
                                eval_right_part_label,
                                if_false_label,
                                BranchingKind::IfFalse,
                            );
                        }
                        _ => unreachable!(),
                    }

                    // Then, emit code to evaluate the right operand
                    ctx.instructions.label(eval_right_part_label);
                    internal_compile(right, ctx, if_true_label, if_false_label, branching_kind);
                }
                NodeVariant::CompBinOp { left, operator, right } => {
                    // Try to compile the operation with a constant-operand shape
                    let already_compiled = match (left.eval_as_constant(), right.eval_as_constant())
                    {
                        (_, Some(right_constant_value)) => {
                            compile_comp_bin_op_with_constant_operand(
                                ctx,
                                node,
                                operator,
                                left,
                                &right_constant_value,
                                if_true_label,
                                if_false_label,
                                branching_kind,
                            )
                        }
                        (Some(left_constant_value), _) => {
                            compile_comp_bin_op_with_constant_operand(
                                ctx,
                                node,
                                operator,
                                right,
                                &left_constant_value,
                                if_true_label,
                                if_false_label,
                                branching_kind,
                            )
                        }
                        _ => false,
                    };

                    // If the node hasn't been compile yet, fallback on emitting
                    // the fully slot-based variant.
                    if !already_compiled {
                        // Get access to left and right values
                        let left_access = left.compile_as_access(ctx, None);
                        let right_access = right.compile_as_access(ctx, None);

                        // Get the inverted comparison operation code according to the
                        // operator.
                        let op = match (operator.variant, branching_kind) {
                            (CompOperatorVariant::Equals, BranchingKind::IfFalse)
                            | (CompOperatorVariant::NotEquals, BranchingKind::IfTrue) => ISNEV,
                            (CompOperatorVariant::NotEquals, BranchingKind::IfFalse)
                            | (CompOperatorVariant::Equals, BranchingKind::IfTrue) => ISEQV,
                            (CompOperatorVariant::Greater, BranchingKind::IfFalse)
                            | (CompOperatorVariant::LessOrEquals, BranchingKind::IfTrue) => ISLE,
                            (CompOperatorVariant::LessOrEquals, BranchingKind::IfFalse)
                            | (CompOperatorVariant::Greater, BranchingKind::IfTrue) => ISGT,
                            (CompOperatorVariant::GreaterOrEquals, BranchingKind::IfFalse)
                            | (CompOperatorVariant::Less, BranchingKind::IfTrue) => ISLT,
                            (CompOperatorVariant::Less, BranchingKind::IfFalse)
                            | (CompOperatorVariant::GreaterOrEquals, BranchingKind::IfTrue) => ISGE,
                        };

                        // Emit code to jump to the provided label if the comparison is
                        // false.
                        ctx.instructions.ad(
                            &node.origin_location,
                            op,
                            left_access.slot(),
                            right_access.slot() as u16,
                        );
                        ctx.goto(match branching_kind {
                            BranchingKind::IfTrue => if_true_label,
                            BranchingKind::IfFalse => if_false_label,
                        });

                        // Release access to values
                        left_access.release(ctx);
                        right_access.release(ctx);
                    }
                }

                // In all other cases, the node is compiled as an access and
                // the value of the latter is tested to branch if required.
                _ => {
                    let result_access = node.compile_as_access(ctx, None);
                    let (op, label) = match branching_kind {
                        BranchingKind::IfTrue => (IST, if_true_label),
                        BranchingKind::IfFalse => (ISF, if_false_label),
                    };
                    ctx.instructions
                        .ad(&node.origin_location, op, 0, result_access.slot() as u16);
                    ctx.goto(label);
                    result_access.release(ctx);
                }
            }
        }

        /// Try compiling the provided comparison operation in a constant
        /// optimized way, returning whether it is successful.
        fn compile_comp_bin_op_with_constant_operand(
            ctx: &mut CompilationContext,
            operation: &Node,
            operator: &CompOperator,
            variable_operand: &Box<Node>,
            constant_operand: &ConstantValue,
            if_true_label: Label,
            if_false_label: Label,
            branching_kind: BranchingKind,
        ) -> bool {
            // Get operation code and the "d" operand if possible
            let maybe_op_and_d = match (operator.variant, branching_kind) {
                (CompOperatorVariant::Equals, BranchingKind::IfFalse)
                | (CompOperatorVariant::NotEquals, BranchingKind::IfTrue) => {
                    match &constant_operand.variant {
                        ConstantValueVariant::Bool(constant_bool) => {
                            Some((ISNEP, if *constant_bool { PRIM_TRUE } else { PRIM_FALSE }))
                        }
                        ConstantValueVariant::String(s) => {
                            Some((ISNES, ctx.unit_data.constants.get_from_string(s)))
                        }
                        _ => constant_operand
                            .to_numeric_constant()
                            .map(|n| (ISNEN, ctx.unit_data.constants.get_from_numeric_constant(n))),
                    }
                }
                (CompOperatorVariant::NotEquals, BranchingKind::IfFalse)
                | (CompOperatorVariant::Equals, BranchingKind::IfTrue) => {
                    match &constant_operand.variant {
                        ConstantValueVariant::Bool(constant_bool) => {
                            Some((ISEQP, if *constant_bool { PRIM_TRUE } else { PRIM_FALSE }))
                        }
                        ConstantValueVariant::String(s) => {
                            Some((ISEQS, ctx.unit_data.constants.get_from_string(s)))
                        }
                        _ => constant_operand
                            .to_numeric_constant()
                            .map(|n| (ISEQN, ctx.unit_data.constants.get_from_numeric_constant(n))),
                    }
                }
                _ => None,
            };

            // Compile the node if possible
            if let Some((op, d)) = maybe_op_and_d {
                // Get an access to the variable operand
                let operand_access = variable_operand.compile_as_access(ctx, None);

                // Emit the branching instruction
                ctx.instructions
                    .ad(&operation.origin_location, op, operand_access.slot(), d);
                ctx.goto(match branching_kind {
                    BranchingKind::IfTrue => if_true_label,
                    BranchingKind::IfFalse => if_false_label,
                });

                // Label the next instruction as such and release the operand
                // access.
                operand_access.release(ctx);
                true
            } else {
                false
            }
        }

        /// This type is used for internal branching optimization, it
        /// represents what kind of branching the current node should compile
        /// to.
        #[derive(Clone, Copy)]
        enum BranchingKind {
            IfTrue,
            IfFalse,
        }
    }

    /// Compile the node as a function body, meaning that the emitted code is
    /// going to return the value of this node to the caller.
    /// This function also emits required instructions to close local up-values
    /// for them to be available to children functions.
    /// This function is used to compile composite expressions in an optimized
    /// way by having return instructions in every branch, and use the tail
    /// call recursion.
    fn compile_as_function_body(&self, ctx: &mut CompilationContext) {
        match &self.variant {
            // In the case of a function / method call, we can emit a tail call
            NodeVariant::FunCall { .. } | NodeVariant::MethodCall { .. } => {
                // Prepare the call by getting working slots
                let call_slots = match &self.variant {
                    NodeVariant::FunCall { callee, positional_args, named_args } => {
                        Self::prepare_function_call(
                            ctx,
                            &self.origin_location,
                            callee,
                            positional_args,
                            named_args,
                        )
                    }
                    NodeVariant::MethodCall {
                        prefix,
                        method_name,
                        is_safe,
                        positional_args,
                        named_args,
                    } => Self::prepare_method_call(
                        ctx,
                        &self.origin_location,
                        prefix,
                        method_name,
                        *is_safe,
                        positional_args,
                        named_args,
                    ),
                    _ => unreachable!(),
                };

                // Enclose required bindings
                emit_closing_instruction(ctx);

                // Emit a function tail call
                ctx.instructions.ad(
                    &self.origin_location,
                    CALLT,
                    call_slots.first,
                    call_slots.count() as u16 - 1,
                );
            }

            // In the case of a conditional expression, avoid useless jump and
            // compile each branch as returning.
            NodeVariant::IfExpr { condition, consequence, alternative } => {
                // Prepare the alternative label
                let alternative_label = ctx.instructions.new_label();

                // Compile the condition as a branching one
                condition.compile_as_branching(ctx, alternative_label);

                // Emit code to return the consequence value
                consequence.compile_as_function_body(ctx);

                // Emit code to return the alternative
                ctx.instructions.label(alternative_label);
                alternative.compile_as_function_body(ctx);
            }

            // In the case of a block expression, compile its result as return
            NodeVariant::BlockExpr { local_symbols, body, val } => {
                // Compile the block body
                let body_elem_tmp = ctx.frame.borrow_mut().get_tmp();
                let parent_frame =
                    Self::compile_block_body(ctx, body_elem_tmp, local_symbols, body);
                ctx.frame.borrow_mut().release_slot(body_elem_tmp);

                // The compile the value as a returning node
                val.compile_as_function_body(ctx);

                // Release the block local bindings
                let death_label = ctx.instructions.new_label();
                ctx.release_locals(death_label);
                ctx.instructions.label(death_label);

                // Finally, restore the previous frame as the current one
                ctx.frame = parent_frame;
            }

            // In the case of a `nil` literal, return nothing
            NodeVariant::NilLiteral => {
                ctx.instructions.ad(&self.origin_location, RET0, 0, 1);
            }

            // In all other cases, just get an access to the value and return
            // it.
            _ => {
                let value_access = self.compile_as_access(ctx, None);
                emit_closing_instruction(ctx);
                ctx.instructions
                    .ad(&self.origin_location, RET1, value_access.slot(), 2);
                value_access.release(ctx);
            }
        }
    }

    /// Util function to compile a child execution unit. This function
    /// initialize the local slot associated to the child at the provided index
    /// with the child identifier as debug name.
    fn compile_child_unit(
        ctx: &mut CompilationContext,
        origin_location: &SourceSection,
        child_index: usize,
    ) {
        // Create the birth label of the local variable
        let birth_label = ctx.instructions.new_label();

        // Retrieve the child execution unit and its name
        let child_unit = &ctx.unit.children_units[child_index];
        let child_name = &child_unit.name;

        // Get the slot to place the functional value in
        let binding_slot = ctx.frame.borrow().get_local(child_name).unwrap();

        // Flag the local slot as initialized before compiling the
        // unit to allow the latter to be recursive.
        ctx.frame.borrow_mut().init_local(child_name, birth_label);
        child_unit.open_frame_and_compile(ctx);

        // Add a child constant in the constant table
        let child_cst = ctx.unit_data.constants.get_child();

        // Finally, emit instruction to set place the functional value
        // in the local slot.
        ctx.instructions
            .ad(origin_location, FNEW, binding_slot.slot, child_cst);
        ctx.instructions.label(birth_label);
    }

    /// Util function used to compile a collection of nodes into a table value.
    fn compile_table(
        ctx: &mut CompilationContext,
        result_slot: u8,
        origin_location: &SourceSection,
        array_part_nodes: &Vec<Node>,
        hash_part_nodes: &Vec<(Identifier, Node)>,
    ) {
        // Create the constant array part
        let mut array_constant_elems = Vec::new();
        let mut array_remains = Vec::new();
        for (i, array_elem) in array_part_nodes.iter().enumerate() {
            if let Some(array_elem_table_constant) = array_elem
                .eval_as_constant()
                .and_then(|c| c.to_table_constant_element())
            {
                array_constant_elems.push(array_elem_table_constant);
            } else {
                array_constant_elems.push(TableConstantElement::Nil);
                array_remains.push((i, array_elem));
            }
        }

        // Create the constant hash part
        let mut hash_constant_elems = Vec::new();
        let mut hash_remains = Vec::new();
        for (name, hash_elem) in hash_part_nodes {
            if let Some(hash_elem_table_constant) = hash_elem
                .eval_as_constant()
                .and_then(|c| c.to_table_constant_element())
            {
                hash_constant_elems.push((
                    TableConstantElement::String(name.text.clone()),
                    hash_elem_table_constant,
                ));
            } else {
                hash_remains.push((name, hash_elem));
            }
        }

        // Emit instruction to create a new table or copy the constant one if
        // the table has constant parts.
        if !array_constant_elems.is_empty() || !hash_constant_elems.is_empty() {
            let table_cst =
                ctx.unit_data
                    .constants
                    .get_from_complex_constant(ComplexConstant::Table {
                        array_part: array_constant_elems,
                        hash_part: hash_constant_elems,
                    });
            ctx.instructions
                .ad(origin_location, TDUP, result_slot, table_cst);
        } else {
            emit_new_table(
                ctx,
                origin_location,
                result_slot,
                array_part_nodes.len() as u16,
                hash_part_nodes.len() as u32,
            );
        }

        // Finally place values that cannot be represented with table constants
        // in the table.
        for (i, array_elem) in array_remains {
            let elem_access = array_elem.compile_as_access(ctx, None);
            emit_table_index_write(ctx, origin_location, elem_access.slot(), result_slot, i + 1);
            elem_access.release(ctx);
        }
        for (name, hash_elem) in hash_remains {
            let elem_access = hash_elem.compile_as_access(ctx, None);
            emit_table_member_write(
                ctx,
                Some(origin_location),
                elem_access.slot(),
                result_slot,
                &name.text,
            );
            elem_access.release(ctx);
        }
    }

    /// Util function to prepare a function call and then return the slot range
    /// in which the function and arguments are placed.
    fn prepare_function_call(
        ctx: &mut CompilationContext,
        origin_location: &SourceSection,
        callee: &Node,
        positional_args: &Vec<Node>,
        named_args: &Vec<(Identifier, Node)>,
    ) -> SlotRange {
        // Reserve slots for the call
        let call_slots = ctx
            .frame
            .borrow_mut()
            .reserve_contiguous_slots(positional_args.len() + 3);

        // Evaluate the callee and place it in the first of the call slots.
        callee.compile_as_value(ctx, call_slots.first);

        // Prepare arguments
        Self::compile_args(
            ctx,
            origin_location,
            positional_args,
            call_slots.first + 3,
            named_args,
            call_slots.first + 2,
        );

        // Finally return slots used for the call
        call_slots
    }

    /// Util function to prepare a method call and then return the slot range
    /// in which the method and arguments (including 'this') are placed.
    fn prepare_method_call(
        ctx: &mut CompilationContext,
        origin_location: &SourceSection,
        prefix: &Node,
        method_name: &Identifier,
        is_safe: bool,
        positional_args: &Vec<Node>,
        named_args: &Vec<(Identifier, Node)>,
    ) -> SlotRange {
        // Reserve all slots for the method call
        let call_slots = ctx
            .frame
            .borrow_mut()
            .reserve_contiguous_slots(positional_args.len() + 4);
        let callee_slot = call_slots.first;
        let this_slot = call_slots.first + 3;

        // Place the "this" argument in the call slots
        prefix.compile_as_value(ctx, this_slot);

        // Compile the field accessing and place it as the callee
        Self::compile_dot_access(
            ctx,
            callee_slot,
            &SourceSection::range(&prefix.origin_location, &method_name.origin_location).unwrap(),
            this_slot,
            method_name,
            is_safe,
        );

        // Compile arguments
        Self::compile_args(
            ctx,
            origin_location,
            positional_args,
            call_slots.first + 4,
            named_args,
            call_slots.first + 2,
        );

        // Finally return slots used for the call
        call_slots
    }

    /// Util function used to compile provided arguments in a way to place
    /// their values in the specified slot range. The first slot is going to
    /// hold the named arguments table, and all others are going to store
    /// positional ones.
    fn compile_args(
        ctx: &mut CompilationContext,
        origin_location: &SourceSection,
        positional_args: &Vec<Node>,
        first_positional_arg_slot: u8,
        named_args: &Vec<(Identifier, Node)>,
        named_args_slot: u8,
    ) {
        // Compile named arguments to a table (or nil if there are no named
        // arguments).
        if named_args.is_empty() {
            ctx.instructions
                .ad(origin_location, KPRI, named_args_slot, PRIM_NIL);
        } else {
            Self::compile_table(ctx, named_args_slot, origin_location, &Vec::new(), named_args);
        }

        // Place positional arguments in the required slots
        for (i, positional_arg) in positional_args.iter().enumerate() {
            positional_arg.compile_as_value(ctx, first_positional_arg_slot + i as u8);
        }
    }

    /// Util function used to compile the body part of a block expression. Note
    /// that this function is opening a new frame and filling it with block
    /// locals without freeing it.
    /// This function returns the parent frame for the caller to restore the
    /// previous compilation context.
    fn compile_block_body(
        ctx: &mut CompilationContext,
        working_slot: u8,
        local_symbols: &Vec<Identifier>,
        body: &Vec<Node>,
    ) -> Rc<RefCell<Frame>> {
        // Open a new lexical frame that will contains all symbols
        // declared in the block.
        let parent_frame = ctx.frame.clone();
        let current_frame = Rc::new(RefCell::new(Frame::new_lexical(parent_frame.clone())));
        ctx.frame = current_frame;

        // Insert all locals in the frame
        ctx.declare_locals(local_symbols);

        // Compile the body of the block
        for body_elem in body {
            let elem_access = body_elem.compile_as_access(ctx, Some(working_slot));

            // TODO: Check that the value returned by the body element
            // is "unit".
            // (https://github.com/HugoGGuerrier/lkqlua_jit/issues/4)

            elem_access.release(ctx);
        }

        // Return the parent frame
        parent_frame
    }

    /// Util function factorizing the compilation process of a (potentially
    /// safe) dot access. This function also emit the code to check the member
    /// exists in the accessed prefix.
    fn compile_dot_access(
        ctx: &mut CompilationContext,
        result_slot: u8,
        origin_location: &SourceSection,
        prefix: u8,
        suffix: &Identifier,
        is_safe: bool,
    ) {
        // Get the table member corresponding to the suffix in the
        // result slot
        emit_table_member_read(ctx, Some(origin_location), result_slot, prefix, &suffix.text);

        // Emit post access check
        let next_label = ctx.instructions.new_label();
        ctx.instructions
            .ad(origin_location, ISNEP, result_slot, PRIM_NIL);
        ctx.goto(next_label);
        if is_safe {
            emit_global_read(ctx, Some(origin_location), result_slot, UNIT_SINGLETON_GLOBAL_NAME);
        } else {
            emit_runtime_error(
                ctx,
                Some(&suffix.origin_location),
                &UNKNOWN_MEMBER,
                &vec![DynamicErrorArg::Static(suffix.text.clone())],
            );
        }

        // Label the next instruction
        ctx.instructions.label(next_label);
    }
}

impl ConstantValue {
    /// Try to compile the constant value by emitting required instructions to
    /// set the `result_slot` to the runtime value represented by the constant.
    /// Return whether the value has been compiled.
    /// For now, only integers lower than [`i32::MAX`] are handled
    /// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/3).
    fn try_to_compile(&self, ctx: &mut CompilationContext, result_slot: u8) -> bool {
        match &self.variant {
            ConstantValueVariant::Null => {
                emit_global_read(ctx, Some(&self.origin_location), result_slot, "<lkql_null>");
                true
            }
            ConstantValueVariant::Unit => {
                emit_global_read(
                    ctx,
                    Some(&self.origin_location),
                    result_slot,
                    UNIT_SINGLETON_GLOBAL_NAME,
                );
                true
            }
            ConstantValueVariant::Bool(value) => {
                ctx.instructions.ad(
                    &self.origin_location,
                    KPRI,
                    result_slot,
                    if *value { PRIM_TRUE } else { PRIM_FALSE },
                );
                true
            }
            ConstantValueVariant::Int(value) => {
                let value_le_bytes = value.to_signed_bytes_le();

                // If the value is in the i16 bounds, we can emit a simple
                // immediate loading instruction.
                if value <= &BigInt::from(i16::MAX) && value >= &BigInt::from(i16::MIN) {
                    let mut le_bytes = [0 as u8; 2];
                    for i in 0..le_bytes.len() {
                        le_bytes[i] = *value_le_bytes.get(i).unwrap_or(&0);
                    }
                    ctx.instructions.ad(
                        &self.origin_location,
                        KSHORT,
                        result_slot,
                        u16::from_le_bytes(le_bytes),
                    );
                }
                // Else, we have to create a new numeric constant and load it
                else if let Some(numeric_constant) = self.to_numeric_constant() {
                    let value_cst = ctx
                        .unit_data
                        .constants
                        .get_from_numeric_constant(numeric_constant);
                    ctx.instructions
                        .ad(&self.origin_location, KNUM, result_slot, value_cst);
                }
                // Else, we have to handle the number as a big integer
                else {
                    panic!("Big integers aren't handled for now");
                }

                true
            }
            ConstantValueVariant::String(value) => {
                let value_cst = ctx.unit_data.constants.get_from_string(value);
                ctx.instructions
                    .ad(&self.origin_location, KSTR, result_slot, value_cst);
                true
            }

            // All other constant values are handled by the "standard"
            // compilation function.
            _ => false,
        }
    }
}

// ----- Compilation helpers -----

/// Emit required instructions to raise a runtime error during the program
/// execution.
/// For now this function call the `error` Lua built-in with a simple text
/// message.
fn emit_runtime_error(
    ctx: &mut CompilationContext,
    maybe_error_location: Option<&SourceSection>,
    error_template: &ErrorTemplate,
    message_args: &Vec<DynamicErrorArg>,
) {
    // Create the runtime error instance object
    let runtime_error_instance =
        DynamicError { template_id: error_template.id, message_args: message_args.clone() };

    // Add constants in the current repository
    let message_cst = ctx
        .unit_data
        .constants
        .get_from_string(&runtime_error_instance.to_json_string());

    // Reserve temporary slots, fill them and call the function
    let call_slots = ctx.frame.borrow_mut().reserve_contiguous_slots(3);
    emit_global_read(ctx, maybe_error_location, call_slots.first, "error");
    ctx.instructions
        .ad_maybe_loc(maybe_error_location, KSTR, call_slots.last, message_cst);
    ctx.instructions
        .abc_maybe_loc(maybe_error_location, CALL, call_slots.first, 1, 2);

    // Free slots allocated for the call
    ctx.frame.borrow_mut().release_slots(call_slots);
}

/// Util function to get a table element by its index.
fn emit_table_index_read(
    ctx: &mut CompilationContext,
    origin_location: &SourceSection,
    result_slot: u8,
    table_slot: u8,
    index: usize,
) {
    _access_table_index(ctx, origin_location, result_slot, table_slot, index, TGETB, TGETV);
}

/// Util function to set a table element by its index.
fn emit_table_index_write(
    ctx: &mut CompilationContext,
    origin_location: &SourceSection,
    source_slot: u8,
    table_slot: u8,
    index: usize,
) {
    _access_table_index(ctx, origin_location, source_slot, table_slot, index, TSETB, TSETV);
}

/// Internal function to factorize table index accesses. Do not use directly.
fn _access_table_index(
    ctx: &mut CompilationContext,
    origin_location: &SourceSection,
    working_slot: u8,
    table_slot: u8,
    index: usize,
    immediate_access_op: u8,
    slot_access_op: u8,
) {
    if index <= u8::MAX as usize {
        ctx.instructions.abc(
            origin_location,
            immediate_access_op,
            working_slot,
            table_slot,
            index as u8,
        );
    } else if index <= i32::MAX as usize {
        let index_tmp = ctx.frame.borrow_mut().get_tmp();
        let index_cst = ctx.unit_data.constants.get_from_int(index as i32);
        ctx.instructions
            .ad(origin_location, KNUM, index_tmp, index_cst);
        ctx.instructions
            .abc(origin_location, slot_access_op, working_slot, table_slot, index_tmp);
        ctx.frame.borrow_mut().release_slot(index_tmp);
    } else {
        panic!("Big integers aren't handled for now")
    }
}

/// Util function to load a table member (designated by a string constant).
/// This function is used to factorize [`TGETS`] emission and optimization.
fn emit_table_member_read(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    result_slot: u8,
    table_slot: u8,
    member_name: &str,
) {
    _access_table_field(
        ctx,
        maybe_origin_location,
        result_slot,
        table_slot,
        member_name,
        TGETS,
        TGETV,
    );
}

/// Util function to set a table member (designated by a string constant). This
/// function is used to factorize [`TSETS`] emission and optimization.
fn emit_table_member_write(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    source_slot: u8,
    table_slot: u8,
    member_name: &str,
) {
    _access_table_field(
        ctx,
        maybe_origin_location,
        source_slot,
        table_slot,
        member_name,
        TSETS,
        TSETV,
    );
}

/// Internal function to factorize table field accesses. Do not use directly.
fn _access_table_field(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    working_slot: u8,
    table_slot: u8,
    member_name: &str,
    immediate_access_op: u8,
    slot_access_op: u8,
) {
    let member_name_cst = ctx.unit_data.constants.get_from_string(member_name);
    if member_name_cst <= u8::MAX as u16 {
        ctx.instructions.abc_maybe_loc(
            maybe_origin_location,
            immediate_access_op,
            working_slot,
            table_slot,
            member_name_cst as u8,
        );
    } else {
        let field_tmp = ctx.frame.borrow_mut().get_tmp();
        ctx.instructions
            .ad_maybe_loc(maybe_origin_location, KSTR, field_tmp, member_name_cst);
        ctx.instructions.abc_maybe_loc(
            maybe_origin_location,
            slot_access_op,
            working_slot,
            table_slot,
            field_tmp,
        );
        ctx.frame.borrow_mut().release_slot(field_tmp);
    }
}

/// Util function used to generate an instruction to create a new table and
/// store it in the provided slot.
fn emit_new_table(
    ctx: &mut CompilationContext,
    origin_location: &SourceSection,
    result_slot: u8,
    array_size: u16,
    hash_size: u32,
) {
    // Ensure the required array size is lower than 2^11
    assert!(array_size <= 2 ^ 11, "Maximum \"array_size\" exceeded: {array_size}");

    // Then get the real hash size
    let real_hash_size = (hash_size as f64).log2().ceil() as u16;

    // Compute the "D" operand for the new instruction
    let d = (real_hash_size << 11) | array_size;

    // Then emit the instruction
    ctx.instructions.ad(origin_location, TNEW, result_slot, d);
}

/// Util function used to generate a global table reading.
fn emit_global_read(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    result_slot: u8,
    global_name: &str,
) {
    let global_name_cst = ctx.unit_data.constants.get_from_string(global_name);
    ctx.instructions
        .ad_maybe_loc(maybe_origin_location, GGET, result_slot, global_name_cst);
}

/// Emit instructions to set the meta-table of the type designated by the
/// provided name as the one of the table at the provided slot.
fn emit_set_metatable(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    table_slot: u8,
    type_implementation: &TypeImplementation,
) {
    // Get slots for the call
    let call_slots = ctx.frame.borrow_mut().reserve_contiguous_slots(4);

    // Get the "setmetatable", fill arguments and make a call
    emit_global_read(ctx, maybe_origin_location, call_slots.first, "setmetatable");
    ctx.instructions.ad_maybe_loc(
        maybe_origin_location,
        MOV,
        call_slots.last - 1,
        table_slot as u16,
    );
    emit_global_read(
        ctx,
        maybe_origin_location,
        call_slots.last,
        &type_implementation.global_field_name(),
    );
    ctx.instructions
        .abc_maybe_loc(maybe_origin_location, CALL, call_slots.first, 1, 3);

    // Release call slot
    ctx.frame.borrow_mut().release_slots(call_slots);
}

/// Emit instructions to check that the type of the value stored in the
/// `actual_value` slot is corresponding to `expected_type`. If types don't
/// match a runtime error is raised.
fn emit_type_check(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    actual_value: u8,
    expected_type: &BuiltinType,
) {
    // First create a label for the instruction next to what this function is
    // emitting.
    let next_label = ctx.instructions.new_label();

    // First get the type tag of the actual value
    let actual_tag = ctx.frame.borrow_mut().get_tmp();
    emit_table_member_read(ctx, maybe_origin_location, actual_tag, actual_value, TYPE_TAG_FIELD);

    // Then compare type tags and if they're not equals, emit a
    // runtime error.
    ctx.instructions.ad_maybe_loc(
        maybe_origin_location,
        ISEQN,
        actual_tag,
        ctx.unit_data
            .constants
            .get_from_int(expected_type.tag as i32),
    );
    ctx.goto(next_label);
    ctx.frame.borrow_mut().release_slot(actual_tag);

    // Now emit the code to raise a runtime error in the case where type tags
    // don't match.
    let actual_name = ctx.frame.borrow_mut().get_tmp();
    emit_table_member_read(ctx, maybe_origin_location, actual_name, actual_value, TYPE_NAME_FIELD);
    emit_runtime_error(
        ctx,
        maybe_origin_location,
        &WRONG_TYPE,
        &vec![
            DynamicErrorArg::Static(String::from(expected_type.display_name())),
            DynamicErrorArg::LocalValue(actual_name),
        ],
    );
    ctx.frame.borrow_mut().release_slot(actual_name);

    // Finally label the next instruction
    ctx.instructions.label(next_label);
}

/// Emit instructions to check that the value stored in the `value_slot` slot
/// is implementing the required built-in trait. If not, a runtime error is
/// raised.
fn emit_trait_check(
    ctx: &mut CompilationContext,
    maybe_origin_location: Option<&SourceSection>,
    value_slot: u8,
    required_trait: &BuiltinTrait,
) {
    // Create the label for the next instruction
    let next_label = ctx.instructions.new_label();

    // Then get the field corresponding to the trait name in the traits table
    let trait_res = ctx.frame.borrow_mut().get_tmp();
    emit_table_member_read(
        ctx,
        maybe_origin_location,
        trait_res,
        value_slot,
        &required_trait.runtime_field(),
    );
    ctx.instructions
        .ad_maybe_loc(maybe_origin_location, IST, 0, trait_res as u16);
    ctx.goto(next_label);
    ctx.frame.borrow_mut().release_slot(trait_res);

    // Emit instructions to raise a runtime error
    let type_name = ctx.frame.borrow_mut().get_tmp();
    emit_table_member_read(ctx, maybe_origin_location, type_name, value_slot, TYPE_NAME_FIELD);
    emit_runtime_error(
        ctx,
        maybe_origin_location,
        &MISSING_TRAIT,
        &vec![
            DynamicErrorArg::Static(String::from(required_trait.name)),
            DynamicErrorArg::LocalValue(type_name),
        ],
    );

    // Finally label the next instructions
    ctx.instructions.label(next_label);
}

/// Emit, if required, the instruction to close local values in the current
/// frame.
fn emit_closing_instruction(ctx: &mut CompilationContext) {
    if let Some(close_from) = ctx.frame.borrow().get_slot_to_close_from() {
        ctx.instructions.ad_no_loc(UCLO, close_from, JUMP_BIASING);
    }
}

// ----- Compilation support -----

/// This type is used to represents the result of an access to a value, it
/// carries the information whether the value is directly accessed through a
/// slot read, or through a temporary value.
enum ValueAccess {
    Direct(u8),
    OwnedTmp(u8),
    BorrowedTmp(u8),
}

impl ValueAccess {
    fn slot(&self) -> u8 {
        match self {
            ValueAccess::Direct(s) | ValueAccess::OwnedTmp(s) | ValueAccess::BorrowedTmp(s) => *s,
        }
    }

    fn release(self, ctx: &mut CompilationContext) {
        match self {
            ValueAccess::OwnedTmp(s) => ctx.frame.borrow_mut().release_slot(s),
            ValueAccess::Direct(_) | ValueAccess::BorrowedTmp(_) => (),
        }
    }
}

/// This type is the main data holder during the compilation process.
struct CompilationContext<'a> {
    /// Built-in symbols that are available during the compilation.
    builtins: HashSet<&'static str>,

    /// The frame that is currently being used in the compilation process. This
    /// stores all information about local symbols, up-values and temporary
    /// slots.
    frame: Rc<RefCell<Frame>>,

    /// Execution unit that is currently being compiled.
    unit: &'a ExecutionUnit,

    /// Data collected for the current compilation unit.
    unit_data: ExecUnitCompilationData,

    /// This is the main result of the compilation process, it is filled during
    /// the compilation of execution units (see [`ExecutionUnit::compile`]).
    prototypes: Vec<Prototype>,

    /// Data required for the runtime and collected during the compilation.
    runtime_data: RuntimeData,

    /// Buffer containing all instructions, result of the compilation process.
    instructions: ExtendedInstructionBuffer,

    /// A collection of diagnostics collected during the compilation process,
    /// if it is not empty, it means that the result of the compilation may be
    /// invalid.
    diagnostics: Vec<Report>,
}

impl<'a> CompilationContext<'a> {
    fn new<'b>(unit: &'b ExecutionUnit) -> Self
    where
        'b: 'a,
    {
        Self {
            builtins: get_builtin_bindings().iter().map(|b| b.name).collect(),
            frame: Rc::new(RefCell::new(Frame::new(None))),
            unit,
            unit_data: ExecUnitCompilationData::new(),
            prototypes: Vec::new(),
            runtime_data: RuntimeData::new(),
            instructions: ExtendedInstructionBuffer::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Add a new `goto` instruction in the instruction buffer with information
    /// about the current frame in the context.
    fn goto(&mut self, label: Label) {
        let next_available_slot = self.frame.borrow_mut().peek_next_slot();
        self.instructions.goto(label, next_available_slot);
    }

    /// Util function to add a collection of symbols in the current frame as local
    /// bindings. This function also checks duplicated declarations and add
    /// corresponding diagnostics in the compilation context.
    fn declare_locals(&mut self, symbols: &Vec<Identifier>) {
        for symbol in symbols {
            let maybe_local_slot = self.frame.borrow().is_conflicting(&symbol.text);
            if let Some(previous_binding) = maybe_local_slot {
                self.diagnostics
                    .push(Report::from_error_template_with_hints(
                        &symbol.origin_location,
                        &DUPLICATED_SYMBOL,
                        &vec![&symbol.text],
                        vec![Hint::new(
                            PREVIOUS_SYMBOL_HINT,
                            &previous_binding.declaration_location,
                        )],
                    ));
            } else {
                self.frame
                    .borrow_mut()
                    .bind_local(&symbol.text, &symbol.origin_location);
            }
        }
    }

    /// Util function to release all local bindings of the current frame and
    /// store remaining data in the current [`ExecUnitCompilationData`]
    /// instance.
    fn release_locals(&mut self, death_label: Label) {
        let old_bindings = self.frame.borrow_mut().bindings.drain().collect::<Vec<_>>();
        for (name, mut old_binding) in old_bindings {
            old_binding.death_label = death_label;
            self.unit_data.dead_bindings.insert(name, old_binding);
        }
    }
}

/// This type contains all data required to compile an [`ExecutionUnit`] object
/// into a bytecode [`Prototype`].
struct ExecUnitCompilationData {
    has_child: bool,
    constants: ConstantRepository,
    dead_bindings: HashMap<String, BindingData>,
}

impl ExecUnitCompilationData {
    fn new() -> Self {
        Self {
            has_child: false,
            constants: ConstantRepository::new(),
            dead_bindings: HashMap::new(),
        }
    }
}

/// This type stores all runtime constants, it is meant to be store in a
/// [`ExecUnitCompilationData`] instance.
struct ConstantRepository {
    complex_constants: Vec<ComplexConstant>,
    numeric_constants: Vec<NumericConstant>,
}

impl ConstantRepository {
    // --- Creation

    fn new() -> Self {
        Self { complex_constants: Vec::new(), numeric_constants: Vec::new() }
    }

    // --- Constant access

    /// Create a new [`ComplexConstant::Child`] and add it to the constant
    /// repository, returning its index.
    fn get_child(&mut self) -> u16 {
        Self::add_constant(ComplexConstant::Child, &mut self.complex_constants)
    }

    /// Get the index in [`Self::numeric_constants`] corresponding to the
    /// provided integer value, adding it as a constant if it is not already
    /// in it.
    fn get_from_int(&mut self, value: i32) -> u16 {
        self.get_from_numeric_constant(NumericConstant::Integer(value))
    }

    /// Get the index in [`Self::complex_constants`] corresponding to the
    /// provided string value, adding it as a constant if it is not already
    /// in it.
    fn get_from_string(&mut self, value: &str) -> u16 {
        self.get_from_complex_constant(ComplexConstant::String(String::from(value)))
    }

    /// More generic function to get the index of a complex constant.
    fn get_from_complex_constant(&mut self, constant: ComplexConstant) -> u16 {
        match constant {
            ComplexConstant::Table { .. } => {
                Self::add_constant(constant, &mut self.complex_constants)
            }
            _ => Self::get_or_add_constant(constant, &mut self.complex_constants),
        }
    }

    /// More generic function to get the index of a numeric constant.
    fn get_from_numeric_constant(&mut self, constant: NumericConstant) -> u16 {
        Self::get_or_add_constant(constant, &mut self.numeric_constants)
    }

    /// Util function to get the index of the provided numeric constant as an
    /// [`u8`] value if possible.
    fn try_from_numeric_constant_as_u8(&mut self, constant: NumericConstant) -> Option<u8> {
        Self::try_adding_constant_as_u8(constant, &mut self.numeric_constants)
    }

    /// Util internal function to add a constant in a vector of constant if
    /// this one doesn't already contain an equivalent one.
    /// The function return the index of the constant in the vector.
    /// This function panics if the number of constant is too high (over
    /// [`u16::MAX`]).
    fn get_or_add_constant<T: PartialEq>(constant: T, constant_vector: &mut Vec<T>) -> u16 {
        if let Some(index) = constant_vector.iter().position(|e| e == &constant) {
            index as u16
        } else {
            Self::add_constant(constant, constant_vector)
        }
    }

    /// Util function to add a constant value in the provided vector
    /// unconditionally, assert that the new vector size doesn't exceed the
    /// [`u16::MAX`] limit, and return the recently added constant index.
    fn add_constant<T>(constant: T, constant_vector: &mut Vec<T>) -> u16 {
        constant_vector.push(constant);
        assert!(constant_vector.len() <= u16::MAX as usize, "Too many constants");
        (constant_vector.len() - 1) as u16
    }

    /// Util internal function to try adding a constant element in the provided
    /// vector if it is not already in it. There are several possibilities:
    ///   - If the constant is not in the vector and its size is lower than
    ///     [`u8::MAX`], then the element is added in the vector and the
    ///     function returns this new index.
    ///   - If the constant is already in the vector and its index is lower
    ///     than [`u8::MAX`], then this index is returned.
    ///   - In all other cases, the vector isn't modified and [`None`] is
    ///     returned.
    fn try_adding_constant_as_u8<T: PartialEq>(
        constant: T,
        constant_vector: &mut Vec<T>,
    ) -> Option<u8> {
        if let Some(index) = constant_vector.iter().position(|e| e == &constant) {
            u8::try_from(index).ok()
        } else {
            if constant_vector.len() < u8::MAX as usize {
                constant_vector.push(constant);
                Some((constant_vector.len() - 1) as u8)
            } else {
                None
            }
        }
    }
}
