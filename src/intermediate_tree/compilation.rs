//! # Intermediate tree compilation module
//!
//! This module contains all required operations to compile any intermediate
//! tree into a [`crate::bytecode::BytecodeBuffer`].

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    rc::Rc,
    u8, usize,
};

use num_bigint::BigInt;

use crate::{
    bytecode::{
        self, BytecodeBuffer, ComplexConstant, JUMP_BIASING, NumericConstant, PRIM_FALSE, PRIM_NIL,
        PRIM_TRUE, Prototype, TableConstantElement, VariableData,
        extended_bytecode::{
            ExtendedInstruction, ExtendedInstructionBuffer, ExtendedInstructionVariant, Label,
        },
        op_codes::*,
    },
    errors::{
        DUPLICATED_SYMBOL, ErrorTemplate, INDEX_OUT_OF_BOUNDS, NO_VALUE_FOR_PARAM,
        POS_AND_NAMED_VALUE_FOR_PARAM, PREVIOUS_SYMBOL_HINT, UNKNOWN_MEMBER, UNKNOWN_SYMBOL,
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
    runtime::{
        DynamicError, DynamicErrorArg, RuntimeData,
        builtins::{
            UNIT_VALUE_NAME, get_builtin_bindings,
            types::{self, metatable_global_field},
        },
    },
    sources::{SourceRepository, SourceSection},
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
    ) -> Result<(BytecodeBuffer, RuntimeData), Report> {
        // Open the initial compilation context and create the prototypes vector
        let mut compile_context = CompilationContext::new();

        // Compile the current execution unit
        self.internal_compile(&mut compile_context);

        // Then return the success result
        if compile_context.diagnostics.is_empty() {
            Ok((
                BytecodeBuffer {
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
    fn open_frame_and_compile(&self, ctx: &mut CompilationContext) {
        // Open a new semantic frame for this function
        let parent_frame = ctx.current_frame.clone();
        let current_frame = Rc::new(RefCell::new(Frame::new(Some(parent_frame.clone()))));
        ctx.current_frame = current_frame;

        // Compile the execution unit
        self.internal_compile(ctx);

        // Restore the parent frame
        ctx.current_frame = parent_frame;
        ctx.current_data().has_child = true;
    }

    /// Compile the execution unit and all its children (direct and indirect
    /// ones), and place the result in the provided  `output` buffer.
    fn internal_compile(&self, ctx: &mut CompilationContext) {
        // Start by adding a new compilation data in the context
        let new_id = ctx
            .exec_unit_data_stack
            .last()
            .map_or(self.name.clone(), |d| self.id(&d.identifier));
        ctx.exec_unit_data_stack
            .push(ExecUnitCompilationData::new(new_id));

        // Create compilation working values
        let mut extended_instructions = ExtendedInstructionBuffer::new();
        let mut arg_count = 0;
        let mut is_variadic = false;

        // Compile the execution unit
        match &self.variant {
            ExecutionUnitVariant::Module { symbols, elements, .. } => {
                // Set the emitted prototype as variadic
                is_variadic = true;

                // Add module symbols to the current frame
                declare_locals(ctx, symbols);

                // Compile module elements
                for elem in elements {
                    let elem_access = elem.compile_as_access(ctx, self, &mut extended_instructions);
                    elem_access.release(ctx);
                }

                // Create a source section at the end of the module
                let end_source_section = SourceSection {
                    source: self.origin_location.source.clone(),
                    start: self.origin_location.end,
                    end: self.origin_location.end,
                };

                // Reserve a slot for the module table
                let result_tmp = ctx.current_frame_mut().get_tmp();

                // Emit instructions to create the module table
                let empty_table_cst = ctx.current_data().constants.get_empty_table();
                extended_instructions.ad(&end_source_section, TDUP, result_tmp, empty_table_cst);
                for symbol in symbols {
                    let local_slot = ctx.current_frame().get_local(&symbol.text).unwrap();
                    emit_table_member_write(
                        ctx,
                        &mut extended_instructions,
                        Some(&end_source_section),
                        local_slot.slot,
                        result_tmp,
                        &symbol.text,
                    );
                }

                // Emit local values closing
                emit_closing_instruction(&ctx.current_frame(), &mut extended_instructions);

                // Emit returning of the module table
                extended_instructions.ad(&end_source_section, RET1, result_tmp, 2);
                ctx.current_frame_mut().release_slot(result_tmp);
            }
            ExecutionUnitVariant::Function { params, body } => {
                // Set the argument count
                arg_count = params.len() + 1;

                // Reserve the first slot of the frame, by convention, this
                // slot is used to provide named arguments.
                let named_args_slot = ctx.current_frame_mut().get_tmp();

                // Get the function parameters identifiers
                let param_identifiers = params.iter().map(|(s, _)| s.clone()).collect::<Vec<_>>();

                // Add function parameters to the current frame
                declare_locals(ctx, &param_identifiers);

                // Emit instructions to ensure all parameters have a valid and
                // unique value.
                for (param_id, maybe_default_value) in params {
                    let param_slot = ctx.current_frame().get_local(&param_id.text).unwrap();

                    // Create working labels
                    let no_value_label = extended_instructions.new_label();
                    let test_both_label = extended_instructions.new_label();
                    let next_label = extended_instructions.new_label();

                    // Test if the parameter has a positional value
                    extended_instructions.ad_no_loc(ISNEP, param_slot.slot, PRIM_NIL);
                    extended_instructions.cgoto(ctx, test_both_label);

                    // If there is no positional value, start by checking if
                    // there is a named value for the parameter.
                    extended_instructions.ad_no_loc(ISEQP, named_args_slot, PRIM_NIL);
                    extended_instructions.cgoto(ctx, no_value_label);
                    emit_table_member_read(
                        ctx,
                        &mut extended_instructions,
                        None,
                        param_slot.slot,
                        named_args_slot,
                        &param_id.text,
                    );
                    extended_instructions.ad_no_loc(ISNEP, param_slot.slot, PRIM_NIL);
                    extended_instructions.cgoto(ctx, next_label);

                    // If parameter has no value, emit an error
                    extended_instructions.label(no_value_label);
                    if let Some(default_value) = maybe_default_value {
                        default_value.compile_as_value(
                            ctx,
                            self,
                            &mut extended_instructions,
                            param_slot.slot,
                        );
                    } else {
                        emit_runtime_error(
                            ctx,
                            &mut extended_instructions,
                            None,
                            &NO_VALUE_FOR_PARAM,
                            &vec![DynamicErrorArg::Static(param_id.text.clone())],
                        );
                    }

                    // Test if the parameter have both positional and named
                    // values
                    extended_instructions.label(test_both_label);
                    extended_instructions.ad_no_loc(ISEQP, named_args_slot, PRIM_NIL);
                    extended_instructions.cgoto(ctx, next_label);
                    emit_table_member_read(
                        ctx,
                        &mut extended_instructions,
                        None,
                        param_slot.slot,
                        named_args_slot,
                        &param_id.text,
                    );
                    extended_instructions.ad_no_loc(ISEQP, param_slot.slot, PRIM_NIL);
                    extended_instructions.cgoto(ctx, next_label);
                    emit_runtime_error(
                        ctx,
                        &mut extended_instructions,
                        None,
                        &POS_AND_NAMED_VALUE_FOR_PARAM,
                        &vec![DynamicErrorArg::Static(param_id.text.clone())],
                    );

                    // Label the next instruction
                    extended_instructions.label(next_label);

                    // Finally, set the parameter slot as initialized
                    ctx.current_frame_mut()
                        .init_local(&param_id.text, next_label);
                }

                // The compile the body as a returning node
                body.compile_as_return(ctx, self, &mut extended_instructions);
            }
        };

        // Release locals of the execution unit
        let death_label = extended_instructions.new_label();
        ctx.release_locals(death_label);
        extended_instructions.label(death_label);

        // Emit additional instructions to initialize closed bindings
        emit_closed_bindings_init(&ctx.current_frame(), &mut extended_instructions);

        // Perform post compilation assertions
        assert!(arg_count <= u8::MAX as usize, "Too many arguments for prototype");

        // Get the data collected during the compilation process by moving the
        // memory to avoid useless copy.
        let data = ctx.exec_unit_data_stack.pop().unwrap();

        // Now we collect variable information to create debug data
        let label_map = extended_instructions.label_map();
        let mut variable_data = data
            .dead_bindings
            .iter()
            .map(|(n, b)| VariableData {
                name: b.debug_name.as_ref().unwrap_or(n).clone(),
                birth_instruction: *label_map.get(&b.birth_label).expect("Unknown label"),
                death_instruction: *label_map.get(&b.death_label).expect("Unknown label"),
            })
            .collect::<Vec<_>>();

        // We sort variable data by birth label
        variable_data.sort_by(|lvd, rvd| lvd.birth_instruction.cmp(&rvd.birth_instruction));

        // Now we get instructions and their locations
        let (instructions, instruction_locations) =
            extended_instructions.as_instructions_and_locations(&self.origin_location);

        // Create the bytecode prototype and add it to the current context
        // result.
        match &ctx.current_frame.borrow().variant {
            FrameVariant::Semantic { maximum_size, up_values, .. } => {
                // Sort up-values from their index
                let mut sorted_up_values = up_values
                    .into_iter()
                    .map(|(n, uv)| (uv.debug_name.as_ref().unwrap_or(n), uv))
                    .collect::<Vec<_>>();
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

        // Finally, collect runtime data for the current prototype and add it
        // to the current context.
        ctx.runtime_data.add_prototype_data(
            self.origin_location.source,
            data.identifier,
            instruction_locations,
        );
    }
}

impl Node {
    /// Compile the node as a value creating unit, meaning that the result of
    /// this node execution is placed into the `target_slot` during the
    /// execution.
    /// Instructions produced by this compilation are placed in the `output`
    /// vector.
    /// The `owning_unit` is the execution unit that this node belongs to.
    fn compile_as_value(
        &self,
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        result_slot: u8,
    ) {
        // If the node can be evaluated as a constant value, emit instruction
        // for it and return.
        if let Some(constant) = self.eval_as_constant() {
            constant.compile(ctx, output, result_slot);
            return;
        }

        // If we reach here the node can't be evaluated as a constant, so we
        // have to compile it manually
        match &self.variant {
            // --- Function call
            NodeVariant::FunCall { callee, positional_args, named_args } => {
                let call_slots = Self::compile_call(
                    ctx,
                    owning_unit,
                    output,
                    self,
                    callee,
                    positional_args,
                    named_args,
                );
                output.ad(&self.origin_location, MOV, result_slot, call_slots.first as u16);
                ctx.current_frame_mut().release_slots(call_slots);
            }

            // --- Composite expressions
            NodeVariant::DottedExpr { prefix, suffix, is_safe } => {
                // Get the access to the prefix expression
                let prefix_access = prefix.compile_as_access(ctx, owning_unit, output);

                // Get the table member corresponding to the suffix in the
                // result slot
                emit_table_member_read(
                    ctx,
                    output,
                    Some(&self.origin_location),
                    result_slot,
                    prefix_access.slot(),
                    &suffix.text,
                );

                // Emit post access check
                let next_label = output.new_label();
                output.ad(&self.origin_location, ISNEP, result_slot, PRIM_NIL);
                output.cgoto(ctx, next_label);
                if *is_safe {
                    emit_global_read(
                        ctx,
                        output,
                        Some(&self.origin_location),
                        result_slot,
                        UNIT_VALUE_NAME,
                    );
                } else {
                    emit_runtime_error(
                        ctx,
                        output,
                        Some(&suffix.origin_location),
                        &UNKNOWN_MEMBER,
                        &vec![DynamicErrorArg::Static(suffix.text.clone())],
                    );
                }

                // Label the next instruction
                output.label(next_label);
            }
            NodeVariant::IndexExpr { indexed_val, index, is_safe } => {
                // Get the access to the indexed value and to the index
                let indexed_value_access = indexed_val.compile_as_access(ctx, owning_unit, output);
                let index_access = index.compile_as_access(ctx, owning_unit, output);

                // Emit code to the the value at the specified index
                output.abc(
                    &self.origin_location,
                    TGETV,
                    result_slot,
                    indexed_value_access.slot(),
                    index_access.slot(),
                );

                // Emit post access check
                let next_label = output.new_label();
                output.ad(&self.origin_location, ISNEP, result_slot, PRIM_NIL);
                output.cgoto(ctx, next_label);
                if *is_safe {
                    emit_global_read(
                        ctx,
                        output,
                        Some(&self.origin_location),
                        result_slot,
                        UNIT_VALUE_NAME,
                    );
                } else {
                    emit_runtime_error(
                        ctx,
                        output,
                        Some(&index.origin_location),
                        &INDEX_OUT_OF_BOUNDS,
                        &vec![DynamicErrorArg::LocalValue(index_access.slot())],
                    );
                }

                // Label the next instruction
                output.label(next_label);
            }
            NodeVariant::InClause { .. } => todo!(),
            NodeVariant::IfExpr { condition, consequence, alternative } => {
                // Create required labels
                let alternative_label = output.new_label();
                let next_label = output.new_label();

                // Compile the condition as a branching node
                condition.compile_as_branching(ctx, owning_unit, output, alternative_label);

                // Then, compile the consequence, that is going to be executed
                // if the condition is true
                consequence.compile_as_value(ctx, owning_unit, output, result_slot);
                output.cgoto(ctx, next_label);

                // Finally, compile the alternative with the appropriate label
                output.label(alternative_label);
                alternative.compile_as_value(ctx, owning_unit, output, result_slot);
                output.label(next_label);
            }
            NodeVariant::BlockExpr { local_symbols, body, val } => {
                // Compile the block body
                let parent_frame =
                    Self::compile_block_body(ctx, owning_unit, output, local_symbols, body);

                // Then compile the expression representing the value of the
                // block.
                val.compile_as_value(ctx, owning_unit, output, result_slot);

                // Release the block local bindings
                let death_label = output.new_label();
                ctx.release_locals(death_label);
                output.label(death_label);

                // Finally, restore the previous frame as the current one
                ctx.current_frame = parent_frame;
            }

            // --- Lazy sequence creation
            NodeVariant::LazySeqExpr { .. } => todo!(),

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
                        owning_unit,
                        output,
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
                        owning_unit,
                        output,
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
                    let left_access = left.compile_as_access(ctx, owning_unit, output);
                    let right_access = right.compile_as_access(ctx, owning_unit, output);
                    let op = match operator.variant {
                        ArithOperatorVariant::Plus => ADDVV,
                        ArithOperatorVariant::Minus => SUBVV,
                        ArithOperatorVariant::Multiply => MULVV,
                        ArithOperatorVariant::Divide => DIVVV,
                    };
                    output.abc(
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
                let if_false_label = output.new_label();
                let next_label = output.new_label();

                // Compile the current node as branching to emit short
                // circuiting instructions
                self.compile_as_branching(ctx, owning_unit, output, if_false_label);

                // Emit the code to set the result to "true"
                output.ad(&self.origin_location, KPRI, result_slot, PRIM_TRUE);
                output.cgoto(ctx, next_label);

                // Emit the code to set the result to "false"
                output.label(if_false_label);
                output.ad(&self.origin_location, KPRI, result_slot, PRIM_FALSE);

                // Label the next instruction as such
                output.label(next_label);
            }
            NodeVariant::MiscBinOp { left, operator, right } => {
                let (left_access, right_access) = match operator.variant {
                    // The concatenation operation requires that values are
                    // stored in contiguous slots.
                    MiscOperatorVariant::Concat => {
                        let operand_slots = ctx.current_frame_mut().reserve_contiguous_slots(2);
                        left.compile_as_value(ctx, owning_unit, output, operand_slots.first);
                        right.compile_as_value(ctx, owning_unit, output, operand_slots.last);
                        (
                            ValueAccess::Tmp(operand_slots.first),
                            ValueAccess::Tmp(operand_slots.last),
                        )
                    }
                };
                let op = match operator.variant {
                    MiscOperatorVariant::Concat => CAT,
                };
                output.abc(
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
                let operand_access = operand.compile_as_access(ctx, owning_unit, output);
                let op = match operator.variant {
                    ArithOperatorVariant::Plus => MOV,
                    ArithOperatorVariant::Minus => UNM,
                    _ => unreachable!(),
                };
                output.ad(&self.origin_location, op, result_slot, operand_access.slot() as u16);
                operand_access.release(ctx);
            }
            NodeVariant::LogicUnOp { operator, operand } => {
                let operand_access = operand.compile_as_access(ctx, owning_unit, output);
                let op = match operator.variant {
                    LogicOperatorVariant::Not => NOT,
                    _ => unreachable!(),
                };
                output.ad(&self.origin_location, op, result_slot, operand_access.slot() as u16);
                operand_access.release(ctx);
            }

            // --- Symbol accesses
            NodeVariant::InitLocal { symbol, val } => {
                // Create the birth label of the local variable
                let birth_label = output.new_label();

                // Then compile the initialization value and place it into the
                // reserved slot.
                let binding_slot = ctx.current_frame().get_local(&symbol.text).unwrap();
                val.compile_as_value(ctx, owning_unit, output, binding_slot.slot);

                // Then label the next instruction as the birthing one and flag
                // the slot as initialized in the current frame.
                output.label(birth_label);
                ctx.current_frame_mut()
                    .init_local(&symbol.text, birth_label);
            }
            NodeVariant::InitLocalFun { symbol, child_index } => {
                Self::compile_child_unit(
                    ctx,
                    owning_unit,
                    output,
                    &self.origin_location,
                    &symbol.text,
                    *child_index as usize,
                );
            }
            NodeVariant::ReadSymbol(identifier) => {
                // First try getting the symbol in the local frame
                let maybe_local_binding = ctx.current_frame().get_local(&identifier.text);
                if let Some(BindingData { slot, is_init: true, .. }) = maybe_local_binding {
                    output.ad(&self.origin_location, MOV, result_slot, slot as u16);
                } else {
                    // Then try to look in the up-values
                    let maybe_up_value = ctx.current_frame_mut().get_up_value(&identifier.text);
                    if let Some(up_value) = maybe_up_value {
                        output.ad(&self.origin_location, UGET, result_slot, up_value.index as u16);
                        if !up_value.is_safe {
                            let next_label = output.new_label();
                            output.ad(&self.origin_location, ISNEP, result_slot, PRIM_NIL);
                            output.cgoto(ctx, next_label);
                            emit_runtime_error(
                                ctx,
                                output,
                                Some(&identifier.origin_location),
                                &UNKNOWN_SYMBOL,
                                &vec![DynamicErrorArg::Static(identifier.text.clone())],
                            );
                            output.label(next_label);
                        }
                    } else {
                        // Finally, if a built-in is named like the accessed
                        // symbol, get it in the global table.
                        if ctx.builtins.contains(identifier.text.as_str()) {
                            emit_global_read(
                                ctx,
                                output,
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
                let lambda_name = &owning_unit.children_units[*child_index as usize].name;
                ctx.current_frame_mut()
                    .bind_local(lambda_name, &self.origin_location);

                // Compile the child unit
                Self::compile_child_unit(
                    ctx,
                    owning_unit,
                    output,
                    &self.origin_location,
                    lambda_name,
                    *child_index as usize,
                );

                // Finally move the lambda value in the result slot
                let lambda_binding = ctx.current_frame().get_local(lambda_name).unwrap();
                output.ad(&self.origin_location, MOV, result_slot, lambda_binding.slot as u16);
            }

            // --- Non-trivial literals
            NodeVariant::TupleLiteral(elements) | NodeVariant::ListLiteral(elements) => {
                // Compile nodes inside the tuple literals and place them in a table
                Self::compile_table(
                    ctx,
                    owning_unit,
                    output,
                    result_slot,
                    &self.origin_location,
                    elements,
                    &Vec::new(),
                );

                // Then set the meta-table of this new table
                emit_set_metatable(
                    ctx,
                    output,
                    Some(&self.origin_location),
                    result_slot,
                    types::tuple::NAME,
                );
            }
            NodeVariant::ObjectLiteral(items) => {
                Self::compile_table(
                    ctx,
                    owning_unit,
                    output,
                    result_slot,
                    &self.origin_location,
                    &Vec::new(),
                    items,
                );

                // TODO: Set the object meta-table accordingly to its type
            }

            // --- All trivial literal nodes should've been compiled by now
            _ => unreachable!(),
        }

        /// Try compiling the provided arithmetic operation in a constant
        /// optimized way, returning whether it is successful.
        fn compile_arith_bin_op_with_constant_operand(
            ctx: &mut CompilationContext,
            owning_unit: &ExecutionUnit,
            output: &mut ExtendedInstructionBuffer,
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
                .current_data()
                .constants
                .try_from_numeric_constant_as_u8(constant_operand)
            {
                // Emit the code the perform the arithmetic operation with the
                // constant operand.
                let operand_access = variable_operand.compile_as_access(ctx, owning_unit, output);
                output.abc(
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

    /// Compile the node as a value, but instead of placing it in an already
    /// reserved slot like the [`Self::compile_as_value`] does, this function
    /// return a value representing an access to the node result.
    /// This is used to optimize accesses to already stored values like local
    /// bindings and avoid useless copy.
    fn compile_as_access(
        &self,
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
    ) -> ValueAccess {
        fn fallback(
            ctx: &mut CompilationContext,
            owning_unit: &ExecutionUnit,
            output: &mut ExtendedInstructionBuffer,
            node: &Node,
        ) -> ValueAccess {
            let result_tmp = ctx.current_frame_mut().get_tmp();
            node.compile_as_value(ctx, owning_unit, output, result_tmp);
            ValueAccess::Tmp(result_tmp)
        }

        match &self.variant {
            NodeVariant::FunCall { callee, positional_args, named_args } => {
                let call_slots = Self::compile_call(
                    ctx,
                    owning_unit,
                    output,
                    self,
                    callee,
                    positional_args,
                    named_args,
                );
                ctx.current_frame_mut().release_slots(SlotRange {
                    first: call_slots.first + 1,
                    last: call_slots.last,
                });
                ValueAccess::Tmp(call_slots.first)
            }

            NodeVariant::ReadSymbol(identifier) => {
                let maybe_local_binding = ctx.current_frame().get_local(&identifier.text);
                if let Some(BindingData { slot, is_init: true, .. }) = maybe_local_binding {
                    ValueAccess::Direct(slot)
                } else {
                    fallback(ctx, owning_unit, output, self)
                }
            }
            _ => fallback(ctx, owning_unit, output, self),
        }
    }

    /// Compile the node as a branching one. It means that the node value is
    /// going to be tested at runtime, and if it is falsy, execution is going
    /// to jump to the provided `if_false_label` label.
    /// This function also optimize boolean evaluation of logic and equality
    /// operations.
    fn compile_as_branching(
        &self,
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        if_false_label: Label,
    ) {
        // Use the `internal_compile` function to compile the node as a
        // branching one.
        let if_true_label = output.new_label();
        internal_compile(
            self,
            ctx,
            owning_unit,
            output,
            if_true_label,
            if_false_label,
            BranchingKind::IfFalse,
        );
        output.label(if_true_label);

        /// Internal branching compilation function, this is used to hide
        /// branching optimization to other compiler parts.
        fn internal_compile(
            node: &Node,
            ctx: &mut CompilationContext,
            owning_unit: &ExecutionUnit,
            output: &mut ExtendedInstructionBuffer,
            if_true_label: Label,
            if_false_label: Label,
            branching_kind: BranchingKind,
        ) {
            match &node.variant {
                NodeVariant::LogicBinOp { left, operator, right } => {
                    // Create a label in case left operand is also a short
                    // circuit operation.
                    let eval_right_part_label = output.new_label();

                    // First, evaluate the left operand as a branching
                    match operator.variant {
                        LogicOperatorVariant::Or => {
                            internal_compile(
                                left,
                                ctx,
                                owning_unit,
                                output,
                                if_true_label,
                                eval_right_part_label,
                                BranchingKind::IfTrue,
                            );
                        }
                        LogicOperatorVariant::And => {
                            internal_compile(
                                left,
                                ctx,
                                owning_unit,
                                output,
                                eval_right_part_label,
                                if_false_label,
                                BranchingKind::IfFalse,
                            );
                        }
                        _ => unreachable!(),
                    }

                    // Then, emit code to evaluate the right operand
                    output.label(eval_right_part_label);
                    internal_compile(
                        right,
                        ctx,
                        owning_unit,
                        output,
                        if_true_label,
                        if_false_label,
                        branching_kind,
                    );
                }
                NodeVariant::CompBinOp { left, operator, right } => {
                    // Try to compile the operation with a constant-operand shape
                    let already_compiled = match (left.eval_as_constant(), right.eval_as_constant())
                    {
                        (_, Some(right_constant_value)) => {
                            compile_comp_bin_op_with_constant_operand(
                                ctx,
                                owning_unit,
                                output,
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
                                owning_unit,
                                output,
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
                        let left_access = left.compile_as_access(ctx, owning_unit, output);
                        let right_access = right.compile_as_access(ctx, owning_unit, output);

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
                        output.ad(
                            &node.origin_location,
                            op,
                            left_access.slot(),
                            right_access.slot() as u16,
                        );
                        output.cgoto(
                            ctx,
                            match branching_kind {
                                BranchingKind::IfTrue => if_true_label,
                                BranchingKind::IfFalse => if_false_label,
                            },
                        );

                        // Release access to values
                        left_access.release(ctx);
                        right_access.release(ctx);
                    }
                }

                // In all other cases, the node is compiled as an access and
                // the value of the latter is tested to branch if required.
                _ => {
                    let result_access = node.compile_as_access(ctx, owning_unit, output);
                    let (op, label) = match branching_kind {
                        BranchingKind::IfTrue => (IST, if_true_label),
                        BranchingKind::IfFalse => (ISF, if_false_label),
                    };
                    output.ad(&node.origin_location, op, 0, result_access.slot() as u16);
                    output.cgoto(ctx, label);
                    result_access.release(ctx);
                }
            }
        }

        /// Try compiling the provided comparison operation in a constant
        /// optimized way, returning whether it is successful.
        fn compile_comp_bin_op_with_constant_operand(
            ctx: &mut CompilationContext,
            owning_unit: &ExecutionUnit,
            output: &mut ExtendedInstructionBuffer,
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
                            Some((ISNES, ctx.current_data().constants.get_from_string(s)))
                        }
                        _ => constant_operand.to_numeric_constant().map(|n| {
                            (ISNEN, ctx.current_data().constants.get_from_numeric_constant(n))
                        }),
                    }
                }
                (CompOperatorVariant::NotEquals, BranchingKind::IfFalse)
                | (CompOperatorVariant::Equals, BranchingKind::IfTrue) => {
                    match &constant_operand.variant {
                        ConstantValueVariant::Bool(constant_bool) => {
                            Some((ISEQP, if *constant_bool { PRIM_TRUE } else { PRIM_FALSE }))
                        }
                        ConstantValueVariant::String(s) => {
                            Some((ISEQS, ctx.current_data().constants.get_from_string(s)))
                        }
                        _ => constant_operand.to_numeric_constant().map(|n| {
                            (ISEQN, ctx.current_data().constants.get_from_numeric_constant(n))
                        }),
                    }
                }
                _ => None,
            };

            // Compile the node if possible
            if let Some((op, d)) = maybe_op_and_d {
                // Get an access to the variable operand
                let operand_access = variable_operand.compile_as_access(ctx, owning_unit, output);

                // Emit the branching instruction
                output.ad(&operation.origin_location, op, operand_access.slot(), d);
                output.cgoto(
                    ctx,
                    match branching_kind {
                        BranchingKind::IfTrue => if_true_label,
                        BranchingKind::IfFalse => if_false_label,
                    },
                );

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

    /// Compile the node as a function top-level instruction, meaning that
    /// the emitted code is going to return the value of this node to the
    /// caller. This function is used to compile composite expressions in an
    /// optimized way, having return instructions in every branch.
    fn compile_as_return(
        &self,
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
    ) {
        match &self.variant {
            // In the case of a function call, we can emit a tail call
            NodeVariant::FunCall { callee, positional_args, named_args } => {
                // Prepare the function call
                let call_slots = Self::prepare_call(
                    ctx,
                    owning_unit,
                    output,
                    self,
                    callee,
                    positional_args,
                    named_args,
                );

                // Close locals before returning
                emit_closing_instruction(&ctx.current_frame(), output);

                // Emit a function tail call
                output.ad(
                    &self.origin_location,
                    CALLT,
                    call_slots.first,
                    positional_args.len() as u16 + 2,
                );
            }

            // In the case of a conditional expression, avoid useless jump and
            // compile each branch as returning.
            NodeVariant::IfExpr { condition, consequence, alternative } => {
                // Prepare the alternative label
                let alternative_label = output.new_label();

                // Compile the condition as a branching one
                condition.compile_as_branching(ctx, owning_unit, output, alternative_label);

                // Emit code to return the consequence value
                consequence.compile_as_return(ctx, owning_unit, output);

                // Emit code to return the alternative
                output.label(alternative_label);
                alternative.compile_as_return(ctx, owning_unit, output);
            }

            // In the case of a block expression, compile its result as return
            NodeVariant::BlockExpr { local_symbols, body, val } => {
                // Compile the block body
                let parent_frame =
                    Self::compile_block_body(ctx, owning_unit, output, local_symbols, body);

                // The compile the value as a returning node
                val.compile_as_return(ctx, owning_unit, output);

                // Release the block local bindings
                let death_label = output.new_label();
                ctx.release_locals(death_label);
                output.label(death_label);

                // Finally, restore the previous frame as the current one
                ctx.current_frame = parent_frame;
            }

            // In all other cases, just get an access to the value and return
            // it.
            _ => {
                let value_access = self.compile_as_access(ctx, owning_unit, output);
                emit_closing_instruction(&ctx.current_frame(), output);
                output.ad(&self.origin_location, RET1, value_access.slot(), 2);
            }
        }
    }

    /// Util function to compile a child execution unit. This function
    /// initialize the local slot associated to the child at the provided index
    /// with the child identifier as debug name.
    fn compile_child_unit(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        origin_location: &SourceSection,
        child_symbol: &str,
        child_index: usize,
    ) {
        // Create the birth label of the local variable
        let birth_label = output.new_label();

        // Get the slot to place the functional value in
        let binding_slot = ctx.current_frame().get_local(child_symbol).unwrap();

        // Create the child unit identifier
        let child_unit = &owning_unit.children_units[child_index];
        let child_unit_id = child_unit.id(&ctx.current_data().identifier);

        // Flag the local slot as initialized before compiling the
        // unit to allow the latter to be recursive.
        ctx.current_frame_mut().init_local_with_debug_name(
            child_symbol,
            &child_unit_id,
            birth_label,
        );
        child_unit.open_frame_and_compile(ctx);

        // Add a child constant in the constant table
        let child_cst = ctx.current_data().constants.get_child();

        // Finally, emit instruction to set place the functional value
        // in the local slot.
        output.ad(origin_location, FNEW, binding_slot.slot, child_cst);
        output.label(birth_label);
    }

    /// Util function used to compile a collection of nodes into a table value.
    fn compile_table(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        result_slot: u8,
        origin_location: &SourceSection,
        array_part_nodes: &Vec<Node>,
        hash_part_nodes: &Vec<(Identifier, Node)>,
    ) {
        // Create the constant array part
        let mut array_part = Vec::new();
        let mut array_remains = Vec::new();
        for (i, array_elem) in array_part_nodes.iter().enumerate() {
            if let Some(array_elem_table_constant) = array_elem
                .eval_as_constant()
                .and_then(|c| c.to_table_constant_element())
            {
                array_part.push(array_elem_table_constant);
            } else {
                array_part.push(TableConstantElement::Nil);
                array_remains.push((i, array_elem));
            }
        }

        // Create the constant hash part
        let mut hash_part = Vec::new();
        let mut hash_remains = Vec::new();
        for (name, hash_elem) in hash_part_nodes {
            if let Some(hash_elem_table_constant) = hash_elem
                .eval_as_constant()
                .and_then(|c| c.to_table_constant_element())
            {
                hash_part.push((
                    TableConstantElement::String(name.text.clone()),
                    hash_elem_table_constant,
                ));
            } else {
                hash_remains.push((name, hash_elem));
            }
        }

        // Create the complex constant and duplicate it
        let table_cst = ctx
            .current_data()
            .constants
            .get_from_complex_constant(ComplexConstant::Table { array_part, hash_part });
        output.ad(origin_location, TDUP, result_slot, table_cst);

        // Finally place the non-constant value in the table
        for (i, array_elem) in array_remains {
            let elem_access = array_elem.compile_as_access(ctx, owning_unit, output);
            emit_table_index_write(
                ctx,
                output,
                origin_location,
                elem_access.slot(),
                result_slot,
                i,
            );
            elem_access.release(ctx);
        }
        for (name, hash_elem) in hash_remains {
            let elem_access = hash_elem.compile_as_access(ctx, owning_unit, output);
            emit_table_member_write(
                ctx,
                output,
                Some(origin_location),
                elem_access.slot(),
                result_slot,
                &name.text,
            );
            elem_access.release(ctx);
        }
    }

    /// Util function to compile provided nodes as a function call and then
    /// return the slot range used for calling operation.
    fn compile_call(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        call: &Node,
        callee: &Node,
        positional_args: &Vec<Node>,
        named_args: &Vec<(Identifier, Node)>,
    ) -> SlotRange {
        // Prepare the call
        let call_slots =
            Self::prepare_call(ctx, owning_unit, output, call, callee, positional_args, named_args);

        // Call the function and return the used slots
        output.abc(
            &call.origin_location,
            CALL,
            call_slots.first,
            2,
            positional_args.len() as u8 + 2,
        );
        call_slots
    }

    /// Util function to prepare a function call and then return the slot range
    /// in which the function and arguments are placed.
    fn prepare_call(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        call: &Node,
        callee: &Node,
        positional_args: &Vec<Node>,
        named_args: &Vec<(Identifier, Node)>,
    ) -> SlotRange {
        // Reserve slots for the call
        let arg_count = positional_args.len() + 2;
        let call_slots = ctx
            .current_frame_mut()
            .reserve_contiguous_slots(arg_count + 1);

        // Evaluate the callee and place it in the first of the call slots.
        callee.compile_as_value(ctx, owning_unit, output, call_slots.first);

        // Compile named arguments to a table (or nil if there is no named
        // arguments).
        if named_args.is_empty() {
            output.ad(&call.origin_location, KPRI, call_slots.first + 2, PRIM_NIL);
        } else {
            Self::compile_table(
                ctx,
                owning_unit,
                output,
                call_slots.first + 2,
                &call.origin_location,
                &Vec::new(),
                named_args,
            );
        }

        // Place positional arguments in the required slots
        for (i, positional_arg) in positional_args.iter().enumerate() {
            positional_arg.compile_as_value(
                ctx,
                owning_unit,
                output,
                call_slots.first + 3 + i as u8,
            );
        }
        call_slots
    }

    /// Util function used to compile the body part of a block expression. Note
    /// that this function is opening a new frame and filling it with block
    /// locals without freeing it.
    /// This function returns the parent frame for the caller to restore the
    /// previous compilation context.
    fn compile_block_body(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        local_symbols: &Vec<Identifier>,
        body: &Vec<Node>,
    ) -> Rc<RefCell<Frame>> {
        // Open a new lexical frame that will contains all symbols
        // declared in the block.
        let parent_frame = ctx.current_frame.clone();
        let current_frame = Rc::new(RefCell::new(Frame::new_lexical(parent_frame.clone())));
        ctx.current_frame = current_frame;

        // Insert all locals in the frame
        declare_locals(ctx, local_symbols);

        // Compile the body of the block
        for body_elem in body {
            let elem_access = body_elem.compile_as_access(ctx, owning_unit, output);

            // TODO: Check that the value returned by the body element
            // is "unit".
            // (https://github.com/HugoGGuerrier/lkqlua_jit/issues/4)

            elem_access.release(ctx);
        }

        // Return the parent frame
        parent_frame
    }
}

impl ConstantValue {
    /// Compile the constant value by emitting required instructions to set the
    /// `result_slot` to the runtime value represented by the constant.
    /// For now, only integers lower than [`i32::MAX`] are handled
    /// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/3).
    fn compile(
        &self,
        ctx: &mut CompilationContext,
        output: &mut ExtendedInstructionBuffer,
        result_slot: u8,
    ) {
        match &self.variant {
            ConstantValueVariant::Null => {
                emit_global_read(
                    ctx,
                    output,
                    Some(&self.origin_location),
                    result_slot,
                    "<lkql_null>",
                );
            }
            ConstantValueVariant::Unit => {
                emit_global_read(
                    ctx,
                    output,
                    Some(&self.origin_location),
                    result_slot,
                    UNIT_VALUE_NAME,
                );
            }
            ConstantValueVariant::Bool(value) => {
                output.ad(
                    &self.origin_location,
                    KPRI,
                    result_slot,
                    if *value { PRIM_TRUE } else { PRIM_FALSE },
                );
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
                    output.ad(
                        &self.origin_location,
                        KSHORT,
                        result_slot,
                        u16::from_le_bytes(le_bytes),
                    );
                }
                // Else, we have to create a new numeric constant and load it
                else if let Some(numeric_constant) = self.to_numeric_constant() {
                    let value_cst = ctx
                        .current_data()
                        .constants
                        .get_from_numeric_constant(numeric_constant);
                    output.ad(&self.origin_location, KNUM, result_slot, value_cst);
                }
                // Else, we have to handle the number as a big integer
                else {
                    panic!("Big integers aren't handled for now");
                }
            }
            ConstantValueVariant::String(value) => {
                let value_cst = ctx.current_data().constants.get_from_string(value);
                output.ad(&self.origin_location, KSTR, result_slot, value_cst);
            }
            ConstantValueVariant::Tuple(values) | ConstantValueVariant::List(values) => {
                // Create the complex constant, base of the table
                let mut array_part = Vec::new();
                let mut remains = Vec::new();
                for (i, value) in values.iter().enumerate() {
                    if let Some(table_constant) = value.to_table_constant_element() {
                        array_part.push(table_constant);
                    } else {
                        array_part.push(TableConstantElement::Nil);
                        remains.push((i + 1, value));
                    }
                }
                let table_cst = ctx.current_data().constants.get_from_complex_constant(
                    ComplexConstant::Table { array_part, hash_part: Vec::new() },
                );
                output.ad(&self.origin_location, TDUP, result_slot, table_cst);

                // Now compute parts that cannot be expressed with table
                // constant elements.
                let outside_tmp = ctx.current_frame_mut().get_tmp();
                for (i, value) in remains {
                    value.compile(ctx, output, outside_tmp);
                    emit_table_index_write(
                        ctx,
                        output,
                        &self.origin_location,
                        outside_tmp,
                        result_slot,
                        i,
                    );
                }
                ctx.current_frame_mut().release_slot(outside_tmp);

                emit_set_metatable(
                    ctx,
                    output,
                    Some(&self.origin_location),
                    result_slot,
                    types::tuple::NAME,
                );
            }
            ConstantValueVariant::Object(items) => {
                // Split constants that can be expressed by a table constant
                // element from constants that need to be loaded separately.
                let mut hash_part = Vec::new();
                let mut remains = Vec::new();
                for (field_name, field_value) in items {
                    if let Some(table_constant) = field_value.to_table_constant_element() {
                        hash_part.push((
                            TableConstantElement::String(field_name.clone()),
                            table_constant,
                        ));
                    } else {
                        remains.push((field_name, field_value));
                    }
                }

                // Create the complex constant, base of the table
                let table_cst = ctx.current_data().constants.get_from_complex_constant(
                    ComplexConstant::Table { array_part: Vec::new(), hash_part },
                );
                output.ad(&self.origin_location, TDUP, result_slot, table_cst);

                // Add members that cannot be expressed through a table
                // constant element.
                let outside_tmp = ctx.current_frame_mut().get_tmp();
                for (field_name, field_value) in remains {
                    field_value.compile(ctx, output, outside_tmp);
                    emit_table_member_write(
                        ctx,
                        output,
                        Some(&self.origin_location),
                        outside_tmp,
                        result_slot,
                        &field_name,
                    );
                }
                ctx.current_frame_mut().release_slot(outside_tmp);

                // TODO: Set the object meta-table accordingly to its type
            }
        }
    }
}

// ----- Compilation helpers -----

impl ExtendedInstructionBuffer {
    /// Shortcut function to emit a "goto" instruction in the provided
    /// compilation context.
    fn cgoto(&mut self, ctx: &CompilationContext, label: Label) {
        self.goto(label, ctx.current_frame().peek_next_slot());
    }
}

/// Util function to add a collection of symbols in the current frame as local
/// bindings. This function also checks duplicated declarations and add
/// corresponding diagnostics in the compilation context.
fn declare_locals(ctx: &mut CompilationContext, symbols: &Vec<Identifier>) {
    for symbol in symbols {
        let maybe_local_slot = ctx.current_frame().is_conflicting(&symbol.text);
        if let Some(previous_binding) = maybe_local_slot {
            ctx.diagnostics.push(Report::from_error_template_with_hints(
                &symbol.origin_location,
                &DUPLICATED_SYMBOL,
                &vec![&symbol.text],
                vec![Hint::new(
                    PREVIOUS_SYMBOL_HINT,
                    &previous_binding.declaration_location,
                )],
            ));
        } else {
            ctx.current_frame_mut()
                .bind_local(&symbol.text, &symbol.origin_location);
        }
    }
}

/// Emit required instructions to raise a runtime error during the program
/// execution.
/// For now this function call the `error` Lua built-in with a simple text
/// message.
fn emit_runtime_error(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    maybe_error_location: Option<&SourceSection>,
    error_template: &ErrorTemplate,
    message_args: &Vec<DynamicErrorArg>,
) {
    // Create the runtime error instance object
    let runtime_error_instance =
        DynamicError { template_id: error_template.id, message_args: message_args.clone() };

    // Add constants in the current repository
    let message_cst = ctx
        .current_data()
        .constants
        .get_from_string(&runtime_error_instance.to_json_string());

    // Reserve temporary slots, fill them and call the function
    let call_slots = ctx.current_frame_mut().reserve_contiguous_slots(3);
    emit_global_read(ctx, output, maybe_error_location, call_slots.first, "error");
    output.ad_maybe_loc(maybe_error_location, KSTR, call_slots.last, message_cst);
    output.abc_maybe_loc(maybe_error_location, CALL, call_slots.first, 1, 2);

    // Free slots allocated for the call
    ctx.current_frame_mut().release_slots(call_slots);
}

/// Util function to get a table element by its index.
fn emit_table_index_read(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    origin_location: &SourceSection,
    source_slot: u8,
    table_slot: u8,
    index: usize,
) {
    _access_table_index(ctx, output, origin_location, source_slot, table_slot, index, TGETB, TGETV);
}

/// Util function to set a table element by its index.
fn emit_table_index_write(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    origin_location: &SourceSection,
    source_slot: u8,
    table_slot: u8,
    index: usize,
) {
    _access_table_index(ctx, output, origin_location, source_slot, table_slot, index, TSETB, TSETV);
}

/// Internal function to factorize table index accesses. Do not use directly.
fn _access_table_index(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    origin_location: &SourceSection,
    working_slot: u8,
    table_slot: u8,
    index: usize,
    immediate_access_op: u8,
    slot_access_op: u8,
) {
    if index <= u8::MAX as usize {
        output.abc(origin_location, immediate_access_op, working_slot, table_slot, index as u8);
    } else if index <= i32::MAX as usize {
        let index_tmp = ctx.current_frame.borrow_mut().get_tmp();
        let index_cst = ctx
            .exec_unit_data_stack
            .last_mut()
            .unwrap()
            .constants
            .get_from_int(index as i32);
        output.ad(origin_location, KNUM, index_tmp, index_cst);
        output.abc(origin_location, slot_access_op, working_slot, table_slot, index_tmp);
        ctx.current_frame.borrow_mut().release_slot(index_tmp);
    } else {
        panic!("Big integers aren't handled for now")
    }
}

/// Util function to load a table member (designated by a string constant).
/// This function is used to factorize [`TGETS`] emission and optimization.
fn emit_table_member_read(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    maybe_origin_location: Option<&SourceSection>,
    target_slot: u8,
    table_slot: u8,
    member_name: &str,
) {
    _access_table_field(
        ctx,
        output,
        maybe_origin_location,
        target_slot,
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
    output: &mut ExtendedInstructionBuffer,
    maybe_origin_location: Option<&SourceSection>,
    source_slot: u8,
    table_slot: u8,
    member_name: &str,
) {
    _access_table_field(
        ctx,
        output,
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
    output: &mut ExtendedInstructionBuffer,
    maybe_origin_location: Option<&SourceSection>,
    working_slot: u8,
    table_slot: u8,
    member_name: &str,
    immediate_access_op: u8,
    slot_access_op: u8,
) {
    let member_name_cst = ctx.current_data().constants.get_from_string(member_name);
    if member_name_cst <= u8::MAX as u16 {
        output.abc_maybe_loc(
            maybe_origin_location,
            immediate_access_op,
            working_slot,
            table_slot,
            member_name_cst as u8,
        );
    } else {
        let field_tmp = ctx.current_frame_mut().get_tmp();
        output.ad_maybe_loc(maybe_origin_location, KSTR, field_tmp, member_name_cst);
        output.abc_maybe_loc(
            maybe_origin_location,
            slot_access_op,
            working_slot,
            table_slot,
            field_tmp,
        );
        ctx.current_frame_mut().release_slot(field_tmp);
    }
}

/// Util function used to generate a global table reading.
fn emit_global_read(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    maybe_origin_location: Option<&SourceSection>,
    result_slot: u8,
    global_name: &str,
) {
    let global_name_cst = ctx.current_data().constants.get_from_string(global_name);
    output.ad_maybe_loc(maybe_origin_location, GGET, result_slot, global_name_cst);
}

/// Emit instructions to set the meta-table of the type designated by the
/// provided name as the one of the table at the provided slot.
fn emit_set_metatable(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    maybe_origin_location: Option<&SourceSection>,
    table_slot: u8,
    type_name: &str,
) {
    // Get slots for the call
    let call_slots = ctx.current_frame_mut().reserve_contiguous_slots(4);

    // Get the "setmetatable", fill arguments and make a call
    emit_global_read(ctx, output, maybe_origin_location, call_slots.first, "setmetatable");
    output.ad_maybe_loc(maybe_origin_location, MOV, call_slots.last - 1, table_slot as u16);
    emit_global_read(
        ctx,
        output,
        maybe_origin_location,
        call_slots.last,
        &metatable_global_field(type_name),
    );
    output.abc_maybe_loc(maybe_origin_location, CALL, call_slots.first, 1, 3);

    // Release call slot
    ctx.current_frame_mut().release_slots(call_slots);
}

/// Emit, if required, the instruction to close local values in the current
/// frame.
pub fn emit_closing_instruction(frame: &Frame, output: &mut ExtendedInstructionBuffer) {
    // If required, emit an instruction to close required slots
    match &frame.variant {
        FrameVariant::Semantic { close_from, .. } => {
            if let Some(s) = close_from {
                output.ad_no_loc(UCLO, *s, JUMP_BIASING);
            }
        }
        FrameVariant::Lexical => emit_closing_instruction(&frame.parent_frame().unwrap(), output),
    }
}

/// Emit instructions in provided output to initialize closed binding that
/// requires it.
pub fn emit_closed_bindings_init(frame: &Frame, output: &mut ExtendedInstructionBuffer) {
    // Set bindings that are unsafely closed to nil
    for binding in frame.bindings.values() {
        if binding.closing_kind == ClosingKind::Unsafe {
            output.insert_instruction(
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
}

// ----- Compilation support -----

/// This type is used to represents the result of an access to a value, it
/// carries the information whether the value is directly accessed through a
/// slot read, or through a temporary value.
enum ValueAccess {
    Direct(u8),
    Tmp(u8),
}

impl ValueAccess {
    fn slot(&self) -> u8 {
        match self {
            ValueAccess::Direct(s) | ValueAccess::Tmp(s) => *s,
        }
    }

    fn release(&self, ctx: &mut CompilationContext) {
        match self {
            ValueAccess::Direct(_) => (),
            ValueAccess::Tmp(s) => ctx.current_frame_mut().release_slot(*s),
        }
    }
}

/// This type is the main data holder during the compilation process.
struct CompilationContext {
    /// Built-in symbols that are available during the compilation.
    builtins: HashSet<&'static str>,

    /// The frame that is currently being used in the compilation process. This
    /// stores all information about local symbols, up-values and temporary
    /// slots.
    current_frame: Rc<RefCell<Frame>>,

    /// A stack of compilation data, the last element being the current one.
    /// Each element stores information about the currently compiled execution
    /// unit (constants, properties, ...).
    exec_unit_data_stack: Vec<ExecUnitCompilationData>,

    /// This is the main result of the compilation process, it is filled during
    /// the compilation of execution units (see [`ExecutionUnit::compile`]).
    prototypes: Vec<Prototype>,

    /// Data required for the runtime and collected during the compilation.
    runtime_data: RuntimeData,

    /// A collection of diagnostics collected during the compilation process,
    /// if it is not empty, it means that the result of the compilation may be
    /// invalid.
    diagnostics: Vec<Report>,
}

impl CompilationContext {
    fn new() -> Self {
        Self {
            builtins: get_builtin_bindings().iter().map(|b| b.name).collect(),
            current_frame: Rc::new(RefCell::new(Frame::new(None))),
            exec_unit_data_stack: Vec::new(),
            prototypes: Vec::new(),
            runtime_data: RuntimeData::new(),
            diagnostics: Vec::new(),
        }
    }

    /// Get a mutable reference to the compilation data of the unit currently
    /// being compiled.
    fn current_data(&mut self) -> &mut ExecUnitCompilationData {
        self.exec_unit_data_stack.last_mut().unwrap()
    }

    /// Get a reference to the current frame.
    fn current_frame(&self) -> Ref<'_, Frame> {
        self.current_frame.borrow()
    }

    /// Get a mutable reference to the current frame.
    fn current_frame_mut(&self) -> RefMut<'_, Frame> {
        self.current_frame.borrow_mut()
    }

    /// Util function to release all local bindings of the current frame and
    /// store remaining data in the current [`ExecUnitCompilationData`]
    /// instance.
    fn release_locals(&mut self, death_label: Label) {
        let old_bindings = self
            .current_frame_mut()
            .bindings
            .drain()
            .collect::<Vec<_>>();
        for (name, mut old_binding) in old_bindings {
            old_binding.death_label = death_label;
            self.current_data().dead_bindings.insert(name, old_binding);
        }
    }
}

/// This type contains all data required to compile an [`ExecutionUnit`] object
/// into a bytecode [`Prototype`].
struct ExecUnitCompilationData {
    identifier: String,
    has_child: bool,
    constants: ConstantRepository,
    dead_bindings: HashMap<String, BindingData>,
}

impl ExecUnitCompilationData {
    fn new(identifier: String) -> Self {
        Self {
            identifier,
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

    /// Get the index of the constant representing the empty table.
    fn get_empty_table(&mut self) -> u16 {
        self.get_from_complex_constant(ComplexConstant::Table {
            array_part: Vec::new(),
            hash_part: Vec::new(),
        })
    }

    /// More generic function to get the index of a complex constant.
    fn get_from_complex_constant(&mut self, constant: ComplexConstant) -> u16 {
        Self::get_or_add_constant(constant, &mut self.complex_constants)
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
