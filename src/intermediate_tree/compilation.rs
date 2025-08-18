//! # Intermediate tree compilation module
//!
//! This module contains all required operations to compile any intermediate
//! tree into a [`crate::bytecode::BytecodeBuffer`].

use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    ops::Range,
    rc::Rc,
    u8,
};

use num_bigint::BigInt;

use crate::{
    builtins::get_builtins,
    bytecode::{
        BytecodeBuffer, ComplexConstant, Instruction, JUMP_BIASING, NumericConstant, PRIM_FALSE,
        PRIM_NIL, PRIM_TRUE, Prototype, TableConstantElement, UpValueConstant,
        extended_bytecode::{ExtendedInstructionBuffer, Label},
        op_codes::*,
    },
    intermediate_tree::{
        ArithOperator, ArithOperatorVariant, CompOperator, CompOperatorVariant, ExecutionUnit,
        ExecutionUnitVariant, Identifier, LogicOperatorVariant, MiscOperatorVariant, Node,
        NodeVariant, constant_eval::ConstantValue,
    },
    report::Report,
    sources::SourceSection,
};

// ----- Compilation processes -----

impl ExecutionUnit {
    /// Compile this execution unit as a LuaJIT bytecode buffer. The result of
    /// this function can be used to execute the semantics described by the
    /// execution unit with the LuaJIT engine.
    pub fn compile(&self) -> Result<BytecodeBuffer, Report> {
        // Open the initial compilation context and create the prototypes vector
        let mut compile_context = CompilationContext::new();

        // Compile the current execution unit
        self.internal_compile(&mut compile_context);

        // Then return the success result
        if compile_context.diagnostics.is_empty() {
            Ok(BytecodeBuffer { prototypes: compile_context.prototypes })
        } else {
            Err(Report::Composed(compile_context.diagnostics))
        }
    }

    /// Open a new frame in the compilation context and compile this execution
    /// unit using the [`Self::internal_compile`] method.
    fn open_frame_and_compile(&self, ctx: &mut CompilationContext) {
        // Open a new semantic frame for this function, and push a new data
        // holder in the compilation context.
        let parent_frame = ctx.current_frame.clone();
        let current_frame = Rc::new(RefCell::new(Frame::new(Some(parent_frame.clone()))));
        ctx.current_frame = current_frame;
        ctx.exec_unit_data_stack
            .push(ExecUnitCompilationData::new());

        // Compile the execution unit
        self.internal_compile(ctx);

        // Restore the parent frame and pop the unit data
        ctx.current_frame = parent_frame;
        ctx.exec_unit_data_stack.pop();
        ctx.current_data().has_child = true;
    }

    /// Compile the execution unit and all its children (direct and indirect
    /// ones), and place the result in the provided  `output` buffer.
    fn internal_compile(&self, ctx: &mut CompilationContext) {
        // Create compilation working values
        let mut instructions = ExtendedInstructionBuffer::new();
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
                    let elem_access = elem.compile_as_access(ctx, self, &mut instructions);
                    elem_access.release(ctx);
                }

                // Reserve a slot for the module table
                let result_tmp = ctx.current_frame_mut().get_tmp();

                // Emit instructions to create the module table
                let empty_table_cst = ctx.current_data().constants.get_empty_table();
                instructions.ad(TDUP, result_tmp, empty_table_cst);
                for symbol in symbols {
                    let local_slot = ctx.current_frame().get_local(&symbol.text).unwrap();
                    emit_table_member_write(
                        ctx,
                        &mut instructions,
                        local_slot.slot,
                        result_tmp,
                        &symbol.text,
                    );
                }

                // Emit local values closing
                ctx.current_frame()
                    .emit_closing_instruction(&mut instructions);

                // Emit returning of the module table
                instructions.ad(RET1, result_tmp, 2);
                ctx.current_frame_mut().release_slot(result_tmp);
            }
            ExecutionUnitVariant::Function { params, body } => {
                // Set the argument count
                arg_count = params.len() + 1;

                // Reserve the first slot of the frame, by convention, this
                // slot is used to provide named arguments.
                let named_args_slot = ctx.current_frame_mut().get_tmp();

                // Add function parameters to the current frame
                declare_locals(ctx, &params.iter().map(|(s, _)| s.clone()).collect());

                // Emit instructions to ensure all parameters have a valid and
                // unique value.
                for (param_id, maybe_default_value) in params {
                    let param_slot = ctx.current_frame().get_local(&param_id.text).unwrap();

                    // Create working labels
                    let no_value_label = instructions.new_label();
                    let test_both_label = instructions.new_label();
                    let next_label = instructions.new_label();

                    // Test if the parameter has a positional value
                    instructions.ad(ISNEP, param_slot.slot, PRIM_NIL);
                    instructions.cgoto(ctx, test_both_label);

                    // If there is no positional value, start by checking if
                    // there is a named value for the parameter.
                    instructions.ad(ISEQP, named_args_slot, PRIM_NIL);
                    instructions.cgoto(ctx, no_value_label);
                    emit_table_member_read(
                        ctx,
                        &mut instructions,
                        param_slot.slot,
                        named_args_slot,
                        &param_id.text,
                    );
                    instructions.ad(ISNEP, param_slot.slot, PRIM_NIL);
                    instructions.cgoto(ctx, next_label);

                    // If parameter has no value, emit an error
                    instructions.label(no_value_label);
                    if let Some(default_value) = maybe_default_value {
                        default_value.compile_as_value(
                            ctx,
                            self,
                            &mut instructions,
                            param_slot.slot,
                        );
                    } else {
                        emit_runtime_error(
                            ctx,
                            &mut instructions,
                            &format!("missing value for \"{}\" parameter", param_id.text),
                        );
                    }

                    // Test if the parameter have both positional and named
                    // values
                    instructions.label(test_both_label);
                    instructions.ad(ISEQP, named_args_slot, PRIM_NIL);
                    instructions.cgoto(ctx, next_label);
                    emit_table_member_read(
                        ctx,
                        &mut instructions,
                        param_slot.slot,
                        named_args_slot,
                        &param_id.text,
                    );
                    instructions.ad(ISEQP, param_slot.slot, PRIM_NIL);
                    instructions.cgoto(ctx, next_label);
                    emit_runtime_error(
                        ctx,
                        &mut instructions,
                        &format!(
                            "parameter \"{}\" has both positional and named values",
                            param_id.text
                        ),
                    );

                    // Label the next instruction
                    instructions.label(next_label);

                    // Finally, set the parameter slot as initialized
                    ctx.current_frame_mut().init_local(&param_id.text);
                }

                // The compile the body as a returning node
                body.compile_as_return(ctx, self, &mut instructions);
            }
        }

        // Emit additional instructions to initialize closed bindings
        ctx.current_frame()
            .emit_closed_bindings_init(&mut instructions);

        // Perform post compilation assertions
        assert!(arg_count <= u8::MAX as usize, "Too many arguments for prototype");

        // Get the result index and data to create the new prototype
        let data = ctx.exec_unit_data_stack.last_mut().unwrap();

        // Add the prototype to the output buffer
        match &ctx.current_frame.borrow().variant {
            FrameVariant::Semantic { maximum_size, up_values, .. } => {
                // Sort up-values from their index
                let mut sorted_up_values = up_values.into_iter().collect::<Vec<_>>();
                sorted_up_values.sort_by(|(_, uv_1), (_, uv_2)| uv_1.index.cmp(&uv_2.index));

                // Then push the new prototype in the result
                ctx.prototypes.push(Prototype {
                    has_child: data.has_child,
                    is_variadic,
                    has_ffi: true,
                    arg_count: arg_count as u8,
                    frame_size: *maximum_size,
                    instructions: instructions.to_instructions(),
                    up_values: sorted_up_values
                        .iter()
                        .map(|(_, uv)| match uv.target {
                            UpValueTarget::ParentSlot(s) => {
                                UpValueConstant::ParentLocalSlot(s as u16)
                            }
                            UpValueTarget::ParentUpValue(u) => {
                                UpValueConstant::ParentUpValue(u as u16)
                            }
                        })
                        .collect(),
                    complex_consts: data.constants.complex_constants.drain(..).collect(),
                    numeric_consts: data.constants.numeric_constants.drain(..).collect(),
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
                    callee,
                    positional_args,
                    named_args,
                );
                output.ad(MOV, result_slot, call_slots.start as u16);
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
                    result_slot,
                    prefix_access.slot(),
                    &suffix.text,
                );

                // Emit post access check
                let next_label = output.new_label();
                output.ad(ISNEP, result_slot, PRIM_NIL);
                output.cgoto(ctx, next_label);
                if *is_safe {
                    emit_global_read(ctx, output, "<lkql_unit>", result_slot);
                } else {
                    emit_runtime_error(
                        ctx,
                        output,
                        &format!("No member named \"{}\" in this value", &suffix.text),
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
                output.abc(TGETV, result_slot, indexed_value_access.slot(), index_access.slot());

                // Emit post access check
                let next_label = output.new_label();
                output.ad(ISNEP, result_slot, PRIM_NIL);
                output.cgoto(ctx, next_label);
                if *is_safe {
                    emit_global_read(ctx, output, "<lkql_unit>", result_slot);
                } else {
                    emit_runtime_error(ctx, output, "Index out of bounds");
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
                for local_symbol in local_symbols {
                    ctx.current_frame_mut().release_local(&local_symbol.text);
                }

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
                    output.abc(op, result_slot, left_access.slot(), right_access.slot());
                    left_access.release(ctx);
                    right_access.release(ctx);
                }
            }
            NodeVariant::LogicBinOp { .. } => {
                // Create required labels
                let if_false_label = output.new_label();
                let next_label = output.new_label();

                // Compile the current node as branching to emit short
                // circuiting instructions
                self.compile_as_branching(ctx, owning_unit, output, if_false_label);

                // Emit the code to set the result to "true"
                output.ad(KPRI, result_slot, PRIM_TRUE);
                output.cgoto(ctx, next_label);

                // Emit the code to set the result to "false"
                output.label(if_false_label);
                output.ad(KPRI, result_slot, PRIM_FALSE);

                // Label the next instruction as such
                output.label(next_label);
            }
            NodeVariant::CompBinOp { .. } => {
                // Create required labels
                let if_false_label = output.new_label();
                let next_label = output.new_label();

                // Compile the current node as branching to emit conditional
                // instructions.
                self.compile_as_branching(ctx, owning_unit, output, if_false_label);

                // Emit the code to set the result to "true"
                output.ad(KPRI, result_slot, PRIM_TRUE);
                output.cgoto(ctx, next_label);

                // Emit the code to set the result to "false"
                output.label(if_false_label);
                output.ad(KPRI, result_slot, PRIM_FALSE);

                // Label the next instruction as such
                output.label(next_label);
            }
            NodeVariant::MiscBinOp { left, operator, right } => {
                let left_access = left.compile_as_access(ctx, owning_unit, output);
                let right_access = right.compile_as_access(ctx, owning_unit, output);
                let op = match operator.variant {
                    MiscOperatorVariant::Concat => CAT,
                };
                output.abc(op, result_slot, left_access.slot(), right_access.slot());
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
                output.ad(op, result_slot, operand_access.slot() as u16);
                operand_access.release(ctx);
            }
            NodeVariant::LogicUnOp { operator, operand } => {
                let operand_access = operand.compile_as_access(ctx, owning_unit, output);
                let op = match operator.variant {
                    LogicOperatorVariant::Not => NOT,
                    _ => unreachable!(),
                };
                output.ad(op, result_slot, operand_access.slot() as u16);
                operand_access.release(ctx);
            }

            // --- Symbol accesses
            NodeVariant::InitLocal { symbol, val } => {
                let binding_slot = ctx.current_frame().get_local(&symbol.text).unwrap();
                val.compile_as_value(ctx, owning_unit, output, binding_slot.slot);
                ctx.current_frame_mut().init_local(&symbol.text);
            }
            NodeVariant::InitRecLocal { symbol, val } => {
                let binding_slot = ctx.current_frame().get_local(&symbol.text).unwrap();
                ctx.current_frame_mut().init_local(&symbol.text);
                val.compile_as_value(ctx, owning_unit, output, binding_slot.slot);
            }
            NodeVariant::ReadSymbol(identifier) => {
                // First try getting the symbol in the local frame
                let maybe_local_slot = ctx.current_frame().get_local(&identifier.text);
                if let Some(BoundSlot { slot, is_init: true, .. }) = maybe_local_slot {
                    output.ad(MOV, result_slot, slot as u16);
                } else {
                    // Then try to look in the up-values
                    let maybe_up_value = ctx.current_frame_mut().get_up_value(&identifier.text);
                    if let Some(up_value) = maybe_up_value {
                        output.ad(UGET, result_slot, up_value.index as u16);
                        if !up_value.is_safe {
                            let next_label = output.new_label();
                            output.ad(ISNEP, result_slot, PRIM_NIL);
                            output.cgoto(ctx, next_label);
                            emit_runtime_error(
                                ctx,
                                output,
                                &format!("Unknown symbol: \"{}\"", identifier.text),
                            );
                            output.label(next_label);
                        }
                    } else {
                        // Finally, if a built-in is named like the accessed
                        // symbol, get it in the global table.
                        if ctx.builtins.contains(identifier.text.as_str()) {
                            emit_global_read(ctx, output, &identifier.text, result_slot);
                        }
                        // If all previous step failed, the symbol doesn't
                        // exists, so we emit an error about it.
                        else {
                            ctx.diagnostics.push(Report::unknown_symbol(
                                self.origin_location.clone(),
                                &identifier.text,
                            ));
                        }
                    }
                }
            }

            // --- Children function access
            NodeVariant::ChildFunRef(index) => {
                // Compile the required child
                owning_unit.children_units[*index as usize]
                    .borrow()
                    .open_frame_and_compile(ctx);

                // Add the child constant in the constant table
                let child_cst = ctx.current_data().constants.get_child();

                // Then emit an instruction to load it locally
                output.ad(FNEW, result_slot, child_cst);
            }

            // --- Non-trivial literals
            NodeVariant::TupleLiteral(elements) | NodeVariant::ListLiteral(elements) => {
                Self::compile_table(ctx, owning_unit, output, elements, &Vec::new(), result_slot);

                // TODO: Set the object meta-table accordingly to its type
            }
            NodeVariant::ObjectLiteral(items) => {
                Self::compile_table(ctx, owning_unit, output, &Vec::new(), items, result_slot);

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
                output.abc(op, result_slot, operand_access.slot(), numeric_cst as u8);
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
                    callee,
                    positional_args,
                    named_args,
                );
                ctx.current_frame_mut()
                    .release_slots((call_slots.start + 1)..call_slots.end);
                ValueAccess::Tmp(call_slots.start)
            }

            NodeVariant::ReadSymbol(identifier) => {
                let maybe_local_slot = ctx.current_frame().get_local(&identifier.text);
                if let Some(BoundSlot { slot, is_init: true, .. }) = maybe_local_slot {
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
                        output.ad(op, left_access.slot(), right_access.slot() as u16);
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
                    output.ad(op, 0, result_access.slot() as u16);
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
                | (CompOperatorVariant::NotEquals, BranchingKind::IfTrue) => match constant_operand
                {
                    ConstantValue::Bool(constant_bool) => {
                        Some((ISNEP, if *constant_bool { PRIM_TRUE } else { PRIM_FALSE }))
                    }
                    ConstantValue::String(s) => {
                        Some((ISNES, ctx.current_data().constants.get_from_string(s)))
                    }
                    _ => constant_operand.to_numeric_constant().map(|n| {
                        (ISNEN, ctx.current_data().constants.get_from_numeric_constant(n))
                    }),
                },
                (CompOperatorVariant::NotEquals, BranchingKind::IfFalse)
                | (CompOperatorVariant::Equals, BranchingKind::IfTrue) => match constant_operand {
                    ConstantValue::Bool(constant_bool) => {
                        Some((ISEQP, if *constant_bool { PRIM_TRUE } else { PRIM_FALSE }))
                    }
                    ConstantValue::String(s) => {
                        Some((ISEQS, ctx.current_data().constants.get_from_string(s)))
                    }
                    _ => constant_operand.to_numeric_constant().map(|n| {
                        (ISEQN, ctx.current_data().constants.get_from_numeric_constant(n))
                    }),
                },
                _ => None,
            };

            // Compile the node if possible
            if let Some((op, d)) = maybe_op_and_d {
                // Get an access to the variable operand
                let operand_access = variable_operand.compile_as_access(ctx, owning_unit, output);

                // Emit the branching instruction
                output.ad(op, operand_access.slot(), d);
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
                    callee,
                    positional_args,
                    named_args,
                );

                // Close locals before returning
                ctx.current_frame().emit_closing_instruction(output);

                // Emit a function tail call
                output.ad(CALLT, call_slots.start, positional_args.len() as u16 + 2);
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
                for local_symbol in local_symbols {
                    ctx.current_frame_mut().release_local(&local_symbol.text);
                }

                // Finally, restore the previous frame as the current one
                ctx.current_frame = parent_frame;
            }

            // In all other cases, just get an access to the value and return
            // it.
            _ => {
                let value_access = self.compile_as_access(ctx, owning_unit, output);
                ctx.current_frame().emit_closing_instruction(output);
                output.ad(RET1, value_access.slot(), 2);
            }
        }
    }

    /// Util function used to compile a collection of nodes into a table value.
    fn compile_table(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        array_part_nodes: &Vec<Node>,
        hash_part_nodes: &Vec<(Identifier, Node)>,
        result_slot: u8,
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
        output.ad(TDUP, result_slot, table_cst);

        // Finally place the non-constant value in the table
        for (i, array_elem) in array_remains {
            let elem_access = array_elem.compile_as_access(ctx, owning_unit, output);
            emit_table_index_write(ctx, output, elem_access.slot(), result_slot, i);
            elem_access.release(ctx);
        }
        for (name, hash_elem) in hash_remains {
            let elem_access = hash_elem.compile_as_access(ctx, owning_unit, output);
            emit_table_member_write(ctx, output, elem_access.slot(), result_slot, &name.text);
            elem_access.release(ctx);
        }
    }

    /// Util function to compile provided nodes as a function call and then
    /// return the slot range used for calling operation.
    fn compile_call(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        callee: &Node,
        positional_args: &Vec<Node>,
        named_args: &Vec<(Identifier, Node)>,
    ) -> Range<u8> {
        // Prepare the call
        let call_slots =
            Self::prepare_call(ctx, owning_unit, output, callee, positional_args, named_args);

        // Call the function and return the used slots
        output.abc(CALL, call_slots.start, 2, positional_args.len() as u8 + 2);
        call_slots
    }

    /// Util function to prepare a function call and then return the slot range
    /// in which the function and arguments are placed.
    fn prepare_call(
        ctx: &mut CompilationContext,
        owning_unit: &ExecutionUnit,
        output: &mut ExtendedInstructionBuffer,
        callee: &Node,
        positional_args: &Vec<Node>,
        named_args: &Vec<(Identifier, Node)>,
    ) -> Range<u8> {
        // Reserve slots for the call
        let arg_count = positional_args.len() + 2;
        let call_slots = ctx
            .current_frame_mut()
            .reserve_contiguous_slots(arg_count + 1);

        // Evaluate the callee and place it in the first of the call slots.
        callee.compile_as_value(ctx, owning_unit, output, call_slots.start);

        // Compile named arguments to a table (or nil if there is no named
        // arguments).
        if named_args.is_empty() {
            output.ad(KPRI, call_slots.start + 2, PRIM_NIL);
        } else {
            Self::compile_table(
                ctx,
                owning_unit,
                output,
                &Vec::new(),
                named_args,
                call_slots.start + 2,
            );
        }

        // Place positional arguments in the required slots
        for (i, positional_arg) in positional_args.iter().enumerate() {
            positional_arg.compile_as_value(
                ctx,
                owning_unit,
                output,
                call_slots.start + 3 + i as u8,
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
        match self {
            ConstantValue::Null => {
                emit_global_read(ctx, output, "<lkql_null>", result_slot);
            }
            ConstantValue::Unit => {
                emit_global_read(ctx, output, "<lkql_unit>", result_slot);
            }
            ConstantValue::Bool(value) => {
                output.ad(KPRI, result_slot, if *value { PRIM_TRUE } else { PRIM_FALSE });
            }
            ConstantValue::Int(value) => {
                let value_le_bytes = value.to_signed_bytes_le();

                // If the value is in the i16 bounds, we can emit a simple
                // immediate loading instruction.
                if value <= &BigInt::from(i16::MAX) && value >= &BigInt::from(i16::MIN) {
                    let mut le_bytes = [0 as u8; 2];
                    for i in 0..le_bytes.len() {
                        le_bytes[i] = *value_le_bytes.get(i).unwrap_or(&0);
                    }
                    output.ad(KSHORT, result_slot, u16::from_le_bytes(le_bytes));
                }
                // Else, we have to create a new numeric constant and load it
                else if let Some(numeric_constant) = self.to_numeric_constant() {
                    let value_cst = ctx
                        .current_data()
                        .constants
                        .get_from_numeric_constant(numeric_constant);
                    output.ad(KNUM, result_slot, value_cst);
                }
                // Else, we have to handle the number as a big integer
                else {
                    panic!("Big integers aren't handled for now");
                }
            }
            ConstantValue::String(value) => {
                let value_cst = ctx.current_data().constants.get_from_string(value);
                output.ad(KSTR, result_slot, value_cst);
            }
            ConstantValue::Tuple(values) | ConstantValue::List(values) => {
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
                output.ad(TDUP, result_slot, table_cst);

                // Now compute parts that cannot be expressed with table
                // constant elements.
                let outside_tmp = ctx.current_frame_mut().get_tmp();
                for (i, value) in remains {
                    value.compile(ctx, output, outside_tmp);
                    emit_table_index_write(ctx, output, outside_tmp, result_slot, i);
                }
                ctx.current_frame_mut().release_slot(outside_tmp);

                // TODO: Set the object meta-table accordingly to its type
            }
            ConstantValue::Object(items) => {
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
                output.ad(TDUP, result_slot, table_cst);

                // Add members that cannot be expressed through a table
                // constant element.
                let outside_tmp = ctx.current_frame_mut().get_tmp();
                for (field_name, field_value) in remains {
                    field_value.compile(ctx, output, outside_tmp);
                    emit_table_member_write(ctx, output, outside_tmp, result_slot, &field_name);
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
/// corresponding diagnosis in the compilation context.
fn declare_locals(ctx: &mut CompilationContext, symbols: &Vec<Identifier>) {
    for symbol in symbols {
        let maybe_local_slot = ctx.current_frame().is_conflicting(&symbol.text);
        if let Some(previous_binding) = maybe_local_slot {
            ctx.diagnostics.push(Report::duplicated_symbols(
                previous_binding.declaration_location,
                symbol.origin_location.clone(),
                &symbol.text,
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
    message: &str,
) {
    // Add constants in the current repository
    let message_cst = ctx.current_data().constants.get_from_string(message);

    // Reserve temporary slots, fill them and call the function
    let call_slots = ctx.current_frame_mut().reserve_contiguous_slots(3);
    emit_global_read(ctx, output, "error", call_slots.start);
    output.ad(KSTR, call_slots.end - 1, message_cst);
    output.abc(CALL, call_slots.start, 1, 2);

    // Free slots allocated for the call
    ctx.current_frame_mut().release_slots(call_slots);
}

/// Emit instructions to print the provided slot.
fn emit_debug_print(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    slot: u8,
) {
    // Generate instructions for the call
    let call_slots = ctx.current_frame_mut().reserve_contiguous_slots(3);
    emit_global_read(ctx, output, "print", call_slots.start);
    output.ad(MOV, call_slots.end - 1, slot as u16);
    output.abc(CALL, call_slots.start, 1, 2);

    // Free slots reserved for the call
    ctx.current_frame_mut().release_slots(call_slots);
}

/// Util functio to get a table element by its index.
fn emit_table_index_read(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    source_slot: u8,
    table_slot: u8,
    index: usize,
) {
    _access_table_index(ctx, output, source_slot, table_slot, index, TGETB, TGETV);
}

/// Util function to set a table element by its index.
fn emit_table_index_write(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    source_slot: u8,
    table_slot: u8,
    index: usize,
) {
    _access_table_index(ctx, output, source_slot, table_slot, index, TSETB, TSETV);
}

/// Internal function to factorize table index accesses. Do not use directly.
fn _access_table_index(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    working_slot: u8,
    table_slot: u8,
    index: usize,
    immediate_access_op: u8,
    slot_access_op: u8,
) {
    if index <= u8::MAX as usize {
        output.abc(immediate_access_op, working_slot, table_slot, index as u8);
    } else if index <= i32::MAX as usize {
        let index_tmp = ctx.current_frame.borrow_mut().get_tmp();
        let index_cst = ctx
            .exec_unit_data_stack
            .last_mut()
            .unwrap()
            .constants
            .get_from_int(index as i32);
        output.ad(KNUM, index_tmp, index_cst);
        output.abc(slot_access_op, working_slot, table_slot, index_tmp);
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
    target_slot: u8,
    table_slot: u8,
    member_name: &str,
) {
    _access_table_field(ctx, output, target_slot, table_slot, member_name, TGETS, TGETV);
}

/// Util function to set a table member (designated by a string constant). This
/// function is used to factorize [`TSETS`] emission and optimization.
fn emit_table_member_write(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    source_slot: u8,
    table_slot: u8,
    member_name: &str,
) {
    _access_table_field(ctx, output, source_slot, table_slot, member_name, TSETS, TSETV);
}

/// Internal function to factorize table field accesses. Do not use directly.
fn _access_table_field(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    working_slot: u8,
    table_slot: u8,
    member_name: &str,
    immediate_access_op: u8,
    slot_access_op: u8,
) {
    let member_name_cst = ctx.current_data().constants.get_from_string(member_name);
    if member_name_cst <= u8::MAX as u16 {
        output.abc(immediate_access_op, working_slot, table_slot, member_name_cst as u8);
    } else {
        let field_tmp = ctx.current_frame_mut().get_tmp();
        output.ad(KSTR, field_tmp, member_name_cst);
        output.abc(slot_access_op, working_slot, table_slot, field_tmp);
        ctx.current_frame_mut().release_slot(field_tmp);
    }
}

/// Util function used to generate a global table reading.
fn emit_global_read(
    ctx: &mut CompilationContext,
    output: &mut ExtendedInstructionBuffer,
    global_name: &str,
    result_slot: u8,
) {
    let global_name_cst = ctx.current_data().constants.get_from_string(global_name);
    output.ad(GGET, result_slot, global_name_cst);
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

    /// A collection of diagnosis collected during the compilation process, if
    /// it is not empty, it means that the result of the compilation may be
    /// semantically invalid.
    diagnostics: Vec<Report>,
}

impl CompilationContext {
    fn new() -> Self {
        Self {
            builtins: get_builtins().iter().map(|b| b.name).collect(),
            current_frame: Rc::new(RefCell::new(Frame::new(None))),
            exec_unit_data_stack: vec![ExecUnitCompilationData::new()],
            prototypes: Vec::new(),
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
}

/// This type contains all data required to compile an [`ExecutionUnit`] object
/// into a bytecode [`Prototype`].
struct ExecUnitCompilationData {
    has_child: bool,
    constants: ConstantRepository,
}

impl ExecUnitCompilationData {
    fn new() -> Self {
        Self { has_child: false, constants: ConstantRepository::new() }
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
        self.get_from_complex_constant(ComplexConstant::Child)
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
        Self::add_constant(constant, &mut self.complex_constants)
    }

    /// More generic function to get the index of a numeric constant.
    fn get_from_numeric_constant(&mut self, constant: NumericConstant) -> u16 {
        Self::add_constant(constant, &mut self.numeric_constants)
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
    fn add_constant<T: PartialEq>(constant: T, constant_vector: &mut Vec<T>) -> u16 {
        if let Some(index) = constant_vector.iter().position(|e| e == &constant) {
            index as u16
        } else {
            constant_vector.push(constant);
            assert!(constant_vector.len() <= u16::MAX as usize, "Too many constants");
            (constant_vector.len() - 1) as u16
        }
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

/// This type represents the concept of framing in the intermediate tree. This
/// is where local variables and up-values lives.
struct Frame {
    /// A reference to the frame that owns this one.
    parent_frame: Option<Rc<RefCell<Frame>>>,

    /// Slots of the frame, each one being associated with its name.
    bindings: HashMap<String, BoundSlot>,

    /// Variant part of the frame, containing additional information.
    variant: FrameVariant,
}

enum FrameVariant {
    /// The case when the frame is a semantic one, it means that it corresponds
    /// to a frame in the resulting bytecode.
    Semantic {
        /// Array representing all the slots in the current semantic frame,
        /// an array element is set to `true` when it is currently used.
        available_slots: [bool; u8::MAX as usize],

        /// Maximum number of slots that are occupied simultaneously.
        maximum_size: u8,

        /// Map of up-values available in this semantic frame.
        up_values: HashMap<String, UpValue>,

        /// The slot from which to close local values in this frame if this
        /// frame has to do it, [`None`] otherwise.
        close_from: Option<u8>,
    },

    /// The case when the frame is only existing in the intermediate
    /// representation. It is not kept in the resulting bytecode and all slots
    /// of this kind of frame are stored in the nearest parent frame that have
    /// the [`FrameVariant::Semantic`] kind.
    Lexical,
}

impl Frame {
    // --- Creation

    /// Create new empty semantic frame with an optional parent frame.
    fn new(parent_frame: Option<Rc<RefCell<Frame>>>) -> Self {
        Frame {
            parent_frame,
            bindings: HashMap::new(),
            variant: FrameVariant::Semantic {
                available_slots: [false; u8::MAX as usize],
                maximum_size: 0,
                up_values: HashMap::new(),
                close_from: None,
            },
        }
    }

    /// Create a new empty lexical frame with the given parent frame (a lexical
    /// frame can not exists without any parent).
    fn new_lexical(parent_frame: Rc<RefCell<Frame>>) -> Self {
        Frame {
            parent_frame: Some(parent_frame),
            bindings: HashMap::new(),
            variant: FrameVariant::Lexical,
        }
    }

    // --- Locals

    /// Get the slot associated to the provided name in the current semantic
    /// frame if any.
    fn get_local(&self, name: &str) -> Option<BoundSlot> {
        match &self.variant {
            FrameVariant::Semantic { .. } => self.bindings.get(name).cloned(),
            FrameVariant::Lexical => self
                .bindings
                .get(name)
                .cloned()
                .or(self.parent_frame().unwrap().get_local(name)),
        }
    }

    /// Get whether the provided name is conflicting with a local in the
    /// current frame, returning the associated slot if so.
    fn is_conflicting(&self, name: &str) -> Option<BoundSlot> {
        self.bindings.get(name).cloned()
    }

    /// Add a new local value to this frame, updating the already registered
    /// one if any.
    fn bind_local(&mut self, name: &str, declaration_location: &SourceSection) {
        let local_slot = self.reserve_contiguous_slots(1).start;
        self.bindings.insert(
            String::from(name),
            BoundSlot::new(declaration_location.clone(), local_slot),
        );
    }

    /// Mark a the local value designated by the provided name as initialized.
    fn init_local(&mut self, name: &str) {
        self.bindings.get_mut(name).unwrap().is_init = true;
    }

    /// Unbind the slot associated to the provided symbol. This function
    /// assumes that the provided symbol is already bound.
    fn release_local(&mut self, name: &str) {
        let old_binding = self.bindings.remove(name).unwrap();
        self.release_slot(old_binding.slot);
    }

    // --- Up-values

    /// Get the up-value associate to the provided name in the current semantic
    /// frame if any.
    fn get_up_value(&mut self, name: &str) -> Option<UpValue> where {
        // If we are a lexical frame, we delegate the request to our parent
        if matches!(self.variant, FrameVariant::Lexical) {
            return self.parent_frame_mut().unwrap().get_up_value(name);
        }

        // Now we know we are in a semantic frame, we start by looking in the
        // frame up-values.
        if let FrameVariant::Semantic { up_values, .. } = &self.variant {
            if let Some(up_value) = up_values.get(name) {
                return Some(up_value.clone());
            }
        }

        // If the up-value is not already in the frame ones, we try searching
        // in the parent frame.
        let new_up_value_index = self.next_up_value_index();
        let maybe_new_up_value = self.parent_frame_mut().and_then(|mut parent_frame| {
            // First look in the parent's locals
            if let Some(parent_slot) = parent_frame.get_local(name) {
                parent_frame.close_binding(name);
                Some(UpValue {
                    declaration_location: parent_slot.declaration_location.clone(),
                    index: new_up_value_index,
                    is_safe: parent_slot.is_init,
                    target: UpValueTarget::ParentSlot(parent_slot.slot),
                })
            }
            // Then, recursively looks in the parent's up-values
            else if let Some(parent_up_value) = parent_frame.get_up_value(name) {
                Some(UpValue {
                    declaration_location: parent_up_value.declaration_location.clone(),
                    index: new_up_value_index,
                    is_safe: parent_up_value.is_safe,
                    target: UpValueTarget::ParentUpValue(parent_up_value.index),
                })
            } else {
                None
            }
        });

        // If we found a result in the parent frame we add it into the
        // up-values map to cache the result.
        if let Some(new_up_value) = &maybe_new_up_value {
            match &mut self.variant {
                FrameVariant::Semantic { up_values, .. } => {
                    up_values.insert(String::from(name), new_up_value.clone());
                }
                _ => unreachable!(),
            }
        }

        // Finally, return the result
        maybe_new_up_value
    }

    /// Ge the next available up-value index, panicking if there is no more.
    fn next_up_value_index(&self) -> u8 {
        match &self.variant {
            FrameVariant::Semantic { up_values, .. } => {
                assert!(up_values.len() <= u8::MAX as usize, "Too many up-values");
                up_values.len() as u8
            }
            FrameVariant::Lexical => self.parent_frame().unwrap().next_up_value_index(),
        }
    }

    /// Update the frame information to close the provided slot.
    fn close_slot(&mut self, slot: u8) {
        match &mut self.variant {
            FrameVariant::Semantic { close_from, .. } => {
                if close_from.is_none() || close_from.unwrap() > slot {
                    let _ = close_from.insert(slot);
                }
            }
            FrameVariant::Lexical => self.parent_frame_mut().unwrap().close_slot(slot),
        }
    }

    /// Update the frame information to close the slot associated to the
    /// provided name. This function assumes that the binding is present in the
    /// current frame.
    fn close_binding(&mut self, name: &str) {
        // Update the binding closing kind
        {
            let binding = self.bindings.get_mut(name).unwrap();
            if !binding.is_init {
                binding.closing_kind = ClosingKind::Unsafe;
            } else if binding.closing_kind == ClosingKind::None {
                binding.closing_kind = ClosingKind::Safe;
            }
        }

        // Mark the bound slot as closed
        self.close_slot(self.bindings.get(name).unwrap().slot);
    }

    /// Emit, if required, the instruction to close local values in the current
    /// frame.
    fn emit_closing_instruction(&self, output: &mut ExtendedInstructionBuffer) {
        // If required, emit an instruction to close required slots
        match &self.variant {
            FrameVariant::Semantic { close_from, .. } => {
                if let Some(s) = close_from {
                    output.ad(UCLO, *s, JUMP_BIASING);
                }
            }
            FrameVariant::Lexical => self
                .parent_frame()
                .unwrap()
                .emit_closing_instruction(output),
        }
    }

    /// Emit instructions in provided output to initialize closed binding that
    /// requires it.
    fn emit_closed_bindings_init(&self, output: &mut ExtendedInstructionBuffer) {
        // Set bindings that are unsafely closed to nil
        for binding in self.bindings.values() {
            if binding.closing_kind == ClosingKind::Unsafe {
                output.insert_inst(0, Instruction::AD { a: binding.slot, d: PRIM_NIL, op: KPRI });
            }
        }
    }

    // --- Temporary values

    /// Get an unnamed temporary slot to store working values.
    fn get_tmp(&mut self) -> u8 {
        self.reserve_contiguous_slots(1).start
    }

    // --- Utils

    /// Get the parent frame of this one, if any.
    fn parent_frame(&self) -> Option<Ref<Frame>> {
        self.parent_frame.as_ref().map(|f| f.borrow())
    }

    /// Get the parent frame of this one as mutable, if any.
    fn parent_frame_mut(&self) -> Option<RefMut<Frame>> {
        self.parent_frame.as_ref().map(|f| f.borrow_mut())
    }

    /// Get the next available slot without flagging it as occupied.
    fn peek_next_slot(&self) -> u8 {
        match self.variant {
            FrameVariant::Semantic { available_slots, .. } => {
                available_slots
                    .iter()
                    .enumerate()
                    .filter(|s: &(usize, &bool)| !s.1)
                    .next()
                    .unwrap()
                    .0 as u8
            }
            FrameVariant::Lexical => self.parent_frame().unwrap().peek_next_slot(),
        }
    }

    /// Get a range of contiguous available slots, reserving all of them. This
    /// function panics if there are not enough available slots.
    fn reserve_contiguous_slots(&mut self, count: usize) -> Range<u8> {
        self.get_slots(count, true, true)
    }

    /// Release the provided slot, making it free to use.
    fn release_slot(&mut self, slot: u8) {
        self.release_slots(slot..slot + 1);
    }

    /// Release all slots in the provided range, making them free to use.
    fn release_slots(&mut self, slots: Range<u8>) {
        match &mut self.variant {
            FrameVariant::Semantic { available_slots, .. } => {
                slots.for_each(|i| available_slots[i as usize] = false)
            }
            FrameVariant::Lexical => self.parent_frame_mut().unwrap().release_slots(slots),
        }
    }

    /// Get a contiguous range of `count` available slots, updating the frame
    /// according to the function arguments.
    /// The returned range's end is exclusive, meaning that it is not reserved
    /// for use.
    fn get_slots(&mut self, count: usize, reserve: bool, update_size: bool) -> Range<u8> {
        match &mut self.variant {
            FrameVariant::Semantic { available_slots, maximum_size, .. } => {
                let mut start_bound = 0;
                for i in 0..available_slots.len() {
                    // We know that `start_bound..(i-1)` slots are available,
                    // we check if this is enough.
                    if i - start_bound == count {
                        if reserve {
                            for j in start_bound..i {
                                available_slots[j] = true;
                            }
                        }
                        if update_size && i as u8 > *maximum_size {
                            *maximum_size = i as u8;
                        }
                        return start_bound as u8..i as u8;
                    }

                    // If the `i`th slot is occupied, we move the cursor on it
                    if available_slots[i] {
                        start_bound = i + 1;
                    }
                }
                panic!("Too many local values");
            }
            FrameVariant::Lexical => self
                .parent_frame_mut()
                .unwrap()
                .reserve_contiguous_slots(count),
        }
    }
}

/// This type represents a frame slot that is bound to a lexical symbol.
#[derive(Debug, Clone)]
struct BoundSlot {
    declaration_location: SourceSection,
    slot: u8,
    is_init: bool,
    closing_kind: ClosingKind,
}

impl BoundSlot {
    fn new(declaration_location: SourceSection, slot: u8) -> Self {
        Self { declaration_location, slot, is_init: false, closing_kind: ClosingKind::None }
    }
}

/// This type represents ways a binding may be closed (marked as accessible for
/// children frames).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClosingKind {
    /// The binding is not closed at all.
    None,

    /// The binding has been closed after its initialization.
    Safe,

    /// The binding has been closed before its initialization.
    Unsafe,
}

/// This type holds information about an up-value in a frame.
#[derive(Debug, Clone)]
struct UpValue {
    /// Location of the declaration of the value ultimately targeted by this
    /// up-value.
    declaration_location: SourceSection,

    /// Index of the up-value in the current frame.
    index: u8,

    /// Whether the up-value is statically proved as initialized and can be
    /// safely read.
    is_safe: bool,

    /// What is the up-value targeting.
    target: UpValueTarget,
}

/// This type represents kinds of target that an up-value may have.
#[derive(Debug, Clone, Copy)]
enum UpValueTarget {
    /// When the up-value to read is a slot of the parent frame.
    ParentSlot(u8),

    /// When the up-value to read is an up-value of the parent frame.
    ParentUpValue(u8),
}
