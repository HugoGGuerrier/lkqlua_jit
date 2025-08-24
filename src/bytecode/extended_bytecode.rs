//! # Extended bytecode module
//!
//! This module defines the "extended bytecode". This is an extension of the
//! bytecode defined in the [`crate::bytecode`] module with additional
//! instructions and information that are used by the compiler to ease code and
//! debug information emission.
//! This extended bytecode can be easily translated into a LuaJIT compatible
//! instruction buffer.

use std::collections::HashMap;

use crate::{
    bytecode::{Instruction, op_codes::JMP},
    sources::SourceSection,
};

/// This type represents a buffer of extended instructions, it owns a
/// [`Vec<ExtendedInstruction>`] that contains all instruction of the buffer.
pub struct ExtendedInstructionBuffer {
    instructions: Vec<ExtendedInstruction>,
    next_label: Label,
}

impl ExtendedInstructionBuffer {
    pub fn new() -> Self {
        Self { instructions: Vec::new(), next_label: 0 }
    }

    // --- Buffer modification methods

    /// Get a brand new label.
    pub fn new_label(&mut self) -> Label {
        let res = self.next_label;
        self.next_label += 1;
        res
    }

    /// Shortcut to add an [`ExtendedInstructionVariant::Goto`] instruction to
    /// the buffer.
    pub fn goto(&mut self, label: Label, next_available_slot: u8) {
        self.add_instruction(None, ExtendedInstructionVariant::Goto { label, next_available_slot });
    }

    /// Shortcut to add a label in the buffer. This label is applying to the
    /// next instruction to be added to this buffer.
    pub fn label(&mut self, label: Label) {
        self.add_instruction(None, ExtendedInstructionVariant::Label(label));
    }

    /// Shortcut to add an `ABC` bytecode instruction to this buffer.
    pub fn abc(&mut self, origin_location: &SourceSection, op_code: u8, a: u8, b: u8, c: u8) {
        self.add_instruction(
            Some(origin_location.clone()),
            ExtendedInstructionVariant::ABC { op_code, a, b, c },
        );
    }

    /// Shortcut to add an `ABC` bytecode instruction to this buffer with no
    /// location information.
    pub fn abc_no_loc(&mut self, op_code: u8, a: u8, b: u8, c: u8) {
        self.add_instruction(None, ExtendedInstructionVariant::ABC { op_code, a, b, c });
    }

    /// Shortcut to add an `ABC` bytecode instruction to this buffer with
    /// eventual location information.
    pub fn abc_maybe_loc(
        &mut self,
        maybe_origin_location: Option<&SourceSection>,
        op_code: u8,
        a: u8,
        b: u8,
        c: u8,
    ) {
        self.add_instruction(
            maybe_origin_location.cloned(),
            ExtendedInstructionVariant::ABC { op_code, a, b, c },
        );
    }

    /// Shortcut to add an `AD` bytecode instruction to this buffer.
    pub fn ad(&mut self, origin_location: &SourceSection, op_code: u8, a: u8, d: u16) {
        self.add_instruction(
            Some(origin_location.clone()),
            ExtendedInstructionVariant::AD { op_code, a, d },
        );
    }

    /// Shortcut to add an `AD` bytecode instruction to this buffer with no
    /// location information.
    pub fn ad_no_loc(&mut self, op_code: u8, a: u8, d: u16) {
        self.add_instruction(None, ExtendedInstructionVariant::AD { op_code, a, d });
    }

    /// Shortcut to add an `AD` bytecode instruction to this buffer with
    /// eventual location information.
    pub fn ad_maybe_loc(
        &mut self,
        maybe_origin_location: Option<&SourceSection>,
        op_code: u8,
        a: u8,
        d: u16,
    ) {
        self.add_instruction(
            maybe_origin_location.cloned(),
            ExtendedInstructionVariant::AD { op_code, a, d },
        );
    }

    /// Add a instruction from [`crate::bytecode::Instruction`] in this buffer.
    pub fn add_instruction(
        &mut self,
        origin_location: Option<SourceSection>,
        variant: ExtendedInstructionVariant,
    ) {
        self.instructions
            .push(ExtendedInstruction { origin_location, variant });
    }

    /// Insert the provided instruction to the buffer at the provided index.
    pub fn insert_instruction(&mut self, index: usize, instruction: ExtendedInstruction) {
        self.instructions.insert(index, instruction);
    }

    // --- Information extraction methods

    /// Generate the instruction vector from this buffer of instructions from
    /// the extended set.
    pub fn to_instructions(&self, owning_prototype_location: &SourceSection) -> Vec<Instruction> {
        // Collect all labels in instructions
        let label_map = self.label_map();

        // Translate extended instructions to LuaJIT instructions
        let mut res = Vec::new();
        let mut previous_source_line = owning_prototype_location.start.line;
        self.foreach_with_index(|index, inst| {
            // Start by getting the source line of the instruction to be
            // emitted.
            let inst_source_line = inst
                .origin_location
                .as_ref()
                .map_or(previous_source_line, |l| l.start.line);

            // Then, translate the extended instruction variant
            match &inst.variant {
                ExtendedInstructionVariant::ABC { op_code, a, b, c } => {
                    res.push(Instruction::abc(*op_code, *a, *b, *c, inst_source_line))
                }
                ExtendedInstructionVariant::AD { op_code, a, d } => {
                    res.push(Instruction::ad(*op_code, *a, *d, inst_source_line))
                }
                ExtendedInstructionVariant::Goto { label, next_available_slot } => {
                    let target_index = *label_map.get(label).expect("Unknown label");
                    let jump_offset = if index > target_index {
                        Instruction::jump_backward((index - target_index) as u16)
                    } else {
                        Instruction::jump_forward((target_index - index) as u16)
                    };
                    res.push(Instruction::ad(
                        JMP,
                        *next_available_slot,
                        jump_offset,
                        inst_source_line,
                    ));
                }
                _ => (),
            }

            // Finally, save the current source line
            previous_source_line = inst_source_line;
        });

        // Then, return the result
        res
    }

    /// Get the map associating each label to the index of the instruction
    /// the it is labeling.
    pub fn label_map(&self) -> HashMap<Label, usize> {
        let mut res: HashMap<Label, usize> = HashMap::new();
        self.foreach_with_index(|index, inst| match inst.variant {
            ExtendedInstructionVariant::Label(l) => {
                res.insert(l, index);
            }
            _ => (),
        });
        res
    }

    /// Apply the provided function on each instruction of this buffer. The
    /// "real" index of the instruction is also provided to the callback as
    /// first argument.
    fn foreach_with_index<F>(&self, mut f: F)
    where
        F: FnMut(usize, &ExtendedInstruction),
    {
        let mut instruction_index: usize = 0;
        for instruction in &self.instructions {
            // Call the function on the instruction
            f(instruction_index, instruction);

            // Increase the index
            match &instruction.variant {
                ExtendedInstructionVariant::Label(_) => (),
                _ => instruction_index += 1,
            }
        }
    }
}

/// This type represents an instruction from the extended instruction set. It
/// is able to represents all LuaJIT instructions and some higher level custom
/// concepts as "labels" and "goto".
/// It also contains the origin location of the instruction which is the source
/// section that this instruction is coming from. It used for error display and
/// debug purposes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExtendedInstruction {
    /// This field is an option because some instructions may not have any
    /// origin location.
    pub origin_location: Option<SourceSection>,

    pub variant: ExtendedInstructionVariant,
}

/// This type represents all variants of an extended instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExtendedInstructionVariant {
    ABC { op_code: u8, a: u8, b: u8, c: u8 },
    AD { op_code: u8, a: u8, d: u16 },
    Label(Label),
    Goto { label: Label, next_available_slot: u8 },
}

pub type Label = usize;

mod tests {
    #[allow(unused)]
    use super::*;
    #[allow(unused)]
    use crate::sources::Location;

    #[test]
    fn test_goto() {
        let mut extended_instructions = ExtendedInstructionBuffer::new();
        extended_instructions.ad_no_loc(0, 0, 0);
        extended_instructions.label(0);
        extended_instructions.ad_no_loc(1, 0, 0);
        extended_instructions.ad_no_loc(2, 0, 0);
        extended_instructions.goto(0, 0);
        extended_instructions.ad_no_loc(3, 0, 0);
        extended_instructions.goto(1, 0);
        extended_instructions.ad_no_loc(4, 0, 0);
        extended_instructions.ad_no_loc(5, 0, 0);
        extended_instructions.ad_no_loc(6, 0, 0);
        extended_instructions.label(1);
        extended_instructions.ad_no_loc(7, 0, 0);

        assert_eq!(
            extended_instructions.to_instructions(&SourceSection {
                source: String::from(""),
                start: Location { line: 42, col: 0 },
                end: Location { line: 43, col: 0 }
            }),
            vec![
                Instruction::ad(0, 0, 0, 42),
                Instruction::ad(1, 0, 0, 42),
                Instruction::ad(2, 0, 0, 42),
                Instruction::ad(JMP, 0, 0x7FFD, 42),
                Instruction::ad(3, 0, 0, 42),
                Instruction::ad(JMP, 0, 0x8003, 42),
                Instruction::ad(4, 0, 0, 42),
                Instruction::ad(5, 0, 0, 42),
                Instruction::ad(6, 0, 0, 42),
                Instruction::ad(7, 0, 0, 42),
            ]
        )
    }
}
