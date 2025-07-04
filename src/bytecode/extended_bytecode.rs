//! # Extended bytecode module
//!
//! This module defines the "extended bytecode". This is an extension of the
//! bytecode defined in the [`crate::bytecode`] module with additional
//! instructions that are used by the compiler to ease code emission.
//! This extended bytecode can be easily translated to a LuaJIT compatible
//! instruction set.

use std::collections::HashMap;

use crate::bytecode::{Instruction, op_codes::JMP};

/// This type represents a buffer of extended instructions, it contains and
/// owns a [`Vec<ExtendedInstruction>`] that represents the buffer semantics.
pub struct ExtendedInstructionBuffer {
    instructions: Vec<ExtendedInstruction>,
    next_label: Label,
}

impl ExtendedInstructionBuffer {
    pub fn new() -> Self {
        Self { instructions: Vec::new(), next_label: 0 }
    }

    /// Get a brand new label.
    pub fn new_label(&mut self) -> Label {
        let res = self.next_label;
        self.next_label += 1;
        res
    }

    /// Add an instruction from the extended set in this buffer.
    pub fn add_ext(&mut self, inst: ExtendedInstruction) {
        self.instructions.push(inst);
    }

    /// Add a instruction from [`crate::bytecode::Instruction`] in this buffer.
    pub fn add_inst(&mut self, inst: Instruction) {
        self.instructions.push(ExtendedInstruction::Basic(inst));
    }

    /// Generate the instruction vector from this buffer of instructions from
    /// the extended set.
    pub fn to_instructions(&self) -> Vec<Instruction> {
        // Collect all labels in instructions
        let mut label_map: HashMap<usize, usize> = HashMap::new();
        let mut inst_index = 0;
        for inst in &self.instructions {
            match inst {
                ExtendedInstruction::Label(label) => {
                    label_map.insert(*label, inst_index);
                }
                _ => inst_index += 1,
            }
        }

        // Translate extended instructions to LuaJIT instructions
        let mut res = Vec::new();
        inst_index = 0;
        for inst in &self.instructions {
            match inst {
                ExtendedInstruction::Basic(instruction) => {
                    res.push(*instruction);
                    inst_index += 1;
                }
                ExtendedInstruction::Goto { label, next_available_slot } => {
                    let target_index = *label_map.get(label).expect("Unknown label");
                    let jump_offset = if inst_index > target_index {
                        Instruction::jump_backward((inst_index - target_index) as u16)
                    } else {
                        Instruction::jump_forward((target_index - inst_index) as u16)
                    };
                    res.push(Instruction::AD { a: *next_available_slot, d: jump_offset, op: JMP });
                    inst_index += 1;
                }
                _ => (),
            }
        }

        // Then, return the result
        res
    }
}

/// This type represents the extended set of instructions, able to represents
/// all LuaJIT instructions additionally to custom ones.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtendedInstruction {
    Basic(Instruction),
    Label(Label),
    Goto { label: Label, next_available_slot: u8 },
}

pub type Label = usize;

mod tests {
    use super::*;

    #[test]
    fn test_goto() {
        let mut extended_instructions = ExtendedInstructionBuffer::new();
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 0 });
        extended_instructions.add_ext(ExtendedInstruction::Label(0));
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 1 });
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 2 });
        extended_instructions
            .add_ext(ExtendedInstruction::Goto { label: 0, next_available_slot: 0 });
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 3 });
        extended_instructions
            .add_ext(ExtendedInstruction::Goto { label: 1, next_available_slot: 0 });
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 4 });
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 5 });
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 6 });
        extended_instructions.add_ext(ExtendedInstruction::Label(1));
        extended_instructions.add_inst(Instruction::AD { a: 0, d: 0, op: 7 });

        assert_eq!(
            extended_instructions.to_instructions(),
            vec![
                Instruction::AD { a: 0, d: 0, op: 0 },
                Instruction::AD { a: 0, d: 0, op: 1 },
                Instruction::AD { a: 0, d: 0, op: 2 },
                Instruction::AD { a: 0, d: 0x7FFD, op: JMP },
                Instruction::AD { a: 0, d: 0, op: 3 },
                Instruction::AD { a: 0, d: 0x8003, op: JMP },
                Instruction::AD { a: 0, d: 0, op: 4 },
                Instruction::AD { a: 0, d: 0, op: 5 },
                Instruction::AD { a: 0, d: 0, op: 6 },
                Instruction::AD { a: 0, d: 0, op: 7 },
            ]
        )
    }
}
