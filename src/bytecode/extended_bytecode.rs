//! # Extended bytecode module
//!
//! This module defines the "extended bytecode". This is an extension of the
//! bytecode defined in the [`crate::bytecode`] module with additional
//! instructions and information that are used by the compiler to ease code and
//! debug information emission.
//! This extended bytecode can be easily translated into a LuaJIT compatible
//! instruction buffer.

use crate::{
    bytecode::{
        BytecodeUnit, ComplexConstant, Instruction, NumericConstant, Prototype, UpValue,
        VariableData,
        op_codes::{JMP, LOOP},
    },
    sources::{SourceId, SourceSection},
};
use std::collections::HashMap;

/// This type represents a label for a bytecode instruction.
pub type Label = usize;

/// This type represents a unit of extended bytecode. It is composed of several
/// prototypes and a source identifier.
pub struct ExtendedBytecodeUnit {
    /// Identifier of the source this buffer is coming from.
    pub source: SourceId,

    /// Prototypes in this bytecode buffer.
    pub prototypes: Vec<ExtendedPrototype>,
}

impl ExtendedBytecodeUnit {
    /// Create a new [`crate::bytecode::BytecodeUnit`] instance with data
    /// contained in this extended bytecode buffer.
    pub fn to_bytecode_unit(&self) -> BytecodeUnit {
        BytecodeUnit {
            prototypes: self.prototypes.iter().map(|p| p.to_prototype()).collect(),
            source_name: String::from(self.source.to_string()),
        }
    }
}

/// This type represents a LuaJIT prototype with some additional information
/// compared to the [`crate::bytecode::Prototype`] type. This is used to
/// collect and store information required at runtime to execute the code
/// represented by this prototype.
pub struct ExtendedPrototype {
    // --- Flags
    pub has_child: bool,
    pub is_variadic: bool,
    pub has_ffi: bool,

    // --- Prototype specification
    pub arg_count: usize,
    pub frame_size: u8,

    // --- Instructions
    pub instructions: ExtendedInstructionBuffer,

    // --- Up-values and constants
    pub up_values: Vec<UpValue>,
    pub complex_consts: Vec<ComplexConstant>,
    pub numeric_consts: Vec<NumericConstant>,

    // --- Debug information
    pub origin_location: SourceSection,
    pub variable_data: Vec<ExtendedVariableData>,
}

impl ExtendedPrototype {
    pub fn to_prototype(&self) -> Prototype {
        // Perform some assertions to crash if there is too much elements
        assert!(self.arg_count <= u8::MAX as usize, "To much arguments in prototype");

        // Get the first line of the prototype
        let first_line = self.origin_location.start.line;

        // Then compute all variables data
        let label_map = self.instructions.label_map();
        let mut variable_data: Vec<VariableData> = self
            .variable_data
            .iter()
            .map(|d| d.to_variable_data(&label_map))
            .collect();
        variable_data.sort_by_key(|d| d.birth_instruction);

        // Finally return the new bytecode prototype
        Prototype {
            has_child: self.has_child,
            is_variadic: self.is_variadic,
            has_ffi: self.has_ffi,
            arg_count: self.arg_count as u8,
            frame_size: self.frame_size as u8,
            instructions: self.instructions.to_instructions(&self.origin_location),
            up_values: self.up_values.clone(),
            complex_consts: self.complex_consts.clone(),
            numeric_consts: self.numeric_consts.clone(),
            first_line,
            line_count: self.origin_location.end.line - first_line,
            variable_data,
        }
    }
}

/// This type represents a buffer of extended instructions, it owns a
/// [`Vec<ExtendedInstruction>`] that contains all instruction of the buffer.
#[derive(Clone)]
pub struct ExtendedInstructionBuffer {
    instructions: Vec<ExtendedInstruction>,
    next_label: Label,
}

impl<'a> IntoIterator for &'a ExtendedInstructionBuffer {
    type Item = (usize, &'a ExtendedInstruction);
    type IntoIter = IntoIter<'a>;

    /// Create an iterator on this extended instructions buffer. This iterator
    /// also provide the index of the current instruction as first element.
    fn into_iter(self) -> Self::IntoIter {
        IntoIter { iterated_instructions: self.instructions.iter(), current_index: 0 }
    }
}

/// Structure used to represents an iterator on a [`ExtendedBytecodeBuffer`].
/// This iterates on each instruction with its associated final index.
pub struct IntoIter<'a> {
    iterated_instructions: std::slice::Iter<'a, ExtendedInstruction>,
    current_index: usize,
}

impl<'a> Iterator for IntoIter<'a> {
    type Item = (usize, &'a ExtendedInstruction);

    fn next(&mut self) -> Option<Self::Item> {
        self.iterated_instructions.next().map(|i| {
            let index_increment = match i.variant {
                ExtendedInstructionVariant::Label(_) => 0,
                _ => 1,
            };
            let res = (self.current_index, i);
            self.current_index += index_increment;
            res
        })
    }
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

    /// Shortcut to add an [`ExtendedInstructionVariant::Loop`] instruction to
    /// the buffer.
    pub fn loop_(&mut self, label: Label, next_available_slot: u8) {
        self.add_instruction(None, ExtendedInstructionVariant::Loop { label, next_available_slot });
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

    /// Get a newly allocated vector with all LuaJIT instructions stored in
    /// this extended bytecode buffer.
    pub fn to_instructions(&self, owning_prototype_location: &SourceSection) -> Vec<Instruction> {
        // Get the map associating each label to an instruction index
        let label_map = self.label_map();

        // Split instructions and locations in separated vectors
        let mut instructions = Vec::new();
        let mut previous_source_line = owning_prototype_location.start.line;
        let mut index = 0;
        for inst in &self.instructions {
            // Start by getting the source line of the instruction to be
            // emitted.
            let inst_source_line = inst
                .origin_location
                .as_ref()
                .map_or(previous_source_line, |l| l.start.line);

            // Then, translate the extended instruction variant to a pure
            // LuaJIT instruction.
            match &inst.variant {
                ExtendedInstructionVariant::ABC { op_code, a, b, c } => {
                    instructions.push(Instruction::abc(*op_code, *a, *b, *c, inst_source_line));
                    index += 1;
                }
                ExtendedInstructionVariant::AD { op_code, a, d } => {
                    instructions.push(Instruction::ad(*op_code, *a, *d, inst_source_line));
                    index += 1;
                }
                ExtendedInstructionVariant::Goto { label, next_available_slot }
                | ExtendedInstructionVariant::Loop { label, next_available_slot } => {
                    let target_index = *label_map.get(label).expect("Unknown label");
                    let jump_offset = if index > target_index {
                        Instruction::jump_backward((index - target_index) as u16)
                    } else {
                        Instruction::jump_forward((target_index - index) as u16)
                    };
                    instructions.push(Instruction::ad(
                        match &inst.variant {
                            ExtendedInstructionVariant::Goto { .. } => JMP,
                            ExtendedInstructionVariant::Loop { .. } => LOOP,
                            _ => unreachable!(),
                        },
                        *next_available_slot,
                        jump_offset,
                        inst_source_line,
                    ));
                    index += 1;
                }
                ExtendedInstructionVariant::Label(_) => (),
            }

            // Finally, save the current source line
            previous_source_line = inst_source_line;
        }

        // Then, return the result
        instructions
    }

    /// Get the map associating each label to the index of the instruction
    /// the it is labeling.
    pub fn label_map(&self) -> HashMap<Label, usize> {
        let mut res: HashMap<Label, usize> = HashMap::new();
        self.into_iter()
            .for_each(|(index, inst)| match inst.variant {
                ExtendedInstructionVariant::Label(l) => {
                    res.insert(l, index);
                }
                _ => (),
            });
        res
    }

    /// Get the location of the instruction in this prototype at the provided
    /// program counter index. This function only considers instructions that
    /// may end in the final [`crate::bytecode::Prototype`] for indexing. See
    /// [`Self::to_instructions`] for more information.
    pub fn get_location(&self, program_counter: usize) -> Option<&SourceSection> {
        self.into_iter().find_map(|(index, instruction)| {
            if index == program_counter
                && !matches!(&instruction.variant, ExtendedInstructionVariant::Label(_))
            {
                instruction.origin_location.as_ref()
            } else {
                None
            }
        })
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
    ABC {
        op_code: u8,
        a: u8,
        b: u8,
        c: u8,
    },
    AD {
        op_code: u8,
        a: u8,
        d: u16,
    },

    /// This instruction variant represents a label in the instruction flow
    /// that may be referred to.
    /// The instruction following this label instruction is considered as
    /// labeled by it.
    Label(Label),

    /// Goto a specific label.
    Goto {
        label: Label,
        next_available_slot: u8,
    },

    /// Flag the start of a loop in the bytecode. This is used to allow the JIT
    /// compiled to perform tracing on the emitted bytecode.
    Loop {
        label: Label,
        next_available_slot: u8,
    },
}

/// This type represents data collected during the compilation for a local
/// variable, this is used for debug information emission.
pub struct ExtendedVariableData {
    /// Debug name of the local variable.
    pub name: String,

    /// Label of the instruction from which this variable becomes available.
    pub birth_label: Label,

    /// Label of the instruction this variable dies at.
    pub death_label: Label,
}

impl ExtendedVariableData {
    /// Get a [`crate::bytecode::VariableData`] instance from this extended
    /// variable data holder.
    fn to_variable_data(&self, label_map: &HashMap<usize, usize>) -> VariableData {
        VariableData {
            name: self.name.clone(),
            birth_instruction: *label_map.get(&self.birth_label).unwrap(),
            death_instruction: *label_map.get(&self.death_label).unwrap(),
        }
    }
}

mod tests {
    #[allow(unused)]
    use super::*;
    #[allow(unused)]
    use crate::bytecode::UpValueVariant;
    #[allow(unused)]
    use crate::sources::Location;
    #[allow(unused)]
    use crate::sources::SourceRepository;

    #[test]
    fn test_extended_bytecode() {
        // Create a source repository and add a dummy source
        let mut source_repo = SourceRepository::new();
        let dummy_source = source_repo.add_source_buffer("<my_source>", "# This is my content");
        let dummy_loc = SourceSection {
            source: dummy_source,
            start: Location { line: 1, col: 1 },
            end: Location { line: 3, col: 1 },
        };

        // Create dummy instructions
        let mut extended_instructions = ExtendedInstructionBuffer::new();
        extended_instructions.ad_no_loc(0, 0, 0);
        extended_instructions.label(0);
        extended_instructions.ad_no_loc(1, 0, 0);
        extended_instructions.ad_no_loc(2, 0, 0);
        extended_instructions.goto(0, 0);
        extended_instructions.ad(
            &SourceSection {
                source: dummy_source,
                start: Location { line: 2, col: 2 },
                end: Location { line: 3, col: 4 },
            },
            3,
            0,
            0,
        );
        extended_instructions.goto(1, 0);
        extended_instructions.ad_no_loc(4, 0, 0);
        extended_instructions.ad_no_loc(5, 0, 0);
        extended_instructions.ad_no_loc(6, 0, 0);
        extended_instructions.label(1);
        extended_instructions.ad_no_loc(7, 0, 0);
        let instructions = vec![
            Instruction::ad(0, 0, 0, 1),
            Instruction::ad(1, 0, 0, 1),
            Instruction::ad(2, 0, 0, 1),
            Instruction::ad(JMP, 0, 0x7FFD, 1),
            Instruction::ad(3, 0, 0, 2),
            Instruction::ad(JMP, 0, 0x8003, 2),
            Instruction::ad(4, 0, 0, 2),
            Instruction::ad(5, 0, 0, 2),
            Instruction::ad(6, 0, 0, 2),
            Instruction::ad(7, 0, 0, 2),
        ];

        // Test instruction location fetching
        assert_eq!(
            extended_instructions.get_location(4),
            Some(&SourceSection {
                source: dummy_source,
                start: Location { line: 2, col: 2 },
                end: Location { line: 3, col: 4 },
            }),
        );
        assert_eq!(extended_instructions.get_location(0), None);
        assert_eq!(extended_instructions.get_location(5), None);

        // Test bytecode buffers
        let bytecode_buffer_1 = ExtendedBytecodeUnit { source: dummy_source, prototypes: vec![] };
        assert_eq!(
            bytecode_buffer_1.to_bytecode_unit(),
            BytecodeUnit {
                prototypes: vec![],
                source_name: String::from(source_repo.get_name_by_id(dummy_source)),
            },
        );

        // Test prototypes
        let prototype_1 = ExtendedPrototype {
            has_child: false,
            is_variadic: false,
            has_ffi: false,
            arg_count: 1,
            frame_size: 2,
            instructions: ExtendedInstructionBuffer::new(),
            up_values: vec![],
            complex_consts: vec![],
            numeric_consts: vec![],
            origin_location: dummy_loc.clone(),
            variable_data: vec![],
        };
        assert_eq!(
            prototype_1.to_prototype(),
            Prototype {
                has_child: false,
                is_variadic: false,
                has_ffi: false,
                arg_count: 1,
                frame_size: 2,
                instructions: vec![],
                up_values: vec![],
                complex_consts: vec![],
                numeric_consts: vec![],
                first_line: 1,
                line_count: 2,
                variable_data: vec![],
            }
        );

        let prototype_2 = ExtendedPrototype {
            has_child: true,
            is_variadic: true,
            has_ffi: true,
            arg_count: 0,
            frame_size: 0,
            instructions: extended_instructions.clone(),
            up_values: vec![UpValue {
                name: String::from("uv_name"),
                variant: UpValueVariant::ParentLocalSlot(10),
            }],
            complex_consts: vec![ComplexConstant::Integer(11)],
            numeric_consts: vec![NumericConstant::Float(12.0)],
            origin_location: dummy_loc.clone(),
            variable_data: vec![ExtendedVariableData {
                name: String::from("local_name"),
                birth_label: 0,
                death_label: 1,
            }],
        };
        assert_eq!(
            prototype_2.to_prototype(),
            Prototype {
                has_child: true,
                is_variadic: true,
                has_ffi: true,
                arg_count: 0,
                frame_size: 0,
                instructions: instructions.clone(),
                up_values: vec![UpValue {
                    name: String::from("uv_name"),
                    variant: UpValueVariant::ParentLocalSlot(10),
                }],
                complex_consts: vec![ComplexConstant::Integer(11)],
                numeric_consts: vec![NumericConstant::Float(12.0)],
                first_line: 1,
                line_count: 2,
                variable_data: vec![VariableData {
                    name: String::from("local_name"),
                    birth_instruction: 1,
                    death_instruction: 9,
                }],
            }
        );
    }
}
