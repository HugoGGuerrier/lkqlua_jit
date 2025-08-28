//! # Runtime Data module
//!
//! This module contains all entities required to store additional data
//! produced during the compilation process and used during the runtime.
//! All information are collected in a
//! [`crate::bytecode::extended_bytecode::ExtendedInstructionBuffer`] instance
//! and you can use it to create a new [`RuntimeData`] instance.

use std::{collections::HashMap, fmt::Debug};

use crate::sources::SourceSection;

/// This type contains all information that may be required during the runtime
/// of the associated source file.
#[derive(Debug)]
pub struct RuntimeData {
    /// A map associating each prototype identifier to its runtime data.
    pub prototype_data_map: HashMap<String, PrototypeData>,
}

impl RuntimeData {
    pub fn new() -> Self {
        Self { prototype_data_map: HashMap::new() }
    }

    /// Map the provided prototype identifier to the specified data.
    pub fn add_prototype_data(
        &mut self,
        prototype_id: String,
        instruction_locations: Vec<Option<SourceSection>>,
    ) {
        self.prototype_data_map
            .insert(prototype_id, PrototypeData { instruction_locations });
    }
}

/// This type contains all information related to a prototype that are required
/// during the runtime.
#[derive(Debug)]
pub struct PrototypeData {
    /// A vector of all instruction locations. The location at index x in this
    /// vector corresponds to the instruction at the same index in the
    /// prototype.
    pub instruction_locations: Vec<Option<SourceSection>>,
}
