//! # The runtime module
//!
//! This module host all data structures that are required to store information
//! during runtime and communicate with the LuaJIT engine.

use std::collections::HashMap;

use crate::sources::{SourceId, SourceSection};

/// This type contains all information collected during the compilation and
/// that may be required during the execution phase.
#[derive(Debug)]
pub struct RuntimeData {
    /// A map associating to each sources a set of prototype identifiers, each
    /// one being associated to a data storage.
    pub source_prototypes: HashMap<SourceId, HashMap<String, PrototypeData>>,
}

impl RuntimeData {
    pub fn new() -> Self {
        Self { source_prototypes: HashMap::new() }
    }

    /// Map the provided prototype identifier to the specified data.
    pub fn add_prototype_data(
        &mut self,
        source: &SourceId,
        prototype_id: String,
        instruction_locations: Vec<Option<SourceSection>>,
    ) {
        // Ensure the source is associated to an existing map
        if !self.source_prototypes.contains_key(source) {
            self.source_prototypes
                .insert(source.clone(), HashMap::new());
        }

        // Then store the provided prototype data
        self.source_prototypes
            .get_mut(source)
            .unwrap()
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
