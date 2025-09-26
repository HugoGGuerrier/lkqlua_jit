//! # The runtime module
//!
//! This module host all data structures that are required to store information
//! during runtime and communicate with the LuaJIT engine.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::sources::{SourceId, SourceSection};

pub mod builtins;
pub mod engine;

/// The default image of a value when the latter doesn't define one.
pub const DEFAULT_VALUE_IMAGE: &'static str = "<lkql_value>";

/// Name of the global value where the execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "@execution_context";

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
        source: SourceId,
        prototype_id: String,
        instruction_locations: Vec<Option<SourceSection>>,
    ) {
        // Ensure the source is associated to an existing map
        if !self.source_prototypes.contains_key(&source) {
            self.source_prototypes.insert(source, HashMap::new());
        }

        // Then store the provided prototype data
        self.source_prototypes
            .get_mut(&source)
            .unwrap()
            .insert(prototype_id, PrototypeData { instruction_locations });
    }

    /// Get the source location associated to the provided program counter in
    /// the prototype identified by the provided id.
    pub fn location_in_prototype(
        &self,
        source: SourceId,
        prototype_id: &str,
        program_counter: usize,
    ) -> Option<&SourceSection> {
        self.source_prototypes
            .get(&source)?
            .get(prototype_id)
            .and_then(|data| {
                data.instruction_locations
                    .get(program_counter)
                    .map_or(None, |res| res.as_ref())
            })
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

/// This type represents an error that happened during the runtime. It contains
/// the error template and arguments for it, alongside the stack trace of the
/// error.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuntimeError {
    pub template_id: usize,
    pub message_args: Vec<String>,
    pub stack_trace: Vec<StackTraceElement>,
}

impl RuntimeError {
    /// Get a runtime error instance from a serialized JSON string.
    pub fn from_json(json: &str) -> Option<Self> {
        serde_json::from_str::<Self>(json).ok()
    }

    /// Get this runtime error serialized as JSON.
    pub fn to_json_string(&self) -> String {
        serde_json::to_string(self).unwrap()
    }
}

/// This type represents an element of a runtime stack trace.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StackTraceElement {
    pub source_name: String,
    pub prototype_identifier: String,
    pub program_counter: usize,
}

/// This type is used to represent a dynamic instantiation of an
/// [`crate::errors::ErrorTemplate`]. It is used to provide dynamic values as
/// parameters of the instantiated template.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DynamicError {
    pub template_id: usize,
    pub message_args: Vec<DynamicErrorArg>,
}

impl DynamicError {
    /// Get a runtime error instance from a serialized JSON string.
    pub fn from_json(json: &str) -> Option<Self> {
        serde_json::from_str::<Self>(json).ok()
    }

    /// Get this runtime error serialized as JSON.
    pub fn to_json_string(&self) -> String {
        serde_json::to_string(self).unwrap()
    }
}

/// This type represents a dynamic argument for an error template.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum DynamicErrorArg {
    /// The argument is known at compile time.
    Static(String),

    /// The argument value is in a frame slot at execution time.
    LocalValue(u8),
}
