//! # The runtime module
//!
//! This module host all data structures that are required to store information
//! during runtime and communicate with the LuaJIT engine.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    lua::{LuaCFunction, LuaState, push_c_function, push_integer, push_string},
    sources::{SourceId, SourceSection},
};

pub mod builtins;
pub mod engine;

/// The default image of a value when the latter doesn't define one.
pub const DEFAULT_VALUE_IMAGE: &str = "<lkql_value>";

/// Name of the global value where the execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "runtime@execution_context";

/// Pseudo-field to use to get the name of the type of a value.
pub const TYPE_NAME_FIELD: &str = "field@type_name";

/// Pseudo-field to use to get the tag of the type of a value.
pub const TYPE_TAG_FIELD: &str = "field@type_tag";

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

/// This type represents the runtime description of a type.
#[derive(Debug, Clone)]
pub struct RuntimeType {
    /// Name of the type as it should be displayed to the user.
    pub name: &'static str,

    /// Tag of the type, its unique identifier for optimized type checking.
    pub tag: isize,

    /// Fields in the type.
    pub fields: HashMap<String, RuntimeTypeField>,
}

/// This type represents a field that belongs to a [`RuntimeType`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeTypeField {
    /// If the field is a constant value.
    Value(RuntimeValue),

    /// If the field is computed value.
    Property(LuaCFunction),

    /// If the field is a method.
    Method(LuaCFunction),
}

/// This type represents a runtime value that can be pushed on a Lua state
/// stack.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeValue {
    Integer(isize),
    String(String),
    Function(LuaCFunction),

    /// Create the value by calling the associated Lua C function that should
    /// push it on the top of the stack.
    FromBuilder(RuntimeValueBuilder),
}

impl RuntimeValue {
    /// Push the value on the top of the stack in the provided Lua state.
    pub fn push_on_stack(&self, l: LuaState) {
        match self {
            RuntimeValue::Integer(i) => push_integer(l, *i),
            RuntimeValue::String(s) => push_string(l, &s),
            RuntimeValue::Function(f) => push_c_function(l, *f),
            RuntimeValue::FromBuilder(builder) => builder(l),
        }
    }
}

/// This type represents functions that take a Lua state as parameter, create
/// a new runtime value and push it on the top of the stack.
pub type RuntimeValueBuilder = fn(LuaState);

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

    /// The argument value is in a frame slot at execution time. This slot
    /// should be 0-indexed.
    LocalValue(u8),
}
