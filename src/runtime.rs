//! # The runtime module
//!
//! This module host all data structures that are required to store information
//! during runtime and communicate with the LuaJIT engine.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    lua::{
        LuaCFunction, LuaState, call, get_top, load_lua_code, move_top_value, push_c_closure,
        push_integer, push_string,
    },
    sources::{SourceId, SourceSection},
};

pub mod builtins;
pub mod engine;

/// The default image of a value when the latter doesn't define one.
pub const DEFAULT_VALUE_IMAGE: &str = "<lkql_value>";

/// String to represents an erroneous runtime value.
pub const ERROR_VALUE: &str = "##ERROR##";

/// Name of the global value where the LKQL execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "value@execution_context";

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
    pub source_prototypes: HashMap<SourceId, Vec<PrototypeData>>,
}

impl RuntimeData {
    pub fn new() -> Self {
        Self { source_prototypes: HashMap::new() }
    }

    /// Associate runtime data to a prototype in the provided source. A new
    /// identifier is created and returned and may be used to fetch those data
    /// later.
    pub fn add_prototype_data(
        &mut self,
        source: SourceId,
        instruction_locations: Vec<Option<SourceSection>>,
    ) -> usize {
        // Ensure the source is associated to an existing map
        if !self.source_prototypes.contains_key(&source) {
            self.source_prototypes.insert(source, Vec::new());
        }

        // Then store the provided prototype data
        let prototypes = self.source_prototypes.get_mut(&source).unwrap();
        prototypes.push(PrototypeData { instruction_locations });
        prototypes.len() - 1
    }

    /// Get the source location associated to the provided program counter in
    /// the prototype identified by the provided id.
    pub fn location_in_prototype(
        &self,
        source: SourceId,
        prototype_id: usize,
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

/// This trait is used to generalize the Lua value concept for all types that
/// may represent one.
pub trait LuaValue {
    /// Place the Lua value represented by the instance of this trait on the
    /// provided Lua state stack (at the top).
    fn push_on_stack(&self, l: LuaState);
}

/// This type represents a runtime value that can be pushed on a Lua state
/// stack.
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Integer(isize),
    String(String),
    Function(FunctionValue),

    /// Create the value by calling the associated Lua C function that should
    /// push it on the top of the stack.
    FromBuilder(RuntimeValueBuilder),
}

impl LuaValue for RuntimeValue {
    fn push_on_stack(&self, l: LuaState) {
        match self {
            RuntimeValue::Integer(i) => push_integer(l, *i),
            RuntimeValue::String(s) => push_string(l, &s),
            RuntimeValue::Function(f) => f.push_on_stack_with_uv(l, 0),
            RuntimeValue::FromBuilder(builder) => builder(l),
        }
    }
}

/// This type abstracts the function concept from the Lua engine perspective.
/// It may represents any executable, function-like runtime value.
#[derive(Debug, Clone)]
pub enum FunctionValue {
    /// In the case where the function value is implemented by a native
    /// function.
    CFunction(LuaCFunction),

    /// In the case where the function is implemented by a Lua function source.
    /// The string in this variant should contains a Lua function expression
    /// that is going to be parsed and the result is going to be used as
    /// runtime value.
    /// Inside this expression the table `__uv` is available to access function
    /// up-values.
    LuaFunction(&'static str),
}

impl LuaValue for FunctionValue {
    fn push_on_stack(&self, l: LuaState) {
        self.push_on_stack_with_uv(l, 0);
    }
}

impl FunctionValue {
    /// Place the runtime value representing this function on the top of the
    /// stack. Also, consider `up_value_count` as the number of values already
    /// on the stack to pop and place as function up-values.
    pub fn push_on_stack_with_uv(&self, l: LuaState, up_value_count: u8) {
        match self {
            FunctionValue::CFunction(function) => push_c_closure(l, *function, up_value_count),
            FunctionValue::LuaFunction(source) => {
                // Create the final Lua source
                let mut final_source = String::with_capacity(source.len());
                if up_value_count > 0 {
                    final_source.push_str("local __uv = {...}; ");
                }
                final_source.push_str("return ");
                final_source.push_str(source);

                // Parse the Lua function source and execute the parsing result
                load_lua_code(l, &final_source, "<lua_function>");
                move_top_value(l, get_top(l) - up_value_count as i32);
                call(l, up_value_count as i32, Some(1));
            }
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
    pub prototype_identifier: usize,
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
