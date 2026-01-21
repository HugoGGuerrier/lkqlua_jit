//! # The runtime module
//!
//! This module host all data structures that are required to store information
//! during runtime and communicate with the LuaJIT engine.

use crate::lua::{
    LuaCFunction, LuaState, call, get_top, load_lua_code, move_top_value, push_c_closure,
    push_integer, push_string,
};
use serde::{Deserialize, Serialize};

pub mod engine;

/// String to represents an erroneous runtime value.
pub const ERROR_VALUE: &str = "##ERROR##";

/// Name of the global value where the LKQL execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "value@execution_context";

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
