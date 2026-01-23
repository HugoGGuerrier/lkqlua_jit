//! # LKQL core engine
//!
//! This module contains all required elements to run bytecode produced by the
//! [`crate::intermediate_tree::compilation`] module.

use crate::{
    ExecutionContext,
    builtins::{get_builtin_bindings, get_builtin_types},
    errors::{ERROR_TEMPLATE_REPOSITORY, ErrorInstance, ErrorInstanceArg, LUA_ENGINE_ERROR},
    lua::{
        LuaCFunction, LuaState, call, close_lua_state, debug_frame, debug_get_local,
        debug_get_source, debug_info, debug_proto_and_pc, get_string, get_top, load_buffer,
        load_lua_code, move_top_value, new_lua_state, open_lua_libs, pop, push_c_closure,
        push_c_function, push_integer, push_string, push_user_data, remove_value, safe_call,
        set_global, to_string,
    },
    report::Report,
    sources::SourceId,
};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::ffi::c_int;

/// String to represents an erroneous runtime value.
pub const ERROR_VALUE: &str = "##ERROR##";

/// Name of the global value where the LKQL execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "value@execution_context";

/// This type represents an engine to execute the bytecode generate by the
/// [`crate::intermediate_tree::compilation`] module.
pub struct Engine {
    lua_state: LuaState,
}

impl Drop for Engine {
    fn drop(&mut self) {
        close_lua_state(self.lua_state);
    }
}

impl Engine {
    /// Create a new engine and initialize it to be ready to run bytecode.
    pub fn new() -> Self {
        // Create a new Lua state
        let lua_state = new_lua_state();

        // Open Lua libraries
        open_lua_libs(lua_state);

        // Create all built-in types and store them
        for builtin_type in get_builtin_types() {
            builtin_type.place_in_lua_context(lua_state);
        }

        // Bind all built-in values to their names
        for builtin_binding in get_builtin_bindings() {
            builtin_binding.value.push_on_stack(lua_state);
            set_global(lua_state, builtin_binding.name);
        }

        // Finally create the engine type and return it
        Self { lua_state }
    }

    /// Run the given bytecode buffer in the engine, returning the potential
    /// report in case of a runtime error.
    pub fn run_bytecode(
        &self,
        ctx: &ExecutionContext,
        source_id: SourceId,
        encoded_bytecode_unit: &Vec<u8>,
    ) -> Result<(), Report> {
        // Place the execution context in the global Lua table
        push_user_data(self.lua_state, ctx);
        set_global(self.lua_state, CONTEXT_GLOBAL_NAME);

        // Set the error handler
        push_c_function(self.lua_state, handle_error);
        let error_handler = get_top(self.lua_state);

        // Load the bytecode buffer in the Lua state
        if !load_buffer(
            self.lua_state,
            encoded_bytecode_unit,
            &ctx.source_repo.get_name_by_id(source_id),
        ) {
            panic!(
                "Cannot load the provided bytecode buffer, error message: {}",
                get_string(self.lua_state, -1).unwrap_or("None")
            );
        }

        // Call the loaded buffer and analyze the result
        let call_res = safe_call(self.lua_state, 0, None, Some(error_handler));

        // Pop the error handler
        remove_value(self.lua_state, error_handler);

        // If there was an error during the execution, parse the error message
        // as JSON to extract all information.
        if let Err(encoded_error) = call_res {
            let runtime_error = RuntimeError::from_json(&encoded_error).unwrap();
            let error_location = runtime_error
                .stack_trace
                .iter()
                .find_map(|e| {
                    ctx.source_repo
                        .get_id_by_name(&e.source_name)
                        .and_then(|source| {
                            ctx.compilation_cache
                                .get(&source)
                                .and_then(|(bytecode_unit, _)| {
                                    bytecode_unit.prototypes[e.prototype_identifier]
                                        .instructions
                                        .get_location(e.program_counter)
                                })
                        })
                })
                .unwrap();
            Err(Report::from_error_template(
                error_location,
                ERROR_TEMPLATE_REPOSITORY[runtime_error.template_id],
                &runtime_error.message_args,
            ))
        }
        // Otherwise, just return the success
        else {
            Ok(())
        }
    }
}

// ----- Runtime error handling -----

/// Error handling function: it is responsible of turning a pure Lua error into
/// an correctly encoded error object containing all information required by
/// the LKQL [`Engine`] to display a beautiful error about the LKQL source.
#[unsafe(no_mangle)]
unsafe extern "C" fn handle_error(l: LuaState) -> c_int {
    // Start by computing the stack trace and fetching the current frame
    let mut stack_trace = Vec::new();
    let mut current_frame = None;
    let mut level = 0;
    let mut no_more_frame = false;
    while !no_more_frame {
        let maybe_frame = debug_frame(l, level);
        if let Some(mut frame) = maybe_frame {
            if debug_info(l, &mut frame, "S") {
                if let Some((proto_id, pc)) = debug_proto_and_pc(l, &mut frame) {
                    let source_name = debug_get_source(&frame).unwrap();
                    stack_trace.push(StackTraceElement {
                        source_name,
                        prototype_identifier: proto_id,
                        // We subtract 1 to the PC because Lua index
                        // instructions from 1.
                        program_counter: pc - 1,
                    });
                    if current_frame.is_none() {
                        let _ = current_frame.insert(frame);
                    }
                }
            }
            level += 1;
        } else {
            no_more_frame = true;
        }
    }

    // Then fetch the raw error message and start by extracting different parts
    // from it.
    let location_header_matcher = Regex::new(r"^\[.*\]:\d+: (.*)$").unwrap();
    let raw_error_message = to_string(l, get_top(l), "No error message");
    let error_message = if let Some(groups) = location_header_matcher.captures(raw_error_message) {
        groups.get(1).unwrap().as_str()
    } else {
        raw_error_message
    };

    // Then process the message part to get the runtime error instance
    let runtime_error = if let Some(runtime_error_instance) =
        ErrorInstance::from_json(error_message)
    {
        // If the message can be parsed as an error instance, we have to fetch
        // message arguments.
        let message_args = runtime_error_instance
            .message_args
            .into_iter()
            .map(|a| match a {
                ErrorInstanceArg::Static(s) => s,
                ErrorInstanceArg::LocalValue(index) => {
                    if let Some(_) =
                        debug_get_local(l, current_frame.as_ref().unwrap(), 1 + index as i32)
                    {
                        let res = String::from(to_string(l, -1, "<lkql_value>"));
                        pop(l, 1);
                        res
                    } else {
                        String::from(ERROR_VALUE)
                    }
                }
            })
            .collect::<Vec<_>>();

        // Then we create the runtime error
        RuntimeError { template_id: runtime_error_instance.template_id, message_args, stack_trace }
    } else {
        let (template_id, message_args) = parse_lua_error(error_message);
        RuntimeError { template_id, message_args, stack_trace }
    };

    // Finally, place the encoded runtime error on the stack as the function
    // result.
    push_string(l, &runtime_error.to_json_string());
    1
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
    pub prototype_identifier: usize,
    pub program_counter: usize,
}

/// Parse the error message coming from the Lua engine and map it to a custom
/// error.
fn parse_lua_error(error_message: &str) -> (usize, Vec<String>) {
    let mut final_message = error_message[..1].to_uppercase();
    final_message.push_str(&error_message[1..]);
    (LUA_ENGINE_ERROR.id, vec![final_message])
}

// ----- Runtime value API -----

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
