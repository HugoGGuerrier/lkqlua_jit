//! # LKQL core engine
//!
//! This module contains all required elements to run bytecode produced by the
//! [`crate::intermediate_tree::compilation`] module.

use std::{ffi::c_int, path::PathBuf, str::FromStr};

use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    builtins::get_builtins,
    engine::runtime::{RuntimeData, RuntimeErrorInstance},
    errors::{ERROR_TEMPLATE_REPOSITORY, LUA_ENGINE_ERROR},
    lua::{
        LuaState, call, close_lua_state, debug_frame, debug_get_local, debug_get_source,
        debug_info, debug_proto_and_pc, get_string, get_top, load_buffer, new_lua_state,
        open_lua_libs, push_c_function, push_string, remove_value, set_global, to_string,
    },
    report::Report,
    sources::{SourceId, SourceRepository},
};

pub mod runtime;

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

        // Set all built-in functions in the environment
        for builtin in get_builtins() {
            push_c_function(lua_state, builtin.c_function);
            set_global(lua_state, builtin.name);
        }

        // Finally create the engine type and return it
        Self { lua_state }
    }

    /// Run the given bytecode buffer in the engine, returning the potential
    /// report in case of a runtime error.
    pub fn run_bytecode(
        &self,
        source_id: SourceId,
        source_repo: &SourceRepository,
        bytecode_buffer: &Vec<u8>,
        runtime_data: &RuntimeData,
    ) -> Result<(), Report> {
        // Set the error handler
        push_c_function(self.lua_state, handle_error);
        let error_handler = get_top(self.lua_state);

        // Load the bytecode buffer in the Lua state
        if !load_buffer(
            self.lua_state,
            bytecode_buffer,
            &source_repo.get_source_by_id(source_id).unwrap().name,
        ) {
            panic!(
                "Cannot load the provided bytecode buffer, error message: {}",
                get_string(self.lua_state, -1).unwrap_or("None")
            );
        }

        // Call the loaded buffer and analyze the result
        let call_res = call(self.lua_state, 0, None, Some(error_handler));

        // Pop the error handler
        remove_value(self.lua_state, error_handler);

        // If there was an error during the execution, parse the error message
        // as JSON to extract all information.
        if let Err(encoded_error) = call_res {
            let runtime_error = serde_json::from_str::<RuntimeError>(&encoded_error).unwrap();
            let error_location = runtime_error
                .stack_trace
                .iter()
                .find_map(|e| {
                    source_repo
                        .get_id_by_name(&e.source_name)
                        .and_then(|source| {
                            runtime_data.location_in_prototype(
                                source,
                                &e.prototype_identifier,
                                e.program_counter,
                            )
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
                if let Some((prototype_name, pc)) = debug_proto_and_pc(l, &mut frame) {
                    let source_name = debug_get_source(&frame).unwrap();
                    let prototype_identifier = if source_name == prototype_name {
                        let source_path = PathBuf::from_str(source_name.as_str()).unwrap();
                        String::from(source_path.file_stem().unwrap().to_str().unwrap())
                    } else {
                        String::from(prototype_name)
                    };
                    stack_trace.push(StackTraceElement {
                        source_name,
                        prototype_identifier,
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
    let raw_error_parser = Regex::new(r"^\[.*\]:\d+: (.*)$").unwrap();
    let raw_error_message = to_string(l, get_top(l), "[]:0: No error message");
    let error_message;
    if let Some(groups) = raw_error_parser.captures(raw_error_message) {
        error_message = groups.get(1).unwrap().as_str();
    } else {
        panic!("Unexpected Lua error message: {}", raw_error_message);
    }

    // Then process the message part to get the runtime error instance
    let runtime_error = if let Ok(runtime_error_instance) =
        serde_json::from_str::<RuntimeErrorInstance>(error_message)
    {
        // If the message can be parsed as an error instance, we have to fetch
        // message arguments.
        let message_args = runtime_error_instance
            .message_args
            .into_iter()
            .map(|a| match a {
                runtime::RuntimeErrorInstanceArg::Static(s) => s,
                runtime::RuntimeErrorInstanceArg::LocalValue(index) => {
                    let _ =
                        debug_get_local(l, current_frame.as_ref().unwrap(), index as i32).unwrap();
                    String::from(to_string(l, -1, "<lkql_value>"))
                }
            })
            .collect::<Vec<_>>();

        // Then we create the runtime error
        RuntimeError {
            template_id: runtime_error_instance.template_id,
            message_args,
            stack_trace: stack_trace,
        }
    } else {
        let (template_id, message_args) = parse_lua_error(error_message);
        RuntimeError { template_id, message_args, stack_trace }
    };

    // Finally, place the encoded runtime error on the stack as the function
    // result.
    push_string(l, &serde_json::to_string(&runtime_error).unwrap());
    1
}

/// Parse the error message coming from the Lua engine and map it to a custom
/// error.
fn parse_lua_error(error_message: &str) -> (usize, Vec<String>) {
    let mut final_message = error_message[..1].to_uppercase();
    final_message.push_str(&error_message[1..]);
    (LUA_ENGINE_ERROR.id, vec![final_message])
}

/// This type represents an error that happened during the runtime. It contains
/// the error template and arguments for it, alongside the stack trace of the
/// error.
#[derive(Debug, Serialize, Deserialize)]
struct RuntimeError {
    template_id: usize,
    message_args: Vec<String>,
    stack_trace: Vec<StackTraceElement>,
}

/// This type represents an element of a runtime stack trace.
#[derive(Debug, Serialize, Deserialize)]
struct StackTraceElement {
    source_name: String,
    prototype_identifier: String,
    program_counter: usize,
}
