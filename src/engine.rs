//! # LKQL core engine
//!
//! This module contains all required elements to run bytecode produced by the
//! [`crate::intermediate_tree::compilation`] module.

use crate::{
    Config, ExecutionContext,
    builtins::{get_builtin_bindings, get_builtin_types},
    bytecode::extended_bytecode::{ExtendedBytecodeUnit, ExtendedPrototype},
    diagnostics::{CallLocation, Diagnostic, DiagnosticCollector},
    engine::analysis_lib::AnalysisLibrary,
    errors::{ERROR_TEMPLATE_REPOSITORY, ErrorInstance, ErrorInstanceArg, LUA_ENGINE_ERROR},
    lua::{
        LuaState, call, close_lua_state, debug_frame, debug_get_local, debug_get_source,
        debug_info, debug_proto_and_pc, get_field, get_global, get_string, get_top, get_user_data,
        load_buffer, new_lua_state, open_lua_libs, pop, push_c_function, push_string,
        push_user_data, remove_value, safe_call, set_global, to_string,
    },
    runtime::CONTEXT_GLOBAL_NAME,
    sources::SourceSection,
};
use regex::Regex;
use std::ffi::c_int;

pub mod analysis_lib;

/// This type represents an engine to execute the bytecode generate by the
/// [`crate::intermediate_tree::compilation`] module.
#[derive(Debug)]
pub struct Engine {
    lua_state: LuaState,
    pub(crate) analysis_lib: AnalysisLibrary,
}

impl Drop for Engine {
    fn drop(&mut self) {
        close_lua_state(self.lua_state);
    }
}

impl Engine {
    /// Create a new engine and initialize it to be ready to run bytecode.
    pub fn new(config: &Config) -> Result<Self, DiagnosticCollector> {
        // Create a new Lua state
        let lua_state = new_lua_state();

        // Open Lua libraries
        open_lua_libs(lua_state);

        // Create all built-in types and store them
        let builtin_types = get_builtin_types();
        for builtin_type in &builtin_types.registered_types {
            builtin_type.place_in_lua_context(lua_state);
        }

        // Bind all built-in values to their names
        for (name, value) in get_builtin_bindings() {
            value.push_on_stack(lua_state);
            set_global(lua_state, name);
        }

        // Load the analysis library
        let analysis_lib = AnalysisLibrary::new(lua_state, config, &builtin_types)?;

        // Finally create the engine type and return it
        Ok(Self { lua_state, analysis_lib })
    }

    /// Run the given bytecode buffer in the engine, returning the potential
    /// diagnostic in case of a runtime error.
    pub fn run_bytecode(
        &self,
        ctx: &ExecutionContext,
        bytecode_unit: &ExtendedBytecodeUnit,
    ) -> Result<(), DiagnosticCollector> {
        let l = self.lua_state;

        // Encode the bytecode unit
        let mut encoded_bytecode_unit = Vec::new();
        bytecode_unit
            .to_bytecode_unit()
            .encode(&mut encoded_bytecode_unit);

        // Place the execution context in the global Lua table
        push_user_data(self.lua_state, ctx);
        set_global(self.lua_state, CONTEXT_GLOBAL_NAME);

        // Set the error handler
        push_c_function(self.lua_state, handle_error);
        let error_handler = get_top(self.lua_state);

        // Load the bytecode buffer in the Lua state
        if !load_buffer(
            self.lua_state,
            &encoded_bytecode_unit,
            ctx.source_repo
                .get_name_by_id(bytecode_unit.source)
                .unwrap(),
        ) {
            panic!(
                "Cannot load the provided bytecode buffer, error message: {}",
                get_string(self.lua_state, -1).unwrap_or("None")
            );
        }

        // Call the loaded buffer and analyze the result
        if ctx.config.do_profiling {
            get_global(l, "require");
            push_string(l, "jit.p");
            call(l, 1, None);
            get_field(l, -1, "start");
            push_string(l, "Fli5");
            call(l, 1, None);
            pop(l, 1);
        }
        let call_res = safe_call(self.lua_state, 0, None, Some(error_handler));
        if ctx.config.do_profiling {
            get_global(l, "require");
            push_string(l, "jit.p");
            call(l, 1, None);
            get_field(l, -1, "stop");
            call(l, 0, None);
            pop(l, 1);
        }

        // Pop the error handler
        remove_value(self.lua_state, error_handler);

        // If there was an error during the execution, parse diagnostics as
        // JSON and return them.
        if let Err(serialized_diagnostics) = call_res {
            Err(DiagnosticCollector::from_json(&serialized_diagnostics).unwrap())
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
extern "C" fn handle_error(l: LuaState) -> c_int {
    /// This type represents an element in the call stack
    struct TraceElement {
        source_id: usize,
        prototype_id: usize,
        program_counter: usize,
    }

    impl TraceElement {
        /// Get the [`ExtendedPrototype`] instance this trace element is
        /// leading to.
        fn get_prototype<'a>(&'a self, ctx: &'a ExecutionContext) -> &'a ExtendedPrototype {
            ctx.compilation_cache
                .get(&self.source_id)
                .unwrap()
                .prototypes
                .get(self.prototype_id)
                .unwrap()
        }

        /// Get the [`SourceSection`] object this trace element is
        /// representing.
        fn to_source_section(&self, ctx: &ExecutionContext) -> SourceSection {
            let current_prototype = self.get_prototype(ctx);
            *current_prototype
                .instructions
                .get_location_or_default(self.program_counter, &current_prototype.origin_location)
        }
    }

    // Create values to store location information
    let mut stack_trace = Vec::new();
    let mut current_frame_and_trace_element = None;

    // Unwind the execution stack
    let mut level = 0;
    let mut no_more_frame = false;
    while !no_more_frame {
        let maybe_frame = debug_frame(l, level);
        if let Some(mut frame) = maybe_frame {
            if debug_info(l, &mut frame, "S")
                && let Some((prototype_id, pc)) = debug_proto_and_pc(l, &mut frame)
                && let Ok(source_id) = debug_get_source(&frame).unwrap().parse::<usize>()
            {
                let trace_element =
                    TraceElement { source_id, prototype_id, program_counter: pc - 1 };
                if current_frame_and_trace_element.is_none() {
                    let _ = current_frame_and_trace_element.insert((frame, trace_element));
                } else {
                    stack_trace.push(trace_element);
                }
            }
            level += 1;
        } else {
            no_more_frame = true;
        }
    }

    // Then fetch the raw error message and start by extracting different parts
    // from it.
    let location_header_matcher = Regex::new(r"^.*:\d+: (.*)$").unwrap();
    let raw_error_message = to_string(l, get_top(l));
    let error_message = if let Some(groups) = location_header_matcher.captures(raw_error_message) {
        groups.get(1).unwrap().as_str()
    } else {
        raw_error_message
    };

    // Then process the message part to get the runtime error instance
    let diagnostics = if let Some(diagnostics) = DiagnosticCollector::from_json(error_message) {
        diagnostics
    } else {
        // Fetch the error template id and all arguments for it
        let (template_id, message_args) =
            if let Some(error_instance) = ErrorInstance::from_json(error_message) {
                // If the message can be parsed as an error instance, we have
                // to fetch error template arguments.
                let message_args = error_instance
                    .message_args
                    .into_iter()
                    .map(|a| match a {
                        ErrorInstanceArg::Static(s) => s,
                        ErrorInstanceArg::LocalValue(index) => {
                            if debug_get_local(
                                l,
                                &current_frame_and_trace_element.as_ref().unwrap().0,
                                1 + index as i32,
                            )
                            .is_some()
                            {
                                let res = String::from(to_string(l, -1));
                                pop(l, 1);
                                res
                            } else {
                                String::from("##ERROR##")
                            }
                        }
                    })
                    .collect::<Vec<_>>();
                (error_instance.template_id, message_args)
            } else {
                // If the error message is not a valid JSON, parse it with the
                // custom function.
                parse_lua_error(error_message)
            };

        // Get the execution context
        get_global(l, CONTEXT_GLOBAL_NAME);
        let execution_context = get_user_data::<ExecutionContext>(l, get_top(l)).unwrap();
        pop(l, 1);

        // Get the location of the diagnostic
        let diagnostic_location = current_frame_and_trace_element
            .unwrap()
            .1
            .to_source_section(execution_context);

        // Finally, create the diagnostic collector with the new diagnostic in
        // it.
        DiagnosticCollector::from(Diagnostic::from_error_template_with_stack_trace(
            &diagnostic_location,
            ERROR_TEMPLATE_REPOSITORY[template_id],
            &message_args,
            stack_trace
                .iter()
                .map(|e| {
                    CallLocation::new(
                        e.get_prototype(execution_context).name.clone(),
                        e.to_source_section(execution_context),
                    )
                })
                .collect(),
        ))
    };

    // Finally, place the encoded runtime error on the stack as the function
    // result.
    push_string(l, &diagnostics.to_json());
    1
}

/// Parse the error message coming from the Lua engine and map it to an LKQL
/// error.
fn parse_lua_error(error_message: &str) -> (usize, Vec<String>) {
    let mut final_message = error_message[..1].to_uppercase();
    final_message.push_str(&error_message[1..]);
    (LUA_ENGINE_ERROR.id, vec![final_message])
}
