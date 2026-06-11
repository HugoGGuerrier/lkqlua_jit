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
        LuaCFunction, LuaState, call, close_lua_state, copy_value, debug_frame, debug_get_local,
        debug_get_source, debug_info, debug_proto_and_pc, get_field, get_global, get_metatable,
        get_string, get_top, get_user_data, load_buffer, load_lua_code, move_top_value,
        new_lua_state, open_lua_libs, pop, push_bool, push_c_closure, push_c_function,
        push_integer, push_string, push_user_data, remove_value, safe_call, set_field, set_global,
        to_string,
    },
    sources::SourceSection,
};
use regex::Regex;
use std::ffi::c_int;

pub mod analysis_lib;

/// String to represents an erroneous runtime value.
pub const ERROR_VALUE: &str = "##ERROR##";

/// Name of the global value where the LKQL execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "value@execution_context";

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
    let raw_error_message = to_string(l, get_top(l), "No error message");
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
                                let res = String::from(to_string(l, -1, "<lkql_value>"));
                                pop(l, 1);
                                res
                            } else {
                                String::from(ERROR_VALUE)
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
    FromBuilder(fn(LuaState)),
}

impl LuaValue for RuntimeValue {
    fn push_on_stack(&self, l: LuaState) {
        match self {
            RuntimeValue::Integer(i) => push_integer(l, *i),
            RuntimeValue::String(s) => push_string(l, s),
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

/// Util function to register the table at the provided index for garbage
/// collection. The meta-method "__gc" of the table is going to be called
/// when it becomes unreachable.
pub(crate) fn register_for_gc(l: LuaState, index: i32) {
    // Create the proxy object
    get_global(l, "newproxy");
    push_bool(l, true);
    call(l, 1, Some(1));

    // Get the metatable of the proxy object
    get_metatable(l, -1);

    // Create a closure to forward the GC call
    copy_value(l, -3);
    FunctionValue::LuaFunction("function(_) getmetatable(__uv[1]).__gc(__uv[1]) end")
        .push_on_stack_with_uv(l, 1);

    // Set the GC forwarder in the proxy metatable
    set_field(l, -2, "__gc");
    pop(l, 1);

    // Finally, place the proxy in the tracked table
    set_field(l, index - 1, "field@proxy");
}
