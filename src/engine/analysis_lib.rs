//! # LKQL analysis library
//!
//! This module contains all entities required to load an analysis library
//! that is going to be used by the LKQL engine to perform queries on the
//! analyzed code base.

use std::{ffi::c_int, i32};

use crate::{
    Config, builtins,
    errors::{ANALYSIS_LIBRARY_ERROR, ErrorInstance, ErrorInstanceArg},
    lua::{
        LuaState, copy_value, find_in_lua_path, get_field, get_global, get_string, load_lua_file,
        pop, push_c_function, push_nil, push_string, push_table, safe_call, set_field, set_global,
        set_index, set_metatable,
    },
};

pub const ANALYSIS_LIB_GLOBAL_NAME: &str = "value@analysis_lib";
pub const ANALYSIS_CONTEXT_GLOBAL_NAME: &str = "value@analysis_context";
pub const ANALYSIS_UNITS_GLOBAL_NAME: &str = "value@analysis_units";

/// This type represents a loaded Langkit Lua analysis library. It offers an
/// abstraction in the Rust world to access all useful information stored in
/// it.
#[derive(Debug)]
pub struct AnalysisLibrary {
    lua_state: LuaState,
}

impl AnalysisLibrary {
    /// Load a Langkit analysis library following the provided configuration
    /// and parse files to analyze.
    pub(super) fn new(lua_state: LuaState, config: &Config) -> Result<Self, Vec<String>> {
        // Util closure to call a function an transform potential errors into
        // an error value.
        let pcall = |arg_count: i32| -> Result<(), Vec<String>> {
            if let Err(msg) = safe_call(lua_state, arg_count, Some(1), None) {
                Err(vec![String::from(msg)])
            } else {
                Ok(())
            }
        };

        // Retrieve the analysis library source
        let module_name = format!("lib{}lang", config.analyzed_lang_name.to_lowercase());
        let module_file = match find_in_lua_path(&module_name) {
            Some(f) => f,
            None => {
                return Err(vec![format!(
                    "Cannot find the {} analysis library, please ensure \"{}\" is accessible \
                     through the LUA_PATH",
                    config.analyzed_lang_name,
                    format!("{module_name}.lua")
                )]);
            }
        };

        // Load the Lua analysis library
        load_lua_file(lua_state, &module_file);
        pcall(0)?;
        set_global(lua_state, ANALYSIS_LIB_GLOBAL_NAME);

        // Create a new analysis context and store it
        get_global(lua_state, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(lua_state, -1, "AnalysisContext");
        get_field(lua_state, -1, "create");
        pcall(0)?;
        set_global(lua_state, ANALYSIS_CONTEXT_GLOBAL_NAME);
        pop(lua_state, 1);

        // Parse all sources to analyze and store resulting units
        push_table(lua_state, 10, 0);
        get_global(lua_state, ANALYSIS_CONTEXT_GLOBAL_NAME);
        for (i, file) in config.files_to_analyze.iter().enumerate() {
            get_field(lua_state, -1, "get_unit_from_file");
            push_nil(lua_state);
            copy_value(lua_state, -3);
            push_string(lua_state, &file.to_string_lossy());
            pcall(3)?;
            set_index(lua_state, -3, (i + 1) as i32);
        }
        pop(lua_state, 1);
        get_global(lua_state, &builtins::types::list::IMPLEMENTATION.global_field_name());
        set_metatable(lua_state, -2);
        set_global(lua_state, ANALYSIS_UNITS_GLOBAL_NAME);

        // Register the analysis library error formatter
        get_field(lua_state, -1, "Exception");
        push_c_function(lua_state, analysis_lib_error_formatter);
        set_field(lua_state, -2, "format_exception_message");
        pop(lua_state, 1);

        // Return the new analysis library object
        Ok(Self { lua_state })
    }
}

/// Callback used to format an error from the analysis library
#[unsafe(no_mangle)]
unsafe extern "C" fn analysis_lib_error_formatter(l: LuaState) -> c_int {
    // Get kind and message of the error
    get_field(l, 1, "kind");
    get_field(l, 1, "message");

    // Create the error instance object
    let error_instance = ErrorInstance {
        template_id: ANALYSIS_LIBRARY_ERROR.id,
        message_args: vec![ErrorInstanceArg::Static(format!(
            "{} [{}]",
            get_string(l, -1).unwrap(),
            get_string(l, -2).unwrap()
        ))],
    };

    // Finally push the encoded error instance as a JSON string
    push_string(l, &error_instance.to_json_string());
    return 1;
}
