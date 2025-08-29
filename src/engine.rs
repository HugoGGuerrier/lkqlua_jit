//! # LKQL core engine
//!
//! This module contains all required elements to run bytecode produced by the
//! [`crate::intermediate_tree::compilation`] module.

use crate::{
    builtins::get_builtins,
    lua::{
        LuaState, call, close_lua_state, get_string, load_buffer, new_lua_state, open_lua_libs,
        push_c_function, set_global,
    },
    report::Report,
};

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
    pub fn run_bytecode(&self, bytecode_buffer: &Vec<u8>) -> Result<(), Report> {
        // Load the bytecode buffer in the Lua state
        if !load_buffer(self.lua_state, bytecode_buffer, "<bytecode>") {
            panic!(
                "Cannot load the provided bytecode buffer, error message: {}",
                get_string(self.lua_state, -1).unwrap_or("None")
            );
        }

        // Call the loaded buffer and analyze the result
        call(self.lua_state, 0, None).map_err(|s| Report::error_msg(s))
    }
}
