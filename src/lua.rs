//! # Lua Rust API
//!
//! This module maps the Lua C API to Rust entry points. This allows to create
//! Lua contexts, modify them and run Lua code and buffers.
//! This module is designed to work with the LuaJIT implementation, thus it
//! offers specialized endpoints to tune the JIT compilation part.

use std::{
    ffi::{CStr, CString, c_char, c_int, c_void},
    ptr,
    str::FromStr,
};

// ----- Constants -----

/// Pseudo-index of the global table in the Lua stack.
const GLOBAL_INDEX: i32 = -10002;

/// Result count to pass to get all results.
const MUTRET: i32 = -1;

// ----- Public types -----

/// This type represents the state object for the LuaJIT engine.
pub type LuaState = *mut c_void;

/// A C function that is going to be called from the Lua environment.
pub type LuaCFunction = unsafe extern "C" fn(LuaState) -> c_int;

// ----- Public API -----

/// Create a new [`LuaState`] and return it.
pub fn new_lua_state() -> LuaState {
    unsafe { luaL_newstate() }
}

/// Close the provided Lua state.
pub fn close_lua_state(l: LuaState) {
    unsafe {
        lua_close(l);
    }
}

/// Open all Lua base libraries in the given state.
pub fn open_lua_libs(l: LuaState) {
    unsafe {
        luaL_openlibs(l);
    }
}

/// Load the given buffer to the Lua stack as a callable value, returning 0 on
/// success, and other value on failure.
pub fn load_buffer(l: LuaState, buffer: &Vec<u8>, buffer_name: &str) -> i32 {
    let ext_buffer = buffer.as_ptr() as *const c_char;
    let ext_buffer_name = CString::from_str(buffer_name).unwrap();
    unsafe { luaL_loadbuffer(l, ext_buffer, buffer.len(), ext_buffer_name.as_ptr()) }
}

/// Push a new C function value to the Lua stack at the given index. If the
/// provided index is [`None`], the value is pushed at the top of the stack.
pub fn push_c_function(l: LuaState, function: LuaCFunction, index: Option<i32>) {
    unsafe {
        lua_pushcclosure(l, function, index.unwrap_or(0));
    }
}

/// Considering that the stack is filled with `arg_count` arguments and a
/// callable value: pop all of those and call it. Place on the stack the
/// specified count of result if any, otherwise, place them all.
/// This function returns an [`Err`] containing the error message if an error
/// has been raised during the call.
pub fn call(l: LuaState, arg_count: i32, res_count: Option<i32>) -> Result<(), String> {
    unsafe {
        let call_res = lua_pcall(l, arg_count, res_count.unwrap_or(MUTRET), 0);
        if call_res == 0 {
            Ok(())
        } else {
            let ext_error_message = lua_tolstring(l, -1, ptr::null_mut());
            let wrapped_ext_error_message = CStr::from_ptr(ext_error_message);
            Err(String::from(wrapped_ext_error_message.to_string_lossy()))
        }
    }
}

/// Place the value currently on the top of the stack in the field of the
/// provided name in the global table.
/// This function pops the value at the top of the stack.
pub fn set_global(l: LuaState, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_setfield(l, GLOBAL_INDEX, c_name.as_ptr());
    }
}

// ----- External functions -----

unsafe extern "C" {
    fn luaL_newstate() -> LuaState;
    fn lua_close(l: LuaState);
    fn luaL_openlibs(l: LuaState);

    fn luaL_loadbuffer(
        l: LuaState,
        buffer: *const c_char,
        buffer_size: usize,
        buffer_name: *const c_char,
    ) -> c_int;

    fn lua_tolstring(l: LuaState, index: c_int, result_size: *mut usize) -> *const c_char;
    fn lua_pushcclosure(l: LuaState, function: LuaCFunction, index: c_int);
    fn lua_pcall(l: LuaState, nargs: c_int, nres: c_int, errfunc: c_int) -> i32;
    fn lua_setfield(l: LuaState, index: c_int, field: *const c_char);
}
