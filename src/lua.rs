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

/// This type represents type tags for Lua values.
#[repr(C)]
pub enum LuaType {
    None = -1,
    Nil = 0,
    Boolean = 1,
    LightUserData = 2,
    Number = 3,
    String = 4,
    Table = 5,
    Function = 6,
    UserData = 7,
    Thread = 8,
}

// ----- Public API -----

/// Create a new [`LuaState`] and return it.
pub fn new_lua_state() -> LuaState {
    unsafe { luaL_newstate() }
}

/// Close the provided Lua state.
pub fn close_lua_state(l: LuaState) {
    unsafe { lua_close(l) }
}

/// Open all Lua base libraries in the given state.
pub fn open_lua_libs(l: LuaState) {
    unsafe { luaL_openlibs(l) }
}

/// Load the given buffer to the Lua stack as a callable value, returning
/// whether the function succeeded.
pub fn load_buffer(l: LuaState, buffer: &Vec<u8>, buffer_name: &str) -> bool {
    unsafe {
        let ext_buffer = buffer.as_ptr() as *const c_char;
        let ext_buffer_name = CString::from_str(buffer_name).unwrap();
        luaL_loadbuffer(l, ext_buffer, buffer.len(), ext_buffer_name.as_ptr()) == 0
    }
}

/// Get the type of the value on the stack at the given index.
pub fn get_type(l: LuaState, index: i32) -> LuaType {
    unsafe { lua_type(l, index) }
}

/// Get the value at the provided index as a boolean, implicitly converting
/// all values that aren't of the boolean type to one.
pub fn get_boolean(l: LuaState, index: i32) -> bool {
    unsafe { lua_toboolean(l, index) != 0 }
}

/// Try to get the value at the provided index as a string and return it. If
/// this is not possible, the result is [`None`].
pub fn get_string(l: LuaState, index: i32) -> Option<&'static str> {
    unsafe {
        let ext_res = lua_tolstring(l, index, ptr::null_mut());
        if ext_res.is_null() {
            None
        } else {
            Some(CStr::from_ptr(ext_res).to_str().unwrap())
        }
    }
}

/// Push a new C function value to the top of the Lua stack.
pub fn push_c_function(l: LuaState, function: LuaCFunction) {
    unsafe { lua_pushcclosure(l, function, 0) }
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

/// Considering that the stack is filled with `arg_count` arguments and a
/// callable value: pop all of those and call it. Place on the stack the
/// specified count of result if any, otherwise, place them all.
/// This function returns an [`Err`] containing the error message if an error
/// has been raised during the call.
pub fn call(l: LuaState, arg_count: i32, res_count: Option<i32>) -> Result<(), String> {
    unsafe {
        let call_res = lua_pcall(l, arg_count, res_count.unwrap_or(MUTRET), 0);
        if call_res == 0 { Ok(()) } else { Err(get_string(l, -1).unwrap().to_string()) }
    }
}

/// Call the provided meta-method on the object at the given index. If the
/// provided index is [`None`], the object on the top of the stack is used.
/// This function returns whether the call succeeded, and if so, the result
/// of the call is push on the top of the stack.
pub fn call_meta(l: LuaState, index: i32, meta_method: &str) -> bool {
    let ext_meta_method = CString::from_str(meta_method).unwrap();
    unsafe { luaL_callmeta(l, index, ext_meta_method.as_ptr()) == 1 }
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

    fn lua_type(l: LuaState, index: c_int) -> LuaType;
    fn lua_toboolean(l: LuaState, index: c_int) -> c_int;
    fn lua_tolstring(l: LuaState, index: c_int, result_size: *mut usize) -> *const c_char;

    fn lua_pushcclosure(l: LuaState, function: LuaCFunction, n: c_int);
    fn lua_setfield(l: LuaState, index: c_int, field: *const c_char);

    fn lua_pcall(l: LuaState, nargs: c_int, nres: c_int, errfunc: c_int) -> i32;
    fn luaL_callmeta(l: LuaState, obj: c_int, meta_method: *const c_char) -> c_int;
}
