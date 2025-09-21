//! # Lua Rust API
//!
//! This module maps the Lua C API to Rust entry points. This allows to create
//! Lua contexts, modify them and run Lua code and buffers.
//! This module is designed to work with the LuaJIT implementation, thus it
//! offers specialized endpoints to tune the JIT compilation part.

use std::{
    ffi::{CStr, CString, c_char, c_int, c_uint, c_void},
    ptr,
    str::FromStr,
};

// ----- Constants -----

/// Pseudo-index of the global table in the Lua stack.
const GLOBAL_INDEX: i32 = -10002;

/// Result count to pass to get all results.
const MUTRET: i32 = -1;

/// Size of short source representation in debug data.
const SHORT_SRC_SIZE: usize = 60;

// ----- Public types -----

/// This type represents the state object for the LuaJIT engine.
pub type LuaState = *mut c_void;

/// A C function that is going to be called from the Lua environment.
pub type LuaCFunction = unsafe extern "C" fn(LuaState) -> c_int;

/// This type represents type tags for Lua values.
#[repr(C)]
#[derive(Debug)]
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

/// This type maps the `Lua_Debug` C type. It contains debugging information
/// about a frame.
#[repr(C)]
#[derive(Debug)]
pub struct LuaDebug {
    pub event: c_int,
    pub name: *const c_char,
    pub name_what: *const c_char,
    pub what: *const c_char,
    pub source: *const c_char,
    pub current_line: c_int,
    pub up_value_count: c_int,
    pub line_defined: c_int,
    pub last_line_defined: c_int,
    pub short_src: [c_char; SHORT_SRC_SIZE],
    pub i_ci: c_int,
}

impl LuaDebug {
    fn new() -> Self {
        Self {
            event: 0,
            name: ptr::null(),
            name_what: ptr::null(),
            what: ptr::null(),
            source: ptr::null(),
            current_line: 0,
            up_value_count: 0,
            line_defined: 0,
            last_line_defined: 0,
            short_src: [0; SHORT_SRC_SIZE],
            i_ci: 0,
        }
    }
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

/// Get the index of the top of the stack. Since the stack is 1-indexed, this
/// function also returns the number of values in the stack.
pub fn get_top(l: LuaState) -> i32 {
    unsafe { lua_gettop(l) }
}

/// Set the top of the lua stack, removing all values that are higher than the
/// new top index.
pub fn set_top(l: LuaState, index: i32) {
    unsafe { lua_settop(l, index) }
}

/// Pop the provided number of elements from the top of the stack.
pub fn pop(l: LuaState, count: i32) {
    set_top(l, count - 1);
}

/// Move the value at the top of the stack to the provided index, shifting all
/// values at this index and above upward.
pub fn move_top_value(l: LuaState, index: i32) {
    unsafe { lua_insert(l, index) }
}

/// Remove the value at the provided index, shifting all values above downward.
pub fn remove_value(l: LuaState, index: i32) {
    unsafe { lua_remove(l, index) }
}

/// Push a new string to the top of the Lua stack.
pub fn push_string(l: LuaState, s: &str) {
    unsafe {
        let ext_s = CString::from_str(s).unwrap();
        lua_pushstring(l, ext_s.as_ptr());
    }
}

/// Create a new table value and push it on the top of the stack. You can
/// provide initial size for the table array and hash parts to avoid later
/// memory allocation.
pub fn push_table(l: LuaState, array_part_size: Option<i32>, hash_part_size: Option<i32>) {
    unsafe { lua_createtable(l, array_part_size.unwrap_or(0), hash_part_size.unwrap_or(0)) }
}

/// Push a new C function value to the top of the Lua stack.
pub fn push_c_function(l: LuaState, function: LuaCFunction) {
    unsafe { lua_pushcclosure(l, function, 0) }
}

/// Get the global value that has the provided `name` and push it on the top of
/// the stack. If this value doesn't exists, the `nil` value is pushed instead.
pub fn get_global(l: LuaState, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_getfield(l, GLOBAL_INDEX, c_name.as_ptr());
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

/// Get the meta-table of the value at the provided `index` and push it on the
/// top of the stack. If the index is invalid or the value hasn't any
/// meta-table, this function returns `false` and doesn't push anything.
pub fn get_metatable(l: LuaState, index: i32) -> bool {
    unsafe { lua_getmetatable(l, index) != 0 }
}

/// Pop the table at the top of the stack and set it as meta-table of the
/// value at the provided `index`. This function returns whether the operation
/// was a success.
pub fn set_metatable(l: LuaState, index: i32) -> bool {
    unsafe { lua_setmetatable(l, index) != 0 }
}

/// Get the field of the provided `name` in the table at the provided `index`
/// and place it on the top of the stack.
pub fn get_field(l: LuaState, index: i32, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_getfield(l, index, c_name.as_ptr());
    }
}

/// Pop the value on the top of the stack and place it in the field of the
/// provided `name` in the table at the provided `index`.
pub fn set_field(l: LuaState, index: i32, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_setfield(l, index, c_name.as_ptr());
    }
}

/// Considering that the stack is filled with `arg_count` arguments and a
/// callable value: pop all of those and call the value with following
/// arguments. Place on the stack the specified count of result if any,
/// otherwise, place them all.
/// This function returns an [`Err`] containing the error message if an error
/// has been raised during the call.
pub fn call(
    l: LuaState,
    arg_count: i32,
    res_count: Option<i32>,
    err_func: Option<i32>,
) -> Result<(), String> {
    unsafe {
        let call_res = lua_pcall(l, arg_count, res_count.unwrap_or(MUTRET), err_func.unwrap_or(0));
        if call_res == 0 {
            Ok(())
        } else {
            Err(String::from(get_string(l, lua_gettop(l)).unwrap()))
        }
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

/// Get debug information about the frame at the specified level. This function
/// returns [`None`] if the specified level is higher that the current stack
/// depth.
pub fn debug_frame(l: LuaState, level: i32) -> Option<LuaDebug> {
    unsafe {
        let mut maybe_res = LuaDebug::new();
        if lua_getstack(l, level, &mut maybe_res) != 0 { Some(maybe_res) } else { None }
    }
}

/// Fill the provided debug data structure with required information in the
/// `what` parameter. See do of `lua_getinfo` for more information.
pub fn debug_info(l: LuaState, ar: &mut LuaDebug, what: &str) -> bool {
    unsafe {
        let ext_what = CString::from_str(what).unwrap();
        lua_getinfo(l, ext_what.as_ptr(), ar) != 0
    }
}

/// Get the name of the source from which the provided debug frame is coming.
pub fn debug_get_source(ar: &LuaDebug) -> Option<String> {
    unsafe {
        if !ar.source.is_null() {
            let source = CStr::from_ptr(ar.source);
            Some(String::from(source.to_str().unwrap()))
        } else {
            None
        }
    }
}

/// Get the name of the prototype currently being executed and the program
/// counter inside this prototype. This function return [`None`] if such
/// information doesn't exists for the current frame.
pub fn debug_proto_and_pc(l: LuaState, ar: &mut LuaDebug) -> Option<(&'static str, usize)> {
    unsafe {
        let mut ext_pc: c_uint = 0;
        let pc_get_res = lua_getpc(l, ar, &mut ext_pc);
        let ext_proto_name = lua_getprotoname(l, ar);
        if pc_get_res != 0 && !ext_proto_name.is_null() {
            let proto_name = CStr::from_ptr(ext_proto_name);
            Some((proto_name.to_str().unwrap(), ext_pc as usize))
        } else {
            None
        }
    }
}

/// Get the local value at the provide index in the provided debug frame, push
/// its value on the current stack. This function returns the local value name
/// if it has been found, [`None`] otherwise.
pub fn debug_get_local(l: LuaState, ar: &LuaDebug, index: i32) -> Option<&'static str> {
    unsafe {
        let ext_var_name = lua_getlocal(l, ar, index);
        if ext_var_name.is_null() {
            None
        } else {
            let var_name = CStr::from_ptr(ext_var_name);
            Some(var_name.to_str().unwrap())
        }
    }
}

// ----- Utils -----

/// Transform the value in the stack at the provided index as a string and
/// return it. This function use the `__tostring` Lua meta-method if the
/// type of the value requires it. If the call to this method fails, the
/// provided default value is returned.
pub fn to_string(l: LuaState, index: i32, default: &'static str) -> &'static str {
    let value_type = get_type(l, index);
    match value_type {
        LuaType::Number | LuaType::String => get_string(l, -1).unwrap(),
        LuaType::Boolean => {
            if get_boolean(l, -1) {
                "true"
            } else {
                "false"
            }
        }
        _ => {
            if call_meta(l, -1, "__tostring") {
                get_string(l, -1).unwrap()
            } else {
                default
            }
        }
    }
}

/// Create a string representation of the current state of the stack.
pub fn dump_stack(l: LuaState) -> String {
    unsafe {
        let mut res = String::new();
        let stack_top = lua_gettop(l);
        if stack_top > 0 {
            res.push_str("--- Stack start\n");
            for i in 1..=stack_top {
                let value_type = get_type(l, i);
                let value_repr = match value_type {
                    LuaType::None => "none".to_string(),
                    LuaType::Nil => "nil".to_string(),
                    LuaType::Boolean => {
                        if get_boolean(l, i) { "bool(true)" } else { "bool(false)" }.to_string()
                    }
                    LuaType::Number => format!("number({})", get_string(l, i).unwrap()),
                    LuaType::String => format!("string({})", get_string(l, i).unwrap()),
                    LuaType::LightUserData => "<light_user_data>".to_string(),
                    LuaType::Table => "<table>".to_string(),
                    LuaType::Function => "<function>".to_string(),
                    LuaType::UserData => "<user_data>".to_string(),
                    LuaType::Thread => "<thread>".to_string(),
                };
                res.push_str(&i.to_string());
                res.push_str(" - ");
                res.push_str(value_repr.as_str());
                res.push('\n');
            }
            res.push_str("--- Stack end");
        } else {
            res.push_str("Empty stack");
        }
        res
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

    fn lua_type(l: LuaState, index: c_int) -> LuaType;
    fn lua_toboolean(l: LuaState, index: c_int) -> c_int;
    fn lua_tolstring(l: LuaState, index: c_int, result_size: *mut usize) -> *const c_char;

    fn lua_pushstring(l: LuaState, s: *const c_char);
    fn lua_pushcclosure(l: LuaState, function: LuaCFunction, n: c_int);
    fn lua_getfield(l: LuaState, index: c_int, field: *const c_char);
    fn lua_setfield(l: LuaState, index: c_int, field: *const c_char);
    fn lua_createtable(l: LuaState, narr: c_int, nrec: c_int);
    fn lua_getmetatable(l: LuaState, index: c_int) -> c_int;
    fn lua_setmetatable(l: LuaState, index: c_int) -> c_int;

    fn lua_gettop(l: LuaState) -> c_int;
    fn lua_settop(l: LuaState, index: c_int);
    fn lua_insert(l: LuaState, index: c_int);
    fn lua_remove(l: LuaState, index: c_int);

    fn lua_pcall(l: LuaState, nargs: c_int, nres: c_int, errfunc: c_int) -> i32;
    fn luaL_callmeta(l: LuaState, obj: c_int, meta_method: *const c_char) -> c_int;

    fn lua_getstack(l: LuaState, level: c_int, ar: *mut LuaDebug) -> c_int;
    fn lua_getinfo(l: LuaState, what: *const c_char, ar: *mut LuaDebug) -> c_int;
    fn lua_getpc(l: LuaState, ar: *const LuaDebug, pc: *mut c_uint) -> c_int;
    fn lua_getprotoname(l: LuaState, ar: *mut LuaDebug) -> *const c_char;
    fn lua_getlocal(l: LuaState, ar: *const LuaDebug, n: c_int) -> *const c_char;
}
