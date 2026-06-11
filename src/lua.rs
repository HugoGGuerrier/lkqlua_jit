//! # Lua Rust API
//!
//! This module maps the Lua C API to Rust entry points, using it you can
//! create and manipulate Lua context.

use std::{
    any::Any,
    env,
    ffi::{CStr, CString, c_char, c_double, c_int, c_uint, c_void},
    path::{Path, PathBuf},
    ptr,
    str::FromStr,
};

// ----- Constants -----

/// Pseudo-index of the global table in the Lua stack.
const GLOBAL_INDEX: i32 = -10002;

/// Result count to pass to get all results.
const MULTRET: i32 = -1;

/// Size of short source representations in debug data.
const SHORT_SRC_SIZE: usize = 60;

// ----- Public types -----

pub type LuaState = *mut c_void;

/// C function that can be called from any Lua code.
pub type LuaCFunction = unsafe extern "C" fn(LuaState) -> c_int;

/// Type tags for Lua values.
#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
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

/// Maps the `Lua_Debug` C type, It contains debugging information about an
/// execution frame.
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
pub(crate) fn new_lua_state() -> LuaState {
    unsafe { luaL_newstate() }
}

/// Close the provided Lua state.
pub(crate) fn close_lua_state(l: LuaState) {
    unsafe { lua_close(l) }
}

/// Open all Lua base libraries in the given state.
pub(crate) fn open_lua_libs(l: LuaState) {
    unsafe { luaL_openlibs(l) }
}

/// Load the given buffer to the Lua stack as a callable value, returning
/// whether the function succeeded.
pub(crate) fn load_buffer(l: LuaState, buffer: &[u8], buffer_name: &str) -> bool {
    unsafe {
        let ext_buffer = buffer.as_ptr() as *const c_char;
        let ext_buffer_name = CString::from_str(buffer_name).unwrap();
        luaL_loadbuffer(l, ext_buffer, buffer.len(), ext_buffer_name.as_ptr()) == 0
    }
}

/// Load the Lua source code in `source` in the current state, returning
/// whether the parsing succeeded.
pub(crate) fn load_lua_code(l: LuaState, source: &str, source_name: &str) -> bool {
    unsafe {
        let ext_source = CString::from_str(source).unwrap();
        let ext_source_name = CString::from_str(source_name).unwrap();
        luaL_loadbuffer(l, ext_source.as_ptr(), source.len(), ext_source_name.as_ptr()) == 0
    }
}

/// Load the provided file as a Lua source and return whether the operation
/// succeeded.
pub(crate) fn load_lua_file(l: LuaState, file: &Path) -> bool {
    unsafe {
        let ext_filename = CString::from_str(file.to_str().unwrap()).unwrap();
        luaL_loadfile(l, ext_filename.as_ptr()) == 0
    }
}

/// Get the type of the value on the stack at the given index.
pub(crate) fn get_type(l: LuaState, index: i32) -> LuaType {
    unsafe { lua_type(l, index) }
}

/// Get whether the value at the provided `index` is `nil`.
pub(crate) fn is_nil(l: LuaState, index: i32) -> bool {
    get_type(l, index) == LuaType::Nil
}

/// Get the length of the object of the provided index. The result is the same
/// as the one obtained through the `#` Lua operator.
pub(crate) fn get_length(l: LuaState, index: i32) -> usize {
    unsafe { lua_objlen(l, index) }
}

/// Get the value at the provided index as a boolean, implicitly converting
/// all values that aren't of the boolean type to one.
pub(crate) fn get_boolean(l: LuaState, index: i32) -> bool {
    unsafe { lua_toboolean(l, index) != 0 }
}

/// Get the value at the provided `index` as an integer, implicitly converting
/// it. If the value is not convertible the behavior is not specified.
pub(crate) fn get_integer(l: LuaState, index: i32) -> isize {
    unsafe { lua_tointeger(l, index) }
}

/// Try to get the value at the provided index as a string and return it. If
/// this is not possible, the result is [`None`].
pub(crate) fn get_string(l: LuaState, index: i32) -> Option<&'static str> {
    unsafe {
        let ext_res = lua_tolstring(l, index, ptr::null_mut());
        if ext_res.is_null() {
            None
        } else {
            Some(CStr::from_ptr(ext_res).to_str().unwrap())
        }
    }
}

/// Try to get user data on the stack at the provided `index`, if this is not
/// possible, return [`None`].
pub(crate) fn get_user_data<'a, T: Any>(l: LuaState, index: i32) -> Option<&'a mut T> {
    unsafe {
        let c_data = lua_topointer(l, index);
        if c_data.is_null() { None } else { Some(&mut *(c_data as *mut T)) }
    }
}

/// Pop a key from the stack and push the next key / values pair from the table
/// at the provided index. This function pushes the key first then the value.
/// Returns whether a new pair has been pushed on the stack.
pub(crate) fn get_next_pair(l: LuaState, index: i32) -> bool {
    unsafe { lua_next(l, index) != 0 }
}

/// Push the `nil` value to the top of the stack.
pub(crate) fn push_nil(l: LuaState) {
    unsafe { lua_pushnil(l) }
}

/// Push a boolean value to the top of the stack.
pub(crate) fn push_bool(l: LuaState, value: bool) {
    unsafe { lua_pushboolean(l, if value { 1 } else { 0 }) }
}

/// Push an integer value to the top of the stack.
pub(crate) fn push_integer(l: LuaState, integer: isize) {
    unsafe { lua_pushinteger(l, integer) }
}

/// Push a number to the top of the Lua stack.
pub(crate) fn push_number(l: LuaState, number: f64) {
    unsafe { lua_pushnumber(l, number) }
}

/// Push a new string to the top of the Lua stack.
pub(crate) fn push_string(l: LuaState, s: &str) {
    unsafe {
        let ext_s = CString::from_str(s).unwrap();
        lua_pushstring(l, ext_s.as_ptr());
    }
}

/// Create a new table value and push it on the top of the stack. You can
/// provide initial size for the table array and hash parts to avoid later
/// memory allocation.
pub(crate) fn push_table(l: LuaState, array_part_size: i32, hash_part_size: i32) {
    unsafe { lua_createtable(l, array_part_size, hash_part_size) }
}

/// Push arbitrary data on the Lua stack as a value.
pub(crate) fn push_user_data<T: Any>(l: LuaState, data: &T) {
    unsafe { lua_pushlightuserdata(l, data as *const T as *mut c_void) }
}

/// Push a pointer to arbitrary data on the Lua stack.
pub(crate) fn push_user_data_ptr<T: Any>(l: LuaState, data: *mut T) {
    unsafe { lua_pushlightuserdata(l, data as *mut c_void) }
}

/// Push a new C function value to the top of the Lua stack.
pub(crate) fn push_c_function(l: LuaState, function: LuaCFunction) {
    unsafe { lua_pushcclosure(l, function, 0) }
}

/// Push a new C function value to the top of the Lua stack. Additionally, this
/// function pops `up_value_count` values from the stack and store them in the
/// newly created function value, making them up-values.
pub(crate) fn push_c_closure(l: LuaState, function: LuaCFunction, up_value_count: u8) {
    unsafe { lua_pushcclosure(l, function, up_value_count as c_int) }
}

/// Copy the value at the provided index on the top of the stack.
pub(crate) fn copy_value(l: LuaState, index: i32) {
    unsafe { lua_pushvalue(l, index) }
}

/// Get the global value that has the provided `name` and push it on the top of
/// the stack. If this value doesn't exists, the `nil` value is pushed instead.
pub(crate) fn get_global(l: LuaState, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_getfield(l, GLOBAL_INDEX, c_name.as_ptr());
    }
}

/// Pop the value at the top of the stack and place it in the field of the
/// provided `name` in the global table.
pub(crate) fn set_global(l: LuaState, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_setfield(l, GLOBAL_INDEX, c_name.as_ptr());
    }
}

/// Get the meta-table of the value at the provided `index` and push it on the
/// top of the stack. If the index is invalid or the value hasn't any
/// meta-table, this function returns `false` and doesn't push anything.
pub(crate) fn get_metatable(l: LuaState, index: i32) -> bool {
    unsafe { lua_getmetatable(l, index) != 0 }
}

/// Pop the table at the top of the stack and set it as meta-table of the
/// value at the provided `index`. This function returns whether the operation
/// was a success.
pub(crate) fn set_metatable(l: LuaState, index: i32) -> bool {
    unsafe { lua_setmetatable(l, index) != 0 }
}

/// Get the field of the provided `name` in the table at the provided `index`
/// and place it on the top of the stack.
pub(crate) fn get_field(l: LuaState, index: i32, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_getfield(l, index, c_name.as_ptr());
    }
}

/// Given a table value at the provided index, get the element at the provided
/// given index in it and place it on the top of the stack.
pub(crate) fn get_index(l: LuaState, index: i32, inner_index: i32) {
    unsafe { lua_rawgeti(l, index, inner_index) }
}

/// Pop the value on the top of the stack and place it in the field of the
/// provided `name` in the table at the provided `index`.
pub(crate) fn set_field(l: LuaState, index: i32, name: &str) {
    unsafe {
        let c_name = CString::from_str(name).unwrap();
        lua_setfield(l, index, c_name.as_ptr());
    }
}

/// Pop the value on the top of the stack and place it at the provided
/// `inner_index` in the table at the provided `index`.
pub(crate) fn set_index(l: LuaState, index: i32, inner_index: i32) {
    unsafe { lua_rawseti(l, index, inner_index) }
}

/// Get the index of the top of the stack. Since the stack is 1-indexed, this
/// function also returns the number of values in the stack.
pub(crate) fn get_top(l: LuaState) -> i32 {
    unsafe { lua_gettop(l) }
}

/// Set the top of the lua stack, removing all values that are higher than the
/// new top index.
pub(crate) fn set_top(l: LuaState, index: i32) {
    unsafe { lua_settop(l, index) }
}

/// Pop the provided number of elements from the top of the stack.
pub(crate) fn pop(l: LuaState, count: i32) {
    set_top(l, -count - 1);
}

/// Move the value at the top of the stack to the provided index, shifting all
/// values at this index and above upward.
pub(crate) fn move_top_value(l: LuaState, index: i32) {
    unsafe { lua_insert(l, index) }
}

/// Remove the value at the provided index, shifting all values above downward.
pub(crate) fn remove_value(l: LuaState, index: i32) {
    unsafe { lua_remove(l, index) }
}

/// Considering the stack is filled with `arg_count` arguments with a callable
/// value on top of it: pops all of those and call the executable value with
/// all arguments.
///
/// Places the specified number of results on the top of the stack, if [`None`]
/// is provided this function all results.
///
/// This function propagate any error raised during the execution of the
/// callable value.
pub(crate) fn call(l: LuaState, arg_count: i32, res_count: Option<i32>) {
    unsafe { lua_call(l, arg_count, res_count.unwrap_or(MULTRET)) }
}

/// Considering the stack is filled with `arg_count` arguments with a callable
/// value on top of it: pops all of those and call the executable value with
/// all arguments.
///
/// Places the specified number of results on the top of the stack, if [`None`]
/// is provided this function all results.
///
/// This function returns an [`Err`] containing the error message if an error
/// has been raised during the execution of the callable value.
pub(crate) fn safe_call(
    l: LuaState,
    arg_count: i32,
    res_count: Option<i32>,
    err_func: Option<i32>,
) -> Result<(), String> {
    unsafe {
        let call_res = lua_pcall(l, arg_count, res_count.unwrap_or(MULTRET), err_func.unwrap_or(0));
        if call_res == 0 {
            Ok(())
        } else {
            Err(String::from(get_string(l, lua_gettop(l)).unwrap()))
        }
    }
}

/// Call the requested `meta_method` on the object at the given `index`.
///
/// This function returns whether the call succeeded, and if so, the result
/// of the call is push on the top of the stack.
pub(crate) fn call_meta(l: LuaState, index: i32, meta_method: &str) -> bool {
    let ext_meta_method = CString::from_str(meta_method).unwrap();
    unsafe { luaL_callmeta(l, index, ext_meta_method.as_ptr()) == 1 }
}

/// Raise an error in the Lua context. This function never returns.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
pub(crate) extern "C" fn raise_error(l: LuaState, message: &str) {
    unsafe {
        let c_message = CString::from_str(message).unwrap();
        luaL_error(l, c_message.as_ptr());
    }
}

/// Get debug information about the frame at the specified `level`. This
/// function returns [`None`] if the specified level is higher that the current
/// stack depth.
pub(crate) fn debug_frame(l: LuaState, level: i32) -> Option<LuaDebug> {
    unsafe {
        let mut maybe_res = LuaDebug::new();
        if lua_getstack(l, level, &mut maybe_res) != 0 { Some(maybe_res) } else { None }
    }
}

/// Fill the provided debug data structure with required information in the
/// `what` parameter.
///
/// See the documentation of the `lua_getinfo` C function for more information.
pub(crate) fn debug_info(l: LuaState, frame: &mut LuaDebug, what: &str) -> bool {
    unsafe {
        let ext_what = CString::from_str(what).unwrap();
        lua_getinfo(l, ext_what.as_ptr(), frame) != 0
    }
}

/// Get the name of the source from which the provided debug `frame` is coming.
pub(crate) fn debug_get_source(frame: &LuaDebug) -> Option<String> {
    unsafe {
        if !frame.source.is_null() {
            let source = CStr::from_ptr(frame.source);
            Some(String::from(source.to_str().unwrap()))
        } else {
            None
        }
    }
}

/// Get the identifier (index) of the prototype being executed in the provided
/// `frame` and the program counter inside this prototype.
///
/// This function returns [`None`] if such information don't exist for the
/// provided frame.
pub(crate) fn debug_proto_and_pc(l: LuaState, frame: &mut LuaDebug) -> Option<(usize, usize)> {
    unsafe {
        let mut ext_pc: c_uint = 0;
        let mut ext_protoid: c_uint = 0;
        let pc_get_res = lua_getpc(l, frame, &mut ext_pc);
        let proto_id_get_res = lua_getprotoid(l, frame, &mut ext_protoid);
        if pc_get_res != 0 && ext_pc > 0 && proto_id_get_res != 0 {
            Some((ext_protoid as usize, ext_pc as usize))
        } else {
            None
        }
    }
}

/// Get the prototype identifier of the function at the provided `index` if
/// possible, [`None`] otherwise.
pub(crate) fn debug_get_func_id(l: LuaState, index: i32) -> Option<usize> {
    unsafe {
        let mut ext_id: c_uint = 0;
        if lua_getfuncid(l, index, &mut ext_id) != 0 {
            Some(ext_id as usize)
        } else {
            None
        }
    }
}

/// Get the local value at the provided `index` in the provided debug frame,
/// push it on the current stack if it exists.
///
/// This function returns the local value name if it has been found, [`None`]
/// otherwise.
pub(crate) fn debug_get_local(l: LuaState, ar: &LuaDebug, index: i32) -> Option<&'static str> {
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

/// Helper function to get the absolute path to the file defining the required
/// module if it exists. This function looks in the `LUA_PATH` environment
/// variable.
pub(crate) fn find_in_lua_path(module_name: &str) -> Option<PathBuf> {
    // Create the list of patterns to use for module search
    let mut searching_patterns: Vec<String> = Vec::new();

    // Add the current working directory to the searching patterns
    if let Ok(current_dir) = env::current_dir() {
        searching_patterns.push(format!("{}/?.lua", current_dir.to_string_lossy()));
    }

    // Then look in the LUA_PATH environment variable
    if let Ok(lua_path) = env::var("LUA_PATH") {
        lua_path
            .split([':', ';'])
            .for_each(|p| searching_patterns.push(String::from(p)));
    }

    // Finally, test each pattern to find the Lua file
    searching_patterns.iter().find_map(|p| {
        let maybe_file = PathBuf::from(p.replace("?", module_name));
        if maybe_file.is_file() { Some(maybe_file) } else { None }
    })
}

/// Transform the value in the stack at the provided `index` as a string and
/// return it. This function use the `__tostring` Lua meta-method if the type
/// of the value requires it. If the call to this method fails, the provided
/// `default` value is returned.
pub(crate) fn to_string(l: LuaState, index: i32, default: &'static str) -> &'static str {
    let value_type = get_type(l, index);
    match value_type {
        LuaType::Number | LuaType::String => get_string(l, index).unwrap(),
        LuaType::Boolean => {
            if get_boolean(l, index) {
                "true"
            } else {
                "false"
            }
        }
        _ => {
            if call_meta(l, index, "__tostring") {
                let res = get_string(l, -1).unwrap();
                pop(l, 1);
                res
            } else {
                default
            }
        }
    }
}

/// Dump the string representation of the Lua stack on the standard output.
#[allow(dead_code)]
pub(crate) fn dump_stack(l: LuaState) {
    println!("{}", stack_image(l));
}

/// Get a string representation of the Lua stack.
fn stack_image(l: LuaState) -> String {
    let stack_top = unsafe { lua_gettop(l) };
    let mut res = String::new();
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
    fn luaL_loadfile(l: LuaState, filename: *const c_char) -> c_int;

    fn lua_type(l: LuaState, index: c_int) -> LuaType;
    fn lua_objlen(l: LuaState, index: c_int) -> usize;
    fn lua_toboolean(l: LuaState, index: c_int) -> c_int;
    fn lua_tointeger(l: LuaState, index: i32) -> isize;
    fn lua_tolstring(l: LuaState, index: c_int, result_size: *mut usize) -> *const c_char;
    fn lua_topointer(l: LuaState, index: c_int) -> *const c_void;
    fn lua_next(l: LuaState, index: c_int) -> c_int;

    fn lua_pushnil(l: LuaState);
    fn lua_pushboolean(l: LuaState, value: c_int);
    fn lua_pushinteger(l: LuaState, integer: isize);
    fn lua_pushnumber(l: LuaState, number: c_double);
    fn lua_pushstring(l: LuaState, s: *const c_char);
    fn lua_pushcclosure(l: LuaState, function: LuaCFunction, n: c_int);
    fn lua_pushlightuserdata(l: LuaState, p: *mut c_void);
    fn lua_pushvalue(l: LuaState, index: c_int);
    fn lua_getfield(l: LuaState, index: c_int, field: *const c_char);
    fn lua_rawgeti(l: LuaState, index: c_int, i: c_int);
    fn lua_setfield(l: LuaState, index: c_int, field: *const c_char);
    fn lua_rawseti(l: LuaState, index: c_int, i: c_int);
    fn lua_createtable(l: LuaState, narr: c_int, nrec: c_int);
    fn lua_getmetatable(l: LuaState, index: c_int) -> c_int;
    fn lua_setmetatable(l: LuaState, index: c_int) -> c_int;

    fn lua_gettop(l: LuaState) -> c_int;
    fn lua_settop(l: LuaState, index: c_int);
    fn lua_insert(l: LuaState, index: c_int);
    fn lua_remove(l: LuaState, index: c_int);

    fn lua_pcall(l: LuaState, nargs: c_int, nres: c_int, errfunc: c_int) -> c_int;
    fn lua_call(l: LuaState, nargs: c_int, nres: c_int);
    fn luaL_callmeta(l: LuaState, obj: c_int, meta_method: *const c_char) -> c_int;
    fn luaL_error(l: LuaState, fmt: *const c_char, ...) -> c_int;

    fn lua_getstack(l: LuaState, level: c_int, ar: *mut LuaDebug) -> c_int;
    fn lua_getinfo(l: LuaState, what: *const c_char, ar: *mut LuaDebug) -> c_int;
    fn lua_getpc(l: LuaState, ar: *const LuaDebug, pc: *mut c_uint) -> c_int;
    fn lua_getprotoid(l: LuaState, ar: *const LuaDebug, id: *mut c_uint) -> c_int;
    fn lua_getfuncid(l: LuaState, index: c_int, id: *mut c_uint) -> c_int;
    fn lua_getlocal(l: LuaState, ar: *const LuaDebug, n: c_int) -> *const c_char;
}
