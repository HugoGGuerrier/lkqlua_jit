//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use std::ffi::c_int;

use crate::lua::{LuaCFunction, LuaState, push_string, to_string};

/// The default image of a value when the latter doesn't define one.
pub const DEFAULT_IMG: &'static str = "<lkql_value>";

/// This type encapsulate a built-in LKQL function. It contains all required
/// information for the compilation and the execution.
pub struct BuiltinFunction {
    pub name: &'static str,
    pub c_function: LuaCFunction,
}

/// Get a new vector containing all LKQL built-ins. This function create a new
/// vector at each call.
pub fn get_builtins() -> Vec<BuiltinFunction> {
    fn b(name: &'static str, c_function: LuaCFunction) -> BuiltinFunction {
        BuiltinFunction { name, c_function }
    }
    vec![b("print", lkql_print), b("img", lkql_img)]
}

/// The "print" function
#[unsafe(no_mangle)]
unsafe extern "C" fn lkql_print(l: LuaState) -> c_int {
    println!("{}", to_string(l, -1, DEFAULT_IMG));
    0
}

/// The "img" function
#[unsafe(no_mangle)]
unsafe extern "C" fn lkql_img(l: LuaState) -> c_int {
    push_string(l, to_string(l, -1, DEFAULT_IMG));
    1
}
