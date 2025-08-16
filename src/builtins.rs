//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use std::ffi::c_int;

use crate::lua::{LuaCFunction, LuaState, LuaType, call_meta, get_boolean, get_string, get_type};

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
    vec![b("print", lkql_print)]
}

/// The "print" function
#[unsafe(no_mangle)]
unsafe extern "C" fn lkql_print(l: LuaState) -> c_int {
    // Get the type of the value on the top of the stack
    let arg_type = get_type(l, -1);

    // According to the argument type, get the string to print
    let to_print = match arg_type {
        LuaType::Number | LuaType::String => get_string(l, -1).unwrap(),
        LuaType::Boolean => {
            if get_boolean(l, -1) {
                "true"
            } else {
                "false"
            }
        }
        _ => {
            if call_meta(l, None, "__tostring") {
                get_string(l, -1).unwrap()
            } else {
                "<lkql_value>"
            }
        }
    };

    // Finally, print the result in output
    println!("{to_print}");

    // Return the success
    0
}
