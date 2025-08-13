//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use std::{ffi::c_int, os::raw::c_void};

use crate::lua::LuaCFunction;

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
unsafe extern "C" fn lkql_print(l: *mut c_void) -> c_int {
    println!("This is my LKQL printing function");
    0
}
