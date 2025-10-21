//! # Built-in functions
//!
//! This module contains all LKQL built-in functions and utils for them.

use std::ffi::c_int;

use std::io::Write;

use crate::{
    ExecutionContext,
    lua::{
        LuaState, get_global, get_string, get_top, get_type, get_user_data, pop, push_string,
        to_string,
    },
    runtime::{
        CONTEXT_GLOBAL_NAME, DEFAULT_VALUE_IMAGE,
        builtins::utils::{get_bool_param, get_param},
    },
};

/// The "print" function
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lkql_print(l: LuaState) -> c_int {
    // Get the function parameter values
    let param_count = get_top(l) - 1;
    let to_print_index = get_param(l, param_count, 1, "to_print");
    let new_line = get_bool_param(l, param_count, 2, "new_line", Some(true));

    // Get the current execution context
    get_global(l, CONTEXT_GLOBAL_NAME);
    let ctx = get_user_data::<ExecutionContext>(l, get_top(l)).unwrap();
    pop(l, 1);

    // Then display the value on the configured standard output
    if new_line {
        writeln!(ctx.config.std_out, "{}", to_string(l, to_print_index, DEFAULT_VALUE_IMAGE))
            .unwrap();
    } else {
        write!(ctx.config.std_out, "{}", to_string(l, to_print_index, DEFAULT_VALUE_IMAGE))
            .unwrap();
    }
    0
}

/// The "img" function
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lkql_img(l: LuaState) -> c_int {
    let param_count = get_top(l) - 1;
    let value_index = get_param(l, param_count, 1, "value");
    match get_type(l, value_index) {
        crate::lua::LuaType::String => {
            push_string(l, &format!("\"{}\"", get_string(l, value_index).unwrap()))
        }
        _ => push_string(l, to_string(l, value_index, DEFAULT_VALUE_IMAGE)),
    }
    1
}
