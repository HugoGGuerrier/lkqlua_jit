//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use std::ffi::c_int;

use crate::{
    engine::runtime::{DynamicError, DynamicErrorArg},
    errors::{NO_VALUE_FOR_PARAM, POS_AND_NAMED_VALUE_FOR_PARAM},
    lua::{
        LuaCFunction, LuaState, LuaType, get_boolean, get_field, get_top, get_type, push_string,
        raise_error, to_string,
    },
};

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
    let param_count = get_top(l) - 1;
    let to_print_index = get_param(l, param_count, 1, "to_print");
    let new_line = get_bool_param(l, param_count, 2, "new_line", Some(true));
    if new_line {
        println!("{}", to_string(l, to_print_index, DEFAULT_IMG));
    } else {
        print!("{}", to_string(l, to_print_index, DEFAULT_IMG));
    }
    0
}

/// The "img" function
#[unsafe(no_mangle)]
unsafe extern "C" fn lkql_img(l: LuaState) -> c_int {
    let param_count = get_top(l) - 1;
    let value_index = get_param(l, param_count, 1, "value");
    push_string(l, to_string(l, value_index, DEFAULT_IMG));
    1
}

// ----- Util functions -----
// Following functions are external because they need to be compatible with the
// Lua error system.

/// Get the value of the specified parameter as boolean. If a default value has
/// been given, then this value is returned if the parameter hasn't any value,
/// otherwise this function raise an error.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
extern "C" fn get_bool_param(
    l: LuaState,
    param_count: i32,
    index: i32,
    name: &str,
    default_value: Option<bool>,
) -> bool {
    let maybe_param_index = _get_param(l, param_count, index, name);
    if let Ok(param_index) = maybe_param_index {
        get_boolean(l, param_index)
    } else if default_value.is_some() {
        default_value.unwrap()
    } else {
        raise_error(l, &maybe_param_index.unwrap_err().to_json_string());
        false
    }
}

/// Get index of the value for the parameter at the provided index with the
/// given name. If the parameter hasn't any value, this function raise an
/// error.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
extern "C" fn get_param(l: LuaState, param_count: i32, index: i32, name: &str) -> i32 {
    let maybe_param_index = _get_param(l, param_count, index, name);
    if let Ok(param_index) = maybe_param_index {
        param_index
    } else {
        raise_error(l, &maybe_param_index.unwrap_err().to_json_string());
        0
    }
}

/// Get index of the value for the parameter at the provided index with the
/// given name. This function isn't raising an error if the parameter hasn't
/// any value, but return [`None`].
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
extern "C" fn get_optional_param(
    l: LuaState,
    param_count: i32,
    index: i32,
    name: &str,
) -> Option<i32> {
    _get_param(l, param_count, index, name).ok()
}

/// Get the index pointing to the value of the parameter at the given `index`
/// (1-based) with the provided `name`.
/// This function also performs checks to ensure the parameter has exactly one
/// value, on the contrary, this function raise an error and return 0.
fn _get_param(l: LuaState, param_count: i32, index: i32, name: &str) -> Result<i32, DynamicError> {
    // Get the whether the parameter has a positional and a named value
    let has_pos_value = index <= param_count;
    let has_named_value = {
        get_type(l, 1) != LuaType::Nil && {
            get_field(l, 1, name);
            get_type(l, -1) != LuaType::Nil
        }
    };

    // Then perform value checks
    if has_pos_value {
        if has_named_value {
            Err(DynamicError {
                template_id: POS_AND_NAMED_VALUE_FOR_PARAM.id,
                message_args: vec![DynamicErrorArg::Static(String::from(name))],
            })
        } else {
            Ok(index + 1)
        }
    } else {
        if has_named_value {
            Ok(get_top(l))
        } else {
            Err(DynamicError {
                template_id: NO_VALUE_FOR_PARAM.id,
                message_args: vec![DynamicErrorArg::Static(String::from(name))],
            })
        }
    }
}
