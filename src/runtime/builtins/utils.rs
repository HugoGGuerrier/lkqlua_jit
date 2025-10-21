//! # Built-in utils
//!
//! This module contains all utils used by built-in values to interact with
//! runtime values.

use crate::{
    errors::{NO_VALUE_FOR_PARAM, POS_AND_NAMED_VALUE_FOR_PARAM},
    lua::{LuaState, LuaType, get_boolean, get_field, get_top, get_type, raise_error},
    runtime::{DynamicError, DynamicErrorArg},
};

/// Get the name of the global field where the meta-table of the type
/// designated by the provided name is stored in.
pub fn metatable_global_field(type_name: &str) -> String {
    return format!("type@{}", type_name);
}

// ----- Parameter reading functions -----

/// Given a Lua state, get the value of the parameter designated by the
/// provided index and the provided name as a boolean value.
/// If this parameter hasn't any value, this function returns the default value
/// if any, otherwise it raises an error using the [`raise_error`] function.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
pub extern "C" fn get_bool_param(
    l: LuaState,
    param_count: i32,
    index: i32,
    name: &str,
    default_value: Option<bool>,
) -> bool {
    let maybe_param_index = get_param_safe(l, param_count, index, name);
    if let Ok(param_index) = maybe_param_index {
        get_boolean(l, param_index)
    } else if default_value.is_some() {
        default_value.unwrap()
    } else {
        raise_error(l, &maybe_param_index.unwrap_err().to_json_string());
        false
    }
}

/// Given a Lua state, get the value of the parameter designated by the
/// provided index and the provided name. If this parameter hasn't any value,
/// this function raises an error using the [`raise_error`] function.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
pub extern "C" fn get_param(l: LuaState, param_count: i32, index: i32, name: &str) -> i32 {
    let maybe_param_index = get_param_safe(l, param_count, index, name);
    if let Ok(param_index) = maybe_param_index {
        param_index
    } else {
        raise_error(l, &maybe_param_index.unwrap_err().to_json_string());
        0
    }
}

/// Get the index pointing to the value of the parameter at the given `index`
/// (1-based) with the provided `name`.
/// This function also performs checks to ensure the parameter has exactly one
/// value, on the contrary, this function returns an [`Err`] with an error
/// instance.
fn get_param_safe(
    l: LuaState,
    param_count: i32,
    index: i32,
    name: &str,
) -> Result<i32, DynamicError> {
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
