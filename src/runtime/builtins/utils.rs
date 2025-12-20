//! # Built-in utils
//!
//! This module contains all utils used by built-in values to interact with
//! runtime values.

use crate::{
    error_templates::{NO_VALUE_FOR_PARAM, POS_AND_NAMED_VALUE_FOR_PARAM, WRONG_ARG_TYPE},
    lua::{
        LuaState, LuaType, get_boolean, get_field, get_integer, get_string, get_top, get_type,
        is_nil, pop, raise_error,
    },
    runtime::{
        DynamicError, DynamicErrorArg, TYPE_NAME_FIELD, TYPE_TAG_FIELD,
        builtins::types::{self, BuiltinType},
    },
};

// ----- Parameter reading functions -----

/// Given a Lua state, get the value of the parameter designated by the
/// provided index and the provided name as a boolean value.
/// This function checks the parameter type, raising an error if it isn't the
/// `bool` one.
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
        check_param_type(l, name, param_index, &types::bool::TYPE);
        get_boolean(l, param_index)
    } else if default_value.is_some() {
        default_value.unwrap()
    } else {
        raise_error(l, &maybe_param_index.unwrap_err().to_json_string());
        false
    }
}

/// Given a Lua state, get the value of the parameter designated by the
/// provided index and the provided name as a string value.
/// This function checks the parameter type, raising an error if it isn't the
/// `str` one.
/// If this parameter hasn't any value, this function returns the default value
/// if any, otherwise it raises an error using the [`raise_error`] function.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
pub extern "C" fn get_string_param(
    l: LuaState,
    param_count: i32,
    index: i32,
    name: &str,
    default_value: Option<&'static str>,
) -> &'static str {
    let maybe_param_index = get_param_safe(l, param_count, index, name);
    if let Ok(param_index) = maybe_param_index {
        check_param_type(l, name, param_index, &types::str::TYPE);
        get_string(l, param_index).unwrap()
    } else if default_value.is_some() {
        default_value.unwrap()
    } else {
        raise_error(l, &maybe_param_index.unwrap_err().to_json_string());
        ""
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

/// This function ensure the parameter at the provided `index` with the given
/// `name` is of the correct type. If types don't match, this function raise
/// an error.
#[unsafe(no_mangle)]
#[allow(improper_ctypes_definitions)]
extern "C" fn check_param_type(
    l: LuaState,
    param_name: &str,
    value_index: i32,
    expected_type: &BuiltinType,
) {
    let param_type_tag = get_type_tag(l, value_index);
    if param_type_tag != expected_type.tag() {
        let param_type_name = get_type_name(l, value_index);
        raise_error(
            l,
            &DynamicError {
                template_id: WRONG_ARG_TYPE.id,
                message_args: vec![
                    DynamicErrorArg::Static(String::from(expected_type.display_name())),
                    DynamicErrorArg::Static(String::from(param_name)),
                    DynamicErrorArg::Static(String::from(param_type_name)),
                ],
            }
            .to_json_string(),
        )
    }
}

// ----- Type checking functions -----

/// Get the type tag of the value at the provided index if applicable.
pub fn get_type_tag(l: LuaState, index: i32) -> isize {
    get_field(l, index, TYPE_TAG_FIELD);
    if is_nil(l, -1) {
        panic!("Invalid runtime value");
    } else {
        let res = get_integer(l, -1);
        pop(l, 1);
        res
    }
}

/// Get the type name of the value at the provided index if applicable.
pub fn get_type_name(l: LuaState, index: i32) -> &'static str {
    get_field(l, index, TYPE_NAME_FIELD);
    if is_nil(l, -1) {
        panic!("Invalid runtime value");
    } else {
        let res = get_string(l, -1).unwrap();
        pop(l, 1);
        res
    }
}
