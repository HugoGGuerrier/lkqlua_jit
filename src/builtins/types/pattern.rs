//! # LKQL "Pattern" type
//!
//! This module defines the LKQL "Pattern" type. This type represents
//! compiled regular expression and can be used as such.

use crate::{
    builtins::{
        types::{
            BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
            img_property, str,
        },
        utils::{get_param, get_string_param},
    },
    lua::{LuaState, get_field, get_top, get_user_data, pop, push_bool, push_integer, push_string},
    runtime::{Function, RuntimeValue},
};
use regex::Regex;
use std::ffi::c_int;

/// The field in which the native regex handle is stored.
pub const NATIVE_HANDLE_FIELD: &str = "field@native_handle";

pub const TYPE: BuiltinType = BuiltinType {
    tag: str::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Pattern",
    fields: &[
        ("img", TypeField::Property(Function::CFunction(img_property))),
        (
            "is_match",
            TypeField::Value(RuntimeValue::Callable(Function::CFunction(pattern_is_match))),
        ),
        (
            "find",
            TypeField::Value(RuntimeValue::Callable(Function::CFunction(pattern_find))),
        ),
    ],
    overloads: &[
        (OverloadTarget::ToString, Function::CFunction(pattern_tostring)),
        (OverloadTarget::Gc, Function::CFunction(pattern_gc)),
    ],
    index_method: None,
    registering_function: None,
};

/// Implementation of the "is_match" method.
#[unsafe(no_mangle)]
extern "C" fn pattern_is_match(l: LuaState) -> c_int {
    // Get the parameter values
    let param_count = get_top(l) - 1;
    let regex_index = get_param(l, param_count, 1, "self");
    let haystack = get_string_param(l, param_count, 2, "haystack", None);

    // Get the Rust compiled regex
    get_field(l, regex_index, NATIVE_HANDLE_FIELD);
    let compiled_regex = get_user_data::<Regex>(l, -1).unwrap();
    pop(l, 1);

    // Get if the provided string matches the regex
    push_bool(l, compiled_regex.is_match(haystack));
    1
}

/// Implementation of the "find" method.
#[unsafe(no_mangle)]
extern "C" fn pattern_find(l: LuaState) -> c_int {
    // Get the parameter values
    let param_count = get_top(l) - 1;
    let regex_index = get_param(l, param_count, 1, "self");
    let haystack = get_string_param(l, param_count, 2, "haystack", None);

    // Get the Rust compiled regex
    get_field(l, regex_index, NATIVE_HANDLE_FIELD);
    let compiled_regex = get_user_data::<Regex>(l, -1).unwrap();
    pop(l, 1);

    // Get if the provided string matches the regex
    push_integer(
        l,
        compiled_regex
            .find(haystack)
            .map(|m| 1 + m.start() as isize)
            .unwrap_or(-1),
    );
    1
}

/// Overload of "__tostring" for the "Pattern" type
#[unsafe(no_mangle)]
extern "C" fn pattern_tostring(l: LuaState) -> c_int {
    // Get the Rust compiled regex
    get_field(l, -1, NATIVE_HANDLE_FIELD);
    let compiled_regex = get_user_data::<Regex>(l, -1).unwrap();
    pop(l, 1);

    // Then push the pattern representation on the stack
    push_string(l, &format!("Pattern<\"{}\">", compiled_regex.as_str()));
    1
}

/// Overload of "__gc" for the "Pattern" type
#[unsafe(no_mangle)]
extern "C" fn pattern_gc(l: LuaState) -> c_int {
    // Get the Rust compiled regex and take its ownership to free it
    get_field(l, -1, NATIVE_HANDLE_FIELD);
    let _ = unsafe { Box::from_raw(get_user_data::<Regex>(l, -1).unwrap()) };
    pop(l, 1);

    // No result returned
    0
}
