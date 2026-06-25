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
        utils::{get_bool_param, get_param, get_string_param},
    },
    errors::{ErrorInstance, ErrorInstanceArg, REGEX_SYNTAX_ERROR, REGEX_TOO_BIG},
    lua::{
        LuaState, get_field, get_global, get_index, get_integer, get_length, get_top,
        get_user_data, pop, push_bool, push_integer, push_string, push_table, push_user_data_ptr,
        raise_error, set_field, set_metatable,
    },
    runtime::{Function, RuntimeValue, register_for_gc},
};
use regex::{Regex, RegexBuilder};
use std::{ffi::c_int, ptr};

/// Name of the function to construct a new pattern.
pub const PATTERN_CONSTRUCTOR: &str = "pattern";

/// The field in which the native regex handle is stored.
pub const NATIVE_HANDLE_FIELD: &str = "field@native_handle";

/// The field containing a table that holds all bytes of the native handle.
pub const NATIVE_HANDLE_TABLE_FIELD: &str = "field@native_handle_table";

pub const TYPE: BuiltinType = BuiltinType {
    tag: str::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Pattern",
    fields: &[
        (
            NATIVE_HANDLE_FIELD,
            TypeField::Property(Function::CFunction(pattern_native_handle)),
        ),
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

/// The "pattern" function
#[unsafe(no_mangle)]
pub extern "C" fn pattern_constructor(l: LuaState) -> c_int {
    // Get the function parameter values
    let param_count = get_top(l) - 1;
    let regex = get_string_param(l, param_count, 1, "regex", None);
    let case_sensitive = get_bool_param(l, param_count, 2, "case_sensitive", Some(true));

    // Now create the Rust regex object
    match RegexBuilder::new(regex)
        .case_insensitive(!case_sensitive)
        .build()
    {
        Ok(compiled_regex) => {
            // Create a new Lua table
            push_table(l, 0, 1);

            // Store a pointer to the Rust compiled regex in it
            push_user_data_ptr(l, Box::into_raw(Box::new(compiled_regex)));
            set_field(l, -2, NATIVE_HANDLE_FIELD);

            // Then set the metatable of the result
            get_global(l, &IMPLEMENTATION.global_field_name());
            set_metatable(l, -2);

            // Finally, register the pattern Lua value for garbage collection
            register_for_gc(l, -1);
            1
        }
        Err(error) => {
            let error_instance = match error {
                regex::Error::Syntax(_) => ErrorInstance::new(
                    REGEX_SYNTAX_ERROR.id,
                    vec![ErrorInstanceArg::Static(String::from(regex))],
                ),
                regex::Error::CompiledTooBig(_) => ErrorInstance::new(
                    REGEX_TOO_BIG.id,
                    vec![ErrorInstanceArg::Static(String::from(regex))],
                ),
                _ => unreachable!(),
            };
            raise_error(l, &error_instance.to_json());
            0
        }
    }
}

/// Implementation of the native handle access method.
#[unsafe(no_mangle)]
extern "C" fn pattern_native_handle(l: LuaState) -> c_int {
    // Get the table containing bytes of the native handle
    get_field(l, 1, NATIVE_HANDLE_TABLE_FIELD);
    let bytes_length = get_length(l, -1);
    let mut bytes = vec![];
    for i in 1..=bytes_length {
        get_index(l, -1, i as i32);
        bytes.push(get_integer(l, -1) as u8);
        pop(l, 1);
    }
    pop(l, 1);

    // Create the pointer from its bytes and store it in the pattern value
    let mut regex_ptr: *mut Regex = ptr::null_mut();
    let mut regex_addr: usize = 0;
    for i in (0..bytes.len()).rev() {
        regex_addr = (regex_addr << 8) | bytes[i] as usize;
    }
    regex_ptr = regex_ptr.with_addr(regex_addr);
    push_user_data_ptr(l, regex_ptr);
    set_field(l, -2, NATIVE_HANDLE_FIELD);

    // Register the pattern Lua value for garbage collection
    register_for_gc(l, -1);

    // Finally get the valid field and return it
    get_field(l, -1, NATIVE_HANDLE_FIELD);
    1
}

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
