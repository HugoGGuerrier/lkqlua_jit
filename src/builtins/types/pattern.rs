//! # LKQL "Pattern" type
//!
//! This module defines the LKQL "Pattern" type. This type represents
//! compiled regular expression and can be used as such.

use crate::{
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationVariant,
        img_property, str,
    },
    engine::FunctionValue,
    lua::{LuaState, get_field, get_user_data, pop, push_string},
};
use regex::Regex;
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: str::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Pattern",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(img_property)))],
    overloads: &[
        (OverloadTarget::ToString, FunctionValue::CFunction(pattern_tostring)),
        (OverloadTarget::Gc, FunctionValue::CFunction(pattern_gc)),
    ],
    index_method: None,
    registering_function: None,
};

/// The field in which the native regex handle is stored.
pub const NATIVE_HANDLE_FIELD: &str = "field@native_handle";

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
