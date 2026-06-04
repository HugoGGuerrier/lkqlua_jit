//! # LKQL "Str" type
//!
//! This module defines the LKQL "Str" type.

use crate::{
    builtins::{
        traits::{self, sized::DEFAULT_SIZED_LENGTH},
        types::{
            BuiltinType, TypeField, TypeImplementation, TypeImplementationVariant, img_property,
            int,
        },
        utils::{get_int_param, get_string_param, verify_param},
    },
    engine::{FunctionValue, RuntimeValue},
    lua::{LuaState, copy_value, get_string, get_top, push_bool, push_string, set_metatable},
};
use std::{ffi::c_int, path::PathBuf, str::FromStr};

pub const TYPE: BuiltinType = BuiltinType {
    tag: int::TYPE.tag + 1,
    traits: &[&traits::sized::TRAIT],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Str",
    fields: &[
        ("img", TypeField::Property(FunctionValue::CFunction(img_property))),
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
        (
            "base_name",
            TypeField::Value(RuntimeValue::Function(FunctionValue::CFunction(str_base_name))),
        ),
        (
            "starts_with",
            TypeField::Value(RuntimeValue::Function(FunctionValue::CFunction(str_starts_with))),
        ),
        (
            "substring",
            TypeField::Value(RuntimeValue::Function(FunctionValue::CFunction(str_substring))),
        ),
    ],
    overloads: &[],
    index_method: None,
    registering_function: Some(register_metatable),
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &TypeImplementation) {
    push_string(l, "");
    copy_value(l, -2);
    set_metatable(l, -2);
}

/// The "base_name" method for the "Str" type
#[unsafe(no_mangle)]
extern "C" fn str_base_name(l: LuaState) -> c_int {
    let path = PathBuf::from_str(get_string(l, 2).unwrap()).unwrap();
    if let Some(base_name) = path.file_name() {
        push_string(l, &base_name.to_string_lossy());
    }
    1
}

/// The "starts_with" method for the "Str" type
#[unsafe(no_mangle)]
extern "C" fn str_starts_with(l: LuaState) -> c_int {
    let param_count = get_top(l) - 1;
    let this = get_string(l, 2).unwrap();
    let prefix = get_string_param(l, param_count, 2, "prefix", None);
    push_bool(l, this.starts_with(prefix));
    1
}

/// The "substring" function for the "Str" type
#[unsafe(no_mangle)]
extern "C" fn str_substring(l: LuaState) -> c_int {
    // Get parameters
    let param_count = get_top(l) - 1;
    let this = get_string(l, 2).unwrap();
    let start = get_int_param(l, param_count, 2, "start", None);
    let end = get_int_param(l, param_count, 3, "end", None);

    // Check bounds validity
    verify_param(l, "start", "start bound should be greater than 0", start > 0);
    verify_param(
        l,
        "end",
        "end bound should be greater that 0 and the start bound",
        end > start,
    );

    push_string(l, &this[(start - 1) as usize..end as usize]);
    1
}
