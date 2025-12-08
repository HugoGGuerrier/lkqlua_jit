//! # LKQL "Object" type
//!
//! This module defines the LKQL "Object" type.

use std::ffi::c_int;

use crate::{
    lua::{LuaState, get_field, get_next_pair, get_string, pop, push_nil, push_string},
    runtime::{
        FunctionValue,
        builtins::{
            functions::lkql_img,
            types::{
                BuiltinType, BuiltinTypeField, OverloadTarget, list, register_metatable_in_globals,
            },
        },
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    name: "Object",
    tag: list::TYPE.tag + 1,
    fields: &[("img", BuiltinTypeField::Property(FunctionValue::CFunction(lkql_img)))],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(obj_tostring))],
    index_method: None,
    register_function: register_metatable_in_globals,
};

/// Overload of "__tostring" for the "Object" type
#[unsafe(no_mangle)]
unsafe extern "C" fn obj_tostring(l: LuaState) -> c_int {
    // Create the vector to place pairs in
    let mut pairs: Vec<(&str, &str)> = Vec::new();

    // Iterate over all pairs in the object
    push_nil(l);
    while get_next_pair(l, 1) {
        get_field(l, -1, "img");
        pairs.push((get_string(l, -3).unwrap(), get_string(l, -1).unwrap()));
        pop(l, 2);
    }

    // Sort the pair vector and create a string from it
    pairs.sort_by(|(left_name, _), (right_name, _)| left_name.cmp(right_name));

    // Finally place it on the stack and return 1
    push_string(
        l,
        &format!(
            "{{{}}}",
            pairs
                .into_iter()
                .map(|(name, value)| format!("\"{name}\": {value}"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    );
    1
}
