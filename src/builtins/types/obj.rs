//! # LKQL "Object" type
//!
//! This module defines the LKQL "Object" type.

use crate::{
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationVariant,
        img_property, stream,
    },
    lua::{LuaState, get_field, get_next_pair, get_string, pop, push_nil, push_string},
    runtime::Function,
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: stream::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Object",
    fields: &[("img", TypeField::Property(Function::CFunction(img_property)))],
    overloads: &[
        (OverloadTarget::ToString, Function::CFunction(obj_tostring)),
        (OverloadTarget::Eq, OBJ_EQ),
    ],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Object" type.
#[unsafe(no_mangle)]
extern "C" fn obj_tostring(l: LuaState) -> c_int {
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

/// Overload of "__eq" for the "Object" type.
const OBJ_EQ: Function = Function::LuaFunction(
    "function(self, other)
        -- Start by checking types
        if getmetatable(self) ~= getmetatable(other) then
            return false
        end

        -- Iterate on self keys and check their values
        for key, elem in pairs(self) do
            if self[key] ~= other[key] then
                return false
            end
        end

        -- Check extra keys in the other object
        for key, _ in pairs(other) do
            if self[key] == nil then
                return false
            end
        end

        -- Finally, return the positive result
        return true
    end",
);
