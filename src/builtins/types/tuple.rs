//! # LKQL "Tuple" type
//!
//! This module defines the LKQL "Tuple" type.

use crate::{
    builtins::{
        traits,
        types::{
            BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationVariant,
            img_property, pattern,
        },
    },
    lua::{LuaState, get_field, get_index, get_length, get_string, push_string, set_top},
    runtime::Function,
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: pattern::TYPE.tag + 1,
    traits: &[&traits::indexable::TRAIT],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Tuple",
    fields: &[("img", TypeField::Property(Function::CFunction(img_property)))],
    overloads: &[
        (OverloadTarget::ToString, Function::CFunction(tuple_tostring)),
        (OverloadTarget::Eq, TUPLE_EQ),
    ],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Tuple" type
#[unsafe(no_mangle)]
extern "C" fn tuple_tostring(l: LuaState) -> c_int {
    // Get image of items inside the tuple
    let tuple_len = get_length(l, 1);
    let mut item_images = Vec::with_capacity(tuple_len);
    for i in 1..tuple_len + 1 {
        get_index(l, 1, i as i32);
        get_field(l, 2, "img");
        item_images.push(get_string(l, 3).unwrap());
        set_top(l, 1);
    }

    // Then create the tuple representation
    push_string(l, &format!("({})", item_images.join(", ")));
    1
}

/// Overload of "__eq" for the "Tuple" type
const TUPLE_EQ: Function = Function::LuaFunction(
    "function(self, other)
        -- Start by checking types
        if getmetatable(self) ~= getmetatable(other) then
            return false
        end

        -- Compare sizes
        if #self ~= #other then
            return false
        end

        -- Then compare elements
        for i, elem in ipairs(self) do
            if other[i] ~= elem then
                return false
            end
        end
        return true
    end",
);
