//! # LKQL "List" type
//!
//! This module defines the LKQL "List" type.

use crate::{
    builtins::{
        functions::lkql_img,
        traits::{
            indexable,
            iterable::{self, ITERATOR_FIELD},
            sized::{self, DEFAULT_SIZED_LENGTH},
        },
        types::{
            BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
            tuple,
        },
    },
    lua::{LuaState, get_field, get_index, get_length, get_string, push_string, set_top},
    runtime::FunctionValue,
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: tuple::TYPE.tag + 1,
    traits: &[&indexable::TRAIT, &iterable::TRAIT, &sized::TRAIT],
    implementation_kind: TypeImplementationKind::Monomorphic { implementation: IMPLEMENTATION },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "List",
    fields: &[
        ("img", TypeField::Property(FunctionValue::CFunction(lkql_img))),
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
        (
            ITERATOR_FIELD,
            TypeField::Property(FunctionValue::LuaFunction(LIST_ITERATOR)),
        ),
    ],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(list_tostring))],
    index_method: None,
    registering_function: None,
};

/// Lua source that represents the "field@iterator" property on the "List"
/// type.
const LIST_ITERATOR: &str = "function (_, self)
    local size = #self
    local cursor = 0
    return function ()
        if cursor < size then
            cursor = cursor + 1
            return self[cursor]
        else
            return nil
        end
    end
end";

/// Overload of "__tostring" for the "List" type
#[unsafe(no_mangle)]
unsafe extern "C" fn list_tostring(l: LuaState) -> c_int {
    // Get image of items inside the list
    let list_len = get_length(l, 1);
    let mut item_images = Vec::with_capacity(list_len);
    for i in 1..list_len + 1 {
        get_index(l, 1, i as i32);
        get_field(l, 2, "img");
        item_images.push(get_string(l, 3).unwrap());
        set_top(l, 1);
    }

    // Then create the list representation
    push_string(l, &format!("[{}]", item_images.join(", ")));
    1
}
