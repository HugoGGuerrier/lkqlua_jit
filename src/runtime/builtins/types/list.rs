//! # LKQL "List" type
//!
//! This module defines the LKQL "List" type.

use std::ffi::c_int;

use crate::{
    lua::{LuaState, get_field, get_index, get_length, get_string, push_string, set_top},
    runtime::{
        FunctionValue,
        builtins::{
            functions::lkql_img,
            traits::{self, sized::DEFAULT_SIZED_LENGTH},
            types::{
                BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
                tuple,
            },
        },
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: tuple::TYPE.tag + 1,
    traits: &[&traits::indexable::TRAIT, &traits::sized::TRAIT],
    implementation_kind: TypeImplementationKind::Monomorphic { implementation: IMPLEMENTATION },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "List",
    fields: &[
        ("img", TypeField::Property(FunctionValue::CFunction(lkql_img))),
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
    ],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(list_tostring))],
    index_method: None,
    registering_function: None,
};

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
