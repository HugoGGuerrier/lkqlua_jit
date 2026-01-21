//! # LKQL "Tuple" type
//!
//! This module defines the LKQL "Tuple" type.

use crate::{
    builtins::{
        functions::lkql_img,
        traits,
        types::{
            BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind, str,
        },
    },
    engine::FunctionValue,
    lua::{LuaState, get_field, get_index, get_length, get_string, push_string, set_top},
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: str::TYPE.tag + 1,
    traits: &[&traits::indexable::TRAIT],
    implementation_kind: TypeImplementationKind::Monomorphic { implementation: IMPLEMENTATION },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Tuple",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(lkql_img)))],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(tuple_tostring))],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Tuple" type
#[unsafe(no_mangle)]
unsafe extern "C" fn tuple_tostring(l: LuaState) -> c_int {
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
