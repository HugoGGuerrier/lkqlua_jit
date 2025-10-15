//! # LKQL "Tuple" type
//!
//! This module defines the LKQL "Tuple" type.

use std::ffi::c_int;

use crate::{
    lua::{
        LuaCFunction, LuaState, get_field, get_index, get_length, get_string, push_string, set_top,
    },
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinMethod, OverloadTarget, str},
    },
};

pub const NAME: &str = "Tuple";
pub const TAG: isize = str::TAG + 1;
pub const METHODS: [(&'static str, BuiltinMethod); 1] =
    [("img", BuiltinMethod { function: lkql_img, is_property: true })];
pub const OVERLOADS: [(OverloadTarget, LuaCFunction); 1] =
    [(OverloadTarget::ToString, tuple_tostring)];

/// Overload of "__tostring" for the "Tuple" type
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
