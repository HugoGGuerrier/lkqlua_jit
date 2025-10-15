//! # LKQL "Int" type
//!
//! This module defines the LKQL "Int" type.

use crate::{
    lua::{LuaCFunction, LuaState, copy_value, push_number, set_metatable},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinMethod, BuiltinType, OverloadTarget, bool},
    },
};

pub const NAME: &str = "Int";
pub const TAG: isize = bool::TAG + 1;
pub const METHODS: [(&'static str, BuiltinMethod); 1] =
    [("img", BuiltinMethod { function: lkql_img, is_property: true })];
pub const OVERLOADS: [(OverloadTarget, LuaCFunction); 0] = [];

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &Box<BuiltinType>) {
    push_number(l, 0f64);
    copy_value(l, -2);
    set_metatable(l, -2);
}
