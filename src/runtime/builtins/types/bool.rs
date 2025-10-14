//! # LKQL "Bool" type
//!
//! This module defines the LKQL "Bool" type.

use crate::{
    lua::{LuaCFunction, LuaState, copy_value, push_bool, set_metatable},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinMethod, BuiltinType, OverloadTarget},
    },
};

pub const NAME: &str = "Bool";
pub const METHODS: [(&'static str, BuiltinMethod); 1] =
    [("img", BuiltinMethod { function: lkql_img, is_property: true })];
pub const OVERLOADS: [(OverloadTarget, LuaCFunction); 0] = [];

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &Box<BuiltinType>) {
    push_bool(l, false);
    copy_value(l, -2);
    set_metatable(l, -2);
}
