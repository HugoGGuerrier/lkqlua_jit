//! # LKQL "Bool" type
//!
//! This module defines the LKQL "Bool" type.

use crate::{
    lua::{LuaCFunction, LuaState, copy_value, push_bool, set_metatable},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinField, BuiltinType, OverloadTarget, unit},
    },
};

pub const NAME: &str = "Bool";
pub const TAG: isize = unit::TAG + 1;
pub const FIELDS: [(&'static str, BuiltinField); 1] = [("img", BuiltinField::Property(lkql_img))];
pub const OVERLOADS: [(OverloadTarget, LuaCFunction); 0] = [];

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &Box<BuiltinType>) {
    push_bool(l, false);
    copy_value(l, -2);
    set_metatable(l, -2);
}
