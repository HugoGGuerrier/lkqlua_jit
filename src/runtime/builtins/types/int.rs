//! # LKQL "Int" type
//!
//! This module defines the LKQL "Int" type.

use crate::{
    lua::{LuaState, copy_value, push_number, set_metatable},
    runtime::{
        RuntimeTypeField,
        builtins::{
            functions::lkql_img,
            types::{BuiltinType, bool},
        },
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    name: "Int",
    tag: bool::TYPE.tag + 1,
    fields: &[("img", RuntimeTypeField::Property(lkql_img))],
    overloads: &[],
    register_function: register_metatable,
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &'static BuiltinType) {
    push_number(l, 0f64);
    copy_value(l, -2);
    set_metatable(l, -2);
}
