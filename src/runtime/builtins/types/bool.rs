//! # LKQL "Bool" type
//!
//! This module defines the LKQL "Bool" type.

use crate::{
    lua::{FunctionValue, LuaState, copy_value, push_bool, set_metatable},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinType, BuiltinTypeField, unit},
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    name: "Bool",
    tag: unit::TYPE.tag + 1,
    fields: &[("img", BuiltinTypeField::Property(FunctionValue::CFunction(lkql_img)))],
    overloads: &[],
    register_function: register_metatable,
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &'static BuiltinType) {
    push_bool(l, false);
    copy_value(l, -2);
    set_metatable(l, -2);
}
