//! # LKQL "Str" type
//!
//! This module defines the LKQL "Str" type.

use std::{ffi::c_int, path::PathBuf, str::FromStr};

use crate::{
    lua::{FunctionValue, LuaState, copy_value, get_string, push_string, set_metatable},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinType, BuiltinTypeField, int},
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    name: "Str",
    tag: int::TYPE.tag + 1,
    fields: &[
        ("img", BuiltinTypeField::Property(FunctionValue::CFunction(lkql_img))),
        (
            "base_name",
            BuiltinTypeField::Method(FunctionValue::CFunction(str_base_name)),
        ),
    ],
    overloads: &[],
    register_function: register_metatable,
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &'static BuiltinType) {
    push_string(l, "");
    copy_value(l, -2);
    set_metatable(l, -2);
}

/// The "base_name" property for the "Str" type
unsafe extern "C" fn str_base_name(l: LuaState) -> c_int {
    let path = PathBuf::from_str(get_string(l, 2).unwrap()).unwrap();
    if let Some(base_name) = path.file_name() {
        push_string(l, &base_name.to_string_lossy());
    }
    1
}
