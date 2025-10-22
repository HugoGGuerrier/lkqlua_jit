//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use std::ffi::c_int;

use crate::{
    lua::{LuaState, push_string},
    runtime::{
        RuntimeTypeField,
        builtins::{
            functions::lkql_img,
            types::{BuiltinType, OverloadTarget, register_metatable_in_globals},
        },
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    name: "Unit",
    tag: 0,
    fields: &[("img", RuntimeTypeField::Property(lkql_img))],
    overloads: &[(OverloadTarget::ToString, unit_tostring)],
    register_function: register_metatable_in_globals,
};

/// Overload of "__tostring" for the "Unit" type
unsafe extern "C" fn unit_tostring(l: LuaState) -> c_int {
    push_string(l, "()");
    1
}
