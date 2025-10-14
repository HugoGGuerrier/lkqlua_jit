//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use std::ffi::c_int;

use crate::{
    lua::{LuaCFunction, LuaState, push_string},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinMethod, OverloadTarget},
    },
};

pub const NAME: &str = "Unit";
pub const METHODS: [(&'static str, BuiltinMethod); 1] =
    [("img", BuiltinMethod { function: lkql_img, is_property: true })];
pub const OVERLOADS: [(OverloadTarget, LuaCFunction); 1] =
    [(OverloadTarget::ToString, unit_tostring)];

/// Overload of "__tostring" for the "Unit" type
unsafe extern "C" fn unit_tostring(l: LuaState) -> c_int {
    push_string(l, "()");
    1
}
