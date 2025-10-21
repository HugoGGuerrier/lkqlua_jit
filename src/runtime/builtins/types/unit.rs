//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use std::ffi::c_int;

use crate::{
    lua::{LuaCFunction, LuaState, push_string},
    runtime::builtins::{
        functions::lkql_img,
        types::{BuiltinField, OverloadTarget},
    },
};

pub const NAME: &str = "Unit";
pub const TAG: isize = 0;
pub const FIELDS: [(&'static str, BuiltinField); 1] = [("img", BuiltinField::Property(lkql_img))];
pub const OVERLOADS: [(OverloadTarget, LuaCFunction); 1] =
    [(OverloadTarget::ToString, unit_tostring)];

/// Overload of "__tostring" for the "Unit" type
unsafe extern "C" fn unit_tostring(l: LuaState) -> c_int {
    push_string(l, "()");
    1
}
