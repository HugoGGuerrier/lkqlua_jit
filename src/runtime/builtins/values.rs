//! # Built-in values
//!
//! This module contains all built-in value accessible during the LKQL runtime.

use crate::{
    lua::{LuaState, get_global, push_table, set_metatable},
    runtime::builtins::types::{self, metatable_global_field},
};

/// This type represents a built-in value. Such a value is place in the global
/// table before any execution.
pub struct BuiltinValue {
    pub name: &'static str,
    pub value_creator: BuiltinValueCreator,
}

/// This type represents function used to create built-in value. Function of
/// this type should create the value and place it on the top of the stack.
pub type BuiltinValueCreator = fn(LuaState);

/// --- The "Unit" singleton

pub const UNIT_VALUE_NAME: &str = "value@unit";
pub fn create_unit_value(l: LuaState) {
    push_table(l, 0, 0);
    get_global(l, &metatable_global_field(types::unit::NAME));
    set_metatable(l, -2);
}
