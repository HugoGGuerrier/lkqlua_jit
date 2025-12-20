//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use std::collections::HashMap;

use crate::{
    lua::{LuaState, get_global, push_table, set_metatable},
    runtime::{
        FunctionValue, RuntimeValue,
        builtins::{
            functions::{lkql_img, lkql_print},
            types::BuiltinType,
        },
    },
};

pub mod functions;
pub mod types;
pub mod utils;

/// Name of the binding leading to the "Unit" singleton.
pub const UNIT_VALUE_NAME: &str = "value@unit";
pub fn create_unit_value(l: LuaState) {
    push_table(l, 0, 0);
    get_global(l, &types::unit::IMPLEMENTATION.global_field_name());
    set_metatable(l, -2);
}

/// This type represents a built-in symbol binding, this symbol is accessible
/// in all lexical environments.
pub struct BuiltinBinding {
    pub name: &'static str,
    pub value: RuntimeValue,
}

/// Allocate a new vector and populate it with all LKQL built-in functions,
/// then return it.
pub fn get_builtin_bindings() -> Vec<BuiltinBinding> {
    fn b(name: &'static str, value: RuntimeValue) -> BuiltinBinding {
        BuiltinBinding { name, value }
    }
    vec![
        b("print", RuntimeValue::Function(FunctionValue::CFunction(lkql_print))),
        b("img", RuntimeValue::Function(FunctionValue::CFunction(lkql_img))),
        b(UNIT_VALUE_NAME, RuntimeValue::FromBuilder(create_unit_value)),
    ]
}

/// Allocate a new vector and populate it with all LKQL built-in types, then
/// return it.
pub fn get_builtin_types() -> Vec<&'static BuiltinType> {
    let mut known_tags = HashMap::new();
    let mut b = |t: &'static BuiltinType| -> &'static BuiltinType {
        // Ensure the type tag is unique
        if let Some(previous) = known_tags.insert(t.tag(), t.display_name()) {
            panic!(
                "Multiple built-in types with the tag {}: {} and {}",
                t.tag(),
                t.display_name(),
                previous
            );
        }

        // Then just return the built-in type
        t
    };

    vec![
        b(&types::unit::TYPE),
        b(&types::bool::TYPE),
        b(&types::int::TYPE),
        b(&types::str::TYPE),
        b(&types::tuple::TYPE),
        b(&types::list::TYPE),
        b(&types::obj::TYPE),
    ]
}
