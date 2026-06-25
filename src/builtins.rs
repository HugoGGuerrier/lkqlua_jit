//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use crate::{
    builtins::{
        functions::{lkql_img, lkql_import, lkql_print, lkql_units},
        types::{
            BuiltinType, BuiltinTypeRepo,
            pattern::{PATTERN_CONSTRUCTOR, pattern_constructor},
        },
    },
    lua::{LuaState, get_global, push_table, set_metatable},
    runtime::{Function, LKQL_IMPORT_GLOBAL_NAME, RuntimeValue, UNIT_SINGLETON_GLOBAL_NAME},
};
use std::collections::HashMap;

pub mod functions;
pub mod traits;
pub mod types;
pub mod utils;

/// Allocate a new vector and populate it with all LKQL built-in functions,
/// then return it.
pub fn get_builtin_bindings() -> HashMap<&'static str, RuntimeValue> {
    let mut res = HashMap::new();
    let mut b = |name: &'static str, value: RuntimeValue| res.insert(name, value);

    // Add all builtins
    b(
        PATTERN_CONSTRUCTOR,
        RuntimeValue::Callable(Function::CFunction(pattern_constructor)),
    );
    b("print", RuntimeValue::Callable(Function::CFunction(lkql_print)));
    b("img", RuntimeValue::Callable(Function::CFunction(lkql_img)));
    b("units", RuntimeValue::Callable(Function::CFunction(lkql_units)));
    b(
        LKQL_IMPORT_GLOBAL_NAME,
        RuntimeValue::Callable(Function::CFunction(lkql_import)),
    );
    b(UNIT_SINGLETON_GLOBAL_NAME, RuntimeValue::FromBuilder(create_unit_value));

    // Then, return the result
    res
}

/// Allocate a new vector and populate it with all LKQL built-in types, then
/// return it.
pub fn get_builtin_types() -> BuiltinTypeRepo {
    let mut known_tags = HashMap::new();
    let mut b = |t: &'static BuiltinType| -> &'static BuiltinType {
        // Ensure the type tag is unique
        if let Some(previous) = known_tags.insert(t.tag, t.display_name()) {
            panic!(
                "Multiple built-in types with the tag {}: {} and {}",
                t.tag,
                t.display_name(),
                previous
            );
        }

        // Check that the type is correcly implementing its required traits
        for tr in t.traits {
            if let Err(missing) = tr.check_type(t) {
                panic!(
                    "Missing fields in {} (or one of its implementation): {}. Required by trait {}",
                    t.display_name(),
                    missing.join(" & "),
                    tr.name
                );
            }
        }

        // Then just return the built-in type
        t
    };

    BuiltinTypeRepo {
        registered_types: vec![
            b(&types::unit::TYPE),
            b(&types::bool::TYPE),
            b(&types::int::TYPE),
            b(&types::str::TYPE),
            b(&types::pattern::TYPE),
            b(&types::tuple::TYPE),
            b(&types::list::TYPE),
            b(&types::stream::TYPE),
            b(&types::obj::TYPE),
            b(&types::namespace::TYPE),
            b(&types::function::TYPE),
        ],
    }
}

/// Function to place the unit value in the provided Lua state.
pub fn create_unit_value(l: LuaState) {
    push_table(l, 0, 0);
    get_global(l, &types::unit::IMPLEMENTATION.global_field_name());
    set_metatable(l, -2);
}
