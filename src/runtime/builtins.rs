//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use crate::{
    lua::LuaCFunction,
    runtime::builtins::{
        functions::{BuiltinFunction, lkql_img, lkql_print},
        types::{BuiltinMethod, BuiltinType, MetatableRegisteringFunction, OverloadTarget},
        values::{BuiltinValue, BuiltinValueCreator},
    },
};

pub mod functions;
pub mod types;
pub mod values;

/// Allocate a new vector and populate it with all LKQL built-in functions,
/// then return it.
pub fn get_builtin_functions() -> Vec<BuiltinFunction> {
    fn b(name: &'static str, c_function: LuaCFunction) -> BuiltinFunction {
        BuiltinFunction { name, c_function }
    }
    vec![b("print", lkql_print), b("img", lkql_img)]
}

/// Allocate a new vector and populate it with all LKQL built-in types, then
/// return it.
pub fn get_builtin_types() -> Vec<BuiltinType> {
    /// Shortcut function to create a type.
    fn b(
        name: &'static str,
        methods: &'static [(&'static str, BuiltinMethod)],
        overloads: &'static [(OverloadTarget, LuaCFunction)],
        register_function: MetatableRegisteringFunction,
    ) -> BuiltinType {
        BuiltinType {
            name,
            methods: methods
                .iter()
                .map(|(name, method)| (String::from(*name), method.clone()))
                .collect(),
            overloads: overloads.iter().map(|(target, f)| (*target, *f)).collect(),
            register_function,
        }
    }

    vec![
        b(
            types::unit::NAME,
            &types::unit::METHODS,
            &types::unit::OVERLOADS,
            types::register_metatable_in_globals,
        ),
        b(
            types::int::NAME,
            &types::int::METHODS,
            &types::int::OVERLOADS,
            types::int::register_metatable,
        ),
        b(
            types::str::NAME,
            &types::str::METHODS,
            &types::str::OVERLOADS,
            types::str::register_metatable,
        ),
        b(
            types::bool::NAME,
            &types::bool::METHODS,
            &types::bool::OVERLOADS,
            types::bool::register_metatable,
        ),
    ]
}

/// Allocate a new vector and populate it with all LKQL built-in values, then
/// return it.
pub fn get_builtin_values() -> Vec<BuiltinValue> {
    fn b(name: &'static str, value_creator: BuiltinValueCreator) -> BuiltinValue {
        BuiltinValue { name, value_creator }
    }
    vec![b(values::UNIT_VALUE_NAME, values::create_unit_value)]
}
