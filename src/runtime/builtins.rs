//! # LKQL Built-ins
//!
//! This module contains information and implementation for every built-in
//! element of the LKQL language (functions and types).

use std::collections::HashMap;

use crate::{
    lua::{
        LuaCFunction, LuaState, get_global, push_c_function, push_integer, push_string, push_table,
        set_metatable,
    },
    runtime::builtins::{
        functions::{lkql_img, lkql_print},
        types::{
            BuiltinMethod, BuiltinType, MetatableRegisteringFunction, OverloadTarget,
            metatable_global_field,
        },
    },
};

pub mod functions;
pub mod types;

/// Name of the binding leading to the "Unit" singleton.
pub const UNIT_VALUE_NAME: &str = "value@unit";
pub fn create_unit_value(l: LuaState) {
    push_table(l, 0, 0);
    get_global(l, &metatable_global_field(types::unit::NAME));
    set_metatable(l, -2);
}

/// This type represents a builder for a built-in values. This function should
/// push the value on the top of the stack of the provided Lua state.
pub type BuiltinValueBuilder = fn(LuaState);

/// This type represents a "static" value that can be used by associa ting it
/// to a symbol.
#[derive(Debug, Clone)]
pub enum BuiltinValue {
    Integer(isize),
    String(String),
    Function(LuaCFunction),

    /// Create the value by calling the associated Lua C function that should
    /// push it on the top of the stack.
    FromBuilder(BuiltinValueBuilder),
}

impl BuiltinValue {
    /// Push the value on the top of the stack in the provided Lua state.
    pub fn push_on_stack(&self, l: LuaState) {
        match self {
            BuiltinValue::Integer(i) => push_integer(l, *i),
            BuiltinValue::String(s) => push_string(l, &s),
            BuiltinValue::Function(f) => push_c_function(l, *f),
            BuiltinValue::FromBuilder(builder) => builder(l),
        }
    }
}

/// This type represents a built-in symbol binding, this symbol is accessible
/// in all lexical environments.
pub struct BuiltinBinding {
    pub name: &'static str,
    pub value: BuiltinValue,
}

/// Allocate a new vector and populate it with all LKQL built-in functions,
/// then return it.
pub fn get_builtin_bindings() -> Vec<BuiltinBinding> {
    fn b(name: &'static str, value: BuiltinValue) -> BuiltinBinding {
        BuiltinBinding { name, value }
    }
    vec![
        b("print", BuiltinValue::Function(lkql_print)),
        b("img", BuiltinValue::Function(lkql_img)),
        b(UNIT_VALUE_NAME, BuiltinValue::FromBuilder(create_unit_value)),
    ]
}

/// Allocate a new vector and populate it with all LKQL built-in types, then
/// return it.
pub fn get_builtin_types() -> Vec<BuiltinType> {
    let mut known_tags = HashMap::new();
    let mut b = |name: &'static str,
                 tag: isize,
                 methods: &'static [(&'static str, BuiltinMethod)],
                 overloads: &'static [(OverloadTarget, LuaCFunction)],
                 register_function: MetatableRegisteringFunction|
     -> BuiltinType {
        if let Some(previous) = known_tags.insert(tag, name) {
            panic!("Multiple built-in types with the tag {}: {} and {}", tag, name, previous);
        }
        BuiltinType {
            name,
            tag,
            methods: methods
                .iter()
                .map(|(name, method)| (String::from(*name), method.clone()))
                .collect(),
            overloads: overloads.iter().map(|(target, f)| (*target, *f)).collect(),
            register_function,
        }
    };

    vec![
        b(
            types::unit::NAME,
            types::unit::TAG,
            &types::unit::METHODS,
            &types::unit::OVERLOADS,
            types::register_metatable_in_globals,
        ),
        b(
            types::bool::NAME,
            types::bool::TAG,
            &types::bool::METHODS,
            &types::bool::OVERLOADS,
            types::bool::register_metatable,
        ),
        b(
            types::int::NAME,
            types::int::TAG,
            &types::int::METHODS,
            &types::int::OVERLOADS,
            types::int::register_metatable,
        ),
        b(
            types::str::NAME,
            types::str::TAG,
            &types::str::METHODS,
            &types::str::OVERLOADS,
            types::str::register_metatable,
        ),
        b(
            types::tuple::NAME,
            types::tuple::TAG,
            &types::tuple::METHODS,
            &types::tuple::OVERLOADS,
            types::register_metatable_in_globals,
        ),
    ]
}
