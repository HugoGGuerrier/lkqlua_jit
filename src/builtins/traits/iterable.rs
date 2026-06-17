//! # LKQL "Iterable" trait
//!
//! This module defines the "Iterable" LKQL trait. This trait requires that
//! implementing type define an `field@iterator` property that return an
//! iterator value for the instance.
//! Iterators are represented by functional values that are called to get the
//! "next" element in the source iterable.

use crate::{
    builtins::{
        traits::{BuiltinTrait, RequiredField},
        types::TypeRef,
    },
    runtime::{Function, LkqlParam, RuntimeValue},
};
use const_format::formatcp;

/// Name of the field to access to get an iterator for a value.
pub const ITERATOR_FIELD: &str = "field@iterator";

pub const TRAIT: BuiltinTrait = BuiltinTrait {
    name: "Iterable",
    required_overloads: &[],
    required_fields: &[
        RequiredField::Property(ITERATOR_FIELD),
        RequiredField::Value("any"),
        RequiredField::Value("all"),
        RequiredField::Value("reduce"),
    ],
};

/// Define a list of parameters required by "any" and "all" methods that
/// implementations may reuse.
pub const ANY_AND_ALL_PARAMS: &[LkqlParam] = &[
    LkqlParam::new("self"),
    LkqlParam::with_type("predicate", TypeRef::Function),
];

/// Default implementation of the "any" method on iterable values.
pub const DEFAULT_ITERABLE_ANY: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: ANY_AND_ALL_PARAMS,
    body: formatcp!(
        "local it = self['{ITERATOR_FIELD}']
        local next = it()
        while next ~= nil do
            if predicate(nil, next) then
                return true
            end
            next = it()
        end
        return false",
    ),
});

/// Default implementation of the "all" method on iterable values.
pub const DEFAULT_ITERABLE_ALL: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: ANY_AND_ALL_PARAMS,
    body: formatcp!(
        "local it = self['{ITERATOR_FIELD}']
        local next = it()
        while next ~= nil do
            if not predicate(nil, next) then
                return false
            end
            next = it()
        end
        return true",
    ),
});

/// List of parameters that the "reduce" method require.
pub const REDUCE_PARAMS: &[LkqlParam] = &[
    LkqlParam::new("self"),
    LkqlParam::with_type("fn", TypeRef::Function),
    LkqlParam::new("init"),
];

/// Default implementation of the "reduce" method on iterable values.
pub const DEFAULT_ITERABLE_REDUCE: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: REDUCE_PARAMS,
    body: formatcp!(
        "local it = self['{ITERATOR_FIELD}']
        local next = it()
        local res = init
        while next ~= nil do
            res = fn(nil, res, next)
            next = it()
        end
        return res",
    ),
});
