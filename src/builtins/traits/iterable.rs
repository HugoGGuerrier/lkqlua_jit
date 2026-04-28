//! # LKQL "Iterable" trait
//!
//! This module defines the "Iterable" LKQL trait. This trait requires that
//! implementing type define an `field@iterator` property that return an
//! iterator value for the instance.
//! Iterators are represented by functional values that are called to get the
//! "next" element in the source iterable.

use crate::{
    builtins::traits::{BuiltinTrait, RequiredField},
    engine::{FunctionValue, RuntimeValue},
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

/// Default implementation of the "any" method on iterable values.
pub const DEFAULT_ITERABLE_ANY: RuntimeValue =
    RuntimeValue::Function(FunctionValue::LuaFunction(formatcp!(
        "function(_, self, predicate)
            local it = self['{iterator}']
            local next = it()
            while next ~= nil do
                if predicate(nil, next) then
                    return true
                end
                next = it()
            end
            return false
        end",
        iterator = ITERATOR_FIELD,
    )));

/// Default implementation of the "all" method on iterable values.
pub const DEFAULT_ITERABLE_ALL: RuntimeValue =
    RuntimeValue::Function(FunctionValue::LuaFunction(formatcp!(
        "function(_, self, predicate)
            local it = self['{iterator}']
            local next = it()
            while next ~= nil do
                if not predicate(nil, next) then
                    return false
                end
                next = it()
            end
            return true
        end",
        iterator = ITERATOR_FIELD,
    )));

/// Default implementation of the "reduce" method on iterable values.
pub const DEFAULT_ITERABLE_REDUCE: RuntimeValue =
    RuntimeValue::Function(FunctionValue::LuaFunction(formatcp!(
        "function(_, self, fn, init)
            local it = self['{iterator}']
            local next = it()
            local res = init
            while next ~= nil do
                res = fn(nil, res, next)
                next = it()
            end
            return res
        end",
        iterator = ITERATOR_FIELD,
    )));
