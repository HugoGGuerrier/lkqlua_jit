//! # Filtering stream implementation
//!
//! This module defines the "filtering" implementation of the "Stream" LKQL
//! type. This implementation generates a new stream by processing another one,
//! opting out all elements that don't validate a predication function.

use crate::{
    builtins::types::{TypeField, TypeImplementation, stream::INTERNAL_NEXT_FIELD},
    runtime::{Function, RuntimeValue},
};
use const_format::formatcp;

/// Name of the field where the source iterator providing values to filter is
/// stored.
pub const SOURCE_ITERATOR_FIELD: &str = "field@source_iterator";

/// Name of the field where the predicate function is stored.
pub const PREDICATE_FUNCTION_FIELD: &str = "field@predicate_function";

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "FilterStream",
    fields: &[(INTERNAL_NEXT_FIELD, TypeField::Value(NEXT))],
    overloads: &[],
    index_method: None,
    registering_function: None,
};

const NEXT: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(formatcp!(
    "function (self)
        local next = self['{SOURCE_ITERATOR_FIELD}']()
        while next ~= nil do
            if self['{PREDICATE_FUNCTION_FIELD}'](nil, next) == true then
                return next
            end
            next = self['{SOURCE_ITERATOR_FIELD}']()
        end
        return nil
    end"
)));
