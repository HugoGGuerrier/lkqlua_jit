//! # Mapping stream implementation
//!
//! This module defines the "mapping" implementation of the "Stream" LKQL type.
//! This implementation generates a new stream by processing another one,
//! applying a mapping function on each of its elements.

use crate::{
    builtins::types::{TypeField, TypeImplementation, stream::INTERNAL_NEXT_FIELD},
    runtime::{Function, RuntimeValue},
};
use const_format::formatcp;

/// Name of the field where the source iterator providing values to map is
/// stored.
pub const SOURCE_ITERATOR_FIELD: &str = "field@source_iterator";

/// Name of the field where the mapping function is stored.
pub const MAP_FUNCTION_FIELD: &str = "field@map_function";

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "MapStream",
    fields: &[(INTERNAL_NEXT_FIELD, TypeField::Value(NEXT))],
    overloads: &[],
    index_method: None,
    registering_function: None,
};

/// Lua function called when fetching the next element of this stream.
const NEXT: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(formatcp!(
    "function (self)
        local next = self['{SOURCE_ITERATOR_FIELD}']()
        if next ~= nil then
            return self['{MAP_FUNCTION_FIELD}'](nil, next)
        end
        return nil
    end"
)));
