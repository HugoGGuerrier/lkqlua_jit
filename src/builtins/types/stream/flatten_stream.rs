//! # Flatten stream implementation
//!
//! This module defines the "flattening" implementation of the "Stream" LKQL
//! type. This implementation generates a new stream by processing another one,
//! concatenating each elements of it to other ones.

use crate::{
    builtins::{
        traits::iterable::ITERATOR_FIELD,
        types::{TypeField, TypeImplementation, stream::INTERNAL_NEXT_FIELD},
    },
    runtime::{Function, RuntimeValue},
};
use const_format::formatcp;

/// Name of the field where the source iterator providing values to flatten is
/// stored.
pub const SOURCE_ITERATOR_FIELD: &str = "field@source_iterator";

/// Name of the field where the current inner iterator is stored.
pub const INNER_ITERATOR_FIELD: &str = "field@inner_iterator";

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "FlattenStream",
    fields: &[(INTERNAL_NEXT_FIELD, TypeField::Value(NEXT))],
    overloads: &[],
    index_method: None,
    registering_function: None,
};

/// Lua function called when fetching the next element of this stream.
const NEXT: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(formatcp!(
    "function (self)
        while self['{INNER_ITERATOR_FIELD}'] ~= nil do
            local next = self['{INNER_ITERATOR_FIELD}']()
            if next ~= nil then
                return next
            else
                local next_inner = self['{SOURCE_ITERATOR_FIELD}']()
                if next_inner ~= nil then
                    self['{INNER_ITERATOR_FIELD}'] = next_inner['{ITERATOR_FIELD}']
                else
                    self['{INNER_ITERATOR_FIELD}'] = nil
                end
            end
        end
        return nil
    end"
)));
