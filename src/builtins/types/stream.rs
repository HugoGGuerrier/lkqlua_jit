//! # LKQL "Stream" type
//!
//! This module defines the LKQL "Stream" type. A stream in LKQL is a lazy
//! collection that can be indexed and iterated. This is a polymorphic type,
//! you can find all implementations of it in submodules.

use crate::{
    builtins::{
        functions::lkql_img,
        traits::{
            indexable,
            iterable::{self, ITERATOR_FIELD},
            sized,
        },
        types::{
            BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
            list,
        },
    },
    lua::{LuaState, push_string},
    runtime::FunctionValue,
};
use const_format::formatcp;
use std::ffi::c_int;

pub mod lazy_comprehension;

pub const TYPE: BuiltinType = BuiltinType {
    tag: list::TYPE.tag + 1,
    traits: &[&indexable::TRAIT, &iterable::TRAIT, &sized::TRAIT],
    implementation_kind: TypeImplementationKind::Polymorphic {
        base_implementation: TypeImplementation {
            name: "Stream",
            fields: &[
                ("img", TypeField::Property(FunctionValue::CFunction(lkql_img))),
                ("length", TypeField::Property(STREAM_LENGTH)),
                (ITERATOR_FIELD, TypeField::Property(STREAM_ITERATOR)),
            ],
            overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(stream_tostring))],
            index_method: Some(STREAM_INDEX),
            registering_function: None,
        },
        specializations: &[lazy_comprehension::SPECIALIZATION],
    },
};

/// Overload of "__tostring" for the "Stream" type.
#[unsafe(no_mangle)]
unsafe extern "C" fn stream_tostring(l: LuaState) -> c_int {
    // We don't want to compute the content of a stream when displaying it
    push_string(l, "<Stream>");
    1
}

/// Lua function to get the length of a stream.
const STREAM_LENGTH: FunctionValue = FunctionValue::LuaFunction(
    "function (_, self)
    local _ = self[0]
    return #self
end",
);

/// Lua function to get an iterator for a stream.
const STREAM_ITERATOR: FunctionValue = FunctionValue::LuaFunction(
    "function (_, self)
    local cursor = 1
    local finished = false
    return function ()
        if not finished then
            local res = self[cursor]
            if res == nil then
                finished = true
                return nil
            else
                cursor = cursor + 1
                return res
            end
        else
            return nil
        end
    end
end",
);

/// Name of the internal method used to get the next element of the stream,
/// there is no default implementation for this method, specializations must
/// implement it.
/// This method must return `nil` if there is no more elements in the stream.
const INTERNAL_NEXT_FIELD: &str = "field@internal_next";

/// Name of the internal field containing the current size of the stream cache.
const CACHE_SIZE_FIELD: &str = "field@cache_size";

/// Lua function used to index inside a stream.
const STREAM_INDEX: FunctionValue = FunctionValue::LuaFunction(formatcp!(
    "function (self, field)
    -- Check if the field is a number, in that case initialize the cache to
    -- this index.
    if type(field) == 'number' then
        -- Prepare working variables
        local next_value = nil
        local cache_size = self['{cache_size_field}'] or 0

        while cache_size < field or field < 1 do
            -- Get the next value and ensure it is not nil
            next_value = self['{next_field}'](self)
            if next_value == nil then break end

            -- Initialize the cache and make it grow
            cache_size = cache_size + 1
            self[cache_size] = next_value
        end

        -- Finally update the stream state and return the result
        self['{cache_size_field}'] = cache_size
        return next_value
    end

    -- In other cases, perform the generic indexing process

    -- Check in type fields
    local res = __uv[1][field]
    if res ~= nil then
        return res
    end

    -- Check in type properties
    res = __uv[2][field]
    if res ~= nil then
        return res(nil, self)
    end

    -- Check in type methods
    res = __uv[3][field]
    if res ~= nil then
        return res
    end

    -- Nothing has been found
    return nil
end",
    cache_size_field = CACHE_SIZE_FIELD,
    next_field = INTERNAL_NEXT_FIELD,
));
