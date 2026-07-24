//! # LKQL "Stream" type
//!
//! This module defines the LKQL "Stream" type. A stream in LKQL is a lazy
//! collection that can be indexed and iterated. This is a polymorphic type,
//! you can find all implementations of it in submodules.

use crate::{
    builtins::{
        traits::{
            indexable,
            iterable::{
                self, DEFAULT_ITERABLE_ALL, DEFAULT_ITERABLE_ANY, DEFAULT_ITERABLE_FILTER,
                DEFAULT_ITERABLE_FIND, DEFAULT_ITERABLE_FLAT_MAP, DEFAULT_ITERABLE_FLATTEN,
                DEFAULT_ITERABLE_MAP, DEFAULT_ITERABLE_REDUCE, DEFAULT_ITERABLE_TO_LIST,
                ITERATOR_FIELD,
            },
            sized,
        },
        types::{
            BuiltinType, IMG_FIELD, OverloadTarget, TypeField, TypeImplementation,
            TypeImplementationKind, list,
        },
    },
    errors::HEAD_OF_EMPTY_STEAM,
    lua::{LuaState, push_string},
    runtime::{Function, LKQL_ERROR_GLOBAL_NAME, LkqlParam, RuntimeValue},
};
use const_format::formatcp;
use std::ffi::c_int;

pub mod filtering;
pub mod flattening;
pub mod lazy_comprehension;
pub mod mapping;
pub mod selector_list;

/// Name of the internal method used to get the next element of the stream,
/// there is no default implementation for this method, specializations must
/// implement it.
/// This method must return `nil` if there is no more elements in the stream.
const INTERNAL_NEXT_FIELD: &str = "field@internal_next";

/// Name of the internal field containing the current size of the stream cache.
const CACHE_SIZE_FIELD: &str = "field@cache_size";

pub const TYPE: BuiltinType = BuiltinType {
    tag: list::TYPE.tag + 1,
    traits: &[&indexable::TRAIT, &iterable::TRAIT, &sized::TRAIT],
    implementation_variant: TypeImplementationKind::new_poly(
        BASE_IMPLEMENTATION,
        &[
            filtering::SPECIALIZATION,
            flattening::SPECIALIZATION,
            lazy_comprehension::SPECIALIZATION,
            mapping::SPECIALIZATION,
            selector_list::SPECIALIZATION,
        ],
    ),
};

const BASE_IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Stream",
    fields: &[
        IMG_FIELD,
        ("length", TypeField::Property(LENGTH)),
        (ITERATOR_FIELD, TypeField::Property(ITERATOR)),
        ("any", TypeField::Value(DEFAULT_ITERABLE_ANY)),
        ("all", TypeField::Value(DEFAULT_ITERABLE_ALL)),
        ("find", TypeField::Value(DEFAULT_ITERABLE_FIND)),
        ("flatten", TypeField::Property(DEFAULT_ITERABLE_FLATTEN)),
        ("filter", TypeField::Value(DEFAULT_ITERABLE_FILTER)),
        ("map", TypeField::Value(DEFAULT_ITERABLE_MAP)),
        ("flat_map", TypeField::Value(DEFAULT_ITERABLE_FLAT_MAP)),
        ("reduce", TypeField::Value(DEFAULT_ITERABLE_REDUCE)),
        ("to_list", TypeField::Property(DEFAULT_ITERABLE_TO_LIST)),
        ("head", TypeField::Property(HEAD)),
        ("head_or", TypeField::Value(HEAD_OR)),
    ],
    overloads: &[(OverloadTarget::ToString, Function::CFunction(stream_tostring))],
    index_method: Some(INDEX),
    registering_function: None,
};

/// Overload of "__tostring" for the "Stream" type.
#[unsafe(no_mangle)]
extern "C" fn stream_tostring(l: LuaState) -> c_int {
    // We don't want to compute the content of a stream when displaying it
    push_string(l, "<Stream>");
    1
}

/// Lua function to get the length of a stream.
const LENGTH: Function = Function::LuaFunction(
    "function (self)
        local _ = self[0]
        return #self
    end",
);

/// Lua function to get an iterator for a stream.
const ITERATOR: Function = Function::LuaFunction(
    "function (self)
        local cursor = 1
        local finished = false
        return function()
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

/// Implementation of the "head" method.
const HEAD: Function = Function::LuaFunction(formatcp!(
    "function (self)
        return self[1] or _G['{LKQL_ERROR_GLOBAL_NAME}']('{}')
    end",
    HEAD_OF_EMPTY_STEAM.id,
));

/// Implementation of the "head_or" method.
const HEAD_OR: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[LkqlParam::new("self"), LkqlParam::new("default_val")],
    body: "return self[1] or default_val",
});

/// Lua function used to index inside a stream.
const INDEX: Function = Function::LuaFunction(formatcp!(
    "function(self, field)
        -- Check if the field is a number, in that case initialize the cache to
        -- this index.
        if type(field) == 'number' then
            -- Prepare working variables
            local next_fn = self['{INTERNAL_NEXT_FIELD}']
            local next_value = nil
            local cache_size = self['{CACHE_SIZE_FIELD}'] or 0

            while cache_size < field or field < 1 do
                -- Get the next value and ensure it is not nil
                next_value = next_fn(self)
                if next_value == nil then break end

                -- Initialize the cache and make it grow
                cache_size = cache_size + 1
                self[cache_size] = next_value
            end

            -- Finally update the stream state and return the result
            self['{CACHE_SIZE_FIELD}'] = cache_size
            return next_value
        end

        -- In other cases, perform the generic indexing process

        -- Check in type properties
        res = __uv[2][field]
        if res ~= nil then
            return res(self)
        end

        -- Then get the result in type fields
        return __uv[1][field]
    end",
));
