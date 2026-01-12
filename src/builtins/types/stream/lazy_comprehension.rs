//! # Lazy comprehension Stream implementation
//!
//! This module defines the "lazy comprehension" implementation of the "Stream"
//! LKQL type. This implementation generates a new stream by processing other
//! iterables with a body function.

use crate::{
    builtins::{
        traits::iterable::ITERATOR_FIELD,
        types::{TypeField, TypeImplementation, stream::INTERNAL_NEXT_FIELD},
    },
    runtime::FunctionValue,
};
use const_format::formatcp;

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "LazyComprehension",
    fields: &[(
        INTERNAL_NEXT_FIELD,
        TypeField::Method(FunctionValue::LuaFunction(LIST_COMP_NEXT_METHOD)),
    )],
    overloads: &[],
    index_method: None,
    registering_function: None,
};

/// Name of the field where the body of the comprehension is stored. This body
/// is a functional value that is going to be called with elements of source
/// collections to process. This function returns the result of the processing,
/// or `nil` if there is no return value.
pub const BODY_FIELD: &str = "field@body";

/// Name of the internal field containing collections to iterate on.
pub const COLLECTIONS_FIELD: &str = "field@collections";

/// Name of the field storing whether the work is finished in the comprehension
/// instance.
const FINISHED_FIELD: &str = "field@finished";

/// Name of the field that contains current iterators on collections to iterate
/// on.
const ITERATORS_FIELD: &str = "field@iterators";

/// Name of the internal field that contains current arguments to pass to the
/// comprehension body.
const ARGS_FIELD: &str = "field@args";

/// Lua function called when fetching the next element of the comprehension.
/// This function mutates the internal state of the `self` value.
const LIST_COMP_NEXT_METHOD: &str = formatcp!(
    "function (self)
    -- First of all check that there are remaining work
    if self['{finished}'] then
        return nil
    end

    -- Ensure iterators and arguments table are initialized
    if self['{iterators}'] == nil or self['{args}'] == nil then
        self['{iterators}'] = {{}}
        self['{args}'] = {{}}
        for i, coll in ipairs(self['{collections}']) do
            self['{iterators}'][i] = coll['{iterator}']
            self['{args}'][i] = self['{iterators}'][i]()
        end
    end

    -- Get the next element of the comprehension
    local res = nil
    while res == nil and not self['{finished}'] do
        -- First, call the body with computed arguments
        res = self['{body}'](unpack(self['{args}']))

        -- Then compute the next argument set
        for i = #self['{iterators}'], 1, -1 do
            self['{args}'][i] = self['{iterators}'][i]()
            if self['{args}'][i] == nil then
                if i == 1 then
                    self['{finished}'] = true
                else
                    self['{iterators}'][i] = self['{collections}'][i]['{iterator}']
                    self['{args}'][i] = self['{iterators}'][i]()
                end
            else
                break
            end
        end
    end
    return res
end",
    finished = FINISHED_FIELD,
    iterators = ITERATORS_FIELD,
    collections = COLLECTIONS_FIELD,
    iterator = ITERATOR_FIELD,
    args = ARGS_FIELD,
    body = BODY_FIELD,
);
