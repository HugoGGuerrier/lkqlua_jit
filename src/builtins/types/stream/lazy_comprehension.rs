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
    runtime::{Function, RuntimeValue},
};
use const_format::formatcp;

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "LazyComprehension",
    fields: &[(
        INTERNAL_NEXT_FIELD,
        TypeField::Value(RuntimeValue::Callable(Function::LuaFunction(LIST_COMP_NEXT_METHOD))),
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
    "function(self)
        -- First of all check that there are remaining work
        if self['{FINISHED_FIELD}'] then
            return nil
        end

        -- Ensure iterators and arguments table are initialized
        if self['{ITERATORS_FIELD}'] == nil or self['{ARGS_FIELD}'] == nil then
            self['{ITERATORS_FIELD}'] = {{}}
            self['{ARGS_FIELD}'] = {{}}
            for i, coll in ipairs(self['{COLLECTIONS_FIELD}']) do
                self['{ITERATORS_FIELD}'][i] = coll['{ITERATOR_FIELD}']
                self['{ARGS_FIELD}'][i] = self['{ITERATORS_FIELD}'][i]()
            end
        end

        -- Get the next element of the comprehension
        local res = nil
        while res == nil and not self['{FINISHED_FIELD}'] do
            -- First, call the body with computed arguments
            res = self['{BODY_FIELD}'](unpack(self['{ARGS_FIELD}']))

            -- Then compute the next argument set
            for i = #self['{ITERATORS_FIELD}'], 1, -1 do
                self['{ARGS_FIELD}'][i] = self['{ITERATORS_FIELD}'][i]()
                if self['{ARGS_FIELD}'][i] == nil then
                    if i == 1 then
                        self['{FINISHED_FIELD}'] = true
                    else
                        self['{ITERATORS_FIELD}'][i] = self['{COLLECTIONS_FIELD}'][i]['{ITERATOR_FIELD}']
                        self['{ARGS_FIELD}'][i] = self['{ITERATORS_FIELD}'][i]()
                    end
                else
                    break
                end
            end
        end
        return res
    end",
);
