//! # List comprehension Stream implementation
//!
//! This module defines the "list comprehension" implementation of the "Stream"
//! LKQL type.

use crate::runtime::{
    FunctionValue,
    builtins::types::{TypeField, TypeImplementation, stream::INTERNAL_NEXT_NAME},
};

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "ListComprehension",
    fields: &[(
        INTERNAL_NEXT_NAME,
        TypeField::Method(FunctionValue::LuaFunction(LIST_COMP_NEXT_METHOD)),
    )],
    overloads: &[],
    index_method: None,
    registering_function: None,
};

/// Name of the field where the body of the list comprehension is stored. This
/// body is a functional value that is going to be called with elements of the
/// source collection. The return value is the result of the processing of
/// those element, or `nil` if the guard failed.
const BODY_FIELD: &str = "field@body";

/// Name of the internal field containing collections to iterate on.
const COLLECTIONS_FIELD: &str = "field@collections";

/// Name of the field that contains current iterators on collections to iterate
/// on.
const ITERATORS_FIELD: &str = "field@iterators";

/// Name of the internal field that contains current arguments to pass to the
/// list comprehension body.
const ARGS_FIELD: &str = "field@args";

const LIST_COMP_NEXT_METHOD: &str = "function (self)
end";
