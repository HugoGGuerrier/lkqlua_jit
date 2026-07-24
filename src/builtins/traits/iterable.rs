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
        types::{
            TYPE_GLOBAL_FIELD_PREFIX, TypeRef, list,
            stream::{
                filtering::{self, PREDICATE_FUNCTION_FIELD},
                flattening::{self, INNER_ITERATOR_FIELD},
                mapping::{self, MAP_FUNCTION_FIELD},
            },
        },
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
        RequiredField::Property("flatten"),
        RequiredField::Value("filter"),
        RequiredField::Value("map"),
        RequiredField::Value("flat_map"),
        RequiredField::Value("reduce"),
        RequiredField::Property("to_list"),
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

/// Default implementation of the "flatten" property on iterable values.
pub const DEFAULT_ITERABLE_FLATTEN: Function = Function::LuaFunction(formatcp!(
    "function (self)
        local it = self['{ITERATOR_FIELD}']
        local inner_it = it()['{ITERATOR_FIELD}']
        return setmetatable(
            {{
                ['{SOURCE_FIELD}'] = it,
                ['{INNER_ITERATOR_FIELD}'] = inner_it,
            }},
            _G['{FLATTEN_STREAM_TYPE}']
        )
    end",
    SOURCE_FIELD = flattening::SOURCE_ITERATOR_FIELD,
    FLATTEN_STREAM_TYPE =
        formatcp!("{TYPE_GLOBAL_FIELD_PREFIX}{}", flattening::SPECIALIZATION.name)
));

/// List of parameters that the "filter" method requires.
pub const FILTER_PARAMS: &[LkqlParam] = &[
    LkqlParam::new("self"),
    LkqlParam::with_type("predicate", TypeRef::Function),
];

/// Default implementation of the "filter" method on iterable values.
pub const DEFAULT_ITERABLE_FILTER: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: FILTER_PARAMS,
    body: formatcp!(
        "return setmetatable(
            {{
                ['{SOURCE_FIELD}'] = self['{ITERATOR_FIELD}'],
                ['{PREDICATE_FUNCTION_FIELD}'] = predicate,
            }},
            _G['{FILTER_STREAM_TYPE}']
        )",
        SOURCE_FIELD = filtering::SOURCE_ITERATOR_FIELD,
        FILTER_STREAM_TYPE =
            formatcp!("{TYPE_GLOBAL_FIELD_PREFIX}{}", filtering::SPECIALIZATION.name)
    ),
});

/// List of parameters that the "map" method requires.
pub const MAP_PARAMS: &[LkqlParam] = &[
    LkqlParam::new("self"),
    LkqlParam::with_type("fn", TypeRef::Function),
];

/// Default implementation of the "map" method on iterable values.
pub const DEFAULT_ITERABLE_MAP: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: MAP_PARAMS,
    body: formatcp!(
        "return setmetatable(
            {{
                ['{SOURCE_FIELD}'] = self['{ITERATOR_FIELD}'],
                ['{MAP_FUNCTION_FIELD}'] = fn,
            }},
            _G['{MAP_STREAM_TYPE}']
        )",
        SOURCE_FIELD = mapping::SOURCE_ITERATOR_FIELD,
        MAP_STREAM_TYPE = formatcp!("{TYPE_GLOBAL_FIELD_PREFIX}{}", mapping::SPECIALIZATION.name)
    ),
});

/// List of parameters that the "flat_map" method requires.
pub const FLAT_MAP_PARAMS: &[LkqlParam] = &[
    LkqlParam::new("self"),
    LkqlParam::with_type("fn", TypeRef::Function),
];

/// Default implementation of the "flat_map" method on iterable values.
pub const DEFAULT_ITERABLE_FLAT_MAP: RuntimeValue =
    RuntimeValue::Callable(Function::LkqlFunction {
        params: FLAT_MAP_PARAMS,
        body: "return self.map(nil, self, fn).flatten",
    });

/// List of parameters that the "reduce" method requires.
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

/// Default implementation of the "to_list" property on iterable values.
pub const DEFAULT_ITERABLE_TO_LIST: Function = Function::LuaFunction(formatcp!(
    "function (self)
        local it = self['{ITERATOR_FIELD}']
        local next = it()
        local res = setmetatable({{}}, _G['{LIST_TYPE}'])
        while next ~= nil do
            table.insert(res, next)
            next = it()
        end
        return res
    end",
    LIST_TYPE = formatcp!("{TYPE_GLOBAL_FIELD_PREFIX}{}", list::IMPLEMENTATION.name)
));
