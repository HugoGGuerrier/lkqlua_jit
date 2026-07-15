//! # LKQL "List" type
//!
//! This module defines the LKQL "List" type.

use crate::{
    builtins::{
        traits::{
            indexable,
            iterable::{self, ANY_AND_ALL_PARAMS, ITERATOR_FIELD, REDUCE_PARAMS},
            sized::{self, DEFAULT_SIZED_LENGTH},
        },
        types::{
            BuiltinType, IMG_FIELD, OverloadTarget, TYPE_NAME_FIELD, TYPE_TAGS_FIELD, TypeField,
            TypeImplementation, TypeImplementationKind, TypeRef, tuple,
        },
    },
    errors::INVALID_OPERATION,
    runtime::{Function, LKQL_ERROR_GLOBAL_NAME, LkqlParam, RuntimeValue},
};
use const_format::formatcp;

const TYPE_TAG: i32 = tuple::TYPE.tag + 1;

const TYPE_NAME: &str = "List";

/// Name of the method to get a sublist from a list.
pub const SUBLIST_NAME: &str = "sublist";

pub const TYPE: BuiltinType = BuiltinType {
    tag: TYPE_TAG,
    traits: &[&indexable::TRAIT, &iterable::TRAIT, &sized::TRAIT],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: TYPE_NAME,
    fields: &[
        IMG_FIELD,
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
        (ITERATOR_FIELD, TypeField::Property(ITERATOR)),
        ("any", TypeField::Value(ANY)),
        ("all", TypeField::Value(ALL)),
        ("reduce", TypeField::Value(REDUCE)),
        ("to_list", TypeField::Property(TO_LIST)),
        (SUBLIST_NAME, TypeField::Value(SUBLIST)),
    ],
    overloads: &[
        (OverloadTarget::ToString, LIST_TOSTRING),
        (OverloadTarget::Eq, LIST_EQ),
        (OverloadTarget::Concat, LIST_CONCAT),
    ],
    index_method: None,
    registering_function: None,
};

/// Lua source that represents the "field@iterator" property on the "List"
/// type.
const ITERATOR: Function = Function::LuaFunction(
    "function (self)
        local size = #self
        local cursor = 0
        return function ()
            if cursor < size then
                cursor = cursor + 1
                return self[cursor]
            else
                return nil
            end
        end
    end",
);

/// Implementation of the "any" method on values of the "List" type.
const ANY: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: ANY_AND_ALL_PARAMS,
    body: "
        for _, next in ipairs(self) do
            if predicate(nil, next) == true then
                    return true
                end
            end
        return false",
});

/// Implementation of the "all" method on values of the "List" type.
const ALL: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: ANY_AND_ALL_PARAMS,
    body: "
        for _, next in ipairs(self) do
            if predicate(nil, next) == false then
                return false
            end
        end
        return true",
});

/// Implementation of the "reduce" method on values of the "List" type.
const REDUCE: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: REDUCE_PARAMS,
    body: "
        local res = init
        for _, next in ipairs(self) do
            res = fn(nil, res, next)
        end
        return res",
});

/// Implementation of the "to_list" method on values of the "List" type.
const TO_LIST: Function = Function::LuaFunction("function (self) return self end");

/// Implementation of the "sublist" method in value of the "List" type.
const SUBLIST: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("low", TypeRef::Int),
        LkqlParam::with_type("high", TypeRef::Int),
    ],
    body: "
        local res = setmetatable({}, getmetatable(self))
        for i=low,high,1 do
            table.insert(res, self[i])
        end
        return res",
});

/// Overload of "__tostring" for the "List" type
const LIST_TOSTRING: Function = Function::LuaFunction(
    "function (self)
        local images = {}
        for _, val in ipairs(self) do
            table.insert(images, val.img)
        end
        return '[' .. table.concat(images, ', ') .. ']'
    end",
);

/// Overload of "__eq" for the "List" type
const LIST_EQ: Function = Function::LuaFunction(formatcp!(
    "function (self, other)
        -- Start by checking types
        if not other['{TYPE_TAGS_FIELD}'][{TYPE_TAG}] then
            return false
        end

        -- Compare sizes
        if self.length ~= other.length then
            return false
        end

        -- Then compare elements
        for i, elem in ipairs(self) do
            if other[i] ~= elem then
                return false
            end
        end
        return true
    end",
));

/// Overload of "__concat" for the "List" type
const LIST_CONCAT: Function = Function::LuaFunction(formatcp!(
    "function (self, other)
        -- Start by checking types
        if not other['{TYPE_TAGS_FIELD}'][{TYPE_TAG}] then
            _G['{LKQL_ERROR_GLOBAL_NAME}'](
                '{}',
                {{
                    '&',
                    self['{TYPE_NAME_FIELD}'],
                    other['{TYPE_NAME_FIELD}']
                }}
            )
        end

        -- Create a new result
        local res = setmetatable({{}}, _G['type@{TYPE_NAME}'])

        -- Place self elements in the result
        local self_len = #self
        for i, v in ipairs(self) do
            res[i] = v
        end
        for i, v in ipairs(other) do
            res[i+self_len] = v
        end

        -- Finally return the result
        return res
    end",
    INVALID_OPERATION.id
));
