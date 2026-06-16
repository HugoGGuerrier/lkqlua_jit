//! # LKQL "List" type
//!
//! This module defines the LKQL "List" type.

use crate::{
    builtins::{
        traits::{
            indexable,
            iterable::{self, ITERATOR_FIELD},
            sized::{self, DEFAULT_SIZED_LENGTH},
        },
        types::{
            BuiltinType, OverloadTarget, TYPE_NAME_FIELD, TYPE_TAGS_FIELD, TypeField,
            TypeImplementation, TypeImplementationVariant, img_property, tuple,
        },
    },
    lua::{LuaState, get_field, get_index, get_length, get_string, push_string, set_top},
    runtime::{Function, RuntimeValue},
};
use const_format::formatcp;
use std::ffi::c_int;

const TYPE_TAG: i32 = tuple::TYPE.tag + 1;

pub const TYPE: BuiltinType = BuiltinType {
    tag: TYPE_TAG,
    traits: &[&indexable::TRAIT, &iterable::TRAIT, &sized::TRAIT],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "List",
    fields: &[
        ("img", TypeField::Property(Function::CFunction(img_property))),
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
        (ITERATOR_FIELD, TypeField::Property(Function::LuaFunction(LIST_ITERATOR))),
        ("any", TypeField::Value(LIST_ANY)),
        ("all", TypeField::Value(LIST_ALL)),
        ("reduce", TypeField::Value(LIST_REDUCE)),
        ("sublist", TypeField::Value(LIST_SUBLIST)),
    ],
    overloads: &[
        (OverloadTarget::ToString, Function::CFunction(list_tostring)),
        (OverloadTarget::Eq, LIST_EQ),
        (OverloadTarget::Concat, LIST_CONCAT),
    ],
    index_method: None,
    registering_function: None,
};

/// Lua source that represents the "field@iterator" property on the "List"
/// type.
const LIST_ITERATOR: &str = "function (self)
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
end";

/// Implementation of the "any" method on values of the "List" type.
const LIST_ANY: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(
    "function (_, self, predicate)
        for _, next in ipairs(self) do
            if predicate(nil, next) == true then
                return true
            end
        end
        return false
    end",
));

/// Implementation of the "all" method on values of the "List" type.
const LIST_ALL: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(
    "function (_, self, predicate)
        for _, next in ipairs(self) do
            if predicate(nil, next) == false then
                return false
            end
        end
        return true
    end",
));

/// Implementation of the "reduce" method on values of the "List" type.
const LIST_REDUCE: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(
    "function (_, self, fn, init)
        local res = init
        for _, next in ipairs(self) do
            res = fn(nil, res, next)
        end
        return res
    end",
));

/// Implementation of the "sublist" method in value of the "List" type.
const LIST_SUBLIST: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(
    "function (_, self, low, high)
        local res = setmetatable({}, getmetatable(self))
        for i=low,high,1 do
            table.insert(res, self[i])
        end
        return res
    end",
));

/// Overload of "__tostring" for the "List" type
#[unsafe(no_mangle)]
extern "C" fn list_tostring(l: LuaState) -> c_int {
    // Get image of items inside the list
    let list_len = get_length(l, 1);
    let mut item_images = Vec::with_capacity(list_len);
    for i in 1..=list_len {
        get_index(l, 1, i as i32);
        get_field(l, 2, "img");
        item_images.push(get_string(l, 3).unwrap());
        set_top(l, 1);
    }

    // Then create the list representation
    push_string(l, &format!("[{}]", item_images.join(", ")));
    1
}

/// Overload of "__eq" for the "List" type
const LIST_EQ: Function = Function::LuaFunction(formatcp!(
    "function(self, other)
        -- Start by checking types
        if not other['{tags_field}'][{list_type_tag}] then
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
    tags_field = TYPE_TAGS_FIELD,
    list_type_tag = tuple::TYPE.tag + 1,
));

/// Overload of "__concat" for the "List" type
const LIST_CONCAT: Function = Function::LuaFunction(formatcp!(
    "function(self, other)
        -- Start by checking types
        if not self['{tags_field}'][{list_type_tag}] then
            error(
                'Attempt to concatenate a list with a \"' ..
                self['{name_field}'] ..
                '\"'
            )
        end

        if not other['{tags_field}'][{list_type_tag}] then
            error(
                'Attempt to concatenate a list with a \"' ..
                other['{name_field}'] ..
                '\"'
            )
        end

        -- Create a new result
        local res = setmetatable({{}}, _G['type@List'])

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
    tags_field = TYPE_TAGS_FIELD,
    list_type_tag = TYPE_TAG,
    name_field = TYPE_NAME_FIELD,
));
