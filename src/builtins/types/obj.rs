//! # LKQL "Object" type
//!
//! This module defines the LKQL "Object" type.

use const_format::formatcp;

use crate::{
    builtins::{
        traits::iterable::{self, ITERATOR_FIELD},
        types::{
            BuiltinType, IMG_FIELD, OverloadTarget, TypeField, TypeImplementation,
            TypeImplementationKind, stream,
        },
    },
    runtime::{Function, LkqlParam, RuntimeValue},
};

/// Name of the method to get a subobject without provided keys.
pub const WITHOUT_KEYS_NAME: &str = "without_keys";

pub const TYPE: BuiltinType = BuiltinType {
    tag: stream::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Object",
    fields: &[
        IMG_FIELD,
        (WITHOUT_KEYS_NAME, TypeField::Value(WITHOUT_KEYS)),
    ],
    overloads: &[
        (OverloadTarget::ToString, OBJ_TOSTRING),
        (OverloadTarget::Eq, OBJ_EQ),
    ],
    index_method: None,
    registering_function: None,
};

/// Implementation of the "without_keys" method.
const WITHOUT_KEYS: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_trait("keys", &iterable::TRAIT),
    ],
    body: formatcp!(
        "local res = setmetatable({{}}, getmetatable(self))
        for k, v in pairs(self) do
            -- Get whether to include the current element in the result
            local do_include = true
            local it = keys['{ITERATOR_FIELD}']
            local next = it()
            while next ~= nil do
                if k == next then
                    do_include = false
                    break
                end
                next = it()
            end

            -- Then insert the current element in the result if required
            if do_include then
                res[k] = v
            end
        end
        return res"
    ),
});

/// Overload of "__tostring" for the "Object" type.
const OBJ_TOSTRING: Function = Function::LuaFunction(
    "function (self)
        -- Get keys and sort them
        local keys = {}
        for key, _ in pairs(self) do
            table.insert(keys, key)
        end
        table.sort(keys)

        -- Then get images of values
        local images = {}
        for _, key in ipairs(keys) do
            table.insert(images, '\"' .. key .. '\": ' .. self[key].img)
        end
        return '{' .. table.concat(images, ', ') .. '}'
    end",
);

/// Overload of "__eq" for the "Object" type.
const OBJ_EQ: Function = Function::LuaFunction(
    "function(self, other)
        -- Start by checking types
        if getmetatable(self) ~= getmetatable(other) then
            return false
        end

        -- Iterate on self keys and check their values
        for key, elem in pairs(self) do
            if self[key] ~= other[key] then
                return false
            end
        end

        -- Check extra keys in the other object
        for key, _ in pairs(other) do
            if self[key] == nil then
                return false
            end
        end

        -- Finally, return the positive result
        return true
    end",
);
