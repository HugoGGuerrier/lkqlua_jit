//! # LKQL "Tuple" type
//!
//! This module defines the LKQL "Tuple" type.

use crate::{
    builtins::{
        traits,
        types::{
            BuiltinType, IMG_FIELD, OverloadTarget, TypeImplementation, TypeImplementationKind,
            pattern,
        },
    },
    runtime::Function,
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: pattern::TYPE.tag + 1,
    traits: &[&traits::indexable::TRAIT],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Tuple",
    fields: &[IMG_FIELD],
    overloads: &[
        (OverloadTarget::ToString, TOSTRING),
        (OverloadTarget::Eq, EQ),
    ],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Tuple" type
const TOSTRING: Function = Function::LuaFunction(
    "function (self)
        local images = {}
        for _, val in ipairs(self) do
            table.insert(images, val.img)
        end
        return '(' .. table.concat(images, ', ') .. ')'
    end",
);

/// Overload of "__eq" for the "Tuple" type
const EQ: Function = Function::LuaFunction(
    "function (self, other)
        -- Start by checking types
        if getmetatable(self) ~= getmetatable(other) then
            return false
        end

        -- Compare sizes
        if #self ~= #other then
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
);
