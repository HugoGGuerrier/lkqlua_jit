//! # LKQL "Str" type
//!
//! This module defines the LKQL "Str" type.

use crate::{
    builtins::{
        traits::{self, sized::DEFAULT_SIZED_LENGTH},
        types::{
            BuiltinType, TypeField, TypeImplementation, TypeImplementationKind, TypeRef,
            img_property, int,
        },
    },
    lua::{LuaState, copy_value, push_string, set_metatable},
    runtime::{Function, LkqlParam, RuntimeValue},
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: int::TYPE.tag + 1,
    traits: &[&traits::sized::TRAIT],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Str",
    fields: &[
        ("img", TypeField::Property(Function::CFunction(img_property))),
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
        ("base_name", TypeField::Property(BASE_NAME)),
        ("starts_with", TypeField::Value(RuntimeValue::Callable(STARTS_WITH))),
        ("ends_with", TypeField::Value(RuntimeValue::Callable(ENDS_WITH))),
        ("substring", TypeField::Value(RuntimeValue::Callable(SUBSTRING))),
    ],
    overloads: &[],
    index_method: None,
    registering_function: Some(register_metatable),
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &TypeImplementation) {
    push_string(l, "");
    copy_value(l, -2);
    set_metatable(l, -2);
}

/// Implementation of the "base_name" method.
const BASE_NAME: Function = Function::LuaFunction(
    "function (self)
        local res = self
        for part in string.gmatch(
            self,
            '([^' .. string.sub(package.config, 1, 1) .. ']+)'
        ) do
            res = part
        end
        return res
    end",
);

/// Implementation of the "starts_with" method.
const STARTS_WITH: Function = Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("prefix", TypeRef::Str),
    ],
    body: "return string.sub(self, 1, #prefix) == prefix",
};

/// Implementation of the "ends_with" method.
const ENDS_WITH: Function = Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("suffix", TypeRef::Str),
    ],
    body: "return string.sub(self, #self - #suffix + 1, #self) == suffix",
};

/// Implementation of the "substring" method.
const SUBSTRING: Function = Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("low", TypeRef::Int),
        LkqlParam::with_type("high", TypeRef::Int),
    ],
    body: "return string.sub(self, low, high)",
};
