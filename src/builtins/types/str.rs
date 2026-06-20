//! # LKQL "Str" type
//!
//! This module defines the LKQL "Str" type.

use crate::{
    builtins::{
        traits::{self, sized::DEFAULT_SIZED_LENGTH},
        types::{
            BuiltinType, TYPE_TAGS_FIELD, TypeField, TypeImplementation, TypeImplementationKind,
            TypeRef, img_property, int,
        },
    },
    lua::{LuaState, copy_value, push_string, set_metatable},
    runtime::{Function, LkqlParam, RuntimeValue},
};
use const_format::formatcp;

const TYPE_TAG: i32 = int::TYPE.tag + 1;

pub const TYPE: BuiltinType = BuiltinType {
    tag: TYPE_TAG,
    traits: &[&traits::sized::TRAIT],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Str",
    fields: &[
        ("img", TypeField::Property(Function::CFunction(img_property))),
        ("length", TypeField::Property(DEFAULT_SIZED_LENGTH)),
        ("base_name", TypeField::Property(BASE_NAME)),
        ("starts_with", TypeField::Value(STARTS_WITH)),
        ("ends_with", TypeField::Value(ENDS_WITH)),
        ("contains", TypeField::Value(CONTAINS)),
        ("find", TypeField::Value(FIND)),
        ("substring", TypeField::Value(SUBSTRING)),
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
const STARTS_WITH: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("prefix", TypeRef::Str),
    ],
    body: "return string.sub(self, 1, #prefix) == prefix",
});

/// Implementation of the "ends_with" method.
const ENDS_WITH: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("suffix", TypeRef::Str),
    ],
    body: "return string.sub(self, #self - #suffix + 1, #self) == suffix",
});

/// Implementation of the "contains" method.
const CONTAINS: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_types("to_find", &[TypeRef::Str, TypeRef::Pattern]),
    ],
    body: formatcp!(
        "if to_find['{TYPE_TAGS_FIELD}'][{TYPE_TAG}] then
            return string.find(self, to_find) ~= nil
        else
            return to_find.is_match(nil, to_find, self)
        end",
    ),
});

/// Implementation of the "find" method.
const FIND: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_types("to_find", &[TypeRef::Str, TypeRef::Pattern]),
    ],
    body: formatcp!(
        "if to_find['{TYPE_TAGS_FIELD}'][{TYPE_TAG}] then
            local res = string.find(self, to_find)
            return res or -1
        else
            return to_find.find(nil, to_find, self)
        end",
    ),
});

/// Implementation of the "substring" method.
const SUBSTRING: RuntimeValue = RuntimeValue::Callable(Function::LkqlFunction {
    params: &[
        LkqlParam::new("self"),
        LkqlParam::with_type("low", TypeRef::Int),
        LkqlParam::with_type("high", TypeRef::Int),
    ],
    body: "return string.sub(self, low, high)",
});
