//! # LKQL "Namespace" type
//!
//! This module defines the LKQL "Namespace" type.

use crate::{
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
        img_property, obj,
    },
    runtime::Function,
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: obj::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Namespace",
    fields: &[("img", TypeField::Property(Function::CFunction(img_property)))],
    overloads: &[(OverloadTarget::ToString, NAMESPACE_TOSTRING)],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Namespace" type
const NAMESPACE_TOSTRING: Function = Function::LuaFunction(
    "function (self)
        -- Get keys and sort them
        local keys = {}
        for key, _ in pairs(self) do
            table.insert(keys, key)
        end
        table.sort(keys)

        -- Then return the string representation of the namespace
        return 'Namespace(' .. table.concat(keys, ', ') .. ')'
    end",
);
