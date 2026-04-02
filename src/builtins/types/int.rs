//! # LKQL "Int" type
//!
//! This module defines the LKQL "Int" type.

use crate::{
    builtins::types::{
        BuiltinType, TypeField, TypeImplementation, TypeImplementationVariant, bool, img_property,
    },
    engine::FunctionValue,
    lua::{LuaState, copy_value, push_number, set_metatable},
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: bool::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationVariant::Monomorphic {
        implementation: IMPLEMENTATION,
    },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Int",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(img_property)))],
    overloads: &[],
    index_method: None,
    registering_function: Some(register_metatable),
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &TypeImplementation) {
    push_number(l, 0f64);
    copy_value(l, -2);
    set_metatable(l, -2);
}
