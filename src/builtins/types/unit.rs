//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use crate::{
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationVariant,
        img_property,
    },
    engine::FunctionValue,
    lua::{LuaState, push_string},
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: 0,
    traits: &[],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Unit",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(img_property)))],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(unit_tostring))],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Unit" type
unsafe extern "C" fn unit_tostring(l: LuaState) -> c_int {
    push_string(l, "()");
    1
}
