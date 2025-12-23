//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use std::ffi::c_int;

use crate::{
    lua::{LuaState, push_string},
    runtime::{
        FunctionValue,
        builtins::{
            functions::lkql_img,
            types::{
                BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
            },
        },
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: 0,
    traits: &[],
    implementation_kind: TypeImplementationKind::Monomorphic { implementation: IMPLEMENTATION },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Unit",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(lkql_img)))],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(unit_tostring))],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Unit" type
unsafe extern "C" fn unit_tostring(l: LuaState) -> c_int {
    push_string(l, "()");
    1
}
