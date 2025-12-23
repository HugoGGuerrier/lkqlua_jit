//! # LKQL "Bool" type
//!
//! This module defines the LKQL "Bool" type.

use crate::{
    lua::{LuaState, copy_value, push_bool, set_metatable},
    runtime::{
        FunctionValue,
        builtins::{
            functions::lkql_img,
            types::{BuiltinType, TypeField, TypeImplementation, TypeImplementationKind, unit},
        },
    },
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: unit::TYPE.tag + 1,
    traits: &[],
    implementation_kind: TypeImplementationKind::Monomorphic { implementation: IMPLEMENTATION },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Bool",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(lkql_img)))],
    overloads: &[],
    index_method: None,
    registering_function: Some(register_metatable),
};

/// Register the meta-table in the provided Lua state.
pub fn register_metatable(l: LuaState, _: &TypeImplementation) {
    push_bool(l, false);
    copy_value(l, -2);
    set_metatable(l, -2);
}
