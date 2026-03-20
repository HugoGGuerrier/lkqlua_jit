//! # LKQL "Function" type
//!
//! This module defines the LKQL "Function" type.

use std::ffi::c_int;

use crate::{
    builtins::{
        functions::lkql_img,
        types::{
            BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
            namespace,
        },
    },
    engine::FunctionValue,
    lua::{LuaState, copy_value, load_lua_code, push_string, set_metatable},
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: namespace::TYPE.tag + 1,
    traits: &[],
    implementation_kind: TypeImplementationKind::Monomorphic { implementation: IMPLEMENTATION },
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Function",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(lkql_img)))],
    overloads: &[(OverloadTarget::ToString, FunctionValue::CFunction(function_tostring))],
    index_method: None,
    registering_function: Some(register_metatable),
};

/// Register the meta-table in the provided Lua state.
fn register_metatable(l: LuaState, _: &TypeImplementation) {
    load_lua_code(l, "return function() end", "<empty_function>");
    copy_value(l, -2);
    set_metatable(l, -2);
}

/// Overload of "__tostring" for the "Function" type
#[unsafe(no_mangle)]
unsafe extern "C" fn function_tostring(l: LuaState) -> c_int {
    // Try to get the name of the function
    push_string(l, "<Function>");
    1
}
