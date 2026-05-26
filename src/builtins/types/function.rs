//! # LKQL "Function" type
//!
//! This module defines the LKQL "Function" type.

use crate::{
    ExecutionContext,
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationVariant,
        img_property, namespace,
    },
    engine::{CONTEXT_GLOBAL_NAME, FunctionValue},
    lua::{
        LuaState, call, copy_value, debug_get_func_id, get_field, get_global, get_string, get_top,
        get_user_data, load_lua_code, pop, push_string, set_metatable,
    },
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: namespace::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationVariant::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Function",
    fields: &[("img", TypeField::Property(FunctionValue::CFunction(img_property)))],
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
    // Define the default function name
    let default_function_name = "anonymous";

    // Try to get the name of the function value
    let function_name = if let Some(proto_id) = debug_get_func_id(l, -1) {
        // Get the name of the source the function is defined in
        get_global(l, "debug");
        get_field(l, -1, "getinfo");
        copy_value(l, -3);
        push_string(l, "S");
        call(l, 2, Some(1));
        get_field(l, -1, "source");

        // Ensure the source name is actually a source id
        if let Some(source_id_str) = get_string(l, -1) {
            if let Ok(source_id) = source_id_str.parse::<usize>() {
                // Get the current execution context
                get_global(l, CONTEXT_GLOBAL_NAME);
                let ctx = get_user_data::<ExecutionContext>(l, get_top(l)).unwrap();
                pop(l, 1);

                // Get the prototype name
                &ctx.compilation_cache.get(&source_id).unwrap().prototypes[proto_id].name
            } else {
                default_function_name
            }
        } else {
            default_function_name
        }
    } else {
        default_function_name
    };

    // Push the string representing the function as a result
    push_string(l, &format!("Function<{function_name}>"));
    1
}
