//! # LKQL "Namespace" type
//!
//! This module defines the LKQL "Namespace" type.

use crate::{
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
        img_property, obj,
    },
    lua::{LuaState, get_next_pair, get_string, pop, push_nil, push_string},
    runtime::Function,
};
use std::ffi::c_int;

pub const TYPE: BuiltinType = BuiltinType {
    tag: obj::TYPE.tag + 1,
    traits: &[],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Namespace",
    fields: &[("img", TypeField::Property(Function::CFunction(img_property)))],
    overloads: &[(OverloadTarget::ToString, Function::CFunction(namespace_tostring))],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Namespace" type
#[unsafe(no_mangle)]
extern "C" fn namespace_tostring(l: LuaState) -> c_int {
    // Create the vector to place symbols in
    let mut symbols: Vec<&str> = Vec::new();

    // Iterate over all pairs in the namespace and extract symbols
    push_nil(l);
    while get_next_pair(l, 1) {
        symbols.push(get_string(l, -2).unwrap());
        pop(l, 1);
    }

    // Sort the symbols vector
    symbols.sort();

    // Finally place it on the stack and return 1
    push_string(l, &format!("Namespace({})", symbols.join(", ")));
    1
}
