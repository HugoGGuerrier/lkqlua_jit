//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use crate::{
    builtins::types::{
        BuiltinType, IMG_FIELD, OverloadTarget, TypeImplementation, TypeImplementationKind,
    },
    runtime::Function,
};

pub const TYPE: BuiltinType = BuiltinType {
    tag: 0,
    traits: &[],
    implementation_variant: TypeImplementationKind::new_mono(IMPLEMENTATION),
};

pub const IMPLEMENTATION: TypeImplementation = TypeImplementation {
    name: "Unit",
    fields: &[IMG_FIELD],
    overloads: &[(OverloadTarget::ToString, TOSTRING)],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Unit" type
const TOSTRING: Function = Function::LuaFunction("function (_) return '()' end");
