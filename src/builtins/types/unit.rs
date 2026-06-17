//! # LKQL "Unit" type
//!
//! This module defines the LKQL "Unit" type.

use crate::{
    builtins::types::{
        BuiltinType, OverloadTarget, TypeField, TypeImplementation, TypeImplementationKind,
        img_property,
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
    fields: &[("img", TypeField::Property(Function::CFunction(img_property)))],
    overloads: &[(OverloadTarget::ToString, TO_STRING)],
    index_method: None,
    registering_function: None,
};

/// Overload of "__tostring" for the "Unit" type
const TO_STRING: Function = Function::LuaFunction("function (_) return '()' end");
