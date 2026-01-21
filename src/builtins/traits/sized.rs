//! # LKQL "Sized" trait
//!
//! This module defines the "Sized" LKQL trait. This trait requires that
//! implementing types define a `length` method that return the "size" of it.
//! It also require overloading on the "__len" Lua meta-method.

use crate::{
    builtins::traits::{BuiltinTrait, RequiredField},
    engine::FunctionValue,
};

pub const TRAIT: BuiltinTrait = BuiltinTrait {
    name: "Sized",
    required_overloads: &[],
    required_fields: &[RequiredField::Property("length")],
};

/// This represents the default "length" property that links directly to the
/// `#` Lua operator.
pub const DEFAULT_SIZED_LENGTH: FunctionValue =
    FunctionValue::LuaFunction("function (_, self) return #self end");
