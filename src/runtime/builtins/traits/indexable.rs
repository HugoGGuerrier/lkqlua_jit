//! # LKQL "Indexable" trait
//!
//! This module defines the "Indexable" LKQL trait.

use crate::runtime::{
    FunctionValue,
    builtins::traits::{BuiltinTrait, RequiredField},
};

pub const TRAIT: BuiltinTrait = BuiltinTrait {
    name: "Indexable",
    required_overloads: &[],
    required_fields: &[RequiredField::Method("get")],
};

/// This function value may be used by type implementing the "Indexable" trait
/// to defined the "get" method.
pub const DEFAULT_INDEXABLE_GET: FunctionValue =
    FunctionValue::LuaFunction("function (self, index) return self[index] end");
