//! # LKQL "Indexable" trait
//!
//! This module defines the "Indexable" LKQL trait. This trait doesn't define
//! any required field because all values are technically indexable. This trait
//! is only used for runtime checking.

use crate::runtime::builtins::traits::BuiltinTrait;

pub const TRAIT: BuiltinTrait =
    BuiltinTrait { name: "Indexable", required_overloads: &[], required_fields: &[] };
