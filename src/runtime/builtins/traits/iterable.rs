//! # LKQL "Iterable" trait
//!
//! This module defines the "Iterable" LKQL trait. This trait requires that
//! implementing type define an `field@iterator` property that return an
//! iterator value for the instance.
//! Iterators are represented by functional values that are called to get the
//! "next" element in the source iterable.

use crate::runtime::builtins::traits::{BuiltinTrait, RequiredField};

/// Name of the field to access to get an iterator for a value.
pub const ITERATOR_FIELD: &str = "field@iterator";

pub const TRAIT: BuiltinTrait = BuiltinTrait {
    name: "Iterable",
    required_overloads: &[],
    required_fields: &[RequiredField::Property(ITERATOR_FIELD)],
};
