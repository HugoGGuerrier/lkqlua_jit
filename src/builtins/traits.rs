//! # Built-in traits
//!
//! This module contains all types and working values for built-in traits.

use crate::{
    builtins::types::{BuiltinType, OverloadTarget, TypeField, TypeImplementationKind},
    engine::FunctionValue,
};

pub mod indexable;
pub mod iterable;
pub mod sized;

/// This type represents an LKQL built-in trait. A trait represents a set of
/// constraints that can be applied to a built-in type. A type may implements
/// multiple traits in the same time.
#[derive(Debug)]
pub struct BuiltinTrait {
    pub name: &'static str,
    pub required_overloads: &'static [OverloadTarget],
    pub required_fields: &'static [RequiredField],
}

impl BuiltinTrait {
    /// Get the name of the field to get on a value to check at runtime if it
    /// implements this trait.
    pub fn runtime_field(&self) -> String {
        format!("trait@{}", self.name)
    }

    /// Check that the provided built-in type is correctly implementing this
    /// trait. If not this function return a list of fields missing in the
    /// type.
    pub fn check_type(&self, t: &'static BuiltinType) -> Result<(), Vec<String>> {
        let mut missing_fields = Vec::new();

        // Check that the type correctly implements required overloads
        fn is_overload_in(
            searched: &OverloadTarget,
            haystack: &[(OverloadTarget, FunctionValue)],
        ) -> bool {
            haystack.iter().any(|(target, _)| target == searched)
        }

        for overload in self.required_overloads {
            let overload_found = match &t.implementation_kind {
                TypeImplementationKind::Monomorphic { implementation } => {
                    is_overload_in(overload, implementation.overloads)
                }
                TypeImplementationKind::Polymorphic { base_implementation, specializations } => {
                    is_overload_in(overload, base_implementation.overloads)
                        || (specializations
                            .iter()
                            .all(|spec| is_overload_in(overload, spec.overloads)))
                }
            };
            if !overload_found {
                missing_fields.push(String::from(overload.metamethod_name()));
            }
        }

        // Check that the type has all required fields
        fn is_field_in(searched: &RequiredField, haystack: &[(&str, TypeField)]) -> bool {
            haystack.iter().any(|(name, field)| match searched {
                RequiredField::Value(n) => matches!(field, TypeField::Value(..)) && name == n,
                RequiredField::Property(n) => matches!(field, TypeField::Property(..)) && name == n,
                RequiredField::Method(n) => matches!(field, TypeField::Method(..)) && name == n,
            })
        }

        for field in self.required_fields {
            let field_found = match &t.implementation_kind {
                TypeImplementationKind::Monomorphic { implementation } => {
                    is_field_in(field, implementation.fields)
                }
                TypeImplementationKind::Polymorphic { base_implementation, specializations } => {
                    is_field_in(field, base_implementation.fields)
                        || specializations
                            .iter()
                            .all(|spec| is_field_in(field, spec.fields))
                }
            };
            if !field_found {
                missing_fields.push(String::from(match field {
                    RequiredField::Value(n)
                    | RequiredField::Property(n)
                    | RequiredField::Method(n) => *n,
                }));
            }
        }

        // Finally return whether the type is valid
        if missing_fields.is_empty() { Ok(()) } else { Err(missing_fields) }
    }
}

/// This type represents a field that is required by a trait, it is used to
/// check that a type which implements a trait is defining required field. Each
/// variant has a string corresponding the name of the required field.
#[derive(Debug)]
pub enum RequiredField {
    Value(&'static str),
    Property(&'static str),
    Method(&'static str),
}
