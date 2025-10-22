//! # Built-in types
//!
//! This module contains all LKQL built-in types and their associated working
//! values.

use std::{collections::HashMap, ffi::c_int};

use crate::{
    lua::{
        LuaCFunction, LuaState, get_string, get_up_value_index, get_user_data, move_top_value,
        push_c_closure, push_c_function, push_nil, push_user_data, remove_value, set_global,
    },
    runtime::{
        RuntimeType, RuntimeTypeField, RuntimeValue, TYPE_NAME_FIELD, TYPE_TAG_FIELD,
        builtins::utils::metatable_global_field,
    },
};

pub mod bool;
pub mod int;
pub mod str;
pub mod tuple;
pub mod unit;

/// This type represents an LKQL built-in type with all its information.
#[derive(Debug)]
pub struct BuiltinType {
    /// Name of the type as it should be displayed to the user.
    pub name: &'static str,

    /// Tag of the type, its unique identifier for optimized type checking.
    pub tag: isize,

    /// Fields in the type.
    pub fields: &'static [(&'static str, RuntimeTypeField)],

    /// Maps used to define custom behavior for the type regarding language
    /// constructions.
    pub overloads: &'static [(OverloadTarget, LuaCFunction)],

    /// The function to call to register this type in a given Lua state.
    pub register_function: MetatableRegisteringFunction,
}

impl BuiltinType {
    /// Create a runtime type representation of this built-in type.
    pub fn as_runtime_type(&self) -> RuntimeType {
        // Collect fields of the built-in type
        let mut fields: HashMap<String, RuntimeTypeField> = self
            .fields
            .iter()
            .map(|(name, field)| (String::from(*name), field.clone()))
            .collect();

        // Then add support fields
        fields.insert(
            String::from(TYPE_NAME_FIELD),
            RuntimeTypeField::Value(RuntimeValue::String(String::from(self.name))),
        );
        fields.insert(
            String::from(TYPE_TAG_FIELD),
            RuntimeTypeField::Value(RuntimeValue::Integer(self.tag)),
        );

        // Return the new runtime type representation
        RuntimeType { name: self.name, tag: self.tag, fields }
    }
}

/// This type represents the target of an overloading definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OverloadTarget {
    Add,
    Sub,
    Mul,
    Div,
    UnMin,
    Eq,
    Lt,
    Le,
    Concat,
    Len,
    Call,
    ToString,
}

impl OverloadTarget {
    /// Get the Lua meta-method name associated with this overloading target.
    pub fn metamethod_name(&self) -> &'static str {
        match self {
            OverloadTarget::Add => "__add",
            OverloadTarget::Sub => "__sub",
            OverloadTarget::Mul => "__mul",
            OverloadTarget::Div => "__div",
            OverloadTarget::UnMin => "___unm",
            OverloadTarget::Eq => "__eq",
            OverloadTarget::Lt => "__lt",
            OverloadTarget::Le => "__le",
            OverloadTarget::Concat => "__concat",
            OverloadTarget::Len => "__len",
            OverloadTarget::Call => "__call",
            OverloadTarget::ToString => "__tostring",
        }
    }
}

/// This type represents a function that register a type meta-table in the
/// provided Lua state. This function should assume that the type meta-table is
/// currently on the top of the stack.
pub type MetatableRegisteringFunction = fn(LuaState, &'static BuiltinType);

// ----- Support functions -----

/// This is the generic meta-table registering function, it places the
/// meta-table on the top of the stack in a global field named from the type's
/// name.
pub fn register_metatable_in_globals(l: LuaState, builtin_type: &'static BuiltinType) {
    set_global(l, &metatable_global_field(builtin_type.name));
}

/// Push a C closure on the top of the stack representing the "__index"
/// meta-method of the provided built-in type.
pub fn create_index_method(l: LuaState, runtime_type: &Box<RuntimeType>) {
    push_user_data(l, runtime_type.as_ref());
    push_c_closure(l, generic_index, 1);
}

/// Generic type indexing function, basing method name resolution on the
/// [`BuiltinType`] instance stored in its up-values.
#[unsafe(no_mangle)]
unsafe extern "C" fn generic_index(l: LuaState) -> c_int {
    // Get the type descriptor in the up-values
    let builtin_type = get_user_data::<RuntimeType>(l, get_up_value_index(1)).unwrap();

    // Fetch the requested field name and check if there is a method
    // corresponding to it.
    let field_name = get_string(l, 2).unwrap();

    // First check in the type methods
    if let Some(builtin_field) = builtin_type.fields.get(field_name) {
        match builtin_field {
            RuntimeTypeField::Value(builtin_value) => builtin_value.push_on_stack(l),
            RuntimeTypeField::Property(property) => {
                remove_value(l, 2);
                push_nil(l);
                move_top_value(l, 1);
                unsafe {
                    (property)(l);
                }
            }
            RuntimeTypeField::Method(method) => {
                push_c_function(l, *method);
            }
        }
        return 1;
    }

    // Return the failure, there is no field corresponding to the requested
    // name.
    0
}
