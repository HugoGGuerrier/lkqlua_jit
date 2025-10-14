//! # Built-in types
//!
//! This module contains all LKQL built-in types and their associated working
//! values.

use std::{collections::HashMap, ffi::c_int};

use crate::lua::{
    LuaCFunction, LuaState, get_string, get_up_value_index, get_user_data, move_top_value,
    push_c_closure, push_c_function, push_nil, push_user_data, remove_value, set_global,
};

pub mod bool;
pub mod int;
pub mod str;
pub mod unit;

/// This type represents an LKQL built-in type with all its information.
#[derive(Debug)]
pub struct BuiltinType {
    /// Name of the type as it should be displayed to the user.
    pub name: &'static str,

    /// Methods in the type.
    pub methods: HashMap<String, BuiltinMethod>,

    /// Maps used to define custom behavior for the type regarding language
    /// constructions.
    pub overloads: HashMap<OverloadTarget, LuaCFunction>,

    /// The function to call to register this type in a given Lua state.
    pub register_function: MetatableRegisteringFunction,
}

/// This type represents a built-in method in a built-in type.
#[derive(Debug, Clone)]
pub struct BuiltinMethod {
    pub function: LuaCFunction,

    /// Whether the method should be called implicitly when accessing it.
    pub is_property: bool,
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
pub type MetatableRegisteringFunction = fn(LuaState, &Box<BuiltinType>);

/// Get the name of the global field where the meta-table of the type
/// designated by the provided name is stored in.
pub fn metatable_global_field(type_name: &str) -> String {
    return format!("type@{}", type_name);
}

/// This is the generic meta-table registering function, it places the
/// meta-table on the top of the stack in a global field named from the type's
/// name.
pub fn register_metatable_in_globals(l: LuaState, type_box: &Box<BuiltinType>) {
    set_global(l, &metatable_global_field(type_box.name));
}

/// Push a C closure on the top of the stack representing the "__index"
/// meta-method of the provided built-in type.
pub fn create_index_method(l: LuaState, type_box: &Box<BuiltinType>) {
    push_user_data(l, type_box.as_ref());
    push_c_closure(l, generic_index, 1);
}

/// Generic type indexing function, basing method name resolution on the
/// [`BuiltinType`] instance stored in its up-values.
#[unsafe(no_mangle)]
unsafe extern "C" fn generic_index(l: LuaState) -> c_int {
    // Get the type descriptor in the up-values
    let builtin_type = get_user_data::<BuiltinType>(l, get_up_value_index(1)).unwrap();

    // Fetch the requested field name and check if there is a method
    // corresponding to it.
    let field_name = get_string(l, 2).unwrap();
    if let Some(builtin_method) = builtin_type.methods.get(field_name) {
        if builtin_method.is_property {
            // Call the method directly with the same stack to avoid frame
            // creation.
            remove_value(l, 2);
            push_nil(l);
            move_top_value(l, 1);
            unsafe { return (builtin_method.function)(l) }
        } else {
            push_c_function(l, builtin_method.function);
            return 1;
        }
    }

    // Return the failure, there is no field corresponding to the requested
    // name.
    0
}
