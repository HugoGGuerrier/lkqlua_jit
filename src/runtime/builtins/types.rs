//! # Built-in types
//!
//! This module contains all LKQL built-in types and their associated working
//! values.

use crate::{
    lua::{LuaState, push_table, set_field, set_global},
    runtime::{
        FunctionValue, RuntimeValue, TYPE_NAME_FIELD, TYPE_TAG_FIELD,
        builtins::utils::metatable_global_field,
    },
};

pub mod bool;
pub mod int;
pub mod str;
pub mod tuple;
pub mod unit;

/// This type represents an LKQL built-in type with all its information.
#[derive(Debug, PartialEq, Eq)]
pub struct BuiltinType {
    /// Name of the type as it should be displayed to the user.
    pub name: &'static str,

    /// Tag of the type, its unique identifier for optimized type checking.
    pub tag: isize,

    /// Fields in the type.
    pub fields: &'static [(&'static str, BuiltinTypeField)],

    /// Maps used to define custom behavior for the type regarding language
    /// constructions.
    pub overloads: &'static [(OverloadTarget, FunctionValue)],

    /// The function to call to register this type in a given Lua state.
    pub register_function: MetatableRegisteringFunction,
}

impl BuiltinType {
    /// String for the generic index function Lua source.
    const GENERIC_INDEX: &str = "function (self, field)
        -- Check in fields
        local res = __uv[1][field]
        if res ~= nil then
            return res
        end

        -- Check in properties
        res = __uv[2][field]
        if res ~= nil then
            return res(nil, self)
        end

        -- Check in methods
        res = __uv[3][field]
        if res ~= nil then
            return res
        end

        return nil
    end";

    /// Push a function on the stack corresponding to the `__index` meta-method
    /// for this built-in type.
    pub fn create_index_method(&self, l: LuaState) {
        self.push_representation_tables(l);
        FunctionValue::LuaFunction(String::from(Self::GENERIC_INDEX)).push_on_stack(l, 3);
    }

    /// Create Lua tables representing at run-time all parts of this built-in
    /// type:
    ///   * The first one being a table of all static fields.
    ///   * The second one is the properties table, all functions in this one
    ///     are automatically executed when accessed.
    ///   * The third one is the methods table.
    /// This function push tables in this order on the stack meaning that the
    /// new top of the stack is the method table of the type.
    fn push_representation_tables(&self, l: LuaState) {
        // Split static values, properties and methods
        let mut static_fields = Vec::new();
        let mut properties = Vec::new();
        let mut methods = Vec::new();
        for (name, field) in self.fields {
            match field {
                BuiltinTypeField::Value(runtime_value) => {
                    static_fields.push((name, runtime_value.clone()))
                }
                BuiltinTypeField::Property(function) => properties.push((name, function)),
                BuiltinTypeField::Method(function) => methods.push((name, function)),
            }
        }

        // Add synthetic fields in the type representation
        static_fields.push((&TYPE_TAG_FIELD, RuntimeValue::Integer(self.tag)));
        static_fields.push((&TYPE_NAME_FIELD, RuntimeValue::String(String::from(self.name))));

        // Create the static values table
        push_table(l, 0, static_fields.len() as i32);
        for (name, value) in static_fields {
            value.push_on_stack(l);
            set_field(l, -2, name);
        }

        // Create the properties table
        push_table(l, 0, properties.len() as i32);
        for (name, property) in properties {
            property.push_on_stack(l, 0);
            set_field(l, -2, name);
        }

        // Create the methods table
        push_table(l, 0, methods.len() as i32);
        for (name, method) in methods {
            method.push_on_stack(l, 0);
            set_field(l, -2, name);
        }
    }
}

/// This type represents a field in a built-in type.
#[derive(Debug, PartialEq, Eq)]
pub enum BuiltinTypeField {
    /// If the field is a constant value.
    Value(RuntimeValue),

    /// If the field is computed value.
    Property(FunctionValue),

    /// If the field is a method.
    Method(FunctionValue),
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
