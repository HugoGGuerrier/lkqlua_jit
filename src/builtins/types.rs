//! # Built-in types
//!
//! This module contains all LKQL built-in types and their associated working
//! values.

use crate::{
    builtins::traits::BuiltinTrait,
    engine::{FunctionValue, LuaValue, RuntimeValue},
    lua::{
        LuaState, get_top, push_bool, push_integer, push_string, push_table, set_field, set_global,
        set_top,
    },
};

pub mod bool;
pub mod int;
pub mod list;
pub mod namespace;
pub mod obj;
pub mod str;
pub mod stream;
pub mod tuple;
pub mod unit;

/// Pseudo-field to use to get the name of the type of a value.
pub const TYPE_NAME_FIELD: &str = "field@type_name";

/// Pseudo-field to use to get the tag of the type of a value.
pub const TYPE_TAG_FIELD: &str = "field@type_tag";

/// This type represents an LKQL built-in type.
#[derive(Debug)]
pub struct BuiltinType {
    pub tag: isize,
    pub traits: &'static [&'static BuiltinTrait],
    pub implementation_kind: TypeImplementationKind,
}

/// This type represents the way a built-in type is implemented.
/// An LKQL type may be either mono or polymorphic. This means that a type can
/// have multiple implementations, while staying the same type to the user.
#[derive(Debug)]
pub enum TypeImplementationKind {
    Monomorphic {
        implementation: TypeImplementation,
    },

    Polymorphic {
        /// Base implementation containing the common behaviors of the type.
        /// This may be overridden by type specializations.
        /// The name defined in this implementation is used to display the type
        /// to the users.
        base_implementation: TypeImplementation,

        /// An array of all type specializations (implementations).
        specializations: &'static [TypeImplementation],
    },
}

impl BuiltinType {
    /// Get the type name to display to the language users.
    pub fn display_name(&self) -> &'static str {
        match &self.implementation_kind {
            TypeImplementationKind::Monomorphic { implementation } => implementation.name,
            TypeImplementationKind::Polymorphic { base_implementation, .. } => {
                base_implementation.name
            }
        }
    }

    /// Place all Lua values required to support this type at run-time in the
    /// provided Lua execution context.
    pub fn place_in_lua_context(&self, l: LuaState) {
        let top = get_top(l);
        match &self.implementation_kind {
            TypeImplementationKind::Monomorphic { implementation } => {
                // Create a new table that is going to be the type meta-table
                push_table(l, 0, implementation.overloads.len() as i32 + 1);

                // Then create the "__index" meta-method for the type
                self.prepare_field_tables(l);
                implementation.fill_field_tables(l);
                implementation.create_index_method(l);
                set_field(l, -2, "__index");

                // Then place all overloads in the meta-table
                implementation.push_overloads(l);

                // Finally register the type implementation in the Lua
                // execution context.
                let reg_fun = implementation
                    .registering_function
                    .unwrap_or(register_metatable_in_globals);
                reg_fun(l, implementation);

                set_top(l, top);
            }
            TypeImplementationKind::Polymorphic { base_implementation, specializations } => {
                for specialization in *specializations {
                    // Create a new table that is going to be the meta-table
                    push_table(l, 0, base_implementation.overloads.len() as i32 + 1);

                    // Create field tables and override it with specializations
                    self.prepare_field_tables(l);
                    base_implementation.fill_field_tables(l);
                    specialization.fill_field_tables(l);

                    // Then create the "__index" meta-method
                    if specialization.index_method.is_some() {
                        specialization.create_index_method(l);
                    } else {
                        base_implementation.create_index_method(l);
                    }
                    set_field(l, -2, "__index");

                    // Place all overloads in the meta-table
                    base_implementation.push_overloads(l);
                    specialization.push_overloads(l);

                    // Finally, register the meta-table in the context
                    let reg_fun = specialization
                        .registering_function
                        .or(base_implementation.registering_function)
                        .unwrap_or(register_metatable_in_globals);
                    reg_fun(l, specialization);
                    set_top(l, top);
                }
            }
        }
    }

    /// Place at the top of the Lua stack, in this order:
    ///   * A table that is going to contain all value fields of the type
    ///   * A table that is going to contain all properties of the type
    ///   * A table that is going to contain all methods of the type
    /// This function pre-fill some of those table with already known values,
    /// but main content is going to be [`TypeImplementation`] specific.
    fn prepare_field_tables(&self, l: LuaState) {
        // Create the value field table
        push_table(l, 0, 2);

        // Store the type tag
        push_integer(l, self.tag);
        set_field(l, -2, TYPE_TAG_FIELD);

        // Store the type display name
        push_string(l, self.display_name());
        set_field(l, -2, TYPE_NAME_FIELD);

        // Store the type implemented traits
        for tr in self.traits {
            push_bool(l, true);
            set_field(l, -2, &tr.runtime_field());
        }

        // Create the properties table
        push_table(l, 0, 0);

        // Create the methods table
        push_table(l, 0, 0);
    }
}

/// This type represents an implementation of a built-in type that accepts
/// multiple implementations.
/// All behaviors defined in this type are going to override ones defined in
/// the associated [`BuiltinType`] instance.
#[derive(Debug)]
pub struct TypeImplementation {
    /// Specific name of the implementation.
    pub name: &'static str,

    /// Fields specific to this implementation.
    pub fields: &'static [(&'static str, TypeField)],

    /// List of built-in behaviors to overload for this implementation.
    pub overloads: &'static [(OverloadTarget, FunctionValue)],

    /// Specific indexing meta-method for this implementation. If [`None`], the
    /// [`GENERIC_INDEX`] is going to be used.
    pub index_method: Option<FunctionValue>,

    /// Function used to store the implementation Lua representation in a given
    /// Lua execution context.
    /// If [`None`], the [`register_metatable_in_globals`] function is going to
    /// be used.
    pub registering_function: Option<MetatableRegisteringFunction>,
}

impl TypeImplementation {
    /// Get the
    pub fn global_field_name(&self) -> String {
        format!("type@{}", self.name)
    }

    /// Fill tables containing fields of this type implementation. This
    /// function assumes that there are at least 3 tables on the top of the
    /// stack, in this order (from bottom to top):
    ///   * The one that contains value fields
    ///   * The one that contains properties
    ///   * The one that contains methods
    fn fill_field_tables(&self, l: LuaState) {
        // Split static values, properties and methods
        let mut static_fields = Vec::new();
        let mut properties = Vec::new();
        let mut methods = Vec::new();
        for (name, field) in self.fields {
            match field {
                TypeField::Value(runtime_value) => {
                    static_fields.push((name, runtime_value.clone()))
                }
                TypeField::Property(function) => properties.push((name, function)),
                TypeField::Method(function) => methods.push((name, function)),
            }
        }

        // Then fill field tables
        fn push_field<T: LuaValue>(l: LuaState, table_index: i32, name: &str, value: &T) {
            value.push_on_stack(l);
            set_field(l, table_index - 1, name);
        }
        static_fields
            .iter()
            .for_each(|(n, v)| push_field(l, -3, n, v));
        properties
            .iter()
            .for_each(|(n, v)| push_field(l, -2, n, *v));
        methods.iter().for_each(|(n, v)| push_field(l, -1, n, *v));
    }

    /// Push a function on the stack to be used as the `__index` meta-method
    /// for this type implementation.
    fn create_index_method(&self, l: LuaState) {
        self.index_method
            .as_ref()
            .unwrap_or(&FunctionValue::LuaFunction(GENERIC_INDEX))
            .push_on_stack_with_uv(l, 3);
    }

    /// Push all overloads meta-methods of this type implementation in the
    /// table at the top of the Lua stack.
    fn push_overloads(&self, l: LuaState) {
        for (target, function) in self.overloads {
            function.push_on_stack(l);
            set_field(l, -2, target.metamethod_name());
        }
    }
}

/// This type represents a field in a built-in type.
#[derive(Debug)]
pub enum TypeField {
    /// If the field is a value.
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

/// This type represents a function that register a type implementation
/// meta-table in the provided Lua execution context.
/// This function should assume that the type meta-table is currently on the
/// top of the stack.
pub type MetatableRegisteringFunction = fn(LuaState, &TypeImplementation);

// ----- Support functions -----

/// Lua function used as generic value indexing function.
const GENERIC_INDEX: &str = "function (self, field)
    -- Check in type fields
    local res = __uv[1][field]
    if res ~= nil then
        return res
    end

    -- Check in type properties
    res = __uv[2][field]
    if res ~= nil then
        return res(nil, self)
    end

    -- Check in type methods
    res = __uv[3][field]
    if res ~= nil then
        return res
    end

    -- Nothing has been found
    return nil
end";

/// This is the generic meta-table registering function, it places the
/// meta-table on the top of the stack in a global field named from the type
/// precise name (the type name in case of a single implementation type, the
/// name of the implementation otherwise).
pub fn register_metatable_in_globals(l: LuaState, type_implementation: &TypeImplementation) {
    set_global(l, &type_implementation.global_field_name());
}
