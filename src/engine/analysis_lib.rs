//! # LKQL analysis library
//!
//! This module contains all entities required to load an analysis library
//! that is going to be used by the LKQL engine to perform queries on the
//! analyzed code base.

use crate::{
    Config,
    builtins::{
        NULL_SINGLETON_GLOBAL_NAME,
        traits::indexable,
        types::{
            BuiltinType, BuiltinTypeRepo, TYPE_NAME_FIELD, TYPE_TAGS_FIELD, TypeField,
            TypeImplementation, img_property, list, obj,
        },
    },
    diagnostics::{Diagnostic, DiagnosticCollector},
    engine::LuaValue,
    errors::{ANALYSIS_LIBRARY_ERROR, ErrorInstance, ErrorInstanceArg},
    lua::{
        LuaState, copy_value, find_in_lua_path, get_field, get_global, get_index, get_length,
        get_string, load_lua_file, pop, push_bool, push_c_function, push_nil, push_string,
        push_table, safe_call, set_field, set_global, set_index, set_metatable,
    },
};
use std::{cmp::min, ffi::c_int, i32, path::PathBuf};

pub const ANALYSIS_LIB_GLOBAL_NAME: &str = "value@analysis_lib";
pub const ANALYSIS_CONTEXT_GLOBAL_NAME: &str = "value@analysis_context";
pub const ANALYSIS_UNITS_GLOBAL_NAME: &str = "value@analysis_units";

/// This type represents a loaded Langkit Lua analysis library. It offers an
/// abstraction in the Rust world to access all useful information stored in
/// it.
#[derive(Debug)]
pub struct AnalysisLibrary {
    lua_state: LuaState,
    pub(crate) struct_types: Vec<String>,
    pub(crate) node_types: NodeTypeRepo,
}

impl AnalysisLibrary {
    /// Load a Langkit analysis library following the provided configuration
    /// and parse files to analyze.
    pub(super) fn new(
        lua_state: LuaState,
        config: &Config,
        builtin_types: &BuiltinTypeRepo,
    ) -> Result<Self, DiagnosticCollector> {
        let l = lua_state;

        // Retrieve the analysis library source
        let module_name = format!("lib{}lang", config.analyzed_lang_name.to_lowercase());
        let module_file = match find_in_lua_path(&module_name) {
            Some(f) => f,
            None => {
                return Err(DiagnosticCollector::from(Diagnostic::error_msg(format!(
                    "Cannot find the {} analysis library, please ensure \"{}\" is accessible \
                     through the LUA_PATH",
                    config.analyzed_lang_name,
                    format!("{module_name}.lua")
                ))));
            }
        };

        // Load the Lua analysis library
        load_lua_file(l, &module_file);
        Self::pcall(l, 0)?;
        set_global(l, ANALYSIS_LIB_GLOBAL_NAME);

        // Initialize the analysis context
        Self::create_analysis_context(l)?;

        // Parse all sources to analyze and store resulting units
        Self::parse_sources(l, &config.files_to_analyze)?;

        // Add the "img" property to all constant analysis library types
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        for lua_type in [
            "BigInt",
            "SourceLocation",
            "SourceLocationRange",
            "Diagnostic",
            "AnalysisUnit",
            "Token",
        ] {
            get_field(l, -1, lua_type);
            get_field(l, -1, "__properties");
            push_c_function(l, img_property);
            set_field(l, -2, "img");
            pop(l, 2);
        }
        pop(l, 1);

        // Configure struct based type type be compatible with the "Object"
        // LKQL type.
        for obj_type in ["SourceLocation", "SourceLocationRange", "Diagnostic"] {
            Self::init_type_compatibility(l, obj_type, &obj::TYPE, &obj::IMPLEMENTATION);
        }

        // Configure the "ArrayBase" type to be compatible with the "List"
        // LKQL type.
        Self::init_type_compatibility(l, "ArrayBase", &list::TYPE, &list::IMPLEMENTATION);

        // Configure all structure types to be compatible with the "Object"
        // LKQL type.
        let struct_types = Self::get_struct_types(l);
        for struct_type in &struct_types {
            Self::init_type_compatibility(l, struct_type, &obj::TYPE, &obj::IMPLEMENTATION);
        }

        // Then extract all node types defined by the library and initialize
        // them.
        let node_types = Self::get_node_types(l, builtin_types.next_free_tag());
        for node_type in &node_types.registered_types {
            Self::init_node_type(l, node_type);
        }

        // Set the LKQL "null" value
        Self::register_null_value(l);

        // Finally, register the analysis library error formatter
        Self::register_error_formatter(l);

        // Return the new analysis library object
        Ok(Self { lua_state, struct_types, node_types })
    }

    /// Internal helper to create a new analysis context from the loaded
    /// analysis library and store it in the Lua state.
    fn create_analysis_context(l: LuaState) -> Result<(), DiagnosticCollector> {
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, "AnalysisContext");
        get_field(l, -1, "create");
        Self::pcall(l, 0)?;
        set_global(l, ANALYSIS_CONTEXT_GLOBAL_NAME);
        pop(l, 2);
        Ok(())
    }

    fn parse_sources(l: LuaState, sources: &Vec<PathBuf>) -> Result<(), DiagnosticCollector> {
        // Create the analysis unit list table
        let array_size = min(sources.len(), i32::MAX as usize) as i32;
        push_table(l, array_size, 0);

        // Parse all requested sources with the analysis context
        get_global(l, ANALYSIS_CONTEXT_GLOBAL_NAME);
        for (i, file) in sources.iter().enumerate() {
            get_field(l, -1, "get_unit_from_file");
            push_nil(l);
            copy_value(l, -3);
            push_string(l, &file.to_string_lossy());
            Self::pcall(l, 3)?;
            set_index(l, -3, (i + 1) as i32);
        }
        pop(l, 1);

        // Set the "List" type to the analysis unit table
        get_global(l, &list::IMPLEMENTATION.global_field_name());
        set_metatable(l, -2);

        // Finally, store analysis unit in the Lua state
        set_global(l, ANALYSIS_UNITS_GLOBAL_NAME);
        Ok(())
    }

    /// Internal helper to get a Rust vector containing all structure types
    /// defined by the analysis library.
    fn get_struct_types(l: LuaState) -> Vec<String> {
        // Get all struct types in Lua
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, "struct_types");

        // Transform the Lua table into a Rust vector
        let struct_type_count = get_length(l, -1);
        let mut res = Vec::with_capacity(struct_type_count);
        for i in 1..=struct_type_count {
            get_index(l, -1, i as i32);
            res.push(String::from(get_string(l, -1).unwrap()));
            pop(l, 1);
        }

        // Cleanup the stack
        pop(l, 2);

        // Return the result vector
        res
    }

    /// Get all node types defined by the loaded analysis library.
    fn get_node_types(l: LuaState, first_tag: i32) -> NodeTypeRepo {
        // Get all node types in Lua
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, "node_types");

        // Then create a vector of all node type info
        let mut next_tag = first_tag;
        let node_type_count = get_length(l, -1);
        let mut node_type_info = Vec::with_capacity(node_type_count);
        for i in 1..=node_type_count {
            // Get the node type name
            get_index(l, -1, i as i32);
            get_field(l, -1, "name");
            let node_type_name = String::from(get_string(l, -1).unwrap());
            pop(l, 1);

            // Get the node base type names
            get_field(l, -1, "bases");
            let base_type_count = get_length(l, -1);
            let mut base_type_names = Vec::with_capacity(base_type_count);
            for j in 1..=base_type_count {
                get_index(l, -1, j as i32);
                base_type_names.push(String::from(get_string(l, -1).unwrap()));
                pop(l, 1);
            }
            pop(l, 2);

            // Add the node type, its tag and its bases to the information
            // vector.
            node_type_info.push((node_type_name, next_tag, base_type_names));
            next_tag += 1;
        }

        // Create the node type repository
        let mut node_type_repo = NodeTypeRepo::new();
        for (name, tag, bases) in &node_type_info {
            node_type_repo.registered_types.push(NodeType::new(
                name.clone(),
                *tag,
                bases
                    .iter()
                    .map(|b| {
                        node_type_info
                            .iter()
                            .find_map(|(n, t, _)| if n == b { Some(*t) } else { None })
                            .unwrap()
                    })
                    .collect(),
            ));
        }

        // Cleanup the stack and return the result
        pop(l, 2);
        node_type_repo
    }

    /// Initialize the type named after `lua_type` in the analysis library to
    /// be fully compatible with the provided `builtin_type`.
    fn init_type_compatibility(
        l: LuaState,
        lua_type: &str,
        builtin_type: &BuiltinType,
        implementation: &TypeImplementation,
    ) {
        // Get the Lua type
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, lua_type);

        // Fill constant fields
        get_field(l, -1, "__fields");
        push_table(l, 0, 0);
        push_bool(l, true);
        set_index(l, -2, builtin_type.tag);
        set_field(l, -2, TYPE_TAGS_FIELD);
        push_string(l, builtin_type.display_name());
        set_field(l, -2, TYPE_NAME_FIELD);

        // Store implemented traits
        for tr in builtin_type.traits {
            push_bool(l, true);
            set_field(l, -2, &tr.runtime_field());
        }

        // Get "__properties" table from the Lua type
        get_field(l, -2, "__properties");

        // Now place all fields and properties in the Lua type
        for (name, field) in implementation.fields {
            let index = match field {
                TypeField::Value(runtime_value) => {
                    runtime_value.push_on_stack(l);
                    -3
                }
                TypeField::Property(function_value) => {
                    function_value.push_on_stack(l);
                    -2
                }
            };
            set_field(l, index, name);
        }
        pop(l, 2);

        // Finally, set type overloads
        for (target, function) in implementation.overloads {
            function.push_on_stack(l);
            set_field(l, -2, target.metamethod_name());
        }

        // Cleanup the stack
        pop(l, 2);
    }

    /// Internal helper to initialize the provided node type in the currently
    /// loaded analysis library.
    fn init_node_type(l: LuaState, node_type: &NodeType) {
        // Get the Lua type
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, &node_type.name);

        // Fill type tags
        get_field(l, -1, "__fields");
        push_table(l, 0, 0);
        push_bool(l, true);
        set_index(l, -2, node_type.tag);
        for base_type in &node_type.base_types {
            push_bool(l, true);
            set_index(l, -2, *base_type as i32);
        }
        set_field(l, -2, TYPE_TAGS_FIELD);

        // Then set the type name
        push_string(l, &node_type.name);
        set_field(l, -2, TYPE_NAME_FIELD);

        // Make node indexable values
        push_bool(l, true);
        set_field(l, -2, &indexable::TRAIT.runtime_field());
        pop(l, 1);

        // Set the "img" property
        get_field(l, -1, "__properties");
        push_c_function(l, img_property);
        set_field(l, -2, "img");

        // Cleanup the stack
        pop(l, 3);
    }

    fn register_null_value(l: LuaState) {
        // Get the name of the root node type
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, "root_node_type");
        let root_node_type = get_string(l, -1).unwrap();
        pop(l, 1);

        // Then get the default value of the root node type
        get_field(l, -1, root_node_type);
        get_field(l, -1, "_default");
        set_global(l, NULL_SINGLETON_GLOBAL_NAME);
        pop(l, 2);
    }

    /// Internal helper to setup the error formatting function in the loaded
    /// analysis library.
    fn register_error_formatter(l: LuaState) {
        get_global(l, ANALYSIS_LIB_GLOBAL_NAME);
        get_field(l, -1, "Exception");
        push_c_function(l, analysis_lib_error_formatter);
        set_field(l, -2, "format_exception_message");
        pop(l, 2);
    }

    /// Internal helper used to call a function value and return [`Err`] if an
    /// error occur during its execution.
    fn pcall(l: LuaState, arg_count: i32) -> Result<(), DiagnosticCollector> {
        if let Err(msg) = safe_call(l, arg_count, Some(1), None) {
            Err(DiagnosticCollector::from(Diagnostic::error_msg(msg)))
        } else {
            Ok(())
        }
    }
}

/// This type is used to represent a repository where node types are stored. A
/// repository is created from an analysis library by using its introspection
/// utils.
#[derive(Debug)]
pub struct NodeTypeRepo {
    pub registered_types: Vec<NodeType>,
}

impl NodeTypeRepo {
    fn new() -> Self {
        Self { registered_types: Vec::new() }
    }

    /// Get the node type corresponding to the provided name if any.
    pub(crate) fn get_type_by_name(&self, type_name: &str) -> Option<&NodeType> {
        self.registered_types.iter().find(|n| n.name == type_name)
    }
}

/// This type represents a node type defined by the currently used analysis
/// library.
#[derive(Debug)]
pub struct NodeType {
    pub name: String,
    pub tag: i32,
    pub base_types: Vec<i32>,
}

impl NodeType {
    /// Create a new node type object.
    pub fn new(name: String, tag: i32, base_types: Vec<i32>) -> Self {
        Self { name, tag, base_types }
    }
}

/// Callback used to format an error from the analysis library
#[unsafe(no_mangle)]
unsafe extern "C" fn analysis_lib_error_formatter(l: LuaState) -> c_int {
    // Get kind and message of the error
    get_field(l, 1, "kind");
    get_field(l, 1, "message");

    // Create the error instance object
    let error_instance = ErrorInstance::new(
        ANALYSIS_LIBRARY_ERROR.id,
        vec![ErrorInstanceArg::Static(format!(
            "{} [{}]",
            get_string(l, -1).unwrap(),
            get_string(l, -2).unwrap()
        ))],
    );

    // Finally push the encoded error instance as a JSON string
    push_string(l, &error_instance.to_json());
    return 1;
}
