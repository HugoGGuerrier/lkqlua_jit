//! # Runtime support module
//!
//! Support for runtime values.

use crate::{
    builtins::{
        traits::BuiltinTrait,
        types::{TYPE_NAME_FIELD, TYPE_TAGS_FIELD, TypeRef},
    },
    errors::{
        ErrorInstance, ErrorInstanceArg, MISSING_PARAM_TRAIT, NO_VALUE_FOR_PARAM,
        POS_AND_NAMED_VALUE_FOR_PARAM, WRONG_PARAM_TYPE,
    },
    lua::{
        LuaCFunction, LuaState, call, copy_value, get_global, get_metatable, get_top,
        load_lua_code, move_top_value, pop, push_bool, push_c_closure, push_integer, push_string,
        set_field,
    },
};

/// Name of the global variable where the "Unit" singleton is stored.
pub const UNIT_SINGLETON_GLOBAL_NAME: &str = "value@unit";

/// Name of the global variable where the "null" value is stored.
pub const NULL_SINGLETON_GLOBAL_NAME: &str = "value@null";

/// Name of the global variable where the importation function is stored.
pub const LKQL_IMPORT_GLOBAL_NAME: &str = "value@lkql_import";

/// Name of the global variable where the LKQL execution context is stored.
pub const CONTEXT_GLOBAL_NAME: &str = "value@execution_context";

/// Name of the global variable where the analysis library is stored.
pub const ANALYSIS_LIB_GLOBAL_NAME: &str = "value@analysis_lib";

/// Name of the global variable where the analysis context is stored.
pub const ANALYSIS_CONTEXT_GLOBAL_NAME: &str = "value@analysis_context";

/// Name of the global variable where the all analysis units are stored.
pub const ANALYSIS_UNITS_GLOBAL_NAME: &str = "value@analysis_units";

/// This type represents a runtime value that can be pushed on a Lua state
/// stack.
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Boolean(bool),
    Integer(isize),
    String(String),
    Callable(Function),

    /// Creates the value by calling the associated function with the Lua state
    /// it should be placed in.
    FromBuilder(fn(LuaState)),
}

impl RuntimeValue {
    /// Place the value represented by this object to the top of the provided
    /// Lua state.
    pub(crate) fn push_on_stack(&self, l: LuaState) {
        match self {
            RuntimeValue::Boolean(b) => push_bool(l, *b),
            RuntimeValue::Integer(i) => push_integer(l, *i),
            RuntimeValue::String(s) => push_string(l, s),
            RuntimeValue::Callable(f) => f.push_on_stack_with_uv(l, 0),
            RuntimeValue::FromBuilder(builder) => builder(l),
        }
    }

    /// Get the Lua literal representation of this runtime value. This function
    /// panics if this is not possible.
    fn lua_literal(&self) -> String {
        match self {
            RuntimeValue::Boolean(b) => b.to_string(),
            RuntimeValue::Integer(i) => i.to_string(),
            RuntimeValue::String(s) => s.clone(),
            _ => panic!("Cannot represents this value as a lua literal: {:?}", self),
        }
    }
}

/// This type represents a function runtime value.
#[derive(Debug, Clone)]
pub enum Function {
    /// A function value that is defined by a native function.
    CFunction(LuaCFunction),

    /// The Lua source that defined this function value. This must be a Lua
    /// function expression that will access up-values through the `__uv`
    /// table.
    LuaFunction(&'static str),

    // / A function following the LKQL ABI. Its body is provided as a Lua source
    // / that should return the result of the function.
    LkqlFunction {
        params: &'static [LkqlParam],
        body: &'static str,
    },
}

impl Function {
    /// Place the runtime value representing this function on the top of the
    /// stack.
    ///
    /// Pop as many values as `up_value_count` from the stack and provide them
    /// as up-values for the new function value.
    pub(crate) fn push_on_stack_with_uv(&self, l: LuaState, up_value_count: u8) {
        match self {
            Function::CFunction(function) => push_c_closure(l, *function, up_value_count),
            Function::LuaFunction(source) => {
                // Create the final Lua source
                let mut final_source = String::with_capacity(source.len());
                if up_value_count > 0 {
                    final_source.push_str("local __uv = {...}; ");
                }
                final_source.push_str("return ");
                final_source.push_str(source);

                // Parse the Lua function source and execute the parsing result
                load_lua_code(l, &final_source, "<lua_function>");
                move_top_value(l, get_top(l) - up_value_count as i32);
                call(l, up_value_count as i32, Some(1));
            }
            Function::LkqlFunction { params, body } => {
                // Create the Lua function parameter list
                let lua_params = format!(
                    "__named_args, {}",
                    params.iter().map(|p| p.name).collect::<Vec<_>>().join(", ")
                );

                // Create statements to fetch parameter values in named
                // parameters if they haven't.
                let named_param_checkers = params
                    .iter()
                    .map(|p| {
                        let name = p.name;
                        let pos_and_named_error = ErrorInstance::new(
                            POS_AND_NAMED_VALUE_FOR_PARAM.id,
                            vec![ErrorInstanceArg::Static(String::from(name))],
                        )
                        .to_json();
                        format!(
                            "local named_value = __named_args['{name}']
                            if {name} == nil then
                                {name} = named_value
                            elseif named_value ~= nil then
                                error('{pos_and_named_error}')
                            end"
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                // Create statements to ensure all parameters have a value or
                // set their default.
                let param_value_checkers = params
                    .iter()
                    .map(|p| {
                        // Create working values
                        let name = p.name;
                        let default = p
                            .default_value
                            .as_ref()
                            .map(RuntimeValue::lua_literal)
                            .unwrap_or(format!(
                                "error('{}')",
                                ErrorInstance::new(
                                    NO_VALUE_FOR_PARAM.id,
                                    vec![ErrorInstanceArg::Static(String::from(name))],
                                )
                                .to_json()
                            ));
                        format!("{name} = {name} or {default}")
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                // Create statements that checks parameter types
                let param_type_checkers = params.iter().filter_map(|p| {
                    p.required_type.as_ref().map(|required_type| {
                        // Create working values
                        let name = p.name;
                        let (checking_expr, error_id, expected_name) = match required_type {
                            ParamType::PreciseType(t) => (
                                format!("__types[{}]", t.as_builtin_type().tag),
                                WRONG_PARAM_TYPE.id,
                                String::from(t.as_builtin_type().display_name()),
                            ),
                            ParamType::UnionType(u) => (
                                u.iter()
                                    .map(|t| format!("__types[{}]", t.as_builtin_type().tag))
                                    .collect::<Vec<_>>()
                                    .join(" or "),
                                WRONG_PARAM_TYPE.id,
                                u.iter()
                                    .map(|t| t.as_builtin_type().display_name())
                                    .collect::<Vec<_>>()
                                    .join("\" or a \""),
                            ),
                            ParamType::Trait(t) => (
                                format!("{name}['{}']", t.runtime_field()),
                                MISSING_PARAM_TRAIT.id,
                                String::from(t.name),
                            ),
                        };
                        let wrong_type_error = ErrorInstance::new(
                            error_id,
                            vec![
                                ErrorInstanceArg::Static(expected_name),
                                ErrorInstanceArg::Static(String::from(name)),
                                ErrorInstanceArg::Static(String::from("%s")),
                            ],
                        )
                        .to_json();

                        // Then create the statement to check the parameter type
                        format!(
                            "local __types = {name}['{TYPE_TAGS_FIELD}']
                            if not {checking_expr} then
                                error(string.format('{wrong_type_error}', {name}['{TYPE_NAME_FIELD}']))
                            end"
                        )
                    })
                }).collect::<Vec<_>>().join("\n");

                // Finally assemble all statements and the function body to
                // create the Lua value.
                let uv_table = if up_value_count > 0 { "{...}" } else { "nil" };
                let final_source = format!(
                    "local __uv = {uv_table}
                    return function({lua_params})
                        -- Check named parameters if there are some
                        if __named_args ~= nil then
                            {named_param_checkers}
                        end

                        -- Check parameter values
                        {param_value_checkers}

                        -- Check parameter types
                        {param_type_checkers}

                        -- Finally execute the function body
                        {body}
                    end"
                );

                // Parse the Lua function source and execute the parsing result
                load_lua_code(l, &final_source, "<lkql_function>");
                move_top_value(l, get_top(l) - up_value_count as i32);
                call(l, up_value_count as i32, Some(1));
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LkqlParam {
    pub name: &'static str,
    pub required_type: Option<ParamType>,
    pub default_value: Option<RuntimeValue>,
}

impl LkqlParam {
    pub(crate) const fn new(name: &'static str) -> Self {
        Self { name, required_type: None, default_value: None }
    }

    pub(crate) const fn with_type(name: &'static str, required_type: TypeRef) -> Self {
        Self {
            name,
            required_type: Some(ParamType::PreciseType(required_type)),
            default_value: None,
        }
    }

    pub(crate) const fn with_trait(
        name: &'static str,
        required_trait: &'static BuiltinTrait,
    ) -> Self {
        Self {
            name,
            required_type: Some(ParamType::Trait(required_trait)),
            default_value: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParamType {
    PreciseType(TypeRef),
    UnionType(&'static [TypeRef]),
    Trait(&'static BuiltinTrait),
}

/// Util function to register the table at the provided index for garbage
/// collection. The meta-method "__gc" of the table is going to be called
/// when it becomes unreachable.
pub(crate) fn register_for_gc(l: LuaState, index: i32) {
    // Create the proxy object
    get_global(l, "newproxy");
    push_bool(l, true);
    call(l, 1, Some(1));

    // Get the metatable of the proxy object
    get_metatable(l, -1);

    // Create a closure to forward the GC call
    copy_value(l, -3);
    Function::LuaFunction("function(_) getmetatable(__uv[1]).__gc(__uv[1]) end")
        .push_on_stack_with_uv(l, 1);

    // Set the GC forwarder in the proxy metatable
    set_field(l, -2, "__gc");
    pop(l, 1);

    // Finally, place the proxy in the tracked table
    set_field(l, index - 1, "field@proxy");
}
