//! # Runtime support module
//!
//! Support for runtime values.

use crate::lua::{
    LuaCFunction, LuaState, call, copy_value, get_global, get_metatable, get_top, load_lua_code,
    move_top_value, pop, push_bool, push_c_closure, push_integer, push_string, set_field,
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
            RuntimeValue::Integer(i) => push_integer(l, *i),
            RuntimeValue::String(s) => push_string(l, s),
            RuntimeValue::Callable(f) => f.push_on_stack_with_uv(l, 0),
            RuntimeValue::FromBuilder(builder) => builder(l),
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
        }
    }
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
