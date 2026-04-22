//! # Built-in functions
//!
//! This module contains all LKQL built-in functions and utils for them.

use crate::{
    ExecutionContext,
    builtins::{
        UNIT_SINGLETON_GLOBAL_NAME,
        types::pattern::{self, NATIVE_HANDLE_FIELD},
        utils::{get_bool_param, get_param, get_string_param},
    },
    engine::{CONTEXT_GLOBAL_NAME, analysis_lib::ANALYSIS_UNITS_GLOBAL_NAME, register_for_gc},
    errors::{
        DEPENDENCY_CYCLE, ERROR_DURING_IMPORTATION, ErrorInstance, ErrorInstanceArg,
        REGEX_SYNTAX_ERROR, REGEX_TOO_BIG,
    },
    lua::{
        LuaState, get_global, get_string, get_top, get_type, get_user_data, pop, push_string,
        push_table, push_user_data_ptr, raise_error, set_field, set_metatable, to_string,
    },
};
use regex::RegexBuilder;
use std::io::Write;
use std::{ffi::c_int, path::Path};

/// The default image of a value when the latter doesn't define one.
const DEFAULT_VALUE_IMAGE: &str = "<lkql_value>";

/// The "pattern" function
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lkql_pattern(l: LuaState) -> c_int {
    // Get the function parameter values
    let param_count = get_top(l) - 1;
    let regex = get_string_param(l, param_count, 1, "regex", None);
    let case_sensitive = get_bool_param(l, param_count, 2, "case_sensitive", Some(true));

    // Now create the Rust regex object
    match RegexBuilder::new(regex)
        .case_insensitive(!case_sensitive)
        .build()
    {
        Ok(compiled_regex) => {
            // Create a new Lua table
            push_table(l, 0, 1);

            // Store a pointer to the Rust compiled regex in it
            push_user_data_ptr(l, Box::into_raw(Box::new(compiled_regex)));
            set_field(l, -2, NATIVE_HANDLE_FIELD);

            // Then set the metatable of the result
            get_global(l, &pattern::IMPLEMENTATION.global_field_name());
            set_metatable(l, -2);

            // Finally, register the pattern Lua value for garbage collection
            register_for_gc(l, -1);
            1
        }
        Err(error) => {
            let error_instance = match error {
                regex::Error::Syntax(_) => ErrorInstance {
                    template_id: REGEX_SYNTAX_ERROR.id,
                    message_args: vec![ErrorInstanceArg::Static(String::from(regex))],
                },
                regex::Error::CompiledTooBig(_) => ErrorInstance {
                    template_id: REGEX_TOO_BIG.id,
                    message_args: vec![ErrorInstanceArg::Static(String::from(regex))],
                },
                _ => unreachable!(),
            };
            raise_error(l, &error_instance.to_json_string());
            0
        }
    }
}

/// The "print" function
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lkql_print(l: LuaState) -> c_int {
    // Get the function parameter values
    let param_count = get_top(l) - 1;
    let to_print_index = get_param(l, param_count, 1, "to_print");
    let new_line = get_bool_param(l, param_count, 2, "new_line", Some(true));

    // Get the current execution context
    get_global(l, CONTEXT_GLOBAL_NAME);
    let ctx = get_user_data::<ExecutionContext>(l, get_top(l)).unwrap();
    pop(l, 1);

    // Then display the value on the configured standard output
    if new_line {
        writeln!(ctx.config.std_out, "{}", to_string(l, to_print_index, DEFAULT_VALUE_IMAGE))
            .unwrap();
    } else {
        write!(ctx.config.std_out, "{}", to_string(l, to_print_index, DEFAULT_VALUE_IMAGE))
            .unwrap();
    }

    // Return the LKQL unit value
    get_global(l, UNIT_SINGLETON_GLOBAL_NAME);
    1
}

/// The "img" function
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lkql_img(l: LuaState) -> c_int {
    let param_count = get_top(l) - 1;
    let value_index = get_param(l, param_count, 1, "value");
    match get_type(l, value_index) {
        crate::lua::LuaType::String => {
            push_string(l, &format!("\"{}\"", get_string(l, value_index).unwrap()))
        }
        _ => push_string(l, to_string(l, value_index, DEFAULT_VALUE_IMAGE)),
    }
    1
}

/// The "units" function
pub unsafe extern "C" fn lkql_units(l: LuaState) -> c_int {
    get_global(l, ANALYSIS_UNITS_GLOBAL_NAME);
    return 1;
}

/// The importation function, this is not intended to be called by the user.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn lkql_import(l: LuaState) -> c_int {
    // Get the name of the file to import
    let module_file = Path::new(get_string(l, 1).unwrap());

    // Get the current execution context
    get_global(l, CONTEXT_GLOBAL_NAME);
    let ctx = get_user_data::<ExecutionContext>(l, get_top(l)).unwrap();
    pop(l, 1);

    // Check dependency cycle
    if let Some(module_source) = &ctx.source_repo.get_id_by_file(module_file) {
        if ctx.execution_stack.contains(module_source) {
            let exec_stack_image = ctx
                .execution_stack
                .iter()
                .map(|s| {
                    Path::new(ctx.source_repo.get_name_by_id(*s))
                        .file_stem()
                        .unwrap()
                        .to_string_lossy()
                })
                .collect::<Vec<_>>()
                .join(" -> ");
            raise_error(
                l,
                &ErrorInstance {
                    template_id: DEPENDENCY_CYCLE.id,
                    message_args: vec![ErrorInstanceArg::Static(format!(
                        "{exec_stack_image} -> {}",
                        module_file.file_stem().unwrap().to_string_lossy()
                    ))],
                }
                .to_json_string(),
            );
        }
    }

    // Then execute the module file, report errors if there are any
    if let Err(report) = ctx.execute_lkql_file(Path::new(module_file)) {
        report.print(&ctx.source_repo, &mut ctx.config.std_err, false);
        raise_error(
            l,
            &ErrorInstance { template_id: ERROR_DURING_IMPORTATION.id, message_args: vec![] }
                .to_json_string(),
        );
        0
    } else {
        1
    }
}
