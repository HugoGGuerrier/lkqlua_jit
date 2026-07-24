//! # Built-in functions
//!
//! This module contains all LKQL built-in functions.

use crate::{
    ExecutionContext,
    builtins::{
        UNIT_SINGLETON_GLOBAL_NAME,
        utils::{get_bool_param, get_param, get_string_param},
    },
    errors::{DEPENDENCY_CYCLE, ErrorInstance, ErrorInstanceArg},
    lua::{
        LuaState, get_global, get_index, get_length, get_string, get_top, get_type, get_user_data,
        is_nil, pop, push_string, raise_error, to_string,
    },
    runtime::{ANALYSIS_ROOTS_GLOBAL_NAME, ANALYSIS_UNITS_GLOBAL_NAME, CONTEXT_GLOBAL_NAME},
};
use std::{ffi::c_int, io::Write, path::Path};

/// The "print" function.
#[unsafe(no_mangle)]
pub extern "C" fn lkql_print(l: LuaState) -> c_int {
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
        writeln!(ctx.config.std_out, "{}", to_string(l, to_print_index)).unwrap();
    } else {
        write!(ctx.config.std_out, "{}", to_string(l, to_print_index)).unwrap();
    }

    // Return the LKQL unit value
    get_global(l, UNIT_SINGLETON_GLOBAL_NAME);
    1
}

/// The "img" function.
#[unsafe(no_mangle)]
pub extern "C" fn lkql_img(l: LuaState) -> c_int {
    let param_count = get_top(l) - 1;
    let value_index = get_param(l, param_count, 1, "value");
    match get_type(l, value_index) {
        crate::lua::LuaType::String => {
            push_string(l, &format!("\"{}\"", get_string(l, value_index).unwrap()))
        }
        _ => push_string(l, to_string(l, value_index)),
    }
    1
}

/// The "units" function.
#[unsafe(no_mangle)]
pub extern "C" fn lkql_units(l: LuaState) -> c_int {
    get_global(l, ANALYSIS_UNITS_GLOBAL_NAME);
    1
}

/// The "roots" function.
#[unsafe(no_mangle)]
pub extern "C" fn lkql_roots(l: LuaState) -> c_int {
    get_global(l, ANALYSIS_ROOTS_GLOBAL_NAME);
    1
}

/// The importation internal function.
#[unsafe(no_mangle)]
pub extern "C" fn lkql_import(l: LuaState) -> c_int {
    // Get the name of the file to import
    let param_count = get_top(l) - 1;
    let module_file = Path::new(get_string_param(l, param_count, 1, "module_file", None));

    // Get the current execution context
    get_global(l, CONTEXT_GLOBAL_NAME);
    let ctx = get_user_data::<ExecutionContext>(l, get_top(l)).unwrap();
    pop(l, 1);

    // Check dependency cycle
    if let Some(module_source) = &ctx.source_repo.get_id_by_file(module_file)
        && ctx.execution_stack.contains(module_source)
    {
        let exec_stack_image = ctx
            .execution_stack
            .iter()
            .map(|s| {
                Path::new(ctx.source_repo.get_name_by_id(*s).unwrap())
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
            })
            .collect::<Vec<_>>()
            .join(" -> ");
        raise_error(
            l,
            &ErrorInstance::new(
                DEPENDENCY_CYCLE.id,
                vec![ErrorInstanceArg::Static(format!(
                    "{exec_stack_image} -> {}",
                    module_file.file_stem().unwrap().to_string_lossy()
                ))],
            )
            .to_json(),
        );
    }

    // Then execute the module file, report errors if there are some
    if let Err(diagnostics) = ctx.execute_lkql_file(Path::new(module_file)) {
        raise_error(l, &diagnostics.to_json());
        0
    } else {
        1
    }
}

/// The internal error raising function. This function expects 2 arguments:
///   * The first one is the identifier of the error template to instantiate as
///     a string.
///   * The second on is a table containing all arguments for the error
///     template.
pub extern "C" fn lkql_error(l: LuaState) -> c_int {
    // Get the template to instantiate
    let template_id = get_string(l, 1).unwrap().parse::<usize>().unwrap();

    // Fetch all arguments for the template
    let mut message_args = Vec::new();
    if !is_nil(l, 2) {
        let message_args_count = get_length(l, 2);
        for i in 1..=message_args_count {
            get_index(l, 2, i as i32);
            message_args.push(ErrorInstanceArg::Static(String::from(to_string(l, -1))));
            pop(l, 1);
        }
    }

    // Create the error instance object and push its JSON representation on
    // the Lua stack.
    raise_error(
        l,
        &ErrorInstance::new(template_id as usize, message_args)
            .to_json()
            .to_string(),
    );
    0
}
