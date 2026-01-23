//! # Built-in functions
//!
//! This module contains all LKQL built-in functions and utils for them.

use crate::{
    ExecutionContext,
    builtins::utils::{get_bool_param, get_param},
    engine::CONTEXT_GLOBAL_NAME,
    errors::{DEPENDENCY_CYCLE, ERROR_DURING_IMPORTATION, ErrorInstance, ErrorInstanceArg},
    lua::{
        LuaState, get_global, get_string, get_top, get_type, get_user_data, pop, push_string,
        raise_error, to_string,
    },
};
use std::io::Write;
use std::{ffi::c_int, path::Path};

/// The default image of a value when the latter doesn't define one.
const DEFAULT_VALUE_IMAGE: &str = "<lkql_value>";

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
    0
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
