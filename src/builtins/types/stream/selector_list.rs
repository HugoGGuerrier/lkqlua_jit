//! # Selector list stream implementation
//!
//! This module defines the "selector list" implementation of the "Stream"
//! LKQL type. This implementation use a selector body to generate its
//! content.

use const_format::formatcp;

use crate::{
    builtins::types::{
        TYPE_NAME_FIELD, TYPE_TAGS_FIELD, TypeField, TypeImplementation,
        stream::INTERNAL_NEXT_FIELD, unit,
    },
    errors::WRONG_TYPE,
    runtime::{Function, G_LKQL_ERROR, RuntimeValue},
};

/// Field that contains the value to recurse on.
pub const REC_RECURSE_FIELD: &str = "field@rec";

/// Field that contains whether to unwrap the recurse value.
pub const REC_RECURSE_UNPACK_FIELD: &str = "field@rec_unwrap";

/// Field that contains the value to include in the result.
pub const REC_RESULT_FIELD: &str = "field@res";

/// Field that contains whether to unwrap the value to include in the result.
pub const REC_RESULT_UNPACK_FIELD: &str = "field@res_unwrap";

/// Field that contains the root of the selector list.
pub const ROOT_FIELD: &str = "field@root_value";

/// Field that contains the precise depth of the selector list.
pub const DEPTH_FIELD: &str = "field@depth";

/// Field that contains the minimum depth of the selector list.
pub const MIN_DEPTH_FIELD: &str = "field@min_depth";

/// Field that contains the maximum depth of the selector list.
pub const MAX_DEPTH_FIELD: &str = "field@max_depth";

/// Field that contains the body of the selector.
pub const BODY_FIELD: &str = "field@body";

/// Field that contains the list of elements to iterate on next.
const RECURSE_LIST_FIELD: &str = "field@recurse_list";

/// Field that contains the list of elements to return
const RESULT_LIST_FIELD: &str = "field@result_list";

pub const SPECIALIZATION: TypeImplementation = TypeImplementation {
    name: "SelectorList",
    fields: &[(INTERNAL_NEXT_FIELD, TypeField::Value(NEXT))],
    overloads: &[],
    index_method: None,
    registering_function: None,
};

const NEXT: RuntimeValue = RuntimeValue::Callable(Function::LuaFunction(formatcp!(
    "function (self)
        -- First check whether the selector list has been initialized
        if self['{ROOT_FIELD}'] ~= nil then
            self['{RECURSE_LIST_FIELD}'] = {{
                {{ value=self['{ROOT_FIELD}'], depth=0 }}
            }}
            self['{RESULT_LIST_FIELD}'] = {{}}
            self['{ROOT_FIELD}'] = nil
        end

        -- Cache result and recurse lists
        local recurse_list = self['{RECURSE_LIST_FIELD}']
        local result_list = self['{RESULT_LIST_FIELD}']

        -- Now process the recurse list
        while #recurse_list ~= 0 or #result_list ~= 0 do
            -- Check if there is an element to return
            if #result_list > 0 then
                return table.remove(result_list, 1)
            end

            -- Get the next recursing step and call the body with it
            local step = table.remove(recurse_list, 1)
            local next = self['{BODY_FIELD}'](step.value)
            local next_depth = step.depth + 1

            -- If the next element is a recurse value, add it to the result
            -- and the recurse list.
            if next['{REC_RECURSE_FIELD}'] ~= nil then
                -- Get working value in the next value
                local rec_value = next['{REC_RECURSE_FIELD}']
                local rec_unpack = next['{REC_RECURSE_UNPACK_FIELD}']
                local res_value = next['{REC_RESULT_FIELD}']
                local res_unpack = next['{REC_RESULT_UNPACK_FIELD}']

                -- Ensure there is a result value
                if res_value == nil then
                    res_value = rec_value
                end
                if res_unpack == nil then
                    res_unpack = rec_unpack
                end

                -- Get whether the result depth is valid and under the maximum
                -- depth.
                local is_valid_depth = true
                local is_under_maximum_depth = true
                if self['{DEPTH_FIELD}'] > -1 then
                    is_valid_depth = next_depth == self['{DEPTH_FIELD}']
                    is_under_maximum_depth = next_depth <= self['{DEPTH_FIELD}']
                else
                    is_under_maximum_depth = (
                        self['{MAX_DEPTH_FIELD}'] <= -1 or
                        next_depth <= self['{MAX_DEPTH_FIELD}']
                    )
                    is_valid_depth = (
                        (self['{MIN_DEPTH_FIELD}'] <= -1 or next_depth >= self['{MIN_DEPTH_FIELD}'])
                        and is_under_maximum_depth
                    )
                end

                -- Insert the next element in the result list if it is at a
                -- valid depth.
                if is_valid_depth then
                    if res_unpack then
                        for _, elem in ipairs(res_value) do
                            table.insert(result_list, elem)
                        end
                    else
                        table.insert(result_list, res_value)
                    end
                end

                -- Now insert the next element in the recurse list if this is
                -- required.
                if is_under_maximum_depth then
                    if rec_unpack then
                        for _, elem in ipairs(rec_value) do
                            table.insert(
                                recurse_list,
                                {{ value=elem, depth=next_depth }}
                            )
                        end
                    else
                        table.insert(
                            recurse_list,
                            {{ value=rec_value, depth=next_depth }}
                        )
                    end
                end

            -- In the other case, check that value is nullish, otherwise raise
            -- an error.
            elseif not (
                next['{TYPE_TAGS_FIELD}'][{UNIT_TYPE_TAG}] or
                (next['@entity'] and next['@entity'].node == nil)
            ) then
                _G['{G_LKQL_ERROR}'](
                    '{WRONG_TYPE_ID}',
                    {{
                        'RecExpr',
                        next['{TYPE_NAME_FIELD}']
                    }}
                )
            end
        end

        -- If we get here, there is no result
        return nil
    end",
    UNIT_TYPE_TAG = unit::TYPE.tag,
    WRONG_TYPE_ID = WRONG_TYPE.id,
)));
