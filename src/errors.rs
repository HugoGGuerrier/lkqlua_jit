//! # Errors module
//!
//! This module is a storing place for all error message related information.
//! It is used to centralize and provide a unique endpoint for error object
//! creation.

use serde::{Deserialize, Serialize};

// ----- Error templates -----

// --- Symbol errors

/// Error when an accessed symbol doesn't exist.
pub const UNKNOWN_SYMBOL: ErrorTemplate = ErrorTemplate {
    id: 0,
    title: "Unknown symbol",
    message_template: "The symbol \"{}\" cannot be resolved in the current scope",
};

pub const UNINITIALIZED_SYMBOL: ErrorTemplate = ErrorTemplate {
    id: UNKNOWN_SYMBOL.id + 1,
    title: "Closed value not initialized",
    message_template: "The closed value \"{}\" is not initialized at this stage",
};

/// Error when trying to add an already existing symbol.
pub const DUPLICATED_SYMBOL: ErrorTemplate = ErrorTemplate {
    id: UNINITIALIZED_SYMBOL.id + 1,
    title: "Symbol declared multiple times",
    message_template: "The symbol \"{}\" already exists in the current scope",
};

/// Error when a key is present multiple times in an object literal.
pub const DUPLICATED_KEY: ErrorTemplate = ErrorTemplate {
    id: DUPLICATED_SYMBOL.id + 1,
    title: "Duplicated key",
    message_template: "The key \"{}\" is present multiple times in the object",
};

// --- Parameter and argument errors

/// Error when a parameter has no value.
pub const NO_VALUE_FOR_PARAM: ErrorTemplate = ErrorTemplate {
    id: DUPLICATED_KEY.id + 1,
    title: "No value for parameter",
    message_template: "Missing value for the \"{}\" parameter",
};

/// Error when a positional and a named value are provided to a parameter.
pub const POS_AND_NAMED_VALUE_FOR_PARAM: ErrorTemplate = ErrorTemplate {
    id: NO_VALUE_FOR_PARAM.id + 1,
    title: "Both positional and named value",
    message_template: "The parameter \"{}\" has been provided a positional and a named value",
};

/// Error when a positional argument is specified after a named one.
pub const POS_AFTER_NAMED_ARGUMENT: ErrorTemplate = ErrorTemplate {
    id: POS_AND_NAMED_VALUE_FOR_PARAM.id + 1,
    title: "Positional argument after a named one",
    message_template: "This positional argument is after a named one",
};

/// Error when an parameter value is not valid
pub const INVALID_PARAM_VALUE: ErrorTemplate = ErrorTemplate {
    id: POS_AFTER_NAMED_ARGUMENT.id + 1,
    title: "Invalid argument value",
    message_template: "Invalid value for the \"{}\" parameter, {}",
};

/// Error when a division by zero occurs.
pub const DIV_BY_ZERO: ErrorTemplate = ErrorTemplate {
    id: INVALID_PARAM_VALUE.id + 1,
    title: "Division by zero",
    message_template: "Cannot perform a division by zero",
};

// --- Indexing and member errors

/// Error when an index is out of the indexable value bounds.
pub const INDEX_OUT_OF_BOUNDS: ErrorTemplate = ErrorTemplate {
    id: DIV_BY_ZERO.id + 1,
    title: "Index out of bounds",
    message_template: "Cannot access index {} in the value, it is out of bounds",
};

/// Error when a unknown member is accessed.
pub const UNKNOWN_MEMBER: ErrorTemplate = ErrorTemplate {
    id: INDEX_OUT_OF_BOUNDS.id + 1,
    title: "Unknown member",
    message_template: "No member named \"{}\" on the value",
};

/// Error when trying to get a member of a "null" value.
pub const NULL_DOT_RECEIVER: ErrorTemplate = ErrorTemplate {
    id: UNKNOWN_MEMBER.id + 1,
    title: "Null receiver",
    message_template: "Trying to access the member of a null value",
};

// --- Type checking errors

/// Error when an expression if not of the valid type.
pub const WRONG_TYPE: ErrorTemplate = ErrorTemplate {
    id: NULL_DOT_RECEIVER.id + 1,
    title: "Invalid expression type",
    message_template: "Expecting a \"{}\", got a \"{}\"",
};

/// Error when an argument is not of a valid type.
pub const WRONG_PARAM_TYPE: ErrorTemplate = ErrorTemplate {
    id: WRONG_TYPE.id + 1,
    title: "Invalid argument type",
    message_template: "Expecting a \"{}\" for the parameter \"{}\", got a \"{}\"",
};

/// Error when a trait is missing from a type.
pub const MISSING_TRAIT: ErrorTemplate = ErrorTemplate {
    id: WRONG_PARAM_TYPE.id + 1,
    title: "Missing required trait",
    message_template: "Trait \"{}\" is required, the type \"{}\" isn't implementing it",
};

/// Error when encountering an unknown node type name.
pub const UNKNOWN_NODE_TYPE: ErrorTemplate = ErrorTemplate {
    id: MISSING_TRAIT.id + 1,
    title: "Unknown node type",
    message_template: "Cannot find the \"{}\" node type",
};

/// Error when an element of a block expression body doesn't return the unit
/// value.
pub const NOT_UNIT_BLOCK_ELEM: ErrorTemplate = ErrorTemplate {
    id: UNKNOWN_NODE_TYPE.id + 1,
    title: "Ignored expression",
    message_template: "Cannot ignore a non-unit expression in a block expression body",
};

// --- Module related errors

/// Error when a module file cannot be located.
pub const MODULE_NOT_FOUND: ErrorTemplate = ErrorTemplate {
    id: NOT_UNIT_BLOCK_ELEM.id + 1,
    title: "Module file not found",
    message_template: "Impossible to locate the file corresponding to the module \"{}\"",
};

/// Error when a module is found in multiple files.
pub const AMBIGUOUS_IMPORT: ErrorTemplate = ErrorTemplate {
    id: MODULE_NOT_FOUND.id + 1,
    title: "Ambiguous module importation",
    message_template: "This module can be imported from multiple files ({})",
};

/// Error when there is a module dependency cycle.
pub const DEPENDENCY_CYCLE: ErrorTemplate = ErrorTemplate {
    id: AMBIGUOUS_IMPORT.id + 1,
    title: "Dependency cycle",
    message_template: "There is a cycle in modules dependency chain ({})",
};

// Error when an error occurs in during the importation of a module.
pub const ERROR_DURING_IMPORTATION: ErrorTemplate = ErrorTemplate {
    id: DEPENDENCY_CYCLE.id + 1,
    title: "Error when importing a module",
    message_template: "An error occurred during the module importation, please check the related file",
};

// --- Regular expression errors

/// Error when there is a syntax error in a regular expression.
pub const REGEX_SYNTAX_ERROR: ErrorTemplate = ErrorTemplate {
    id: ERROR_DURING_IMPORTATION.id + 1,
    title: "Invalid regular expression syntax",
    message_template: "Syntax error in the \"{}\" regular expression",
};

/// Error when a regular expression is too big to be compiled.
pub const REGEX_TOO_BIG: ErrorTemplate = ErrorTemplate {
    id: REGEX_SYNTAX_ERROR.id + 1,
    title: "Regular expression is too big",
    message_template: "Cannot compile the \"{}\" regular expression, consider splitting it",
};

// --- Pattern related errors

pub const SUBPATTERN_AFTER_SPLAT: ErrorTemplate = ErrorTemplate {
    id: REGEX_TOO_BIG.id + 1,
    title: "Sub-pattern after a splat one",
    message_template: "This sub-pattern is after a splat one",
};

/// Error when a selector call in a pattern detail is not correctly formed.
pub const INVALID_SELECTOR_CALL: ErrorTemplate = ErrorTemplate {
    id: SUBPATTERN_AFTER_SPLAT.id + 1,
    title: "Invalid selector call",
    message_template: "It must be either a name or a call",
};

// --- Misc errors

/// Error coming from the Lua engine and that cannot be mapped to a more
/// precise error.
pub const LUA_ENGINE_ERROR: ErrorTemplate = ErrorTemplate {
    id: INVALID_SELECTOR_CALL.id + 1,
    title: "Error from the Lua engine",
    message_template: "{}",
};

/// Error coming from the execution of the analysis library.
pub const ANALYSIS_LIBRARY_ERROR: ErrorTemplate = ErrorTemplate {
    id: LUA_ENGINE_ERROR.id + 1,
    title: "Error in the analysis library",
    message_template: "{}",
};

// --- The error repository

/// This vector contains all error templates stored at the index of their
/// identifier.
pub const ERROR_TEMPLATE_REPOSITORY: &[&ErrorTemplate] = &[
    &UNKNOWN_SYMBOL,
    &UNINITIALIZED_SYMBOL,
    &DUPLICATED_SYMBOL,
    &DUPLICATED_KEY,
    &NO_VALUE_FOR_PARAM,
    &POS_AND_NAMED_VALUE_FOR_PARAM,
    &POS_AFTER_NAMED_ARGUMENT,
    &INVALID_PARAM_VALUE,
    &DIV_BY_ZERO,
    &INDEX_OUT_OF_BOUNDS,
    &UNKNOWN_MEMBER,
    &NULL_DOT_RECEIVER,
    &WRONG_TYPE,
    &WRONG_PARAM_TYPE,
    &MISSING_TRAIT,
    &UNKNOWN_NODE_TYPE,
    &NOT_UNIT_BLOCK_ELEM,
    &MODULE_NOT_FOUND,
    &AMBIGUOUS_IMPORT,
    &DEPENDENCY_CYCLE,
    &ERROR_DURING_IMPORTATION,
    &REGEX_SYNTAX_ERROR,
    &REGEX_TOO_BIG,
    &SUBPATTERN_AFTER_SPLAT,
    &INVALID_SELECTOR_CALL,
    &LUA_ENGINE_ERROR,
    &ANALYSIS_LIBRARY_ERROR,
];

// --- Hint message

/// Hint for [`DUPLICATED_SYMBOL`].
pub const PREVIOUS_SYMBOL_HINT: &str = "Previously declared here";

/// Hint for [`POS_AND_NAMED_VALUE_FOR_PARAM`].
pub const PREVIOUS_NAMED_ARG_HINT: &str = "Previous named argument is here";

// ----- Support types -----

/// This type represents a template for an error message.
pub struct ErrorTemplate {
    pub id: usize,
    pub title: &'static str,
    message_template: &'static str,
}

impl ErrorTemplate {
    /// Render the message template with the provided arguments. This function
    /// panics if the provided argument count is not the same as the expected
    /// one.
    pub fn render_message<T>(&self, args: &Vec<T>) -> String
    where
        T: AsRef<str>,
    {
        // Create working variables and result
        let mut split_template = self.message_template.split("{}");
        let mut res = String::from(split_template.next().unwrap());
        let message_parts = split_template.collect::<Vec<_>>();

        // Ensure there is the correct number of provided arguments
        if message_parts.len() != args.len() {
            panic!(
                "Invalid argument count: expected {}, got {}",
                message_parts.len(),
                args.len()
            );
        }

        // Fill the result string
        args.iter().zip(message_parts).for_each(|(arg, next_part)| {
            res.push_str(arg.as_ref());
            res.push_str(next_part);
        });

        // Finally return the result
        res
    }
}

/// This type represents an instantiation an [`ErrorTemplate`], it carries all
/// arguments for its associated template that can be runtime values.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ErrorInstance {
    pub template_id: usize,
    pub message_args: Vec<ErrorInstanceArg>,
}

impl ErrorInstance {
    /// Get an error instance from a serialized JSON string.
    pub fn from_json(json: &str) -> Option<Self> {
        serde_json::from_str::<Self>(json).ok()
    }

    /// Serialize this error instance as JSON.
    pub fn to_json_string(&self) -> String {
        serde_json::to_string(self).unwrap()
    }
}

/// This type represents an argument for an error instance.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorInstanceArg {
    /// The argument is known at compile time.
    Static(String),

    /// The argument value is in a frame slot at execution time. This slot
    /// should be 0-indexed.
    LocalValue(u8),
}

// ----- Testing -----

mod test {
    #[allow(unused)]
    use super::*;

    #[allow(unused)]
    const NO_ARGS: ErrorTemplate =
        ErrorTemplate { id: 0, title: "Dummy title", message_template: "No arguments" };

    #[allow(unused)]
    const ONE_ARGS: ErrorTemplate =
        ErrorTemplate { id: 0, title: "Dummy title", message_template: "Argument is \"{}\"" };

    #[allow(unused)]
    const THREE_ARGS: ErrorTemplate = ErrorTemplate {
        id: 0,
        title: "Dummy title",
        message_template: "Arguments: 1={}, 2={}, 3={}",
    };

    #[test]
    fn test_valid_template_rendering() {
        // Now ensure valid rendering
        assert_eq!(&NO_ARGS.render_message::<&str>(&vec![]), "No arguments");
        assert_eq!(&ONE_ARGS.render_message(&vec!["a"]), "Argument is \"a\"");
        assert_eq!(
            &THREE_ARGS.render_message(&vec!["a", "b", "c"]),
            "Arguments: 1=a, 2=b, 3=c"
        );
        assert_eq!(
            &THREE_ARGS.render_message(&vec!["a", "c", "b"]),
            "Arguments: 1=a, 2=c, 3=b"
        );
    }

    #[test]
    #[should_panic(expected = "Invalid argument count: expected 0, got 2")]
    fn test_too_many_args_for_no_args() {
        NO_ARGS.render_message(&vec!["invalid", "arg"]);
    }

    #[test]
    #[should_panic(expected = "Invalid argument count: expected 3, got 4")]
    fn test_too_many_args_for_three_args() {
        THREE_ARGS.render_message(&vec!["invalid", "arg", "too", "many"]);
    }

    #[test]
    #[should_panic(expected = "Invalid argument count: expected 1, got 0")]
    fn test_too_few_args_for_one_args() {
        ONE_ARGS.render_message::<&str>(&vec![]);
    }

    #[test]
    #[should_panic(expected = "Invalid argument count: expected 3, got 2")]
    fn test_too_few_args_for_three_args() {
        THREE_ARGS.render_message(&vec!["invalid", "arg"]);
    }
}
