//! # Errors module
//!
//! This module is a storing place for all error message related information.
//! It is used to centralize and provide a unique endpoint for error object
//! creation.

// ----- Error templates -----

// --- Symbol errors

/// Error when an accessed symbol doesn't exist.
pub const UNKNOWN_SYMBOL: ErrorTemplate = ErrorTemplate {
    id: 0,
    title: "Unknown symbol",
    message_template: "The symbol \"{}\" cannot be resolved in the current scope",
};

/// Error when trying to add an already existing symbol.
pub const DUPLICATED_SYMBOL: ErrorTemplate = ErrorTemplate {
    id: UNKNOWN_SYMBOL.id + 1,
    title: "Symbol declared multiple times",
    message_template: "The symbol \"{}\" already exists in the current scope",
};

// --- Parameter and argument errors

/// Error when a parameter has no value.
pub const NO_VALUE_FOR_PARAM: ErrorTemplate = ErrorTemplate {
    id: DUPLICATED_SYMBOL.id + 1,
    title: "No value for parameter",
    message_template: "Missing value for the \"{}\" parameter",
};

/// Error when a positional and a named value are provided to a parameter.
pub const POS_AND_NAMED_VALUE_FOR_PARAM: ErrorTemplate = ErrorTemplate {
    id: NO_VALUE_FOR_PARAM.id + 1,
    title: "Both positional and named value",
    message_template: "The parameter \"{}\" has a positional and a named value",
};

/// Error when a positional argument is specified after a named one.
pub const POS_AFTER_NAMED_ARGUMENT: ErrorTemplate = ErrorTemplate {
    id: POS_AND_NAMED_VALUE_FOR_PARAM.id + 1,
    title: "Positional argument after a named one",
    message_template: "This positional argument is after a named one",
};

// --- Indexing and member errors

/// Error when an index is out of the indexable value bounds.
pub const INDEX_OUT_OF_BOUNDS: ErrorTemplate = ErrorTemplate {
    id: POS_AFTER_NAMED_ARGUMENT.id + 1,
    title: "Index out of bounds",
    message_template: "Cannot access index {} in the value, it is out of bounds",
};

/// Error when a unknown member is accessed.
pub const UNKNOWN_MEMBER: ErrorTemplate = ErrorTemplate {
    id: INDEX_OUT_OF_BOUNDS.id + 1,
    title: "Unknown member",
    message_template: "No member named \"{}\" on the value",
};

// --- The error repository

/// This vector contains all error templates stored at the index of their
/// identifier.
pub const ERROR_TEMPLATE_REPOSITORY: [&'static ErrorTemplate; 7] = [
    &UNKNOWN_SYMBOL,
    &DUPLICATED_SYMBOL,
    &NO_VALUE_FOR_PARAM,
    &POS_AND_NAMED_VALUE_FOR_PARAM,
    &POS_AFTER_NAMED_ARGUMENT,
    &INDEX_OUT_OF_BOUNDS,
    &UNKNOWN_MEMBER,
];

// --- Hint message

/// Hint for [`DUPLICATED_SYMBOL`].
pub const PREVIOUS_SYMBOL_HINT: &'static str = "Previously declared here";

/// Hint for [`POS_AND_NAMED_VALUE_FOR_PARAM`].
pub const PREVIOUS_NAMED_ARG_HINT: &'static str = "Previous named argument is here";

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
    pub fn render_message(&self, args: &Vec<&str>) -> String {
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
            res.push_str(arg);
            res.push_str(next_part);
        });

        // Finally return the result
        res
    }
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
        assert_eq!(&NO_ARGS.render_message(&vec![]), "No arguments");
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
        ONE_ARGS.render_message(&vec![]);
    }

    #[test]
    #[should_panic(expected = "Invalid argument count: expected 3, got 2")]
    fn test_too_few_args_for_three_args() {
        THREE_ARGS.render_message(&vec!["invalid", "arg"]);
    }
}
