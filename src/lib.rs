//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

pub mod bytecode;
pub mod intermediate_tree;
pub mod sources;

use sources::SourceSection;

/// This structure represents a diagnostic emitted during the source code
/// compilation or execution. It contains all required data to print a useful
/// and pretty report to the user.
pub struct Diagnostic {
    message: String,
    location: SourceSection,
    hints: Vec<Hint>,
}

/// This structure represents an hint in a diagnostic. A hint is a located
/// additional piece of information that brings more context or advices about
/// a diagnostic.
pub struct Hint {
    message: String,
    location: SourceSection,
}

/// This enumeration represents any error that may happen in the LKQL engine.
/// We use a unified representation to factorize and centralize handling of
/// error across all the engine.
#[derive(Debug, Clone)]
pub enum Error {
    /// Case when an error just have a simple message.
    Messaged(String),

    /// Case when an error may be located in a source.
    Located { location: SourceSection, message: String },
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Self::Messaged(format!("{value}"))
    }
}

impl From<liblkqllang::Exception> for Error {
    fn from(value: liblkqllang::Exception) -> Self {
        Self::Messaged(value.information)
    }
}
