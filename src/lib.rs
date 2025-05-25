//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

mod bytecode;
mod sources;

use sources::SourceSection;

/// This enumeration represents any error that may happen in the LKQL engine.
/// We use a unified representation to factorize and centralize handling of
/// error across all the engine.
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
