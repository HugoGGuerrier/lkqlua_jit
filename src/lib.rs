//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

pub mod bytecode;
pub mod intermediate_tree;
pub mod sources;

use sources::SourceSection;

/// This type is the top-level of all report that can be emitted by the engine.
/// This type is designed to be used in [`Result::Err`] values, and can be
/// easily created from existing error types.
#[derive(Debug, Clone)]
pub enum Report {
    Single { kind: ReportKind, variant: ReportVariant },
    Composed(Vec<Report>),
}

impl From<std::io::Error> for Report {
    fn from(value: std::io::Error) -> Self {
        Self::error_msg(format!("{value}"))
    }
}

impl From<liblkqllang::Exception> for Report {
    fn from(value: liblkqllang::Exception) -> Self {
        Self::error_msg(value.information)
    }
}

impl Report {
    // --- Information reports

    /// Create a new information report with a message.
    pub fn info_msg(message: String) -> Self {
        Self::Single { kind: ReportKind::Info, variant: ReportVariant::Message(message) }
    }

    /// Create a new information report with a located message.
    pub fn info_diag(message: String, location: SourceSection) -> Self {
        Self::Single {
            kind: ReportKind::Info,
            variant: ReportVariant::Diagnostic { location, message, hints: vec![] },
        }
    }

    // --- Warning reports

    /// Create a new warning report with a message.
    pub fn warning_msg(message: String) -> Self {
        Self::Single { kind: ReportKind::Warning, variant: ReportVariant::Message(message) }
    }

    /// Create a new warning report with a located message.
    pub fn warning_diag(message: String, location: SourceSection) -> Self {
        Self::Single {
            kind: ReportKind::Warning,
            variant: ReportVariant::Diagnostic { location, message, hints: vec![] },
        }
    }

    // --- Error reports

    /// Create a new error report with a message.
    pub fn error_msg(message: String) -> Self {
        Self::Single { kind: ReportKind::Error, variant: ReportVariant::Message(message) }
    }

    /// Create a new error report with a located message.
    pub fn error_diag(message: String, location: SourceSection) -> Self {
        Self::Single {
            kind: ReportKind::Error,
            variant: ReportVariant::Diagnostic { location, message, hints: vec![] },
        }
    }

    // --- Bug reports

    /// Create a new bug report with a message.
    pub fn bug_msg(message: String) -> Self {
        Self::Single { kind: ReportKind::Bug, variant: ReportVariant::Message(message) }
    }

    /// Create a new bug report with a located message.
    pub fn bug_diag(message: String, location: SourceSection) -> Self {
        Self::Single {
            kind: ReportKind::Bug,
            variant: ReportVariant::Diagnostic { location, message, hints: vec![] },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ReportKind {
    Info,
    Warning,
    Error,
    Bug,
}

#[derive(Debug, Clone)]
pub enum ReportVariant {
    Message(String),
    Diagnostic { location: SourceSection, message: String, hints: Vec<Hint> },
}

/// This structure represents an hint in a diagnostic. A hint is a located
/// additional piece of information that brings more context or advices about
/// a diagnostic.
#[derive(Debug, Clone)]
pub struct Hint {
    message: String,
    location: SourceSection,
}
