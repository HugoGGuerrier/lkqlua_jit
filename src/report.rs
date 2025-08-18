//! # Report module
//!
//! This module contains all required components to create, emit and display
//! reports.

use std::io::Write;

use crate::sources::SourceSection;
use ariadne::{self, Label, StdoutFmt};
use liblkqllang::{AnalysisUnit, Diagnostic};

use crate::sources::{Location, SourceRepository};

const INFO_KIND_COLOR: ariadne::Color = ariadne::Color::BrightCyan;
const WARNING_KIND_COLOR: ariadne::Color = ariadne::Color::BrightYellow;
const ERROR_KIND_COLOR: ariadne::Color = ariadne::Color::BrightRed;
const BUG_KIND_COLOR: ariadne::Color = ariadne::Color::Red;
const HINT_COLOR: ariadne::Color = ariadne::Color::Fixed(69);

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
        Self::single_msg(ReportKind::Info, message)
    }

    /// Create a new information report with a located message.
    pub fn info_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(ReportKind::Info, location, title, message, vec![])
    }

    // --- Warning reports

    /// Create a new warning report with a message.
    pub fn warning_msg(message: String) -> Self {
        Self::single_msg(ReportKind::Warning, message)
    }

    /// Create a new warning report with a located message.
    pub fn warning_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(ReportKind::Warning, location, title, message, vec![])
    }

    // --- Error reports

    /// Create a new error report with a message.
    pub fn error_msg(message: String) -> Self {
        Self::single_msg(ReportKind::Error, message)
    }

    /// Create a new error report with a located message.
    pub fn error_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(ReportKind::Error, location, title, message, vec![])
    }

    /// Create a new error report with a located message and additional hints.
    pub fn error_diag_and_hints(
        title: String,
        message: String,
        location: SourceSection,
        hints: Vec<Hint>,
    ) -> Self {
        Self::single_diag(ReportKind::Error, location, title, message, hints)
    }

    // --- Bug reports

    /// Create a new bug report with a message.
    pub fn bug_msg(message: String) -> Self {
        Self::single_msg(ReportKind::Bug, message)
    }

    /// Create a new bug report with a located message.
    pub fn bug_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(ReportKind::Bug, location, title, message, vec![])
    }

    // --- Lowering diagnostics

    /// Report a positional argument after a named one in a function call.
    pub fn pos_arg_after_named(pos_arg: SourceSection, named_arg: SourceSection) -> Self {
        Self::error_diag_and_hints(
            String::from("Positional argument after a named one"),
            String::from("This positional argument is after a named one"),
            pos_arg,
            vec![Hint {
                location: named_arg,
                message: String::from("Previous named argument is here"),
            }],
        )
    }

    // --- Compilation diagnostics

    /// Report a duplicated symbol declaration.
    pub fn duplicated_symbols(
        first_decl: SourceSection,
        clashing_decl: SourceSection,
        duplicated_symbol: &str,
    ) -> Self {
        Self::error_diag_and_hints(
            String::from("Symbol declared multiple times"),
            format!("The symbol \"{duplicated_symbol}\" already exists in the current scope"),
            clashing_decl,
            vec![Hint { location: first_decl, message: String::from("Previously declared here") }],
        )
    }

    /// Report an access to an non-existing symbol.
    pub fn unknown_symbol(access_location: SourceSection, accessed_symbol: &str) -> Self {
        Self::error_diag(
            String::from("Unknown symbol"),
            format!("The symbol \"{accessed_symbol}\" cannot be resolved in the current scope"),
            access_location,
        )
    }

    // --- Creation helpers

    /// Create a new report from an LKQL parsing diagnostic.
    pub fn from_lkql_diagnostic(
        unit: &AnalysisUnit,
        diagnostic: &Diagnostic,
    ) -> Result<Self, Report> {
        Ok(Self::single_diag(
            ReportKind::Error,
            SourceSection {
                source: unit.filename()?,
                start: Location::from_lkql_location(diagnostic.sloc_range.start),
                end: Location::from_lkql_location(diagnostic.sloc_range.end),
            },
            String::from("Parsing error"),
            diagnostic.message.clone(),
            vec![],
        ))
    }

    /// Shortcut function to create a single message report.
    fn single_msg(kind: ReportKind, message: String) -> Self {
        Self::Single { kind, variant: ReportVariant::Message(message) }
    }

    /// Shortcut function to create a single diagnostic report.
    fn single_diag(
        kind: ReportKind,
        location: SourceSection,
        title: String,
        message: String,
        hints: Vec<Hint>,
    ) -> Self {
        Self::Single {
            kind: kind,
            variant: ReportVariant::Diagnostic { location, title, message, hints },
        }
    }

    // --- Other methods

    /// Combine two report in a [`Report::Composed`] one.
    pub fn combine(self, other: Report) -> Self {
        match (self, other) {
            (
                Report::Single { kind: self_kind, variant: self_variant },
                Report::Single { kind: other_kind, variant: other_variant },
            ) => Self::Composed(vec![
                Self::Single { kind: self_kind, variant: self_variant },
                Self::Single { kind: other_kind, variant: other_variant },
            ]),
            (Report::Composed(mut reports), Report::Single { kind, variant })
            | (Report::Single { kind, variant }, Report::Composed(mut reports)) => {
                reports.push(Self::Single { kind, variant });
                Self::Composed(reports)
            }
            (Report::Composed(mut self_reports), Report::Composed(mut other_reports)) => {
                self_reports.append(&mut other_reports);
                Self::Composed(self_reports)
            }
        }
    }

    /// Format this report and output the result in provided writable object.
    /// If the result is going to be printed on `stdout`, please set the
    /// related parameter accordingly.
    pub fn print<W: Write>(
        &self,
        source_repo: &SourceRepository,
        output: &mut W,
        for_stdout: bool,
    ) {
        match self {
            Report::Composed(reports) => reports
                .iter()
                .for_each(|r| r.print(source_repo, output, for_stdout)),
            Report::Single { kind, variant } => match variant {
                ReportVariant::Message(msg) => {
                    let line_prefix = format!("{}:", kind.label()).fg(kind.color());
                    output
                        .write(format!("{line_prefix} {msg}").as_bytes())
                        .expect(format!("Error while printing report \"{msg}\"").as_str());
                }
                ReportVariant::Diagnostic { location, title, message, hints } => {
                    // Create a new report builder
                    let rep_builder = ariadne::Report::build(
                        kind.to_ariadne_kind(),
                        location.to_span(source_repo),
                    )
                    // Add the error message
                    .with_message(title)
                    // Add the main label with the diagnostic color
                    .with_label(
                        Label::new(location.to_span(source_repo))
                            .with_message(message)
                            .with_color(kind.color())
                            .with_priority(10)
                            .with_order(0),
                    )
                    // Add all hints
                    .with_labels(hints.iter().map(|h| {
                        Label::new(h.location.to_span(source_repo))
                            .with_message(
                                format!("{} {}", "Hint:".fg(HINT_COLOR), &h.message).as_str(),
                            )
                            .with_color(HINT_COLOR)
                            .with_priority(1)
                            .with_order(1)
                    }));

                    // Then print the report
                    if for_stdout {
                        rep_builder.finish().write_for_stdout(source_repo, output)
                    } else {
                        rep_builder.finish().write(source_repo, output)
                    }
                    .expect(format!("Error while printing report {:?}", self).as_str())
                }
            },
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

impl ReportKind {
    fn to_ariadne_kind(&self) -> ariadne::ReportKind {
        ariadne::ReportKind::Custom(self.label(), self.color())
    }

    fn color(&self) -> ariadne::Color {
        match self {
            ReportKind::Info => INFO_KIND_COLOR,
            ReportKind::Warning => WARNING_KIND_COLOR,
            ReportKind::Error => ERROR_KIND_COLOR,
            ReportKind::Bug => BUG_KIND_COLOR,
        }
    }

    fn label(&self) -> &str {
        match self {
            ReportKind::Info => "Info",
            ReportKind::Warning => "Warning",
            ReportKind::Error => "Error",
            ReportKind::Bug => "Bug",
        }
    }
}

#[derive(Debug, Clone)]
pub enum ReportVariant {
    Message(String),
    Diagnostic { location: SourceSection, title: String, message: String, hints: Vec<Hint> },
}

/// This structure represents an hint in a diagnostic. A hint is a located
/// additional piece of information that brings more context or advices about
/// a diagnostic.
#[derive(Debug, Clone)]
pub struct Hint {
    message: String,
    location: SourceSection,
}
