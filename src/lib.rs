//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

pub mod bytecode;
pub mod intermediate_tree;
pub mod sources;

use std::io::Write;

use ariadne::{self, Label, StdoutFmt};
use liblkqllang::{AnalysisUnit, Diagnostic};
use sources::SourceSection;

use crate::sources::{Location, SourceRepository};

const INFO_KIND_COLOR: ariadne::Color = ariadne::Color::BrightCyan;
const WARNING_KIND_COLOR: ariadne::Color = ariadne::Color::BrightYellow;
const ERROR_KIND_COLOR: ariadne::Color = ariadne::Color::BrightRed;
const BUG_KIND_COLOR: ariadne::Color = ariadne::Color::Red;
const HINT_COLOR: ariadne::Color = ariadne::Color::Fixed(69);
// const HINT_MESSAGE_COLOR: ariadne::Color = ariadne::Color::Fixed(249);

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

    /// Create a new error report with a located message and additional hints.
    pub fn error_diag_and_hints(
        message: String,
        location: SourceSection,
        hints: Vec<Hint>,
    ) -> Self {
        Self::Single {
            kind: ReportKind::Error,
            variant: ReportVariant::Diagnostic { location, message, hints },
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

    // --- Lowering diagnostics

    /// Report a positional argument after a named one in a function call.
    pub fn pos_arg_after_named(pos_arg: SourceSection, named_arg: SourceSection) -> Self {
        Self::error_diag_and_hints(
            String::from("Positional argument after a named one"),
            pos_arg,
            vec![Hint {
                location: named_arg,
                message: String::from("Previous named argument is here"),
            }],
        )
    }

    // --- Creation methods

    /// Create a new report from an LKQL parsing diagnostic.
    pub fn from_lkql_diagnostic(
        unit: &AnalysisUnit,
        diagnostic: &Diagnostic,
    ) -> Result<Self, Report> {
        Ok(Self::Single {
            kind: ReportKind::Error,
            variant: ReportVariant::Diagnostic {
                location: SourceSection {
                    source: unit.filename()?,
                    start: Location::from_lkql_location(diagnostic.sloc_range.start),
                    end: Location::from_lkql_location(diagnostic.sloc_range.end),
                },
                message: diagnostic.message.clone(),
                hints: None,
            },
        })
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
                ReportVariant::Diagnostic { location, message, hints } => {
                    // Create a new report builder
                    let rep_builder = ariadne::Report::build(
                        kind.to_ariadne_kind(),
                        location.to_span(source_repo).unwrap(),
                    )
                    // Add the error message
                    .with_message(message)
                    // Add the main label with the diagnostic color
                    .with_label(
                        Label::new(location.to_span(source_repo).unwrap())
                            .with_message(message)
                            .with_color(kind.color())
                            .with_priority(10)
                            .with_order(0),
                    )
                    // Add all hints
                    .with_labels(hints.iter().map(|h| {
                        Label::new(h.location.to_span(source_repo).unwrap())
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
