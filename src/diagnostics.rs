//! # Diagnostics module
//!
//! This module contains all required components to create, emit and display
//! diagnostics.

use crate::sources::{Location, SourceRepository};
use crate::{
    errors::ErrorTemplate,
    sources::{SourceId, SourceSection},
};
use ariadne::{Color, Label, Report, ReportKind, StdoutFmt};
use liblkqllang;
use std::io::Write;

pub const INFO_KIND_COLOR: Color = Color::BrightCyan;
pub const WARNING_KIND_COLOR: Color = Color::BrightYellow;
pub const ERROR_KIND_COLOR: Color = Color::BrightRed;
pub const BUG_KIND_COLOR: Color = Color::Red;
pub const HINT_COLOR: Color = Color::Fixed(69);
pub const ADVICE_COLOR: Color = Color::Fixed(147);

/// This type is the top-level of all diagnostics that can be emitted by the
/// engine.
/// This type is designed to be used in [`Result::Err`] values, and can be
/// easily created from existing error types.
#[derive(Debug, Clone)]
pub enum Diagnostic {
    Single { kind: DiagnosticKind, variant: DiagnosticVariant },
    Composed(Vec<Diagnostic>),
}

impl From<String> for Diagnostic {
    fn from(value: String) -> Self {
        Self::error_msg(value)
    }
}

impl From<std::io::Error> for Diagnostic {
    fn from(value: std::io::Error) -> Self {
        Self::error_msg(format!("{value}"))
    }
}

impl From<liblkqllang::Exception> for Diagnostic {
    fn from(value: liblkqllang::Exception) -> Self {
        Self::error_msg(value.information)
    }
}

impl Diagnostic {
    // --- Information diagnostics

    /// Create a new information diagnostic with a message.
    pub fn info_msg(message: String) -> Self {
        Self::single_msg(DiagnosticKind::Info, message)
    }

    /// Create a new information diagnostic with a located message.
    pub fn info_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(DiagnosticKind::Info, location, title, message, vec![], vec![])
    }

    // --- Warning diagnostics

    /// Create a new warning diagnostic with a message.
    pub fn warning_msg(message: String) -> Self {
        Self::single_msg(DiagnosticKind::Warning, message)
    }

    /// Create a new warning diagnostic with a located message.
    pub fn warning_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(DiagnosticKind::Warning, location, title, message, vec![], vec![])
    }

    // --- Error diagnostics

    /// Create a new error diagnostic with a message.
    pub fn error_msg(message: String) -> Self {
        Self::single_msg(DiagnosticKind::Error, message)
    }

    /// Create a new error diagnostic with a located message.
    pub fn error_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(DiagnosticKind::Error, location, title, message, vec![], vec![])
    }

    /// Create a new error diagnostic from an error template with message
    /// arguments.
    pub fn from_error_template<T>(
        location: &SourceSection,
        error_template: &ErrorTemplate,
        message_args: &Vec<T>,
    ) -> Self
    where
        T: AsRef<str>,
    {
        Self::single_diag(
            DiagnosticKind::Error,
            location.clone(),
            String::from(error_template.title),
            error_template.render_message(message_args),
            vec![],
            vec![],
        )
    }

    /// Create a new error diagnostic from an error template with message
    /// arguments with hints.
    pub fn from_error_template_with_hints<T>(
        location: &SourceSection,
        error_template: &ErrorTemplate,
        message_args: &Vec<T>,
        hints: Vec<Hint>,
    ) -> Self
    where
        T: AsRef<str>,
    {
        Self::single_diag(
            DiagnosticKind::Error,
            location.clone(),
            String::from(error_template.title),
            error_template.render_message(message_args),
            hints,
            vec![],
        )
    }

    /// Create a new error diagnostic from an error template with arguments and
    /// a stack trace.
    pub fn from_error_template_with_stack_trace<T>(
        location: &SourceSection,
        error_template: &ErrorTemplate,
        message_args: &Vec<T>,
        stack_trace: Vec<CallLocation>,
    ) -> Self
    where
        T: AsRef<str>,
    {
        Self::single_diag(
            DiagnosticKind::Error,
            location.clone(),
            String::from(error_template.title),
            error_template.render_message(message_args),
            vec![],
            stack_trace,
        )
    }

    // --- Bug diagnostics

    /// Create a new bug diagnostic with a message.
    pub fn bug_msg(message: String) -> Self {
        Self::single_msg(DiagnosticKind::Bug, message)
    }

    /// Create a new bug diagnostic with a located message.
    pub fn bug_diag(title: String, message: String, location: SourceSection) -> Self {
        Self::single_diag(DiagnosticKind::Bug, location, title, message, vec![], vec![])
    }

    // --- Creation helpers

    /// Create a new diagnostic from an LKQL parsing diagnostic in the provided
    /// source.
    pub fn from_lkql_diagnostic(
        source: SourceId,
        diagnostic: &liblkqllang::Diagnostic,
    ) -> Result<Self, Diagnostic> {
        Ok(Self::single_diag(
            DiagnosticKind::Error,
            SourceSection::new(
                source,
                Location::from_lkql_location(diagnostic.sloc_range.start),
                Location::from_lkql_location(diagnostic.sloc_range.end),
            ),
            String::from("Parsing error"),
            diagnostic.message.clone(),
            vec![],
            vec![],
        ))
    }

    /// Shortcut function to create a single message diagnostic.
    fn single_msg(kind: DiagnosticKind, message: String) -> Self {
        Self::Single { kind, variant: DiagnosticVariant::Message(message) }
    }

    /// Shortcut function to create a single diagnostic diagnostic.
    fn single_diag(
        kind: DiagnosticKind,
        location: SourceSection,
        title: String,
        message: String,
        hints: Vec<Hint>,
        stack_trace: Vec<CallLocation>,
    ) -> Self {
        Self::Single {
            kind: kind,
            variant: DiagnosticVariant::Located { location, title, message, hints, stack_trace },
        }
    }

    /// Combine two diagnostic in a [`Diagnostic::Composed`] one.
    pub fn combine(self, other: Diagnostic) -> Self {
        match (self, other) {
            (l @ Diagnostic::Single { .. }, r @ Diagnostic::Single { .. }) => {
                Self::Composed(vec![l, r])
            }
            (Diagnostic::Composed(mut diags), single @ Diagnostic::Single { .. })
            | (single @ Diagnostic::Single { .. }, Diagnostic::Composed(mut diags)) => {
                diags.push(single);
                Self::Composed(diags)
            }
            (Diagnostic::Composed(mut self_diags), Diagnostic::Composed(mut other_diags)) => {
                self_diags.append(&mut other_diags);
                Self::Composed(self_diags)
            }
        }
    }

    // --- Other methods

    /// Format this diagnostic and output the result in provided writable
    /// object. If the result is going to be printed on `stdout`, please set
    /// the related parameter accordingly.
    pub fn print<W: Write>(
        &self,
        source_repo: &SourceRepository,
        output: &mut W,
        for_stdout: bool,
    ) {
        match self {
            Diagnostic::Composed(diags) => diags
                .iter()
                .for_each(|r| r.print(source_repo, output, for_stdout)),
            Diagnostic::Single { kind, variant } => match variant {
                DiagnosticVariant::Message(msg) => {
                    let line_prefix = format!("{}:", kind.label()).fg(kind.color());
                    writeln!(output, "{line_prefix} {msg}")
                        .expect(format!("Error while printing diagnostic \"{msg}\"").as_str());
                }
                DiagnosticVariant::Located { location, title, message, hints, stack_trace } => {
                    // Create a new diagnostic builder
                    let rep_builder =
                        Report::build(kind.to_ariadne_kind(), location.to_span(source_repo))
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
                                        format!("{} {}", "Hint:".fg(HINT_COLOR), &h.message)
                                            .as_str(),
                                    )
                                    .with_color(HINT_COLOR)
                                    .with_priority(1)
                                    .with_order(1)
                            }));

                    // Then print the diagnostic
                    if for_stdout {
                        rep_builder
                            .finish()
                            .write_for_stdout(source_repo, &mut *output)
                    } else {
                        rep_builder.finish().write(source_repo, &mut *output)
                    }
                    .expect(format!("Error while printing diagnostic {:?}", self).as_str());

                    // Print the error stack trace
                    for trace_element in stack_trace {
                        let trace_element_builder = Report::build(
                            ReportKind::Advice,
                            trace_element.location.to_span(source_repo),
                        )
                        .with_label(
                            Label::new(trace_element.location.to_span(source_repo))
                                .with_color(ADVICE_COLOR),
                        )
                        .with_message(format!("Called in \"{}\"", trace_element.call_context));
                        if for_stdout {
                            trace_element_builder
                                .finish()
                                .write_for_stdout(source_repo, &mut *output)
                        } else {
                            trace_element_builder
                                .finish()
                                .write(source_repo, &mut *output)
                        }
                        .expect(format!("Error while printing diagnostic {:?}", self).as_str());
                    }
                }
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticKind {
    Info,
    Warning,
    Error,
    Bug,
}

impl DiagnosticKind {
    fn to_ariadne_kind(&self) -> ReportKind<'_> {
        ReportKind::Custom(self.label(), self.color())
    }

    fn color(&self) -> Color {
        match self {
            DiagnosticKind::Info => INFO_KIND_COLOR,
            DiagnosticKind::Warning => WARNING_KIND_COLOR,
            DiagnosticKind::Error => ERROR_KIND_COLOR,
            DiagnosticKind::Bug => BUG_KIND_COLOR,
        }
    }

    fn label(&self) -> &str {
        match self {
            DiagnosticKind::Info => "Info",
            DiagnosticKind::Warning => "Warning",
            DiagnosticKind::Error => "Error",
            DiagnosticKind::Bug => "Bug",
        }
    }
}

#[derive(Debug, Clone)]
pub enum DiagnosticVariant {
    Message(String),
    Located {
        location: SourceSection,
        title: String,
        message: String,
        hints: Vec<Hint>,
        stack_trace: Vec<CallLocation>,
    },
}

/// This structure represents an hint in a diagnostic. A hint is a located
/// additional piece of information that brings more context or advices about
/// a diagnostic.
#[derive(Debug, Clone)]
pub struct Hint {
    pub message: String,
    pub location: SourceSection,
}

impl Hint {
    /// Create a new hint object.
    pub fn new(message: String, location: SourceSection) -> Self {
        Self { message, location }
    }
}

/// This structure represents the location and the context of a call in a stack
/// trace.
#[derive(Debug, Clone)]
pub struct CallLocation {
    pub call_context: String,
    pub location: SourceSection,
}

impl CallLocation {
    /// Create a new call location object.
    pub fn new(call_context: String, location: SourceSection) -> Self {
        Self { call_context, location }
    }
}
