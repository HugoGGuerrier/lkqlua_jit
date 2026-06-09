//! # Diagnostics module
//!
//! This module contains all required components to create, emit and display
//! diagnostics.

use crate::{
    errors::ErrorTemplate,
    sources::{Location, SourceId, SourceRepository, SourceSection},
};
use ariadne::{Color, Label, Report, ReportKind, StdoutFmt};
use core::slice;
use liblkqllang;
use serde::{Deserialize, Serialize};
use std::{io::Write, vec};

pub const INFO_KIND_COLOR: Color = Color::BrightCyan;
pub const WARNING_KIND_COLOR: Color = Color::BrightYellow;
pub const ERROR_KIND_COLOR: Color = Color::BrightRed;
pub const BUG_KIND_COLOR: Color = Color::Red;
pub const HINT_COLOR: Color = Color::Fixed(69);
pub const ADVICE_COLOR: Color = Color::Fixed(147);

/// This type may be used to collect diagnostics and iterate over them to
/// create a final report.
#[derive(Debug, Serialize, Deserialize)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticCollector {
    /// Create a new diagnostic collector.
    pub fn new() -> Self {
        Self { diagnostics: Vec::new() }
    }

    /// Add a diagnostic in this collector.
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Get whether this diagnostic collector hasn't any diagnostic.
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    /// Get a diagnostic collector from a serialized JSON string.
    pub fn from_json(json: &str) -> Option<Self> {
        serde_json::from_str::<Self>(json).ok()
    }

    /// Serialize this diagnostic collector as JSON.
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).unwrap()
    }
}

impl Default for DiagnosticCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Diagnostic> for DiagnosticCollector {
    fn from(value: Diagnostic) -> Self {
        Self { diagnostics: vec![value] }
    }
}

impl From<Vec<Diagnostic>> for DiagnosticCollector {
    fn from(diagnostics: Vec<Diagnostic>) -> Self {
        Self { diagnostics }
    }
}

impl<'a> IntoIterator for &'a DiagnosticCollector {
    type Item = &'a Diagnostic;
    type IntoIter = slice::Iter<'a, Diagnostic>;

    fn into_iter(self) -> Self::IntoIter {
        self.diagnostics.iter()
    }
}

/// This type represents a diagnostic that may be emitted by the tool.
/// This type is designed to be used in [`Result::Err`] values, and can be
/// easily created from existing error types.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    title: Option<String>,
    message: String,
    location: Option<SourceSection>,
    hints: Vec<Hint>,
    stack_trace: Vec<CallLocation>,
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

impl From<liblkqllang::Exception> for Box<Diagnostic> {
    fn from(value: liblkqllang::Exception) -> Self {
        Self::new(Diagnostic::from(value))
    }
}

impl Diagnostic {
    // --- Information diagnostics

    /// Create a new information diagnostic with a message.
    pub fn info_msg(message: String) -> Self {
        Self {
            kind: DiagnosticKind::Info,
            title: None,
            message,
            location: None,
            hints: vec![],
            stack_trace: vec![],
        }
    }

    /// Create a new information diagnostic with a located message.
    pub fn info_located(title: String, message: String, location: SourceSection) -> Self {
        Self {
            kind: DiagnosticKind::Info,
            title: Some(title),
            message,
            location: Some(location),
            hints: vec![],
            stack_trace: vec![],
        }
    }

    // --- Warning diagnostics

    /// Create a new warning diagnostic with a message.
    pub fn warning_msg(message: String) -> Self {
        Self {
            kind: DiagnosticKind::Warning,
            title: None,
            message,
            location: None,
            hints: vec![],
            stack_trace: vec![],
        }
    }

    /// Create a new warning diagnostic with a located message.
    pub fn warning_located(title: String, message: String, location: SourceSection) -> Self {
        Self {
            kind: DiagnosticKind::Warning,
            title: Some(title),
            message,
            location: Some(location),
            hints: vec![],
            stack_trace: vec![],
        }
    }

    // --- Error diagnostics

    /// Create a new error diagnostic with a message.
    pub fn error_msg(message: String) -> Self {
        Self {
            kind: DiagnosticKind::Error,
            title: None,
            message,
            location: None,
            hints: vec![],
            stack_trace: vec![],
        }
    }

    /// Create a new error diagnostic with a located message.
    pub fn error_located(title: String, message: String, location: SourceSection) -> Self {
        Self {
            kind: DiagnosticKind::Error,
            title: Some(title),
            message,
            location: Some(location),
            hints: vec![],
            stack_trace: vec![],
        }
    }

    /// Create a new error diagnostic from an error template with message
    /// arguments.
    pub fn error_from_template<T>(
        location: &SourceSection,
        error_template: &ErrorTemplate,
        message_args: &[T],
    ) -> Self
    where
        T: AsRef<str>,
    {
        Self {
            kind: DiagnosticKind::Error,
            title: Some(String::from(error_template.title)),
            message: error_template.render_message(message_args),
            location: Some(*location),
            hints: vec![],
            stack_trace: vec![],
        }
    }

    /// Create a new error diagnostic from an error template with message
    /// arguments with hints.
    pub fn from_error_template_with_hints<T>(
        location: &SourceSection,
        error_template: &ErrorTemplate,
        message_args: &[T],
        hints: Vec<Hint>,
    ) -> Self
    where
        T: AsRef<str>,
    {
        Self {
            kind: DiagnosticKind::Error,
            title: Some(String::from(error_template.title)),
            message: error_template.render_message(message_args),
            location: Some(*location),
            hints,
            stack_trace: vec![],
        }
    }

    /// Create a new error diagnostic from an error template with arguments and
    /// a stack trace.
    pub fn from_error_template_with_stack_trace<T>(
        location: &SourceSection,
        error_template: &ErrorTemplate,
        message_args: &[T],
        stack_trace: Vec<CallLocation>,
    ) -> Self
    where
        T: AsRef<str>,
    {
        Self {
            kind: DiagnosticKind::Error,
            title: Some(String::from(error_template.title)),
            message: error_template.render_message(message_args),
            location: Some(*location),
            hints: vec![],
            stack_trace,
        }
    }

    // --- Bug diagnostics

    /// Create a new bug diagnostic with a message.
    pub fn bug_msg(message: String) -> Self {
        Self {
            kind: DiagnosticKind::Bug,
            title: None,
            message,
            location: None,
            hints: vec![],
            stack_trace: vec![],
        }
    }

    /// Create a new bug diagnostic with a located message.
    pub fn bug_diag(title: String, message: String, location: SourceSection) -> Self {
        Self {
            kind: DiagnosticKind::Bug,
            title: Some(title),
            message,
            location: Some(location),
            hints: vec![],
            stack_trace: vec![],
        }
    }

    // --- Creation helpers

    /// Create a new diagnostic from an LKQL parsing diagnostic in the provided
    /// source.
    pub fn from_lkql_diagnostic(source: SourceId, diagnostic: &liblkqllang::Diagnostic) -> Self {
        Self {
            kind: DiagnosticKind::Error,
            title: Some(String::from("Parsing error")),
            message: diagnostic.message.clone(),
            location: Some(SourceSection::new(
                source,
                Location::from_lkql_location(diagnostic.sloc_range.start),
                Location::from_lkql_location(diagnostic.sloc_range.end),
            )),
            hints: vec![],
            stack_trace: vec![],
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
        if let Some(ref location) = self.location {
            // Create a new diagnostic builder
            let rep_builder =
                Report::build(self.kind.as_ariadne_kind(), location.to_span(source_repo))
                    // Add the report title
                    .with_message(self.title.as_ref().unwrap_or(&format!(
                        "Untitled {} diagnostic",
                        self.kind.label().to_lowercase()
                    )))
                    // Add the main label with the diagnostic color
                    .with_label(
                        Label::new(location.to_span(source_repo))
                            .with_message(&self.message)
                            .with_color(self.kind.color())
                            .with_priority(10)
                            .with_order(0),
                    )
                    // Add all hints
                    .with_labels(self.hints.iter().map(|h| {
                        Label::new(h.location.to_span(source_repo))
                            .with_message(
                                format!("{} {}", "Hint:".fg(HINT_COLOR), &h.message).as_str(),
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
            .unwrap();

            // Print the error stack trace
            for trace_element in &self.stack_trace {
                let trace_element_builder =
                    Report::build(ReportKind::Advice, trace_element.location.to_span(source_repo))
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
                .unwrap();
            }
        } else {
            self.print_message(output);
        }
    }

    /// Print the diagnostic in the format "<kind_name>: <message>". This can
    /// be used at places where you cannot access the source repository.
    pub fn print_message<W: Write>(&self, output: &mut W) {
        let line_prefix = format!("{}:", self.kind.label()).fg(self.kind.color());
        writeln!(output, "{line_prefix} {}", self.message).unwrap();
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum DiagnosticKind {
    Info,
    Warning,
    Error,
    Bug,
}

impl DiagnosticKind {
    fn as_ariadne_kind(&self) -> ReportKind<'_> {
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

/// This structure represents an hint in a diagnostic. A hint is a located
/// additional piece of information that brings more context or advices about
/// a diagnostic.
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
