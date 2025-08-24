//! # Abstract sources module
//!
//! This module contains all utils to create and manipulate abstract sources.
//! This is used to factorize all operations on sources processed by the
//! compiler.
//!
//! This module provide a [`SourceRepository`] type to store and cache analyzed
//! sources.

use std::{collections::HashMap, env::current_dir, fmt::Display, fs, ops::Range, path::Path};

use ariadne::{Cache, Source};
use liblkqllang::{AnalysisContext, AnalysisUnit, LkqlNode, SourceLocation};

use crate::report::Report;

/// This structure is the main entry point of abstract source handling, it
/// holds all created sources, associating each one to its identifier.
#[derive(Debug)]
pub struct SourceRepository {
    lkql_context: AnalysisContext,
    source_map: HashMap<SourceId, Source>,
}

impl Cache<SourceId> for &SourceRepository {
    type Storage = String;

    fn fetch(
        &mut self,
        source_id: &SourceId,
    ) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        self.get_source(source_id)
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<impl std::fmt::Display + 'a> {
        Some(id)
    }
}

impl SourceRepository {
    /// Create a new empty source repository with default config
    /// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/2).
    pub fn new() -> Self {
        Self {
            lkql_context: AnalysisContext::create_default().unwrap(),
            source_map: HashMap::new(),
        }
    }

    /// Register a new file designated by the provided path in the source
    /// repository. If the operation is successful, then return the identifier
    /// of the newly created source. Note that if the file has already been
    /// loaded in this source repository and the `update` parameter is `true`,
    /// then the file is loaded again and the repository is updated.
    /// This method may fail if:
    ///   * The provided file has already been loaded in this source repository
    ///     and the `update` parameter is `false`
    ///   * The provided path doesn't designate an existing file
    ///   * The file designated by the provided file is not UTF-8 encoded
    ///     (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/1)
    pub fn add_source_file(&mut self, file_path: &Path, update: bool) -> Result<SourceId, Report> {
        let canonical_path = file_path.canonicalize()?;
        let source_id = canonical_path.to_string_lossy().to_string();
        self.add_source(source_id, || fs::read_to_string(&canonical_path), update)
    }

    /// Register the provided buffer in the source repository, returning the
    /// identifier of the newly created source. If the `update` parameter is
    /// `true`, the source is updated if it is already presents in the
    /// repository.
    /// This method may fail if:
    ///   * The provided file has already been loaded in this source repository
    ///     and the `update` parameter is `false`
    pub fn add_source_buffer(
        &mut self,
        name: &str,
        content: &str,
        update: bool,
    ) -> Result<SourceId, Report> {
        // We add the current working directory before the buffer name in order
        // to be compatible with Langkit unit's `filename`.
        let mut canonical_path = current_dir()?.canonicalize()?;
        canonical_path.push(name);
        let source_id = canonical_path.to_string_lossy().to_string();
        self.add_source(source_id, || Ok::<String, Report>(String::from(content)), update)
    }

    /// Internal function for adding sources.
    fn add_source<F, E>(
        &mut self,
        id: SourceId,
        content_provider: F,
        update: bool,
    ) -> Result<SourceId, Report>
    where
        F: Fn() -> Result<String, E>,
        Report: From<E>,
    {
        if self.source_map.contains_key(&id) && !update {
            return Err(Report::bug_msg(format!(
                "Source \"{id}\" is already in the source repository"
            )));
        }
        self.source_map
            .insert(id.clone(), Source::from(content_provider()?));
        Ok(id)
    }

    /// Get the source designated by the provided identifier, if there is no
    /// sources associated to it, return an [`Result::Err`].
    pub fn get_source(&self, source_id: &SourceId) -> Result<&Source, Report> {
        if let Some(source) = self.source_map.get(source_id) {
            Ok(source)
        } else {
            Err(Report::bug_msg(format!("Unknown source \"{source_id}\"")))
        }
    }

    /// Get the source designated by the provided identifier, panicking if
    /// there is no source related to it.
    pub fn get_source_unsafe(&self, source_id: &SourceId) -> &Source {
        self.get_source(source_id).expect("Unknown source")
    }

    /// Parse the source designated by the provided identifier using the LKQL
    /// parsing library. If the parsing succeeds, this function return the
    /// resulting analysis unit that contains the parsing tree.
    /// This method may fail if:
    ///   * An exception occurs in the parsing library
    ///   * There is no source corresponding to the provided identifier
    ///   * The source designated by the provided identifier is not a valid
    ///     LKQL source
    pub fn parse_as_lkql(&mut self, source_id: &SourceId) -> Result<AnalysisUnit, Report> {
        // Parse the source as LKQL
        let source = self.get_source_unsafe(source_id);
        let unit = self
            .lkql_context
            .get_unit_from_buffer(source_id, source.text(), None, None)?;

        // Check parsing diagnostics
        let parsing_diags = unit.diagnostics()?;
        if !parsing_diags.is_empty() {
            let mut parsing_reports: Vec<Report> = Vec::with_capacity(parsing_diags.len());
            for diag in &parsing_diags {
                parsing_reports.push(Report::from_lkql_diagnostic(&unit, diag)?);
            }
            Err(Report::Composed(parsing_reports))
        } else {
            Ok(unit)
        }
    }
}

/// A source identifier is just a string.
pub type SourceId = String;

/// This structure represents an extract from a source object.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSection {
    pub source: SourceId,
    pub start: Location,
    pub end: Location,
}

impl Display for SourceSection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.source, self.start.line, self.start.col)
    }
}

impl SourceSection {
    /// Create a new source section from a [`liblkqllang::LkqlNode`] object.
    pub fn from_lkql_node(node: &LkqlNode) -> Result<Self, Report> {
        let sloc_range = node.sloc_range()?;
        Ok(Self {
            source: node.unit()?.unwrap().filename()?,
            start: Location::from_lkql_location(sloc_range.start),
            end: Location::from_lkql_location(sloc_range.end),
        })
    }

    /// Create a new source section start from the `from` start location and
    /// finishing at the `to` end.
    /// This method returns an error if:
    ///   * Sections are not about the same source
    ///   * `to`'s end is before `from`'s start
    pub fn range(from: &Self, to: &Self) -> Result<Self, Report> {
        // Ensure both sections are about the same source
        if from.source != to.source {
            return Err(Report::bug_msg(format!(
                "Cannot get a range from sections about different sources"
            )));
        }

        // Ensure the `from` start is lower or equals to `to` end
        if from.start > to.end {
            return Err(Report::bug_msg(format!("Cannot create a span from {from} to {to}")));
        }

        // Finally create the new source section
        Ok(Self { source: from.source.clone(), start: from.start.clone(), end: to.end.clone() })
    }

    /// Create an [`ariadne::Span`] value from this source section.
    pub fn to_span(&self, source_repo: &SourceRepository) -> (SourceId, Range<usize>) {
        let source = source_repo.get_source_unsafe(&self.source);

        // Here, we ensure the start offset is included in the source
        let maybe_start_offset = source
            .line(self.start.line - 1)
            .map(|l| l.offset() + self.start.col - 1);
        let start_offset = maybe_start_offset.unwrap_or(if self.start.line > 1 {
            source.lines().last().unwrap().span().end
        } else {
            0
        });

        // Doing the same thing for the end offset
        let maybe_end_offset = source
            .line(self.end.line - 1)
            .map(|l| l.offset() + self.end.col - 1);
        let end_offset = maybe_end_offset.unwrap_or(if self.end.line > 1 {
            source.lines().last().unwrap().span().end
        } else {
            0
        });

        // Return the resulting span
        (self.source.clone(), start_offset..end_offset)
    }
}

/// This structure represents a location in a source, defined by a line and
/// a colon.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self.line.partial_cmp(&other.line) {
            Some(core::cmp::Ordering::Equal) => self.col.partial_cmp(&other.col),
            ord => ord,
        }
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line.cmp(&other.line).then(self.col.cmp(&other.col))
    }
}

impl Location {
    /// Create a new location from a [`liblkqllang::SourceLocation`] object.
    pub fn from_lkql_location(location: SourceLocation) -> Self {
        Self { line: location.line as usize, col: location.column as usize }
    }
}
