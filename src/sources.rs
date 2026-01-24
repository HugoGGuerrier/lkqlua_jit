//! # Abstract sources module
//!
//! This module contains all components required to create and manipulate
//! abstract sources.
//! This module provide a [`SourceRepository`] type to load, retrieve and parse
//! sources.

use crate::report::Report;
use ariadne::Cache;
use liblkqllang::{AnalysisContext, AnalysisUnit, SourceLocation};
use std::{
    collections::HashMap,
    fmt::Display,
    fs,
    ops::Range,
    path::{Path, PathBuf},
    time::SystemTime,
};

/// This structure is the main entry point of abstract source handling, it
/// holds all created sources, associating each one from its name to its
/// identifier.
#[derive(Debug)]
pub struct SourceRepository {
    lkql_context: AnalysisContext,
    sources: Vec<Source>,

    /// Internal field to store all loaded files and link them to their
    /// corresponding source.
    file_to_source: HashMap<PathBuf, SourceId>,
}

impl Cache<SourceId> for &SourceRepository {
    type Storage = String;

    fn fetch(
        &mut self,
        source_id: &SourceId,
    ) -> Result<&ariadne::Source<Self::Storage>, impl std::fmt::Debug> {
        self.get_source_by_id(*source_id)
            .map_or(Err(format!("No sources with id {source_id}")), |s| Ok(&s.content()))
    }

    fn display<'a>(&self, source_id: &'a SourceId) -> Option<impl std::fmt::Display + 'a> {
        self.get_source_by_id(*source_id)
            .map(|s| String::from(s.name()))
    }
}

impl SourceRepository {
    /// Create a new empty source repository with default config
    /// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/2).
    pub fn new() -> Self {
        Self {
            lkql_context: AnalysisContext::create_default().unwrap(),
            sources: Vec::new(),
            file_to_source: HashMap::new(),
        }
    }

    /// Read the provided file and store it as a source in this repository.
    /// This function tries to avoid useless file reload, so before reading
    /// the file, it checks if the latter has been modified since the last
    /// load. If not, the source repository isn't modified, and the second
    /// part of the result is [`false`].
    /// This method may fail if:
    ///   * The provided path doesn't designate an existing file
    ///   * The file designated by the provided file is not UTF-8 encoded
    ///     (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/1)
    pub fn add_source_file(&mut self, file_path: &Path) -> Result<(SourceId, bool), Report> {
        // Get the absolute path to the file
        let canonical_path = file_path.canonicalize()?;

        // Then get the last time the file has been modified
        let last_modification = fs::metadata(&canonical_path)
            .and_then(|md| md.modified())
            .ok();

        // Then, if this file has already been loaded in this repository,
        // compare the stored modification date with the one from metadata.
        if let Some(&source_id) = self.file_to_source.get(&canonical_path) {
            if let Some(known_last_change) = self.sources[source_id].last_modification() {
                if let Some(current_last_change) = last_modification {
                    if known_last_change == current_last_change {
                        return Ok((source_id, false));
                    }
                }
            }
        }

        // If we're here, the source must be updated or inserted in the
        // repository.
        let source_id = self
            .file_to_source
            .get(&canonical_path)
            .copied()
            .unwrap_or(self.sources.len());
        self.file_to_source
            .insert(canonical_path.clone(), source_id);
        let source = Source::File {
            content: ariadne::Source::from(fs::read_to_string(&canonical_path)?),
            path: canonical_path,
            last_modification,
        };
        self.sources.insert(source_id, source);
        Ok((source_id, true))
    }

    /// Register the provided buffer in the source repository, returning the
    /// identifier of the newly created source. This function store the new
    /// buffer unconditionally.
    pub fn add_source_buffer(&mut self, name: &str, content: &str) -> SourceId {
        let source = Source::Buffer {
            name: String::from(name),
            content: ariadne::Source::from(String::from(content)),
        };
        let source_id = self.sources.len();
        self.sources.insert(source_id, source);
        source_id
    }

    /// Get a reference to the source object associated to the provided
    /// identifier, if any.
    pub fn get_source_by_id(&self, source_id: SourceId) -> Option<&Source> {
        self.sources.get(source_id)
    }

    /// Get the name of a source from its identifier.
    pub fn get_name_by_id(&self, source_id: SourceId) -> &str {
        self.sources.get(source_id).unwrap().name()
    }

    /// Get the source identifier corresponding to the provided file path, if
    /// any.
    pub fn get_id_by_file(&self, file: &Path) -> Option<SourceId> {
        file.canonicalize()
            .ok()
            .and_then(|p| self.file_to_source.get(&p).copied())
    }

    /// Parse the source designated by the provided identifier using the LKQL
    /// parsing library. If the parsing succeeds, this function return the
    /// resulting analysis unit that contains the parsing tree.
    /// This method may fail if:
    ///   * An exception occurs in the parsing library
    ///   * There is no source corresponding to the provided identifier
    ///   * The source designated by the provided identifier is not a valid
    ///     LKQL source
    pub fn parse_as_lkql(&mut self, source_id: SourceId) -> Result<AnalysisUnit, Report> {
        // Parse the source as LKQL
        let source = self
            .get_source_by_id(source_id)
            .ok_or(format!("No sources with id {source_id}"))?;
        let unit = self.lkql_context.get_unit_from_buffer(
            &source.name(),
            source.content().text(),
            None,
            None,
        )?;

        // Check parsing diagnostics
        let parsing_diags = unit.diagnostics()?;
        if !parsing_diags.is_empty() {
            let mut parsing_reports: Vec<Report> = Vec::with_capacity(parsing_diags.len());
            for diag in &parsing_diags {
                parsing_reports.push(Report::from_lkql_diagnostic(source_id, diag)?);
            }
            Err(Report::Composed(parsing_reports))
        } else {
            Ok(unit)
        }
    }
}

/// A source identifier is just an unsigned integer.
pub type SourceId = usize;

/// This type represents an abstract source.
#[derive(Debug)]
pub enum Source {
    File {
        /// An absolute path to the file.
        path: PathBuf,

        /// Content of the file.
        content: ariadne::Source<String>,

        /// Last time the file has been modified when this instance has been
        /// created.
        last_modification: Option<SystemTime>,
    },
    Buffer {
        /// Name of the buffer.
        name: String,

        /// Content of the buffer.
        content: ariadne::Source<String>,
    },
}

impl Source {
    /// Get the name of the source.
    pub fn name(&self) -> &str {
        match self {
            Source::File { path, .. } => path.to_str().unwrap(),
            Source::Buffer { name, .. } => name,
        }
    }

    /// Get the content of the source.
    pub fn content(&self) -> &ariadne::Source<String> {
        match self {
            Source::File { content, .. } | Source::Buffer { content, .. } => content,
        }
    }

    /// Get the last time the source origin has been modified, if this is
    /// applicable to it.
    pub fn last_modification(&self) -> Option<SystemTime> {
        match self {
            Source::File { last_modification, .. } => last_modification.clone(),
            Source::Buffer { .. } => None,
        }
    }
}

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
        let source = source_repo.get_source_by_id(self.source).unwrap();

        // Here, we ensure the start offset is included in the source
        let maybe_start_offset = source
            .content()
            .line(self.start.line as usize - 1)
            .map(|l| l.offset() + self.start.col as usize - 1);
        let start_offset = maybe_start_offset.unwrap_or(if self.start.line > 1 {
            source.content().lines().last().unwrap().span().end
        } else {
            0
        });

        // Doing the same thing for the end offset
        let maybe_end_offset = source
            .content()
            .line(self.end.line as usize - 1)
            .map(|l| l.offset() + self.end.col as usize - 1);
        let end_offset = maybe_end_offset.unwrap_or(if self.end.line > 1 {
            source.content().lines().last().unwrap().span().end
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
    pub line: u32,
    pub col: u16,
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
        Self { line: location.line, col: location.column }
    }
}
