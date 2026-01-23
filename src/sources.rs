//! # Abstract sources module
//!
//! This module contains all components required to create and manipulate
//! abstract sources.
//! This module provide a [`SourceRepository`] type to load, retrieve and parse
//! sources.

use crate::report::Report;
use ariadne::Cache;
use liblkqllang::{AnalysisContext, AnalysisUnit, LkqlNode, SourceLocation};
use std::{collections::HashMap, env::current_dir, fmt::Display, fs, ops::Range, path::Path};

/// This structure is the main entry point of abstract source handling, it
/// holds all created sources, associating each one from its name to its
/// identifier.
#[derive(Debug)]
pub struct SourceRepository {
    lkql_context: AnalysisContext,
    sources: Vec<Source>,
    source_name_map: HashMap<String, SourceId>,
}

impl Cache<SourceId> for &SourceRepository {
    type Storage = String;

    fn fetch(
        &mut self,
        source_id: &SourceId,
    ) -> Result<&ariadne::Source<Self::Storage>, impl std::fmt::Debug> {
        self.get_source_by_id(*source_id)
            .map_or(Err(format!("No sources with id {source_id}")), |s| Ok(&s.buffer))
    }

    fn display<'a>(&self, source_id: &'a SourceId) -> Option<impl std::fmt::Display + 'a> {
        self.get_source_by_id(*source_id).map(|s| s.name.clone())
    }
}

impl SourceRepository {
    /// Create a new empty source repository with default config
    /// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/2).
    pub fn new() -> Self {
        Self {
            lkql_context: AnalysisContext::create_default().unwrap(),
            sources: Vec::new(),
            source_name_map: HashMap::new(),
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
        let source_name = canonical_path.to_string_lossy().to_string();
        self.add_source(source_name, || fs::read_to_string(&canonical_path), update)
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
        // to be compatible with Langkit unit naming convention.
        let mut canonical_path = current_dir()?.canonicalize()?;
        canonical_path.push(name);
        let source_name = canonical_path.to_string_lossy().to_string();
        self.add_source(source_name, || Ok::<_, Report>(String::from(content)), update)
    }

    /// Internal function for adding sources.
    fn add_source<F, E>(
        &mut self,
        source_name: String,
        content_provider: F,
        update: bool,
    ) -> Result<SourceId, Report>
    where
        F: Fn() -> Result<String, E>,
        Report: From<E>,
    {
        // Start by checking if a source with the same name already exists
        if self.source_name_map.contains_key(&source_name) && !update {
            return Err(Report::bug_msg(format!(
                "A source named \"{source_name}\" is already in the repository"
            )));
        }

        // If we get here, we have to create the new source object and save it
        // in the repository.
        let source_id = self.sources.len();
        self.source_name_map.insert(source_name.clone(), source_id);
        let source_object =
            Source { name: source_name, buffer: ariadne::Source::from(content_provider()?) };
        self.sources.push(source_object);

        // Finally return the identifier of the new source
        Ok(source_id)
    }

    /// Get a reference to the source object associated to the provided
    /// identifier, if any.
    pub fn get_source_by_id(&self, source_id: SourceId) -> Option<&Source> {
        self.sources.get(source_id)
    }

    /// Get the source identifier associated to the provided source name, if
    /// any.
    pub fn get_id_by_name(&self, source_name: &str) -> Option<SourceId> {
        self.source_name_map.get(source_name).copied()
    }

    /// Get the name of a source from its identifier.
    pub fn get_name_by_id(&self, source_id: SourceId) -> &String {
        &self.sources.get(source_id).unwrap().name
    }

    /// Get the source identifier corresponding to the provided file path, if
    /// any.
    pub fn get_id_by_file(&self, file: &Path) -> Option<SourceId> {
        if let Ok(canonical_file) = file.canonicalize() {
            self.source_name_map
                .get(&canonical_file.to_string_lossy().to_string())
                .copied()
        } else {
            None
        }
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
            &source.name,
            source.buffer.text(),
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

/// This type represent a source
#[derive(Debug)]
pub struct Source {
    /// Name of the source, either the absolute path of the file, or the
    /// provided buffer name, preceded by the current directory absolute path.
    pub name: String,

    /// Content of the source.
    pub buffer: ariadne::Source<String>,
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
    /// Create a new section corresponding to the provided node location range
    /// in the provided source.
    pub fn from_lkql_node(source: SourceId, node: &LkqlNode) -> Result<Self, Report> {
        let sloc_range = node.sloc_range()?;
        Ok(Self {
            source,
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
        let source = source_repo.get_source_by_id(self.source).unwrap();

        // Here, we ensure the start offset is included in the source
        let maybe_start_offset = source
            .buffer
            .line(self.start.line as usize - 1)
            .map(|l| l.offset() + self.start.col as usize - 1);
        let start_offset = maybe_start_offset.unwrap_or(if self.start.line > 1 {
            source.buffer.lines().last().unwrap().span().end
        } else {
            0
        });

        // Doing the same thing for the end offset
        let maybe_end_offset = source
            .buffer
            .line(self.end.line as usize - 1)
            .map(|l| l.offset() + self.end.col as usize - 1);
        let end_offset = maybe_end_offset.unwrap_or(if self.end.line > 1 {
            source.buffer.lines().last().unwrap().span().end
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
