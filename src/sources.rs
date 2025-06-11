//! # Abstract sources module
//!
//! This module contains all utils to create and manipulate abstract sources.
//! This is used to factorize all operations on sources processed by the
//! compiler.
//!
//! This module provide a [`SourceRepository`] type to store and cache analyzed
//! sources.

use std::{collections::HashMap, fmt::Display, fs, path::Path};

use ariadne::{Cache, Source};
use liblkqllang::{AnalysisContext, AnalysisUnit, LkqlNode, SourceLocation};

use crate::Report;

/// This structure is the main entry point of abstract source handling, it
/// holds all created sources, associating each one to its identifier.
#[derive(Debug)]
pub struct SourceRepository {
    lkql_context: AnalysisContext,
    source_map: HashMap<SourceId, Source>,
    lkql_unit_map: HashMap<SourceId, AnalysisUnit>,
}

impl SourceRepository {
    /// Create a new empty source repository with default config
    /// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/2).
    pub fn new() -> Self {
        Self {
            lkql_context: AnalysisContext::create_default().unwrap(),
            source_map: HashMap::new(),
            lkql_unit_map: HashMap::new(),
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
        let source_id = String::from(name);
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
                "File \"{id}\" is already in the source repository"
            )));
        }
        self.source_map
            .insert(id.clone(), Source::from(content_provider()?));
        Ok(id)
    }

    /// Parse the source designated by the provided identifier using the LKQL
    /// parsing library. If the parsing succeeds, or if the source has already
    /// been successfully parsed as LKQL, this function return the resulting
    /// analysis unit.
    /// This method may fail if:
    ///   * An exception occurs in the parsing library
    ///   * The source designated by the provided identifier is not a valid
    ///     LKQL source
    pub fn parse_as_lkql(
        &mut self,
        source: &SourceId,
        reparse: bool,
    ) -> Result<AnalysisUnit, Report> {
        todo!()
    }
}

impl Cache<SourceId> for SourceRepository {
    type Storage = String;

    fn fetch(&mut self, id: &SourceId) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        self.source_map
            .get(id)
            .map_or(Err(Report::bug_msg(format!("No source identified by \"{id}\""))), |s| {
                Ok(s)
            })
    }

    fn display<'a>(&self, id: &'a SourceId) -> Option<impl std::fmt::Display + 'a> {
        Some(id)
    }
}

/// A source identifier is just a string.
type SourceId = String;

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
}

/// This structure represents a location in a source, defined by a line and
/// a colon.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u64,
    pub col: u32,
}

impl Location {
    /// Create a new location from a [`liblkqllang::SourceLocation`] object.
    pub fn from_lkql_location(location: SourceLocation) -> Self {
        Self { line: location.line as u64, col: location.column as u32 }
    }
}
