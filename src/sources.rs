//! # Abstract sources module
//!
//! This module contains all utils to create and manipulate abstract sources.
//! This is used to factorize all operations on sources processed by the
//! compiler.
//!
//! This module provide a [`SourceRepository`] type to store and cache analyzed
//! sources.

use std::{cell::OnceCell, collections::HashMap, fmt::Display, fs, path::Path};

use liblkqllang::{LkqlNode, SourceLocation};

use crate::Error;

/// This structure is the main entry point of abstract source handling, it
/// holds all created sources, associating each one to its identifier.
#[derive(Debug)]
pub struct SourceRepository {
    source_map: HashMap<SourceId, Source>,
}

impl SourceRepository {
    /// Create a new empty source repository.
    pub fn new() -> Self {
        Self { source_map: HashMap::new() }
    }

    /// Get the source object for the provided file path, if it is already
    /// stored in the [`self`] repository return a reference to it.
    /// Otherwise, create a new source object and store it into the repository
    /// before returning a referece to it.
    /// This function returns an [`Error`] if the provided path is not
    /// designating an existing file.
    pub fn get_file_source(&mut self, file_path: &Path) -> Result<&Source, Error> {
        let source_id = SourceId::from(file_path.canonicalize()?.to_str().unwrap());

        // If it is not already present, insert the source in the map
        if !self.source_map.contains_key(&source_id) {
            self.source_map.insert(
                source_id.clone(),
                Source::File { canonical_path: source_id.clone(), content: OnceCell::new() },
            );
        }

        Ok(self.source_map.get(&source_id).unwrap())
    }

    /// Get the source object associated to the provided buffer, if it is
    /// already store in the [`self`] repository, return a reference to it.
    /// Otherwise, create a new source object and store it into the repository
    /// before returning a reference to it.
    pub fn get_buffer_source(&mut self, name: &str, content: &str) -> &Source {
        let source_id = SourceId::from(name);

        // If it is not already present, insert the source in the map
        if !self.source_map.contains_key(&source_id) {
            self.source_map.insert(
                source_id.clone(),
                Source::Buffer { name: source_id.clone(), content: String::from(content) },
            );
        }

        self.source_map.get(&source_id).unwrap()
    }
}

/// A source identifier is just a string.
type SourceId = String;

/// This enumeration is an abstraction over the source concepts. It can
/// represents either a source file located on the disk, or a source buffer.
/// It is not meant to be directly created, you should use a
/// [`SourceRepository`] instance to create new source objects.
/// For now only the UTF-8 sources are handled
/// (see https://github.com/HugoGGuerrier/lkqlua_jit/issues/1).
#[derive(Debug, Eq)]
pub enum Source {
    File { canonical_path: String, content: OnceCell<String> },
    Buffer { name: String, content: String },
}

/// Sources are defined to be equal if they have the same [`fn@Self::id`],
/// meaning that a [`Source::Buffer`] may be equals to a [`Source::File`].
impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl Source {
    pub fn id(&self) -> &SourceId {
        match self {
            Source::File { canonical_path, content: _ } => canonical_path,
            Source::Buffer { name, content: _ } => name,
        }
    }

    pub fn content(&self) -> &String {
        match self {
            Source::File { canonical_path, content } => content.get_or_init(|| {
                fs::read_to_string(canonical_path).expect(
                    format!("An error occured while reading the \"{canonical_path}\" file")
                        .as_str(),
                )
            }),
            Source::Buffer { name: _, content } => content,
        }
    }
}

/// This structure represents an extract from a [`Source`].
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
    /// Create a [`SourceSection`] object from a [`liblkqllang`] node.
    pub fn from_lkql_node(node: &LkqlNode) -> Result<Self, Error> {
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
    pub fn from_lkql_location(location: SourceLocation) -> Self {
        Self { line: location.line as u64, col: location.column as u32 }
    }
}
