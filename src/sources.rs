//! # Abstract sources module
//!
//! This module contains all utils to create and manipulate abstract sources.
//! This is used to factorize all operations on sources processed by the
//! compiler.
//!
//! This module provide a [`SourceRepository`] type to store and cache analyzed
//! sources.

use std::{cell::OnceCell, collections::HashMap, fmt::Display, fs, path::Path, rc::Rc};

use liblkqllang::LkqlNode;

use crate::Error;

/// This structure is the main entry point of abstract source handling, it
/// holds all created sources, associating each one to its identifier.
#[derive(Debug)]
pub struct SourceRepository {
    source_map: HashMap<SourceId, Rc<Source>>,
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
    pub fn get_file_source(&mut self, file_path: &Path) -> Result<Rc<Source>, Error> {
        let source_id = SourceId::from(file_path.canonicalize()?.to_str().unwrap());

        // Check if the source map already contains a source for this file
        if let Some(source) = self.source_map.get(&source_id) {
            Ok(source.clone())
        }
        // Else, create the source object and store it into the map
        else {
            let res_source = Rc::new(Source::File {
                canonical_path: source_id.clone(),
                content: OnceCell::new(),
            });
            self.source_map.insert(source_id, res_source.clone());
            Ok(res_source)
        }
    }

    /// Get the source object associated to the provided buffer, if it is
    /// already store in the [`self`] repository, return a reference to it.
    /// Otherwise, create a new source object and store it into the repository
    /// before returning a reference to it.
    pub fn get_buffer_source(&mut self, name: &str, content: &str) -> Rc<Source> {
        let source_id = SourceId::from(name);

        // Check if the source map already contains a a source for this buffer
        // name.
        if let Some(source) = self.source_map.get(&source_id) {
            source.clone()
        }
        // Else, create a new source object and store it into the repository
        else {
            let res_source =
                Rc::new(Source::Buffer { name: source_id.clone(), content: String::from(content) });
            self.source_map.insert(source_id, res_source.clone());
            res_source
        }
    }

    /// Get whether this repository has a registered source with the provided
    /// name.
    pub fn has_source_with_id(&self, source_id: &SourceId) -> bool {
        self.source_map.contains_key(source_id)
    }

    /// Get the source section corresponding to the provided LKQL node. Returns
    /// an [`Error`] in the following cases:
    ///   * If there is any exception in [`liblkqllang`]
    ///   * If the node has no related unit
    ///   * If the node's unit source is not included in this repository
    pub fn section_from_lkql_node(&self, node: &LkqlNode) -> Result<SourceSection, Error> {
        // Get the node's unit
        if let Some(unit) = node.unit()? {
            // Get required information about the node and its unit
            let slocr = node.sloc_range()?;
            let source_id = unit.filename()?;

            // If the source is in the repository, return a successful result
            if let Some(source) = self.source_map.get(&source_id) {
                Ok(SourceSection {
                    source: source.clone(),
                    start: Location {
                        line: slocr.start.line as u64,
                        col: slocr.start.column as u32,
                    },
                    end: Location { line: slocr.end.line as u64, col: slocr.end.column as u32 },
                })
            }
            // Else, the source is not included in the repository, return an
            // error.
            else {
                Err(Error::Messaged(format!(
                    "Cannot retrieve a source corresponding to \"{source_id}\""
                )))
            }
        }
        // Else, the node has no related unit (this is very weird), return an
        // error.
        else {
            Err(Error::Messaged(format!("Cannot get unit from node {node}")))
        }
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
    pub source: Rc<Source>,
    pub start: Location,
    pub end: Location,
}

impl Display for SourceSection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.source.id(), self.start.line, self.start.col)
    }
}

/// This structure represents a location in a source, defined by a line and
/// a colon.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub line: u64,
    pub col: u32,
}
