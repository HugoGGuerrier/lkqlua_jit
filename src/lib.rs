//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

use crate::{
    bytecode::extended_bytecode::ExtendedBytecodeUnit,
    diagnostics::{Diagnostic, DiagnosticCollector},
    engine::{Engine, analysis_lib::NodeTypeRepo},
    intermediate_tree::ExecutionUnit,
    lua::{get_string, next, pop, push_nil, set_global},
    sources::{SourceId, SourceRepository},
};
use clap::ValueEnum;
use liblkqllang::{AnalysisContext, AnalysisUnit};
use pretty_hex::PrettyHex;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs::File,
    io::{Stderr, Stdout, Write},
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

pub mod builtins;
pub mod bytecode;
pub mod diagnostics;
pub mod engine;
pub mod errors;
pub mod intermediate_tree;
pub mod lowering;
pub mod lua;
pub mod runtime;
pub mod sources;

/// Content of the LKQL prelude.
const PRELUDE_SOURCE: &str = include_str!("prelude.lkql");

/// This type holds all required data to run LKQL sources using LuaJIT as a
/// backend. This is what you have to use.
#[derive(Debug)]
pub struct ExecutionContext<'a> {
    pub config: Config,
    pub source_repo: &'a mut SourceRepository,

    /// Analysis context used to parse LKQL sources.
    lkql_context: AnalysisContext,

    /// Cache where parsing result are stored, associated to their source.
    parsing_cache: HashMap<SourceId, AnalysisUnit>,

    /// Cache were compilation result are placed, associated to their source.
    compilation_cache: HashMap<SourceId, ExtendedBytecodeUnit>,

    /// Inner execution engine to run LuaJIT bytecode buffers.
    engine: Engine,

    /// This vector stores sources that are currently being executed in their
    /// execution order (oldest first).
    execution_stack: Vec<SourceId>,

    /// Map used to store source time measurements associated to executed
    /// sources.
    pub timings: BTreeMap<SourceId, Timings>,
}

impl<'a> ExecutionContext<'a> {
    /// Create an initialize a new execution context. This is the first entry
    /// point to the LKQL engine.
    ///
    /// If any error occurs during the execution context initialization, this
    /// function returns [`Err`] with all error messages.
    pub fn new<'b: 'a>(
        config: Config,
        source_repo: &'b mut SourceRepository,
    ) -> Result<Self, DiagnosticCollector> {
        // Create the resulting execution context
        let mut res = Self {
            engine: Engine::new(&config)?,
            config,
            source_repo,
            lkql_context: AnalysisContext::create_default().map_err(Diagnostic::from)?,
            parsing_cache: HashMap::new(),
            compilation_cache: HashMap::new(),
            execution_stack: Vec::new(),
            timings: BTreeMap::new(),
        };

        // Execute the prelude source
        let prelude_source_id = res.add_source_buffer("__prelude", PRELUDE_SOURCE);
        res.execute_source(prelude_source_id)?;

        // Fetch all prelude symbols and add them to the engine
        let l = res.engine.lua_state;
        push_nil(l);
        while next(l, -2) {
            let name = get_string(l, -2).unwrap();
            set_global(l, name);
            res.engine.add_global(name);
        }

        // Finally, return the initialized execution context
        Ok(res)
    }

    /// Get all symbols accessible as globals in this execution context by
    /// fetching them from the engine.
    pub fn get_globals(&self) -> &HashSet<String> {
        &self.engine.registered_globals
    }

    /// Get all node types registered in this execution context by fetching
    /// them in the loaded analysis library.
    pub fn get_node_types(&self) -> &NodeTypeRepo {
        &self.engine.analysis_lib.node_types
    }

    /// Execute the provided LKQL file, returning possible
    /// [`DiagnosticCollector`] if the execution is not successful. This
    /// method doesn't return the namespace produced by the provided LKQL
    /// script, it only applies its side effects.
    pub fn execute_lkql_script(&mut self, file: &Path) -> Result<(), DiagnosticCollector> {
        // Add the source to this context and execute it
        let source = self.add_source_file(file)?;
        self.execute_source(source)?;

        // Pop the result from the Lua stack
        pop(self.engine.lua_state, 1);

        // Return the success
        Ok(())
    }

    /// Add the provided `file` as a source in this execution context and
    /// return the identifier of it.
    fn add_source_file(&mut self, file: &Path) -> Result<SourceId, DiagnosticCollector> {
        let (res, updated) = self.source_repo.add_source_file(file)?;

        // If the file has been changed, remove its entry in the compilation
        // cache.
        if updated {
            self.parsing_cache.remove(&res);
            self.compilation_cache.remove(&res);
        }

        Ok(res)
    }

    /// Add a new source described by the provided `name` and `content`
    /// information, returning its new identifier.
    fn add_source_buffer(&mut self, name: &str, content: &str) -> SourceId {
        let res = self.source_repo.add_source_buffer(name, content);
        self.parsing_cache.remove(&res);
        self.compilation_cache.remove(&res);
        res
    }

    /// Parse the source designated by the provided identifier using the LKQL
    /// parsing library. If the parsing succeeds, this function populates the
    /// [`Self::parsing_cache`] with the resulting analysis unit that contains the
    /// syntax tree.
    ///
    /// If the parsing encounter syntax error or an internal exception, an
    /// [`Err`] instance is returned with all collected diagnostics.
    ///
    /// This method may panic if:
    ///   * There is no source corresponding to the provided identifier
    fn parse_source(&mut self, source: SourceId) -> Result<(), DiagnosticCollector> {
        if !self.parsing_cache.contains_key(&source) {
            // We know that the source hasn't been parsed before, so do it now
            let src = self.source_repo.get_source_by_id(source).unwrap();
            let unit = self
                .lkql_context
                .get_unit_from_buffer(src.name(), src.content().text(), None, None)
                .map_err(Diagnostic::from)?;

            // Check parsing diagnostics
            let lkql_parsing_diags = unit.diagnostics().map_err(Diagnostic::from)?;
            if !lkql_parsing_diags.is_empty() {
                let mut diagnostics = DiagnosticCollector::new();
                for lkql_diag in &lkql_parsing_diags {
                    diagnostics.add(Diagnostic::from_lkql_diagnostic(source, lkql_diag));
                }
                return Err(diagnostics);
            }

            // Finally place the parsing result in the cache
            self.parsing_cache.insert(source, unit);
        }

        // Finally, if we're here, we know that the parsing has been
        // successfully made and place in the parsing cache.
        Ok(())
    }

    /// Compile the source designated by the provided identifier. If the
    /// compilation succeeds, this function populates the
    /// [`Self::compilation_cache`] with the resulting bytecode.
    ///
    /// If the compilation encounter an error, an [`Err`] instance is returned
    /// with all collected diagnostics.
    fn compile_source(&mut self, source: SourceId) -> Result<(), DiagnosticCollector> {
        if !self.compilation_cache.contains_key(&source) {
            // Here we know that the source hasn't been compiled before, so we
            // perform the compilation.
            let mut time_point: Instant;

            // Parse the source file
            time_point = Instant::now();
            self.parse_source(source)?;
            let unit = self.parsing_cache.get(&source).unwrap();
            let root = unit.root().map_err(Diagnostic::from)?.unwrap();
            self.get_timings_for_source(source).parsing = time_point.elapsed();

            // If required, display the parsing tree
            if self.config.is_verbose(VerboseElement::ParsingTree) {
                writeln!(self.config.std_out, "===== Parsing tree =====\n").unwrap();
                writeln!(self.config.std_out, "{}\n", root.tree_dump(0).map_err(Diagnostic::from)?)
                    .unwrap();
            }

            // Lower the parsing tree
            time_point = Instant::now();
            let lowering_tree = ExecutionUnit::lower_lkql_node(self, source, &root)?;
            self.get_timings_for_source(source).lowering = time_point.elapsed();

            // If required, display the lowered tree
            if self.config.is_verbose(VerboseElement::LoweringTree) {
                writeln!(self.config.std_out, "===== Lowering tree =====\n").unwrap();
                writeln!(self.config.std_out, "{}\n", lowering_tree).unwrap();
            }

            // Compile the lowering tree to the extended bytecode format
            time_point = Instant::now();
            let extended_bytecode_unit = lowering_tree.compile(self.get_globals())?;
            self.get_timings_for_source(source).compilation = time_point.elapsed();

            // Transform the extended bytecode unit into a standard bytecode unit
            let bytecode_unit = extended_bytecode_unit.to_bytecode_unit();

            // If required, display the compiled bytecode
            if self.config.is_verbose(VerboseElement::Bytecode) {
                writeln!(self.config.std_out, "===== Bytecode =====\n").unwrap();
                writeln!(self.config.std_out, "{}\n", bytecode_unit).unwrap();
            }

            // If required, display the raw bytecode buffer
            if self.config.is_verbose(VerboseElement::RawBytecode) {
                let mut encoded_bytecode_unit = Vec::new();
                bytecode_unit.encode(&mut encoded_bytecode_unit);
                writeln!(self.config.std_out, "===== Raw bytecode =====\n").unwrap();
                writeln!(self.config.std_out, "{:?}\n", encoded_bytecode_unit.hex_dump()).unwrap();
            }

            // Store the compilation result in the cache
            self.compilation_cache
                .insert(source, extended_bytecode_unit);
        }

        Ok(())
    }

    /// Execute the source designated by the provided identifier, leaving its
    /// execution result in the top of the Lua stack.
    fn execute_source(&mut self, source: SourceId) -> Result<(), DiagnosticCollector> {
        // Push the source on the execution stack
        self.execution_stack.push(source);

        // Get the compilation result of the source from the cache
        self.compile_source(source)?;
        let bytecode_unit = self.compilation_cache.get(&source).unwrap();

        // Run the bytecode
        let time_point = Instant::now();
        self.engine.run_bytecode(self, bytecode_unit)?;
        self.get_timings_for_source(source).execution = time_point.elapsed();

        // Pop the source from the execution stack
        self.execution_stack.pop();

        // Finally, return the success
        Ok(())
    }

    /// Get a mutable reference to timings associated to the given source.
    fn get_timings_for_source(&mut self, source: SourceId) -> &mut Timings {
        self.timings.entry(source).or_default();
        self.timings.get_mut(&source).unwrap()
    }
}

#[derive(Debug)]
pub struct Config {
    /// Writable to use as standard output.
    pub std_out: Writable,

    /// Writable to use as error output.
    pub std_err: Writable,

    /// Whether to perform profiling during the run.
    pub do_profiling: bool,

    /// All elements to display additional information about.
    pub verbose_elements: HashSet<VerboseElement>,

    /// Name of the language to analyze.
    pub analyzed_lang_name: String,

    /// All files to analyze as sources of the specified language.
    pub files_to_analyze: Vec<PathBuf>,

    /// Arguments that should be processed by the LKQL engine.
    pub additional_args: Vec<String>,
}

impl Config {
    pub fn is_verbose(&self, element: VerboseElement) -> bool {
        self.verbose_elements.contains(&element)
            || self.verbose_elements.contains(&VerboseElement::All)
    }
}

/// This type represents different elements that can be written bytes to.
#[derive(Debug)]
pub enum Writable {
    Stdout(Stdout),
    Stderr(Stderr),
    File(File),
    ByteBuffer(Vec<u8>),
}

impl Write for Writable {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Writable::Stdout(o) => o.write(buf),
            Writable::Stderr(o) => o.write(buf),
            Writable::File(file) => file.write(buf),
            Writable::ByteBuffer(buffer) => {
                buffer.extend_from_slice(buf);
                Ok(buf.len())
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Writable::Stdout(o) => o.flush(),
            Writable::Stderr(o) => o.flush(),
            Writable::File(file) => file.flush(),
            Writable::ByteBuffer(_) => Ok(()),
        }
    }
}

/// This enum contains all elements that can be added to a "classic" output
/// during a run of the engine.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, ValueEnum)]
pub enum VerboseElement {
    All,
    ParsingTree,
    LoweringTree,
    Bytecode,
    RawBytecode,
}

/** This structure is used to store timing information about a source. */
#[derive(Debug)]
pub struct Timings {
    pub parsing: Duration,
    pub lowering: Duration,
    pub compilation: Duration,
    pub execution: Duration,
}

impl Default for Timings {
    fn default() -> Self {
        Self::new()
    }
}

impl Timings {
    pub fn new() -> Self {
        Timings {
            parsing: Duration::ZERO,
            lowering: Duration::ZERO,
            compilation: Duration::ZERO,
            execution: Duration::ZERO,
        }
    }
}
