//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

use crate::{
    bytecode::extended_bytecode::ExtendedBytecodeUnit,
    engine::Engine,
    intermediate_tree::ExecutionUnit,
    report::Report,
    sources::{SourceId, SourceRepository},
};
use clap::ValueEnum;
use pretty_hex::PrettyHex;
use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Write,
    os::fd::FromRawFd,
    path::Path,
    time::{Duration, Instant},
};

pub mod builtins;
pub mod bytecode;
pub mod engine;
pub mod errors;
pub mod intermediate_tree;
pub mod lua;
pub mod report;
pub mod sources;

/// This type holds all required data to run LKQL sources using LuaJIT as a
/// backend. This is what you have to use.
pub struct ExecutionContext {
    pub config: EngineConfig,
    pub source_repo: SourceRepository,
    pub compilation_cache: HashMap<SourceId, (ExtendedBytecodeUnit, Vec<u8>)>,
    pub engine: Engine,

    /// This vector stores sources that are being executed in their execution
    /// order (oldest first).
    execution_stack: Vec<SourceId>,
}

impl ExecutionContext {
    /// Create an initialize a new execution context.
    pub fn new(config: EngineConfig) -> Self {
        Self {
            config,
            source_repo: SourceRepository::new(),
            compilation_cache: HashMap::new(),
            engine: Engine::new(),
            execution_stack: Vec::new(),
        }
    }

    /// Just run the provided LKQL file, don't return anything and report all
    /// diagnostics and messages in the [`EngineConfig::std_err`] of the
    /// related configuration.
    pub fn just_run_lkql_file(&mut self, file: &Path) {
        // Execute the file and get the result
        let exec_res = self.execute_lkql_file(file);

        // If there are errors, display them on STDERR
        if let Err(report) = exec_res {
            report.print(&self.source_repo, &mut self.config.std_err, false);
        }
    }

    /// Execute the provided LKQL file, returning possible [`Report`] if the
    /// execution is not successful.
    pub fn execute_lkql_file(&mut self, file: &Path) -> Result<(), Report> {
        // Add the source file to the source repo updating it if required
        let source = self.source_repo.add_source_file(file, true)?;
        self.compilation_cache.remove(&source);

        // Push the source on the execution stack
        self.execution_stack.push(source);

        // Create working variables to measure time
        let mut timings: Vec<(String, Duration)> = Vec::new();

        // Then compile the LKQL source and get the result in the cache
        self.compile_lkql_source(source, &mut timings)?;
        let (_, encoded_bytecode_unit) = self.compilation_cache.get(&source).unwrap();

        // Then, run the encoded bytecode with the custom engine
        let time_point = Instant::now();
        let res = self
            .engine
            .run_bytecode(self, source, &encoded_bytecode_unit);
        timings.push((String::from("execution"), time_point.elapsed()));

        // If required, display timing information
        if self.config.perform_timings {
            writeln!(self.config.std_out, "")?;
            self.display_timings(
                &timings,
                &format!("Executing {}", self.source_repo.get_name_by_id(source)),
            )?;
        }

        // Pop the source from the execution stack
        self.execution_stack.pop();

        // Finally, return the execution result
        res
    }

    /// Inner function that compile the provided source as an LKQL input and
    /// place the result of this compilation in the cache.
    fn compile_lkql_source(
        &mut self,
        source: SourceId,
        timings: &mut Vec<(String, Duration)>,
    ) -> Result<(), Report> {
        // First of all check in the compilation cache whether the source has
        // already been compiled. In that case, don't perform compilation.
        if self.compilation_cache.contains_key(&source) {
            return Ok(());
        }

        // Here we know that the source hasn't been compiled before, so we
        // do the compilation.
        let mut time_point: Instant;

        // Parse the source file
        time_point = Instant::now();
        let unit = self.source_repo.parse_as_lkql(source)?;
        let root = unit.root()?.unwrap();
        timings.push((String::from("parsing"), time_point.elapsed()));

        // If required, display the parsing tree
        if self.config.is_verbose(VerboseElement::ParsingTree) {
            writeln!(self.config.std_out, "===== Parsing tree =====\n")?;
            writeln!(self.config.std_out, "{}\n", root.tree_dump(0)?)?;
        }

        // Lower the parsing tree
        time_point = Instant::now();
        let lowering_tree = ExecutionUnit::lower_lkql_node(source, &root)?;
        timings.push((String::from("lowering"), time_point.elapsed()));

        // If required, display the lowered tree
        if self.config.is_verbose(VerboseElement::LoweringTree) {
            writeln!(self.config.std_out, "===== Lowering tree =====\n")?;
            writeln!(self.config.std_out, "{}\n", lowering_tree)?;
        }

        // Compile the lowering tree to the extended bytecode format
        time_point = Instant::now();
        let extended_bytecode_unit = lowering_tree.compile()?;
        timings.push((String::from("compilation"), time_point.elapsed()));

        // Transform the extended bytecode unit into a standard bytecode unit
        let bytecode_unit = extended_bytecode_unit.to_bytecode_unit(&self.source_repo);

        // If required, display the compiled bytecode
        if self.config.is_verbose(VerboseElement::Bytecode) {
            writeln!(self.config.std_out, "===== Bytecode =====\n")?;
            writeln!(self.config.std_out, "{}\n", bytecode_unit)?;
        }

        // Encode the bytecode unit to a byte buffer
        let mut encoded_bytecode_unit = Vec::new();
        bytecode_unit.encode(&mut encoded_bytecode_unit);

        // If required, display the raw bytecode buffer
        if self.config.is_verbose(VerboseElement::RawBytecode) {
            writeln!(self.config.std_out, "===== Raw bytecode =====\n")?;
            writeln!(self.config.std_out, "{:?}\n", encoded_bytecode_unit.hex_dump())?;
        }

        // Store the compilation result in the cache
        self.compilation_cache
            .insert(source, (extended_bytecode_unit, encoded_bytecode_unit));

        // Finally, return the success
        Ok(())
    }

    /// Util function to show a timing vector in a pretty way.
    fn display_timings(
        &mut self,
        timings: &Vec<(String, Duration)>,
        header: &str,
    ) -> Result<(), Report> {
        let full_header = format!("===== {header} =====");
        writeln!(self.config.std_out, "{full_header}")?;
        let longest_event_name = timings
            .iter()
            .max_by_key(|(name, _)| name.len())
            .unwrap()
            .0
            .len();
        for (event, duration) in timings {
            let duration_min = duration.as_secs() / 60;
            let duration_sec = duration.as_secs() % 60;
            let duration_ms = duration.as_millis() % 1000;
            let fill = " ".repeat(longest_event_name - event.len());
            writeln!(
                self.config.std_out,
                "  {event}:{fill}  {duration_min}m{duration_sec}.{:0>3}s",
                duration_ms
            )?;
        }
        writeln!(self.config.std_out, "{}", "=".repeat(full_header.len()))?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct EngineConfig {
    pub std_out: Writable,
    pub std_err: Writable,
    pub verbose_elements: HashSet<VerboseElement>,
    pub perform_timings: bool,
}

impl EngineConfig {
    pub fn is_verbose(&self, element: VerboseElement) -> bool {
        self.verbose_elements.contains(&element)
            || self.verbose_elements.contains(&VerboseElement::All)
    }
}

/// This type represents different elements that can be written bytes to.
#[derive(Debug)]
pub enum Writable {
    File(File),
    ByteBuffer(Vec<u8>),
}

impl Write for Writable {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Writable::File(file) => file.write(buf),
            Writable::ByteBuffer(buffer) => {
                buffer.extend_from_slice(buf);
                Ok(buf.len())
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Writable::File(file) => file.flush(),
            Writable::ByteBuffer(_) => Ok(()),
        }
    }
}

impl Writable {
    /// Get a writable object for the current process standard output.
    pub fn stdout() -> Self {
        unsafe { Self::File(File::from_raw_fd(1)) }
    }

    /// Get a writable object for the current process standard error output.
    pub fn stderr() -> Self {
        unsafe { Self::File(File::from_raw_fd(2)) }
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
