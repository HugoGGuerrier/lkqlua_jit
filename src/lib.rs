//! # LKQLua JIT
//!
//! The library entry point of the LKQL engine, here you can find all required
//! stuff to parse, compile and execute LKQL sources.

use std::{collections::HashSet, io::Write, path::Path};

use clap::ValueEnum;

use crate::{intermediate_tree::ExecutionUnit, report::Report, sources::SourceRepository};

pub mod bytecode;
pub mod intermediate_tree;
pub mod lua;
pub mod report;
pub mod sources;

/// This type holds all required data to run LKQL sources using LuaJIT as a
/// backend. This is what you have to use.
pub struct ExecutionContext<O: Write, E: Write> {
    source_repo: SourceRepository,
    config: EngineConfig<O, E>,
}

impl<O: Write, E: Write> ExecutionContext<O, E> {
    /// Create an initialize a new execution context.
    pub fn new(config: EngineConfig<O, E>) -> Self {
        Self { source_repo: SourceRepository::new(), config }
    }

    /// Just run the provided LKQL file, don't return anything and report all
    /// diagnosis and messages in the [`EngineConfig::std_err`] of the related
    /// configuration.
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

        // Parse the source file
        let unit = self.source_repo.parse_as_lkql(&source)?;
        let root = unit.root()?.unwrap();

        // If required, display the parsing tree
        if self.config.is_verbose(VerboseElement::ParsingTree) {
            writeln!(self.config.std_out, "===== Parsing tree =====\n")?;
            writeln!(self.config.std_out, "{}\n", root.tree_dump(0)?)?;
        }

        // Lower the parsing tree
        let lowering_tree = ExecutionUnit::lower_lkql_node(&root)?;

        // If required, display the lowered tree
        if self.config.is_verbose(VerboseElement::LoweringTree) {
            writeln!(self.config.std_out, "===== Lowering tree =====\n")?;
            writeln!(self.config.std_out, "{}\n", lowering_tree.borrow())?;
        }

        // Compile the lowering tree and execute it
        let bytecode_buffer = lowering_tree.borrow().compile()?;

        // If required, display the compiled bytecode
        if self.config.is_verbose(VerboseElement::Bytecode) {
            writeln!(self.config.std_out, "===== Bytecode =====\n")?;
            writeln!(self.config.std_out, "{}\n", bytecode_buffer)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct EngineConfig<O: Write, E: Write> {
    pub std_out: O,
    pub std_err: E,
    pub verbose_elements: HashSet<VerboseElement>,
}

impl<O: Write, E: Write> EngineConfig<O, E> {
    pub fn is_verbose(&self, element: VerboseElement) -> bool {
        self.verbose_elements.contains(&element)
            || self.verbose_elements.contains(&VerboseElement::All)
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
}
