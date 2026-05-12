//! # Source lowering module
//!
//! This module is the base of all the source lowering process. Its purpose is
//! to hold all common logic used by source lowering passes.

use crate::{ExecutionContext, report::Report, sources::SourceId};
use std::collections::HashMap;

pub mod lkql;

/// This structure is used to hold the context during a lowering pass.
struct LoweringContext<'a, T> {
    /// Execution context the lowering takes place in.
    execution_context: &'a ExecutionContext,

    /// The source that is currently being lowered.
    lowered_source: SourceId,

    /// Map each execution unit declaration node to the its index in its parent
    /// children unit array.
    child_index_map: HashMap<T, u16>,

    /// Counter of encountered lambdas, used for naming them.
    lambda_counter: usize,

    /// Counter of encountered list comprehension, used for naming their
    /// execution units.
    lazy_comprehension_counter: usize,

    /// Counter of encountered selector sub-pattern, used for naming their
    /// execution units.
    selector_pattern_counter: usize,

    /// Counter of created temporary values.
    tmp_counter: usize,

    /// The list of diagnostics emitted during the lowering.
    diagnostics: Vec<Report>,
}

impl<'a, T> LoweringContext<'a, T> {
    pub fn new<'b>(execution_context: &'b ExecutionContext, lowered_source: SourceId) -> Self
    where
        'b: 'a,
    {
        Self {
            execution_context,
            lowered_source,
            child_index_map: HashMap::new(),
            lambda_counter: 0,
            lazy_comprehension_counter: 0,
            selector_pattern_counter: 0,
            tmp_counter: 0,
            diagnostics: Vec::new(),
        }
    }

    /// Get the next available lambda name, incrementing the counter.
    fn next_lambda_name(&mut self) -> String {
        let res = format!("#lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        res
    }

    /// Get the next available list comprehension name, incrementing the
    /// counter.
    fn next_lazy_comprehension_name(&mut self) -> String {
        let res = format!("#lazy_comprehension_{}", self.lazy_comprehension_counter);
        self.lazy_comprehension_counter += 1;
        res
    }

    /// Get the next available name for a selector sub-pattern, incrementing
    /// the counter.
    fn next_selector_pattern_name(&mut self) -> String {
        let res = format!("#selector_pattern_{}", self.selector_pattern_counter);
        self.selector_pattern_counter += 1;
        res
    }

    /// Get a fresh temporary value identifier.
    fn new_tmp_id(&mut self) -> usize {
        let res = self.tmp_counter;
        self.tmp_counter += 1;
        res
    }
}

/// Unescape the provided string following LKQL escaping sequences.
fn unescape_string(string: &str) -> String {
    string
        .replace("\\n", "\n")
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\\"", "\"")
        .replace("\\'", "'")
        .replace("\\\\", "\\")
}
