use std::{
    io::{stderr, stdout},
    path::PathBuf,
};

use clap::Parser;
use lkqlua_jit::{EngineConfig, ExecutionContext, VerboseElement};

#[derive(Parser, Debug)]
#[command(about)]
struct LauncherArgs {
    #[arg(long, short = 'S', help = "LKQL file to run", value_name = "FILE")]
    script: Option<PathBuf>,

    #[arg(long, short, help = "Display additional information during the run")]
    verbose: Vec<VerboseElement>,

    #[arg(
        long,
        short,
        help = "Perform time measurements during compilation and run, and display those information"
    )]
    timing: bool,
}

fn main() {
    // Parse CLI arguments
    let args = LauncherArgs::parse();

    // Turn CLI arguments into an engine configuration
    let config = EngineConfig {
        std_out: stdout(),
        std_err: stderr(),
        verbose_elements: args.verbose.into_iter().collect(),
        perform_timings: args.timing,
    };

    // Create a new LKQL execution context
    let mut ctx = ExecutionContext::new(config);

    // Run the provided script if any
    if let Some(ref script) = args.script {
        ctx.just_run_lkql_file(&script);
    }
}
