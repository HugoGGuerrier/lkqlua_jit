use clap::Parser;
use lkqlua_jit::{Config, ExecutionContext, Timings, VerboseElement, Writable};
use std::{path::PathBuf, time::Duration};

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
    timings: bool,
}

fn main() {
    // Parse CLI arguments
    let args = LauncherArgs::parse();

    // Turn CLI arguments into a context configuration
    let config = Config {
        std_out: Writable::stdout(),
        std_err: Writable::stderr(),
        verbose_elements: args.verbose.into_iter().collect(),
    };

    // Create a new LKQL execution context
    let mut ctx = ExecutionContext::new(config);

    // Run the provided script if any
    if let Some(ref script) = args.script {
        ctx.just_run_lkql_file(&script);
    }

    // If required, display timings collected by the execution context
    if args.timings {
        println!();
        for (source, timings) in ctx.timings {
            display_timings(ctx.source_repo.get_name_by_id(source), &timings);
        }
    }
}

/// Util function to show a timing vector in a pretty way.
fn display_timings(source_name: &str, timings: &Timings) {
    /// Format a duration
    fn format_duration(duration: &Duration) -> String {
        let duration_min = duration.as_secs() / 60;
        let duration_sec = duration.as_secs() % 60;
        let duration_ms = duration.as_millis() % 1000;
        format!("{duration_min}m{duration_sec}.{:0>3}s", duration_ms)
    }

    // Show all time measurements for the given source
    let full_header = format!("===== \"{source_name}\" timings =====");
    println!("{full_header}");
    println!("  parsing:     {}", format_duration(&timings.parsing));
    println!("  lowering:    {}", format_duration(&timings.lowering));
    println!("  compilation: {}", format_duration(&timings.compilation));
    println!("  execution:   {}", format_duration(&timings.execution));
    println!("{}\n", "=".repeat(full_header.len()));
}
