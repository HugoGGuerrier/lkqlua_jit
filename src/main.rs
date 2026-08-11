use clap::{
    Parser,
    builder::{
        Styles,
        styling::{AnsiColor, Color, Style},
    },
};
use lkqlua_jit::{
    Config, DebugElement, ExecutionContext, Timings, Writable, sources::SourceRepository,
};
use std::{
    io::{stderr, stdout},
    path::PathBuf,
    time::Duration,
};

#[derive(Parser, Debug)]
#[command(about = "Run LKQL scripts", long_about = None, version, styles = get_styles())]
struct LauncherArgs {
    #[arg(help = "Source files to analyze", value_parser = check_is_file)]
    analyzed_files: Vec<PathBuf>,

    #[arg(long, short = 'S', help = "LKQL file to run", value_name = "FILE", value_parser = check_is_file)]
    script: Option<PathBuf>,

    #[arg(
        long,
        short,
        help = "Name of the language to analyze",
        value_name = "LANG_NAME",
        default_value = "ada"
    )]
    lang_name: String,

    #[arg(long, short, help = "Display debug information during the run")]
    debug: Vec<DebugElement>,

    #[arg(
        long,
        short,
        help = "Perform time measurements during compilation and run, and display those information"
    )]
    timings: bool,

    #[arg(long, short, help = "Enable profiling information collection and display")]
    profiling: bool,

    #[arg(
        help = "Additional options for the LKQL engine",
        allow_hyphen_values = true,
        last = true
    )]
    engine_args: Vec<String>,
}

/// Get a styles descriptor destined to clap.
fn get_styles() -> Styles {
    Styles::styled()
        .header(Style::new().bold().underline())
        .usage(
            Style::new()
                .bold()
                .italic()
                .fg_color(Some(Color::Ansi(AnsiColor::BrightBlack))),
        )
        .placeholder(Style::new().dimmed())
        .context_value(Style::new().italic().dimmed())
        .literal(Style::new().bold())
        .valid(Style::new().fg_color(Some(Color::Ansi(AnsiColor::Green))))
        .error(
            Style::new()
                .bold()
                .fg_color(Some(Color::Ansi(AnsiColor::BrightRed))),
        )
}

/// Validator used to make sure a provided file argument is actually an
/// existing file.
fn check_is_file(s: &str) -> Result<PathBuf, String> {
    let maybe_res = PathBuf::from(s).canonicalize();
    if let Ok(res) = maybe_res
        && res.is_file()
    {
        Ok(res)
    } else {
        Err(format!("Cannot find file \"{s}\""))
    }
}

fn main() {
    // Parse CLI arguments
    let args = LauncherArgs::parse();

    // Turn CLI arguments into a context configuration
    let config = Config {
        std_out: Writable::Stdout(stdout()),
        std_err: Writable::Stderr(stderr()),
        do_profiling: args.profiling,
        debug_elements: args.debug.into_iter().collect(),
        analyzed_lang_name: args.lang_name,
        files_to_analyze: args.analyzed_files,
        additional_args: args.engine_args,
    };

    // Create the source repository
    let mut source_repo = SourceRepository::new();

    // Create a new execution context
    let t = ExecutionContext::new(config, &mut source_repo);
    let mut ctx = match t {
        Ok(ctx) => ctx,
        Err(diagnostics) => {
            diagnostics
                .into_iter()
                .for_each(|diag| diag.print(&source_repo, &mut stderr(), false));
            return;
        }
    };

    // Run the provided script if any and display errors if there are some
    if let Some(ref script) = args.script
        && let Err(diagnostics) = ctx.execute_lkql_script(script)
    {
        for diag in &diagnostics {
            diag.print(ctx.source_repo, &mut ctx.config.std_err, false);
        }
    }

    // If required, display timings collected by the execution context
    if args.timings {
        println!();
        header("Timings");
        let mut sorted_sources = ctx.timings.keys().collect::<Vec<_>>();
        sorted_sources.sort();
        for source in sorted_sources {
            display_timings(
                ctx.source_repo.get_source_by_id(*source).unwrap().name(),
                ctx.timings.get(source).unwrap(),
            );
        }
    }

    // If required display profiling data
    if args.profiling {
        // Show the profiling section title
        println!();
        header("Profiling result");

        // Create a list of sources with profiling data and sort them by
        // samples count.
        let mut profiled_sources = ctx.profiling_data.source_data.iter().collect::<Vec<_>>();
        profiled_sources.sort_by_key(|(_, d)| d.total_sample_count);

        // For each source display its part of the total time and all functions
        // in it.
        for (source_name, source_data) in profiled_sources.into_iter().rev() {
            println!();
            println!(
                "{source_name} ({}%):",
                percentage(source_data.total_sample_count, ctx.profiling_data.total_sample_count)
            );

            // Now get all function profiled in the source and sort them by
            // samples count.
            let mut profiled_functions = source_data.function_data.iter().collect::<Vec<_>>();
            profiled_functions.sort_by_key(|(_, d)| d.total_sample_count);

            // For each function, display its time portion and its lines
            for (function_name, function_data) in profiled_functions.into_iter().rev() {
                println!(
                    "  {function_name} ({}%):",
                    percentage(function_data.total_sample_count, source_data.total_sample_count)
                );

                // Then, get all lines that have profiling data and sort them
                // by samples count.
                let mut profiled_lines = function_data.line_counters.iter().collect::<Vec<_>>();
                profiled_lines.sort_by_key(|(_, d)| *d);

                // For each line, display it with its portion
                for (line_num, sample_count) in profiled_lines.into_iter().rev() {
                    println!(
                        "    line {line_num} - {}%",
                        percentage(*sample_count, function_data.total_sample_count)
                    );
                }
            }
        }
    }
}

/// Display a section header
fn header(section_title: &str) {
    println!("===== {section_title} =====");
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
    println!();
    println!("{source_name}:");
    println!("  parsing:     {}", format_duration(&timings.parsing));
    println!("  lowering:    {}", format_duration(&timings.lowering));
    println!("  compilation: {}", format_duration(&timings.compilation));
    println!("  execution:   {}", format_duration(&timings.execution));
}

/// Util function to get the percentage that `portion` represents of the
/// `total` as a rounded integer.
fn percentage(portion: u128, total: u128) -> u8 {
    ((portion * 100) as f64 / total as f64).round() as u8
}
