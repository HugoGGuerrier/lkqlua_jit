use ariadne::Fmt;
use clap::{
    Parser,
    builder::{
        Styles,
        styling::{AnsiColor, Color, Style},
    },
};
use lkqlua_jit::{
    Config, ExecutionContext, Timings, VerboseElement, Writable, diagnostics::ERROR_KIND_COLOR,
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

    #[arg(long, short, help = "Display additional information during the run")]
    verbose: Vec<VerboseElement>,

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
        verbose_elements: args.verbose.into_iter().collect(),
        analyzed_lang_name: args.lang_name,
        files_to_analyze: args.analyzed_files,
        additional_args: args.engine_args,
    };

    // Create a new execution context
    let t = ExecutionContext::new(config);
    let mut ctx = match t {
        Ok(ctx) => ctx,
        Err(messages) => {
            let header = "Error: ".fg(ERROR_KIND_COLOR);
            messages.iter().for_each(|m| eprintln!("{header}{m}"));
            return;
        }
    };

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
