use std::path::PathBuf;

use clap::Parser;

/// The command line arguments for Amp.
#[derive(Parser)]
#[command(name = "amp", bin_name = "amp", version)]
pub enum Cli {
    Build(Build),
    Run(Run),
}

/// The command line arguments for the `build` subcommand.
#[derive(clap::Args)]
#[command(author, version, about, long_about = None)]
pub struct Build {
    /// Input files to compile.
    #[clap(required = true)]
    pub input: Vec<String>,

    /// The path to output to.
    #[clap(short)]
    pub output_path: String,

    /// Links the provided file to the executable.
    #[clap(short)]
    pub link: Vec<String>,

    /// A list of directories which the compiler should search for modules.
    #[clap(short = 'I')]
    pub import_dirs: Vec<PathBuf>,
}

/// The command line arguments for the `run` subcommand.
#[derive(clap::Args)]
#[command(author, version, about, long_about = None)]
pub struct Run {
    /// Input files to compile.
    #[clap(required = true)]
    pub input: String,

    /// Links the provided file to the executable.
    #[clap(short)]
    pub link: Vec<String>,

    /// A list of directories which the compiler should search for modules.
    #[clap(short = 'I')]
    pub import_dirs: Vec<PathBuf>,

    /// The arguments to pass to the program.
    pub args: Vec<String>,
}
