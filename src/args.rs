use clap::Parser;

/// The command line arguments for Amp.
#[derive(Parser)]
#[command(name = "amp", bin_name = "amp", version)]
pub enum Cli {
    Build(Build),
}

/// The command line arguments for the `build` subcommand.
#[derive(clap::Args)]
#[command(author, version, about, long_about = None)]
pub struct Build {
    /// The path to the source file to compile.
    pub input_path: String,

    /// The path to output to.
    #[clap(short)]
    pub output_path: String,

    /// Links the provided file to the executable.
    #[clap(short)]
    pub link: Vec<String>,
}
