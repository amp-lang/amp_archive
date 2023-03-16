use std::{process::ExitCode, time::Instant};

use args::Cli;
use ast::Source;
use clap::Parser;
use codespan_reporting::{
    diagnostic::Diagnostic,
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use scanner::Scanner;
use span::FileId;
use tempfile::NamedTempFile;
use typechecker::Typechecker;

use crate::codegen::Codegen;

pub mod args;
pub mod ast;
pub mod codegen;
pub mod diagnostic;
pub mod error;
pub mod linker;
pub mod parser;
pub mod scanner;
pub mod span;
pub mod typechecker;

fn main() -> ExitCode {
    let Cli::Build(mut args) = Cli::parse();

    let mut files = SimpleFiles::new();

    // Load the source file
    let Ok(src) = std::fs::read_to_string(&args.input_path) else {
        diagnostic::display_diagnostic(&files, &Diagnostic::error().with_message("Could not read source file"));
        return ExitCode::FAILURE;
    };

    // Time the compilation
    let start_time = Instant::now();

    let file_id = FileId::new(files.add(args.input_path, src));
    let scanner = Scanner::new(file_id, files.get(file_id.0 as usize).unwrap().source());
    let mut parser = parser::Parser::new(scanner);

    let res = if let Some(res) = parser.parse::<Source>() {
        match res {
            Ok(value) => value,
            Err(value) => {
                diagnostic::display_diagnostic(&files, &value.as_diagnostic());
                return ExitCode::FAILURE;
            }
        }
    } else {
        // TODO: handle empty source
        return ExitCode::SUCCESS;
    };

    let mut checker = Typechecker::new();
    match checker.check(&res) {
        Ok(_) => {}
        Err(value) => {
            diagnostic::display_diagnostic(&files, &value.as_diagnostic());
            return ExitCode::FAILURE;
        }
    }

    let mut codegen = Codegen::new();
    codegen.compile(checker);
    let binary = codegen.finish();

    let file = NamedTempFile::new().unwrap();
    if let Err(_) = std::fs::write(&file, binary) {
        diagnostic::display_diagnostic(
            &files,
            &Diagnostic::error().with_message("could not write object file"),
        );
        return ExitCode::FAILURE;
    }
    args.link.push(file.path().to_str().unwrap().to_owned());
    if let Err(_) = linker::link(&args.link, args.output_path) {
        diagnostic::display_diagnostic(&files, &Diagnostic::error().with_message("linking failed"));
        return ExitCode::FAILURE;
    }

    let compile_time = start_time.elapsed().as_nanos() as f64 / 1_000_000.0;

    println!("Binary written in {}ms", compile_time);

    ExitCode::SUCCESS
}
