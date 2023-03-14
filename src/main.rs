use std::process::ExitCode;

use args::Cli;
use ast::Source;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use scanner::Scanner;
use span::FileId;
use typechecker::Typechecker;

use crate::codegen::Codegen;

pub mod args;
pub mod ast;
pub mod codegen;
pub mod diagnostic;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod span;
pub mod typechecker;

fn main() -> ExitCode {
    let Cli::Build(args) = Cli::parse();

    let mut files = SimpleFiles::new();

    // Load the source file
    // TODO: read from command line arguments
    let src = std::fs::read_to_string(&args.input_path).unwrap();
    let file_id = FileId::new(files.add(args.input_path, src));

    let scanner = Scanner::new(file_id, files.get(file_id.0 as usize).unwrap().source());
    let mut parser = parser::Parser::new(scanner);

    let res = if let Some(res) = parser.parse::<Source>() {
        match res {
            Ok(value) => value,
            Err(value) => {
                let config = diagnostic::config();
                let mut stdout = StandardStream::stderr(term::termcolor::ColorChoice::Auto);
                term::emit(&mut stdout, &config, &files, &value.as_diagnostic()).unwrap();
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
            let config = diagnostic::config();
            let mut stdout = StandardStream::stderr(term::termcolor::ColorChoice::Auto);
            term::emit(&mut stdout, &config, &files, &value.as_diagnostic()).unwrap();
            return ExitCode::FAILURE;
        }
    }
    // dbg!(checker);

    let mut codegen = Codegen::new();
    codegen.compile(checker);
    std::fs::write(args.output_path, codegen.finish()).unwrap();

    ExitCode::SUCCESS
}
