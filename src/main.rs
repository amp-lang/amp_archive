use std::{process::ExitCode};

use args::Cli;
use ast::Source;
use clap::Parser;
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles};
use error::Error;
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

fn build_path(file_id: FileId, files: &SimpleFiles<String, String>, input: &str) -> Result<Option<NamedTempFile>, Error> {
    let scanner = Scanner::new(file_id, input);
    let mut parser = parser::Parser::new(scanner);
    let mut checker = Typechecker::new();
    if let Some(res) = parser.parse::<Source>() {
        dbg!(&res);
        checker.check(&res?)?;
    };


    let mut codegen = Codegen::new();
    codegen.compile(checker);
    let binary = codegen.finish();

    let file = NamedTempFile::new().unwrap();
    if let Err(_) = std::fs::write(&file, binary) {
        diagnostic::display_diagnostic(
            &files,
            &Diagnostic::error().with_message("could not write object file"),
        );
        return Ok(None);
    }
    

    Ok(Some(file))
}

fn main() -> ExitCode {
    let Cli::Build(mut args) = Cli::parse();

    let mut files = SimpleFiles::new();
    let mut object_files: Vec<NamedTempFile> = Vec::new(); // compiled object files

    for input in args.input {
        let Ok(src) = std::fs::read_to_string(&input) 
        else {
            diagnostic::display_diagnostic(&files, &Diagnostic::error().with_message("Could not read source file"));
            return ExitCode::FAILURE;
        };

        let id = files.add(input, src);
        let file_id = FileId::new(id);

        object_files.push(match build_path(file_id, &files, files.get(id).unwrap().source()) {
            Ok(res) => match res {
                Some(file) => file,
                None => return ExitCode::FAILURE,
            },
            Err(err) => {
                diagnostic::display_diagnostic(&files, &err.as_diagnostic());
                return ExitCode::FAILURE;
            }
        })
    }

    for file in &object_files {
        args.link.push(file.path().to_str().unwrap().to_owned());
    }

    if let Err(_) = linker::link(&args.link, args.output_path) {
        diagnostic::display_diagnostic(&files, &Diagnostic::error().with_message("linking failed"));
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}
