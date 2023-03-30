use std::{
    path::Path,
    process::{Command, ExitCode},
};

use args::Cli;
use ast::Source;
use clap::Parser;
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles};
use error::Error;
use import::Importer;
use include_dir::include_dir;
use path_absolutize::Absolutize;
use scanner::Scanner;
use span::FileId;
use tempfile::NamedTempFile;
use typechecker::{
    module::{Module, ModuleId},
    Typechecker,
};

use crate::codegen::Codegen;

pub mod args;
pub mod ast;
pub mod codegen;
pub mod diagnostic;
pub mod error;
pub mod import;
pub mod linker;
pub mod parser;
pub mod scanner;
pub mod span;
pub mod typechecker;

fn build_path(
    file_id: FileId,
    files: &mut SimpleFiles<String, String>,
    importer: &Importer,
    input: &str,
) -> Result<Option<NamedTempFile>, Error> {
    let scanner = Scanner::new(file_id, input);
    let mut parser = parser::Parser::new(scanner);
    let mut checker = Typechecker::new();
    if let Some(res) = parser.parse::<Source>() {
        let module = Module::new(
            ModuleId(0),
            files.get(file_id.0 as usize).unwrap().name().to_string(),
            res?,
        );
        checker.check(module, files, importer)?;
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

static LIB: include_dir::Dir = include_dir!("lib");

fn main() -> ExitCode {
    match Cli::parse() {
        Cli::Build(mut args) => {
            // Load standard libraries
            let tmp = tempfile::tempdir().unwrap();
            LIB.extract(&tmp).unwrap();

            // Add the standard libraries to library search paths
            let lib_dir = tmp.path();
            let runtime = lib_dir.join("_Runtime.amp");
            args.import_dirs.push(lib_dir.to_path_buf());
            args.input.push(runtime.to_string_lossy().to_string());

            let mut files = SimpleFiles::new();
            let mut object_files: Vec<NamedTempFile> = Vec::new(); // compiled object files

            let importer = Importer::new(args.import_dirs);

            for input in args.input {
                let Ok(src) = std::fs::read_to_string(&input)
                else {
                    diagnostic::display_diagnostic(
                        &files,
                        &Diagnostic::error()
                            .with_message(format!("Could not read source file {}", input))
                    );
                    return ExitCode::FAILURE;
                };

                let id = files.add(
                    Path::new(&input)
                        .absolutize()
                        .unwrap()
                        .to_string_lossy()
                        .to_string(),
                    src.clone(),
                );
                let file_id = FileId::new(id);

                object_files.push(match build_path(file_id, &mut files, &importer, &src) {
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
                diagnostic::display_diagnostic(
                    &files,
                    &Diagnostic::error().with_message("linking failed"),
                );
                return ExitCode::FAILURE;
            }

            ExitCode::SUCCESS
        }
        Cli::Run(mut args) => {
            let mut input = vec![args.input];

            // Load standard libraries
            let tmp = tempfile::tempdir().unwrap();
            LIB.extract(&tmp).unwrap();

            // Add the standard libraries to library search paths
            let lib_dir = tmp.path();
            let runtime = lib_dir.join("_Runtime.amp");
            args.import_dirs.push(lib_dir.to_path_buf());
            input.push(runtime.to_string_lossy().to_string());

            let mut files = SimpleFiles::new();
            let mut object_files: Vec<NamedTempFile> = Vec::new(); // compiled object files

            let importer = Importer::new(args.import_dirs);

            for input in input {
                let Ok(src) = std::fs::read_to_string(&input)
                else {
                    diagnostic::display_diagnostic(
                        &files,
                        &Diagnostic::error()
                            .with_message(format!("Could not read source file {}", input))
                    );
                    return ExitCode::FAILURE;
                };

                let id = files.add(
                    Path::new(&input)
                        .absolutize()
                        .unwrap()
                        .to_string_lossy()
                        .to_string(),
                    src.clone(),
                );
                let file_id = FileId::new(id);

                object_files.push(match build_path(file_id, &mut files, &importer, &src) {
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

            let output_path = NamedTempFile::new().unwrap();
            if let Err(_) =
                linker::link(&args.link, output_path.path().to_string_lossy().to_string())
            {
                diagnostic::display_diagnostic(
                    &files,
                    &Diagnostic::error().with_message("linking failed"),
                );
                return ExitCode::FAILURE;
            }

            let output_path = output_path.path().to_str().unwrap().to_owned();
            match Command::new(output_path).args(args.args).status() {
                Ok(_) => {}
                Err(_) => {
                    diagnostic::display_diagnostic(
                        &files,
                        &Diagnostic::error().with_message("program failed"),
                    );
                    return ExitCode::FAILURE;
                }
            }

            ExitCode::SUCCESS
        }
    }
}
