use std::process::ExitCode;

use ast::Func;
use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use parser::Parser;
use scanner::Scanner;
use span::FileId;

pub mod ast;
pub mod diagnostic;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod span;

fn main() -> ExitCode {
    let mut exit_code = ExitCode::SUCCESS;
    let mut files = SimpleFiles::new();

    // Load the source file
    // TODO: read from command line arguments
    let src = std::fs::read_to_string("test.amp").unwrap();
    let file_id = FileId::new(files.add("test.amp", src));

    let scanner = Scanner::new(file_id, files.get(file_id.0 as usize).unwrap().source());
    let mut parser = Parser::new(scanner);

    match parser.parse::<Func>() {
        Some(res) => match res {
            Ok(value) => println!("{:#?}", value),
            Err(value) => {
                exit_code = ExitCode::FAILURE;

                let config = diagnostic::config();
                let mut stdout = StandardStream::stderr(term::termcolor::ColorChoice::Auto);
                term::emit(&mut stdout, &config, &files, &value.as_diagnostic()).unwrap()
            }
        },
        None => {}
    }

    // while let Some(next_char) = scanner.next() {
    //     match next_char {
    // Ok(value) => println!("{:?} \"{}\"", value, scanner.slice()),
    // Err(value) => {
    //     exit_code = ExitCode::FAILURE;

    //     let config = diagnostic::config();
    //     let mut stdout = StandardStream::stderr(term::termcolor::ColorChoice::Auto);
    //     term::emit(&mut stdout, &config, &files, &value.as_diagnostic()).unwrap()
    // }
    //     }
    // }

    exit_code
}
