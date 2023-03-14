use std::process::ExitCode;

use ast::Source;
use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use parser::Parser;
use scanner::Scanner;
use span::FileId;
use typechecker::Typechecker;

pub mod ast;
pub mod diagnostic;
pub mod error;
pub mod parser;
pub mod scanner;
pub mod span;
pub mod typechecker;

fn main() -> ExitCode {
    let mut files = SimpleFiles::new();

    // Load the source file
    // TODO: read from command line arguments
    let src = std::fs::read_to_string("test.amp").unwrap();
    let file_id = FileId::new(files.add("test.amp", src));

    let scanner = Scanner::new(file_id, files.get(file_id.0 as usize).unwrap().source());
    let mut parser = Parser::new(scanner);

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
    dbg!(checker);

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

    ExitCode::SUCCESS
}
