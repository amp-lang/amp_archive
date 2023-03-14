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

use crate::codegen::Codegen;

pub mod ast;
pub mod codegen;
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
    // dbg!(checker);

    let mut codegen = Codegen::new();
    codegen.compile(checker);
    std::fs::write("test.o", codegen.finish()).unwrap();

    ExitCode::SUCCESS
}
