use std::process::ExitCode;

use codespan_reporting::{
    files::SimpleFiles,
    term::{self, termcolor::StandardStream},
};
use scanner::Scanner;
use span::FileId;

pub mod diagnostic;
pub mod error;
pub mod scanner;
pub mod span;

fn main() -> ExitCode {
    let src = std::fs::read_to_string("test.amp").unwrap();
    let mut files = SimpleFiles::new();
    let file_id = FileId::new(files.add("test.amp", src));
    let mut scanner = Scanner::new(file_id, files.get(file_id.0 as usize).unwrap().source());

    let mut exit_code = ExitCode::SUCCESS;

    while let Some(next_char) = scanner.next() {
        match next_char {
            Ok(value) => println!("{:?} \"{}\"", value, scanner.slice()),
            Err(value) => {
                exit_code = ExitCode::FAILURE;
                let mut stdout = StandardStream::stderr(term::termcolor::ColorChoice::Auto);
                let config = diagnostic::config();
                term::emit(&mut stdout, &config, &files, &value.as_diagnostic()).unwrap()
            }
        }
    }

    exit_code
}
