use codespan_reporting::{
    diagnostic::Diagnostic,
    files::SimpleFiles,
    term::{
        self,
        termcolor::{Color, ColorChoice, ColorSpec, StandardStream},
        Chars, Config, DisplayStyle, Styles,
    },
};

/// Returns the configuration that is used for displaying diagnostics.
pub fn config() -> Config {
    let mut intense = ColorSpec::new();
    intense.set_bold(true);
    intense.set_intense(true);

    let mut red = intense.clone();
    red.set_fg(Some(Color::Red));
    let mut yellow = intense.clone();
    yellow.set_fg(Some(Color::Yellow));
    let mut green = intense.clone();
    green.set_fg(Some(Color::Green));
    let mut cyan = intense.clone();
    cyan.set_fg(Some(Color::Cyan));
    let mut white = intense.clone();
    white.set_fg(Some(Color::White));

    Config {
        display_style: DisplayStyle::Rich,
        tab_width: 4,
        styles: Styles {
            header_bug: red.clone(),
            header_error: red.clone(),
            header_help: green.clone(),
            header_note: green.clone(),
            header_warning: yellow.clone(),
            header_message: white.clone(),
            primary_label_bug: red.clone(),
            primary_label_error: red.clone(),
            primary_label_help: green.clone(),
            primary_label_note: green.clone(),
            primary_label_warning: yellow.clone(),
            secondary_label: cyan.clone(),
            line_number: cyan.clone(),
            source_border: cyan.clone(),
            note_bullet: cyan.clone(),
        },
        chars: Chars::ascii(),
        start_context_lines: 3,
        end_context_lines: 1,
    }
}

pub fn display_diagnostic(files: &SimpleFiles<String, String>, diagnostic: &Diagnostic<usize>) {
    let config = config();
    let mut stdout = StandardStream::stderr(ColorChoice::Auto);
    term::emit(&mut stdout, &config, files, diagnostic).unwrap();
}
