use codespan_reporting::term::{
    termcolor::{Color, ColorSpec},
    Chars, Config, DisplayStyle, Styles,
};

// pub const RED: ColorSpec = {
//     let red = ColorSpec::new();
//     red
// };

// // pub const CONFIG: Config = Config {
// //     display_style: DisplayStyle::Rich,
// //     tab_width: 4,
// //     styles: Styles {

// //     }
// // };
// // }

pub fn config() -> Config {
    let mut red = ColorSpec::new();
    red.set_fg(Some(Color::Red));
    red.set_bold(true);
    red.set_intense(true);

    let mut yellow = ColorSpec::new();
    yellow.set_fg(Some(Color::Yellow));
    yellow.set_bold(true);
    yellow.set_intense(true);

    let mut green = ColorSpec::new();
    green.set_fg(Some(Color::Green));
    green.set_bold(true);
    green.set_intense(true);

    let mut cyan = ColorSpec::new();
    cyan.set_fg(Some(Color::Cyan));
    cyan.set_bold(true);
    cyan.set_intense(true);

    let mut white = ColorSpec::new();
    white.set_fg(Some(Color::White));
    white.set_bold(true);
    white.set_intense(true);

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
