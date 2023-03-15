use std::{path::Path, process::Command};

/// Links the provided files together using GCC.
pub fn link(files: &[String], output: String) {
    let mut cmd = Command::new("gcc");

    cmd.args(files).args([
        "-o".to_owned(),
        match std::env::consts::EXE_EXTENSION {
            "" => output,
            extension => Path::new(&output)
                .with_extension(extension)
                .to_string_lossy()
                .to_string(),
        },
    ]);

    cmd.status().expect("failed to execute process");
}
