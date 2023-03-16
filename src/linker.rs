use std::{
    path::Path,
    process::{Command, ExitStatus},
};

/// Links the provided files together using GCC.
pub fn link(files: &[String], output: String) -> std::io::Result<ExitStatus> {
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

    cmd.status()
}
