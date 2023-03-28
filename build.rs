use std::{env, path::PathBuf};

fn main() {
    println!("cargo:rerun-if-changed=lib");

    // Copy libraries to the output directory
    let profile = env::var("PROFILE").unwrap();
    let lib_in = PathBuf::from("lib");
    let lib_out = PathBuf::from(format!("target/{}", profile));

    // make sure the old library is removed
    if lib_in.exists() {
        fs_extra::dir::remove(lib_out.join("lib")).unwrap();
    }
    fs_extra::dir::copy(&lib_in, &lib_out, &fs_extra::dir::CopyOptions::new()).unwrap();
}
