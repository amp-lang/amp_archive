//! Resolves import paths.

use std::path::PathBuf;

use path_absolutize::Absolutize;

/// A trait for resolving import paths.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Importer {
    /// A list of directories which can hold imported files.
    pub import_dirs: Vec<PathBuf>,
}

impl Importer {
    /// Creates a new `Importer` with the given list of import directories.
    pub fn new(import_dirs: Vec<PathBuf>) -> Self {
        Self { import_dirs }
    }

    /// Resolves a module in the provided directory.
    fn resolve_for_dir(dir: &PathBuf, name: &str) -> Option<PathBuf> {
        let mut file = dir.clone();
        file.push(name);
        file.set_extension("amp");

        if file.exists() {
            Some(
                match file.canonicalize() {
                    Ok(path) => path,
                    _ => return None,
                }
                .to_path_buf(),
            )
        } else {
            file.set_extension("");

            if file.is_dir() {
                file.push("Main.amp");

                if file.exists() {
                    return Some(file.absolutize().unwrap().to_path_buf());
                }
            }

            None
        }
    }

    /// Resolves the given import path to a file path.
    pub fn resolve(&self, parent: &PathBuf, path: &str) -> Option<PathBuf> {
        if let Some(file) = Self::resolve_for_dir(parent, path) {
            return Some(file);
        }

        for dir in &self.import_dirs {
            if let Some(file) = Self::resolve_for_dir(dir, path) {
                return Some(file);
            }
        }

        None
    }
}
