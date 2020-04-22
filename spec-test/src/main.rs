#![forbid(unsafe_code)]

mod error;
mod parser;
mod runner;
mod wast;

use runner::Runner;
use std::env;
use std::ffi::OsStr;
use std::io;
use std::path::{Path, PathBuf};
use std::process::exit;

#[derive(Debug)]
enum Target {
    File(PathBuf, String),
    Dir(PathBuf),
}

impl Target {
    fn path(&self) -> &Path {
        match self {
            Target::File(p, _) => p,
            Target::Dir(p) => p,
        }
    }
}

fn main() -> io::Result<()> {
    let target = if let Some(arg) = env::args().nth(1) {
        if arg == "-h" || arg == "--help" {
            println!("Usage: spec-tester [file or dir]");
            exit(0);
        }
        let p = PathBuf::from(arg);
        if p.is_dir() {
            Target::Dir(p)
        } else {
            let f = p.file_name().and_then(OsStr::to_str).unwrap().to_string();
            Target::File(p, f)
        }
    } else {
        // /path/to/wain/spec-test
        let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        p.push("wasm-testsuite");
        Target::Dir(p)
    };

    println!("Running tests for {:?}...", target.path());

    let stdout = io::stdout();
    let mut runner = Runner::new(stdout.lock());
    let success = match target {
        Target::Dir(dir) => runner.run_dir(&dir)?,
        Target::File(path, file) => runner.run_file(&path, &file)?.success(),
    };
    if !success {
        exit(1);
    }

    Ok(())
}
