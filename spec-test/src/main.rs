#![forbid(unsafe_code)]

mod crash;
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

struct Options {
    help: bool,
    fast_fail: bool,
    target: Target,
}

fn parse_options() -> Options {
    let mut opts = Options {
        help: false,
        fast_fail: false,
        target: {
            let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // /path/to/wain/spec-test
            p.push("wasm-testsuite");
            Target::Dir(p)
        },
    };

    for arg in env::args().skip(1) {
        match arg.as_str() {
            "-h" | "--help" => {
                opts.help = true;
                break;
            }
            "-f" | "--fast-fail" => {
                opts.fast_fail = true;
            }
            _ => {
                let p = PathBuf::from(arg);
                opts.target = if p.is_dir() {
                    Target::Dir(p)
                } else {
                    let f = p.file_name().and_then(OsStr::to_str).unwrap().to_string();
                    Target::File(p, f)
                };
            }
        }
    }

    opts
}

fn main() -> io::Result<()> {
    let opts = parse_options();
    if opts.help {
        println!("Usage: spec-tester [-f|--fast-fail|-h|--help] [file or dir]");
        exit(0);
    }

    println!("Running tests for {:?}...", opts.target.path());

    let stdout = io::stdout();
    let mut runner = Runner::new(stdout.lock(), opts.fast_fail);
    let success = match &opts.target {
        Target::Dir(dir) => runner.run_dir(dir)?,
        Target::File(path, file) => runner.run_file(path, file)?.success(),
    };
    if !success {
        exit(1);
    }

    Ok(())
}
