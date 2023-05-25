#![forbid(unsafe_code)]

mod crash;
mod error;
mod importer;
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
    write: Option<PathBuf>,
    target: Target,
    allowed_failures: Option<u32>,
}

fn parse_options() -> Result<Options, &'static str> {
    let mut opts = Options {
        help: false,
        fast_fail: false,
        write: None,
        target: {
            let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR")); // /path/to/wain/spec-test
            p.push("wasm-testsuite");
            Target::Dir(p)
        },
        allowed_failures: None,
    };

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" | "--help" => {
                opts.help = true;
                break;
            }
            "-f" | "--fast-fail" => {
                opts.fast_fail = true;
            }
            "-w" | "--write-summary" => {
                if let Some(path) = args.next() {
                    opts.write = Some(PathBuf::from(path));
                } else {
                    return Err("-w or --write-summary must be followed by file path argument");
                }
            }
            "--allowed-failures" => {
                if let Some(arg) = args.next() {
                    if let Ok(n) = arg.parse() {
                        opts.allowed_failures = Some(n);
                    } else {
                        return Err("--allowed-failures must be followed by uint argument");
                    }
                } else {
                    return Err("--allowed-failures must be followed by uint argument");
                }
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

    Ok(opts)
}

fn main() -> io::Result<()> {
    let opts = match parse_options() {
        Ok(opts) => opts,
        Err(msg) => {
            println!("{}. Please see --help", msg);
            exit(1);
        }
    };

    if opts.help {
        println!(
            "Usage: spec-tester [-f|--fast-fail|-w {{file}}|--write-summary {{file}}|-h|--help] [{{file}}|{{dir}}]"
        );
        exit(0);
    }

    println!("Running tests for {:?}...", opts.target.path());

    let stdout = io::stdout();
    let mut runner = Runner::new(stdout.lock(), opts.fast_fail, opts.write);
    let summary = match &opts.target {
        Target::Dir(dir) => runner.run_dir(dir)?,
        Target::File(path, file) => runner.run_file(path, file)?,
    };
    if let Some(allowed) = opts.allowed_failures {
        let actual = summary.failures();
        if allowed < actual {
            eprintln!(
                "Expected {} or fewer failures but actually got {} failures",
                allowed, actual
            );
            exit(1);
        }
    } else if summary.success() {
        println!("🎊");
    } else {
        exit(1);
    }

    Ok(())
}
