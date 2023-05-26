#![forbid(unsafe_code)]
#![warn(clippy::dbg_macro)]

use std::env;
use std::fmt;
use std::fs;
use std::io::{self, Read};
use std::process::exit;

enum InputOption {
    Text(String),
    Binary(String),
    Stdin,
}

enum Input {
    Text(String),
    Binary(Vec<u8>),
}

impl InputOption {
    fn filename(&self) -> Option<&str> {
        match self {
            InputOption::Text(f) => Some(f),
            InputOption::Binary(f) => Some(f),
            InputOption::Stdin => None,
        }
    }

    fn read(&self) -> io::Result<Input> {
        match self {
            InputOption::Text(f) => Ok(Input::Text(fs::read_to_string(f)?)),
            InputOption::Stdin => {
                let mut stdin = vec![];
                io::stdin().read_to_end(&mut stdin)?;
                if stdin.starts_with(&[0x00, 0x61, 0x73, 0x6d]) {
                    Ok(Input::Binary(stdin))
                } else {
                    match String::from_utf8(stdin) {
                        Ok(s) => Ok(Input::Text(s)),
                        Err(e) => Err(io::Error::new(io::ErrorKind::InvalidData, e)),
                    }
                }
            }
            InputOption::Binary(f) => Ok(Input::Binary(fs::read(f)?)),
        }
    }
}

struct Options {
    file: InputOption,
    help: bool,
    validate_only: bool,
    version: bool,
}

fn parse_args() -> Result<Options, String> {
    let mut file = InputOption::Stdin;
    let mut help = false;
    let mut validate_only = false;
    let mut version = false;

    for arg in env::args().skip(1) {
        if arg == "--help" || arg == "-h" {
            help = true;
            break;
        }

        if arg == "--version" || arg == "-v" {
            version = true;
            break;
        }

        if arg == "--validate-only" {
            validate_only = true;
            continue;
        }

        if let Some(f) = file.filename() {
            return Err(format!(
                "Only one file can be specified for now. But '{}' and '{}' are specified. See --help",
                f, arg
            ));
        }

        if arg.ends_with(".wasm") {
            file = InputOption::Binary(arg);
            continue;
        }

        if arg.ends_with(".wat") {
            file = InputOption::Text(arg);
            continue;
        }

        return Err(format!(
            "File '{}' ends with neither '.wasm' nor '.wat'. See --help",
            arg
        ));
    }

    Ok(Options {
        file,
        help,
        version,
        validate_only,
    })
}

fn help() -> ! {
    println!(
        "\
wain: a WebAssembly INterpreter written in Safe Rust with zero dependencies

USAGE:
    wain [OPTIONS] [{{file}}]

OPTIONS:
    --validate-only : Only validate the Wasm module
    --help | -h     : Show this help
    --version | -v  : Show version

ARGUMENTS:
    Currently only a single '.wat' file or '.wasm' file can be specified.
    If no file is given, STDIN will be interpreted as binary or text Wasm
    module. wain automatically detects binary-format or text-format from the
    input.

REPOSITORY:
    https://github.com/rhysd/wain
"
    );
    exit(0);
}

fn unwrap<T, E: fmt::Display>(phase: &'static str, result: Result<T, E>) -> T {
    match result {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error on {}: {}", phase, e);
            exit(1);
        }
    }
}

fn interpret<S: wain_ast::source::Source>(ast: wain_ast::Root<'_, S>, validate_only: bool) {
    unwrap("validation", wain_validate::validate(&ast));
    if !validate_only {
        unwrap("executing wasm module", wain_exec::execute(&ast.module));
    }
}

fn main() {
    let opts = unwrap("parsing command line", parse_args());

    if opts.help {
        help();
    }

    if opts.version {
        println!("{}", env!("CARGO_PKG_VERSION"));
        exit(0);
    }

    match unwrap("reading input", opts.file.read()) {
        #[cfg(feature = "binary")]
        Input::Binary(bin) => interpret(
            unwrap("parsing binary format", wain_syntax_binary::parse(&bin)),
            opts.validate_only,
        ),
        #[cfg(feature = "text")]
        Input::Text(text) => interpret(
            unwrap("parsing text format", wain_syntax_text::parse(&text)),
            opts.validate_only,
        ),
        #[cfg(not(all(feature = "binary", feature = "text")))]
        input => {
            let format = match input {
                Input::Text(_) => "text",
                Input::Binary(_) => "binary",
            };
            eprintln!("Executing {0} format is unsupported. Build with '{0}' feature", format,);
            exit(1);
        }
    }
}
