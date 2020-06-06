#![forbid(unsafe_code)]

use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::process::exit;

#[allow(dead_code)]
enum InputOption {
    Text(String),
    Binary(String),
    Stdin,
}

enum Input {
    Text(String),
    Binary(Vec<u8>),
}

#[allow(dead_code)]
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
    version: bool,
}

fn parse_args() -> Result<Options, String> {
    let mut file = InputOption::Stdin;
    let mut help = false;
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

        if let Some(f) = file.filename() {
            return Err(format!(
                "Only one file can be specified for now. But '{}' and '{}' are specified. See --help",
                f, arg
            ));
        }

        if arg.ends_with(".wasm") {
            #[cfg(feature = "binary")]
            {
                file = InputOption::Binary(arg);
                continue;
            }
        }

        if arg.ends_with(".wat") {
            #[cfg(feature = "text")]
            {
                file = InputOption::Text(arg);
                continue;
            }
        }

        return Err(format!(
            "File '{}' does not end with '.wasm' nor '.wat'. See --help",
            arg
        ));
    }

    Ok(Options {
        file,
        help,
        version,
    })
}

fn help() -> ! {
    println!(
        "\
wain: a WebAssembly INterpreter written in Safe Rust with zero dependencies

USAGE:
    wain [OPTIONS] [{{file}}]

OPTIONS:
    --help | -h    : Show this help
    --version | -v : Show version

ARGUMENTS:
    Currently one '.wat' file or '.wasm' file can be specified. If no file is
    specified, STDIN will be interpreted as binary or text. wain automatically
    detect binary-format or text-format from the input.

REPOSITORY:
    https://github.com/rhysd/wain
"
    );
    exit(0);
}

fn unwrap<T, E: std::fmt::Display>(phase: &'static str, result: Result<T, E>) -> T {
    match result {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error on {}: {}", phase, e);
            exit(1);
        }
    }
}

fn run<S: wain_ast::source::Source>(ast: wain_ast::Root<'_, S>) {
    unwrap("validation", wain_validate::validate(&ast));
    unwrap("running wasm", wain_exec::execute(&ast.module))
}

#[cfg(feature = "binary")]
fn run_binary(bin: Vec<u8>) {
    run(unwrap("parsing", wain_syntax_binary::parse(&bin)))
}
#[cfg(not(feature = "binary"))]
fn run_binary(_: Vec<u8>) {
    unimplemented!("running binary format is not supported since built without 'binary' feature")
}

#[cfg(feature = "text")]
fn run_text(text: String) {
    run(unwrap("parsing", wain_syntax_text::parse(&text)))
}
#[cfg(not(feature = "text"))]
fn run_text(_: String) {
    unimplemented!("running text format is not supported since built without 'text' feature")
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
        Input::Binary(bin) => run_binary(bin),
        Input::Text(text) => run_text(text),
    }
}
