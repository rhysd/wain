use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::process::exit;

struct Options {
    file: Option<String>,
    help: bool,
}

fn parse_args() -> Result<Options, String> {
    let mut file = None;
    let mut help = false;

    for arg in env::args().skip(1) {
        if arg == "--help" || arg == "-h" {
            help = true;
            break;
        }
        if !arg.ends_with(".wat") {
            return Err(format!("File '{}' does not end with '.wat'. Currently only text format wasm file is supported. See --help", arg));
        }
        if let Some(f) = file {
            return Err(format!(
                "Only one file can be specified for now. But '{}' and '{}' are specified. See --help",
                f, arg
            ));
        }
        file = Some(arg);
    }

    Ok(Options { file, help })
}

fn help() -> ! {
    println!(
        "\
wain: a WebAssembly INterpreter written in Safe Rust with zero dependency

USAGE:
    wain [OPTIONS] [{{file}}]

OPTIONS:
    --help | -h : Show this help

ARGUMENTS:
    Currently one '.wat' file can be specified. If not specified, STDIN will be
    interpreted as text format wasm file.

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

fn read_source(file: &Option<String>) -> Result<String, io::Error> {
    if let Some(file) = file {
        fs::read_to_string(file)
    } else {
        let mut stdin = String::new();
        io::stdin().read_to_string(&mut stdin)?;
        Ok(stdin)
    }
}

fn main() {
    let opts = unwrap("parsing command line", parse_args());

    if opts.help {
        help();
    }

    let source = unwrap("reading input", read_source(&opts.file));
    let ast = unwrap("parsing", wain_syntax_text::parse(&source));
    unwrap("validation", wain_validate::validate(&ast));
    let run = unwrap("running wasm", wain_exec::execute(ast.module));

    if let wain_exec::Run::Warning(msg) = run {
        eprintln!("Warning: {}", msg);
    }
}
