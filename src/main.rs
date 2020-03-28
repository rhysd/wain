use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::process::exit;

enum InputFile {
    Text(String),
    Binary(String),
    Stdin,
}

impl InputFile {
    fn filename(&self) -> Option<&str> {
        match self {
            InputFile::Text(f) => Some(f),
            InputFile::Binary(f) => Some(f),
            InputFile::Stdin => None,
        }
    }
}

struct Options {
    file: InputFile,
    help: bool,
}

fn parse_args() -> Result<Options, String> {
    let mut file = InputFile::Stdin;
    let mut help = false;

    for arg in env::args().skip(1) {
        if arg == "--help" || arg == "-h" {
            help = true;
            break;
        }

        if let Some(f) = file.filename() {
            return Err(format!(
                "Only one file can be specified for now. But '{}' and '{}' are specified. See --help",
                f, arg
            ));
        }

        if arg.ends_with(".wasm") {
            file = InputFile::Binary(arg);
        } else if arg.ends_with(".wat") {
            file = InputFile::Text(arg);
        } else {
            return Err(format!("File '{}' does not end with '.wat'. Currently only text format wasm file is supported. See --help", arg));
        }
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

fn run<'s, S: wain_ast::source::Source>(ast: wain_ast::Root<'s, S>) -> wain_exec::Run {
    unwrap("validation", wain_validate::validate(&ast));
    unwrap("running wasm", wain_exec::execute(ast.module))
}

fn main() {
    let opts = unwrap("parsing command line", parse_args());

    if opts.help {
        help();
    }

    let result = match opts.file {
        InputFile::Binary(file) => {
            let source = unwrap("reading .wasm file", fs::read(file));
            let ast = unwrap("parsing", wain_syntax_binary::parse(&source));
            run(ast)
        }
        InputFile::Text(file) => {
            let source = unwrap("reading .wat file", fs::read_to_string(file));
            let ast = unwrap("parsing", wain_syntax_text::parse(&source));
            run(ast)
        }
        InputFile::Stdin => {
            let mut stdin = String::new();
            unwrap("reading stdin", io::stdin().read_to_string(&mut stdin));
            let ast = unwrap("parsing", wain_syntax_text::parse(&stdin));
            run(ast)
        }
    };

    if let wain_exec::Run::Warning(msg) = result {
        eprintln!("Warning: {}", msg);
    }
}
