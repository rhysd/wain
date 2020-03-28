use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::process::exit;

#[allow(dead_code)]
enum InputFile {
    Text(String),
    Binary(String),
    Stdin,
}

#[allow(dead_code)]
impl InputFile {
    fn filename(&self) -> Option<&str> {
        match self {
            InputFile::Text(f) => Some(f),
            InputFile::Binary(f) => Some(f),
            InputFile::Stdin => None,
        }
    }

    fn read_text(&self) -> io::Result<String> {
        match self {
            InputFile::Text(f) => fs::read_to_string(f),
            InputFile::Stdin => {
                let mut stdin = String::new();
                io::stdin().read_to_string(&mut stdin)?;
                Ok(stdin)
            }
            InputFile::Binary(_) => unreachable!(),
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
            #[cfg(feature = "binary")]
            {
                file = InputFile::Binary(arg);
                continue;
            }
        }

        if arg.ends_with(".wat") {
            #[cfg(feature = "text")]
            {
                file = InputFile::Text(arg);
                continue;
            }
        }

        return Err(format!(
            "File '{}' does not end with '.wasm' nor '.wat'. See --help",
            arg
        ));
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
    Currently one '.wat' file or '.wasm' file can be specified. If not
    specified, STDIN will be interpreted as text format wasm file.

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

fn run<S: wain_ast::source::Source>(ast: wain_ast::Root<'_, S>) -> wain_exec::Run {
    unwrap("validation", wain_validate::validate(&ast));
    unwrap("running wasm", wain_exec::execute(ast.module))
}

#[cfg(feature = "binary")]
fn run_binary(file: &str) -> wain_exec::Run {
    let source = unwrap("reading .wasm file", fs::read(file));
    let ast = unwrap("parsing", wain_syntax_binary::parse(&source));
    run(ast)
}
#[cfg(not(feature = "binary"))]
fn run_binary(_: &str) -> wain_exec::Run {
    unreachable!()
}

#[cfg(feature = "text")]
fn run_text(file: InputFile) -> wain_exec::Run {
    let source = unwrap("reading text file", file.read_text());
    let ast = unwrap("parsing", wain_syntax_text::parse(&source));
    run(ast)
}
#[cfg(not(feature = "text"))]
fn run_text(_: InputFile) -> wain_exec::Run {
    unreachable!()
}

fn main() {
    let opts = unwrap("parsing command line", parse_args());

    if opts.help {
        help();
    }

    let result = match &opts.file {
        InputFile::Binary(file) => run_binary(file),
        InputFile::Text(_) => run_text(opts.file),
        InputFile::Stdin => run_text(opts.file),
    };

    if let wain_exec::Run::Warning(msg) = result {
        eprintln!("Warning: {}", msg);
    }
}
