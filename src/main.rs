use std::io;
use std::io::Read;
use std::process::exit;

fn main() {
    let mut stdin = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut stdin) {
        eprintln!("Error: {}", e);
        exit(1);
    }
    let ast = match wain_syntax_text::parse(&stdin) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Error: {}", e);
            exit(1);
        }
    };
    if let Err(e) = wain_validate::validate(&ast) {
        eprintln!("Error: {}", e);
        exit(1);
    }
    if let Some(e) = wain_exec::execute(ast.module) {
        eprintln!("Error: {}", e);
        exit(1);
    }
}
