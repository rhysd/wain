use std::io;
use std::io::Read;
use std::process::exit;

fn main() {
    let mut stdin = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut stdin) {
        eprintln!("Error: {}", e);
        exit(1);
    }
    let _ast = match wain_syntax_text::parse(&stdin) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Error: {}", e);
            exit(1);
        }
    };
}
