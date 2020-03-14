use std::io;
use std::io::Read;
use std::process::exit;

fn main() {
    let mut stdin = String::new();
    if let Err(e) = io::stdin().read_to_string(&mut stdin) {
        eprintln!("{}", e);
        exit(1);
    }
    let _ast = match wain_wat::parse(&stdin) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            exit(1);
        }
    };
}
