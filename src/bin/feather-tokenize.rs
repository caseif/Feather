use std::fs::File;
use std::io::Read;
use std::process::exit;

use feather_compiler::ast::tokenizer;

fn main() {
    let args = std::env::args();
    if args.len() < 2 {
        eprintln!("Usage: feather-tokenize [file]");
        exit(1);
    }

    let input_file_name = args.skip(1).next().unwrap();
    let mut input_file = File::open(input_file_name).unwrap();
    let mut input_str: String = String::new();
    input_file.read_to_string(&mut input_str).unwrap();

    let tokens = match tokenizer::tokenize(&input_str) {
        Ok(t) => t,
        Err(e) => {
            let caret_line = format!("{: >1$}", "^", e.col);
            eprint!("Unexpected token at input:{}:{}\n{}\n{caret_line}\n", e.line, e.col, e.line_content);
            exit(1);
        },
    };

    for token in tokens {
        println!("{}", token);
    }
}
