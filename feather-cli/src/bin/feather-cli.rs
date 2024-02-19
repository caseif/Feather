use std::fs::File;
use std::io::Read;
use std::process::exit;

use featherast::tokenizer;

const ACTION_TOKENIZE: &str = "tokenize";

const ALL_ACTIONS: [&str; 1] = [
    ACTION_TOKENIZE
];

fn main() {
    let args = std::env::args();
    if args.len() < 2 {
        eprintln!("Usage: feather-cli <action> [args]");
        exit(1);
    }

    let args_vec: Vec<String> = args.collect();
    let action = &args_vec[1];

    match action.as_str() {
        ACTION_TOKENIZE => do_tokenize(args_vec.iter().skip(2).collect()),
        _ => {
            let mut sorted_actions = ALL_ACTIONS.clone();
            sorted_actions.sort();
            eprintln!("Invalid action '{action}'\nPossible actions:");
            for avail_action in sorted_actions {
                eprintln!("    {avail_action}");
            }
        },
    };
}

fn do_tokenize(args: Vec<&String>) {
    if args.len() < 1 {
        eprintln!("Usage: feather-cli tokenize <input file>");
        exit(1);
    }

    let input_file_name = args[0];
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
