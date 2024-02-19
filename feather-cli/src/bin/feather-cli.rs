use std::fs::File;
use std::io::{Read, stdout, Write};
use std::path::PathBuf;
use std::process::exit;
use exitcode;
use structopt::StructOpt;

use featherast::tokenizer;

#[derive(Debug, PartialEq, StructOpt)]
struct TokenizeArgs {
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,
    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,
}

#[derive(Debug, PartialEq, StructOpt)]
#[structopt(about = "Transforms an input file into a sequence of discrete tokens")]
enum Subcommand {
    Tokenize(TokenizeArgs),
}

#[derive(Debug, PartialEq, StructOpt)]
#[structopt(name = "feather-cli", about = "Utilities for working with Feather sources")]
struct GeneralArgs {
    #[structopt(subcommand)]
    subcommand: Subcommand,
}

fn main() {
    let args = GeneralArgs::from_args();
    match args.subcommand {
        Subcommand::Tokenize(sub) => do_tokenize(sub),
    }
}

fn do_tokenize(args: TokenizeArgs) {
    let input_file_path = args.input_file;
    let mut input_file = match File::open(input_file_path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to open input file: {e}");
            exit(exitcode::NOINPUT);
        },
    };
    let mut input_str: String = String::new();
    input_file.read_to_string(&mut input_str).unwrap();

    let tokens = match tokenizer::tokenize(&input_str) {
        Ok(t) => t,
        Err(e) => {
            let caret_line = format!("{: >1$}", "^", e.col);
            eprint!("Unexpected token at input:{}:{}\n{}\n{caret_line}\n", e.line, e.col, e.line_content);
            exit(exitcode::DATAERR);
        },
    };

    let out_file = match args.output_file {
        Some(path) => {
            match File::create(path) {
                Ok(f) => Box::new(f) as Box<dyn Write>,
                Err(e) => {
                    eprintln!("Failed to open output file: {e}");
                    exit(exitcode::CANTCREAT);
                },
            }
        },
        None => Box::new(stdout()) as Box<dyn Write>,
    };

    if let Err(e) = serde_json::to_writer_pretty(out_file, &tokens) {
        eprintln!("Failed to write to output file: {e}");
        exit(exitcode::IOERR);
    }
}
