use exitcode;
use std::fs::File;
use std::io::{Read, stdout, Write};
use std::path::PathBuf;
use std::process::exit;
use structopt::StructOpt;

use featherast::parser::cst::generate_cst;
use featherast::parser::generate_ast;
use featherast::tokenizer;
use featherast::tokenizer::TokenList;

#[derive(Debug, PartialEq, StructOpt)]
struct TokenizeArgs {
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,
    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,
}

#[derive(Debug, PartialEq, StructOpt)]
struct BuildCstArgs {
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,
    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,
}

#[derive(Debug, PartialEq, StructOpt)]
struct BuildAstArgs {
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,
    #[structopt(short, long, parse(from_os_str))]
    output_file: Option<PathBuf>,
}

#[derive(Debug, PartialEq, StructOpt)]
enum Subcommand {
    #[structopt(about = "Transforms an input file into a sequence of discrete tokens")]
    Tokenize(TokenizeArgs),
    #[structopt(about = "Transforms an input file into a concrete syntax tree")]
    BuildCst(BuildCstArgs),
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
        Subcommand::BuildCst(sub) => do_build_cst(sub),
    }
}

fn read_file_to_string(path: PathBuf) -> String {

    let mut input_file = match File::open(path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("Failed to open input file: {e}");
            exit(exitcode::NOINPUT);
        },
    };
    let mut input_str: String = String::new();
    input_file.read_to_string(&mut input_str).unwrap();
    return input_str;
}

fn tokenize_input(input: &String) -> TokenList {
    match tokenizer::tokenize(&input) {
        Ok(t) => t,
        Err(e) => {
            let caret_line = format!("{: >1$}", "^", e.col);
            eprint!("Unexpected token at input:{}:{}\n{}\n{caret_line}\n", e.line, e.col, e.line_content);
            exit(exitcode::DATAERR);
        },
    }
}

fn open_out_file(file_name: &Option<PathBuf>) -> Box<dyn Write> {
    match file_name {
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
    }
}

fn do_tokenize(args: TokenizeArgs) {
    let input_file_path = args.input_file;
    let input_str: String = read_file_to_string(input_file_path);

    let tokens = tokenize_input(&input_str);

    let out_file = open_out_file(&args.output_file);

    println!("{:?}", tokens.tokens);

    if let Err(e) = serde_json::to_writer_pretty(out_file, &tokens) {
        eprintln!("Failed to write to output file: {e}");
        exit(exitcode::IOERR);
    }
}

fn do_build_cst(args: BuildCstArgs) {
    let input_file_path = args.input_file;
    let input_str: String = read_file_to_string(input_file_path);

    let tokens = tokenize_input(&input_str);

    let out_file = open_out_file(&args.output_file);

    match generate_cst(tokens) {
        Ok(cst) => {
            if let Err(e) = serde_json::to_writer(out_file, &cst) {
                eprintln!("Failed to write to output file: {e}");
                exit(exitcode::IOERR);
            }
        },
        Err(e) => {
            let t = e.next_token;
            eprintln!("Parse failed at {}:{}", t.line, t.col);
        }
    }
}

fn do_build_ast(args: BuildAstArgs) {
    let input_file_path = args.input_file;
    let input_str: String = read_file_to_string(input_file_path);

    let tokens = tokenize_input(&input_str);

    let out_file = open_out_file(&args.output_file);

    match generate_ast(tokens) {
        Ok(ast) => {
            if let Err(e) = serde_json::to_writer(out_file, &ast) {
                eprintln!("Failed to write to output file: {e}");
                exit(exitcode::IOERR);
            }
        },
        Err(e) => {
            let t = e.next_token;
            eprintln!("Parse failed at {}:{}", t.line, t.col);
        }
    }
}
