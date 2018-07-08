use std::process::exit;

extern crate clap;
extern crate rcc;

const PROGRAM_NAME: &'static str = "rcc";
const VERSION_NUMBER: &'static str = "0.4.0";
const PROGRAM_AUTHOR: &'static str = "Kai Wells <support@kaiwells.me>";
const PROGRAM_DESCRIPTION: &'static str = "A very simple C compiler";

fn main() {
    let matches = clap::App::new(PROGRAM_NAME)
        .version(VERSION_NUMBER)
        .author(PROGRAM_AUTHOR)
        .about(PROGRAM_DESCRIPTION)
        .args_from_usage(
            "<INPUT>      'Source file to compile'
            [-o <OUTPUT>] 'Destination file for compilation'
            -v            'Print debug messages'"
        )
        .subcommand(clap::SubCommand::with_name("lex")
            .about("Just emit the lex'd tokens"))
        .subcommand(clap::SubCommand::with_name("parse")
            .about("Just emit the parsed AST"))
        .get_matches();
    
    let input_file = matches.value_of("INPUT").unwrap_or_else(|| {
        println!("Missing source file");
        exit(1);
    });
    let default_output_file = input_file.to_owned() + ".s";
    let output_file = matches.value_of("OUTPUT").unwrap_or(&default_output_file);
    let verbose = matches.is_present("v");

    if let Some(_) = matches.subcommand_matches("lex") {
        for token in read_file_and_lex(input_file, verbose) {
            println!("{:?}", token);
        }
    } else if let Some(_) = matches.subcommand_matches("parse") {
        let ast = read_file_lex_and_parse(input_file, verbose);
        println!("{:?}", ast);
    } else if let Err(e) = rcc::compile(&input_file, &output_file, verbose) {
        eprintln!("Compilation error: {}", e);
        exit(1);
    }
}

fn read_file_and_lex(input_file: &str, verbose: bool) -> Vec<rcc::lexer::DebugToken> {
    if verbose { println!("Reading source from {}", input_file); }
    let characters = rcc::read_file_bytes(input_file).unwrap_or_else(|e| {
        eprintln!("Could not read {}: {}", input_file, e);
        exit(1);
    });

    if verbose { println!("Lexing {} characters", &characters.len()); }
    
    rcc::lexer::lex(&characters)
}

fn read_file_lex_and_parse(input_file: &str, verbose: bool) -> rcc::parser::Program {
    let tokens = read_file_and_lex(input_file, verbose);
    if verbose { println!("Parsing {} tokens", &tokens.len()); }
    rcc::parser::parse(&tokens).unwrap_or_else(|e| {
        eprintln!("Could not parse tokens: {}", e);
        exit(1);
    })
}