use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::io::prelude::*;

pub mod lexer;
pub mod parser;
pub mod generator_gas;

pub fn read_file_bytes(filename: &str) -> Result<Vec<u8>, Box<Error>> {
    let mut src_file = File::open(filename)?;
    let mut src_text = String::new();
    src_file.read_to_string(&mut src_text)?;
    Ok((&src_text).as_bytes().to_vec())
}

pub fn compile(input: &str, output: &str, verbose: bool) -> Result<(), Box<Error>> {
    if verbose { println!("Reading source from {}", input); }
    let characters = read_file_bytes(input)?;

    if verbose { println!("Lexing {} characters", &characters.len()); }
    let tokens: Vec<lexer::Token> = lexer::lex(&characters)
        .into_iter()
        .map(|t| t.token )
        .collect();

    if verbose { println!("Parsing {} tokens", &tokens.len()); }
    let ast = parser::parse(&tokens)?;

    if verbose { println!("Generating code for: {:?}", ast); }
    let asm = generator_gas::generate(ast);
    if verbose { println!("{}", asm); }

    if verbose { println!("Writing code to {}", output); }
    let mut dest_file = File::create(output)?;
    dest_file.write((&asm).as_bytes())?;

    Ok(())
}
