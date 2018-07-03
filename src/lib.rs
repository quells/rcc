use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::io::prelude::*;

pub mod config;
mod lexer;
mod parser;
mod generator_gas;

pub fn run(config: config::Config) -> Result<(), Box<Error>> {
    println!("Reading source from {}", config.src_filename);
    let mut sf = File::open(config.src_filename)?;
    let mut contents = String::new();
    sf.read_to_string(&mut contents)?;
    let characters = (&contents).as_bytes();

    println!("Lexing {} characters", characters.len());
    let tokens = lexer::lex(characters);

    println!("Parsing {} tokens", tokens.len());
    let ast = parser::parse(tokens)?;

    println!("Generating code for: {:?}", ast);
    let asm = generator_gas::generate(ast);
    println!("{}", asm);

    println!("Writing code to {}", config.dest_filename);
    let mut df = File::create(config.dest_filename)?;
    df.write((&asm).as_bytes())?;

    Ok(())
}
