use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::io::prelude::*;

pub mod lexer;
pub mod parser;
pub mod generator_gas;

pub fn run(input: &str, output: &str, verbose: bool) -> Result<(), Box<Error>> {
    if verbose { println!("Reading source from {}", input); }
    let mut src_file = File::open(input)?;
    let mut src_text = String::new();
    src_file.read_to_string(&mut src_text)?;
    let characters = (&src_text).as_bytes();

    if verbose { println!("Lexing {} characters", &characters.len()); }
    let tokens = lexer::lex(characters);

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
