use std::clone::Clone;
use std::collections::HashMap;
use std::collections::LinkedList;
use std::env;
use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;
use std::str::FromStr;

// https://stackoverflow.com/questions/25576748/how-to-compare-enum-without-pattern-matching
macro_rules! matches(
    ($e:expr, $p:pat) => {
        match $e {
            $p => true,
            _ => false
        }
    }
);

pub fn run(config: Config) -> Result<(), Box<Error>> {
    println!("Reading source from {}", config.src_filename);
    let mut sf = File::open(config.src_filename)?;
    let mut contents = String::new();
    sf.read_to_string(&mut contents)?;
    let characters = (&contents).as_bytes();

    println!("Lexing {} characters", characters.len());
    let tokens = lex(characters);

    println!("Parsing {} tokens", tokens.len());
    let ast = parse(tokens)?;

    println!("Generating code for:\n{}", ast);
    let asm = generate(ast);
    println!("{}", asm);

    println!("Writing code to {}", config.dest_filename);
    let mut df = File::create(config.dest_filename)?;
    df.write((&asm).as_bytes())?;

    Ok(())
}

fn generate(program: ParseProgram) -> String {
    let mut asm = String::new();
    match program {
        ParseProgram::Function(f) => {
            match f {
                ParseFunction::IntVoid(id, stmt) => {
                    asm = format!("{}  .global _{}\n_{}:\n", asm, id, id);
                    match stmt {
                        ParseStatement::Return(exp) => {
                            match exp {
                                ParseExp::Int(i) => {
                                    asm = format!("{}  movl    ${}, %eax\n  ret\n", asm, i);
                                }
                            }
                        }
                    }
                }
            }
        }
    };
    return asm
}

enum ParseProgram {
    Function(ParseFunction),
}
impl fmt::Display for ParseProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseProgram::Function(ref func) => write!(f, "{}", func),
        }
    }
}

enum ParseFunction {
    IntVoid(String, ParseStatement), // identifier, statement
}
impl fmt::Display for ParseFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseFunction::IntVoid(ref id, ref s) => write!(f, "int {}:<{}>", id, s),
        }
    }
}

enum ParseStatement {
    Return(ParseExp),
}
impl fmt::Display for ParseStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseStatement::Return(ref e) => write!(f, "return({})", e),
        }
    }
}

enum ParseExp {
    Int(i32),
}
impl fmt::Display for ParseExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseExp::Int(ref i) => write!(f, "{}", i),
        }
    }
}

fn _eat(mut tokens: LinkedList<LexToken>, _expect: LexToken) -> Result<LinkedList<LexToken>, &'static str> {
    match tokens.pop_front() {
        Some(token) => {
            if matches!(token, _expect) {
                return Ok(tokens)
            }
            // println!("expected {}, found {}", expect, token);
            return Err("found unexpected token")
        },
        None => Err("reached end of tokens")
    }
}

fn _parse_exp(mut tokens: LinkedList<LexToken>) -> Result<(ParseExp, LinkedList<LexToken>), &'static str> {
    match tokens.pop_front() {
        Some(exp_val) => {
            match exp_val {
                LexToken::Integer(val) => Ok((ParseExp::Int(val), tokens)),
                _ => Err("missing expected integer")
            }
        },
        None => Err("missing expected expression value")
    }
}

fn _parse_statement(tokens: LinkedList<LexToken>) -> Result<(ParseStatement, LinkedList<LexToken>), &'static str> {
    // eventually: parse assignment
    match _eat(tokens, LexToken::Keyword(LexTokenKeyword::Return)) {
        Ok(remaining_tokens) => {
            match _parse_exp(remaining_tokens) {
                Ok((parsed_exp, remaining_tokens)) => {
                    match _eat(remaining_tokens, LexToken::Semicolon) {
                        Ok(remaining_tokens) => Ok((ParseStatement::Return(parsed_exp), remaining_tokens)),
                        Err(e) => Err(e)
                    }
                },
                Err(e) => Err(e)
            }
        },
        Err(e) => Err(e)
    }
}

fn _parse_function(tokens: LinkedList<LexToken>) -> Result<(ParseFunction, LinkedList<LexToken>), &'static str> {
    match _eat(tokens, LexToken::Keyword(LexTokenKeyword::Int)) {
        Ok(mut remaining_tokens) => {
            match remaining_tokens.pop_front() {
                Some(fn_id) => {
                    match fn_id {
                        LexToken::Identifier(id) => {
                            match _eat(remaining_tokens, LexToken::LParen) {
                                Ok(remaining_tokens) => {
                                    // eventually: parse function paramenters
                                    match _eat(remaining_tokens, LexToken::RParen) {
                                        Ok(remaining_tokens) => {
                                            match _eat(remaining_tokens, LexToken::LBracket) {
                                                Ok(remaining_tokens) => {
                                                    // eventually: parse list of statements
                                                    match _parse_statement(remaining_tokens) {
                                                        Ok((parsed_statement, remaining_tokens)) => {
                                                            match _eat(remaining_tokens, LexToken::RBracket) {
                                                                Ok(remaining_tokens) => Ok((ParseFunction::IntVoid(id, parsed_statement), remaining_tokens)),
                                                                Err(e) => Err(e)
                                                            }
                                                        },
                                                        Err(e) => Err(e)
                                                    }
                                                },
                                                Err(e) => Err(e)
                                            }
                                        },
                                        Err(e) => Err(e)
                                    }
                                },
                                Err(e) => Err(e)
                            }
                        },
                        _ => Err("missing expected identifier token")
                    }
                },
                None => Err("missing expected function identifier")
            }
        },
        Err(e) => Err(e)
    }
}

fn _parse_program(tokens: LinkedList<LexToken>) -> Result<(ParseProgram, LinkedList<LexToken>), &'static str> {
    match _parse_function(tokens) {
        Ok((function, remaining)) => Ok((ParseProgram::Function(function), remaining)),
        Err(err) => Err(err),
    }
}

fn parse(tokens: LinkedList<LexToken>) -> Result<ParseProgram, &'static str> {
    match _parse_program(tokens) {
        Ok((program, remaining)) => {
            match remaining.len() {
                0 => Ok(program),
                _ => Err("unexpected tokens found after parsing program"),
            }
        },
        Err(err) => Err(err),
    }
}

#[derive(PartialEq)]
enum LexTokenKeyword {
    Int,
    Return,
}

impl fmt::Display for LexTokenKeyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LexTokenKeyword::Int => write!(f, "<INT>"),
            &LexTokenKeyword::Return => write!(f, "<RETURN>"),
        }
    }
}

#[derive(PartialEq)]
enum LexToken {
    Unknown(u8),
    Keyword(LexTokenKeyword),
    Identifier(String),
    Integer(i32),
    Semicolon,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Whitespace,
}

impl fmt::Display for LexToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LexToken::Unknown(ref c) => write!(f, "<UNKNOWN: {}>", c),
            &LexToken::Keyword(ref keyword) => write!(f, "<KEYWORD: {}>", keyword),
            &LexToken::Identifier(ref id) => write!(f, "<ID: {}>", id),
            &LexToken::Integer(ref i) => write!(f, "<INT: {}>", i),
            &LexToken::Semicolon => write!(f, "<SEMICOLON>"),
            &LexToken::LParen => write!(f, "<LPAREN>"),
            &LexToken::RParen => write!(f, "<RPAREN>"),
            &LexToken::LBracket => write!(f, "<LBRACKET>"),
            &LexToken::RBracket => write!(f, "<RBRACKET>"),
            &LexToken::Whitespace => write!(f, "<WHITESPACE>"),
        }
    }
}

// fn LexTokensEqual(a: LexToken, b: LexToken) -> bool {
//     match a {
//         LexToken::Unknown => match b {
//             LexToken::Unknown => true,
//             _ => false
//         },
//         LexToken::Keyword(ka) => match b {
//             LexToken::Keyword(kb) => ka == kb,
//             _ => false
//         },
//         Identifier => match b {
//             Identifier => true,
//             _ => false
//         },
//         Integer => match b {
//             Integer => true,
//             _ => false
//         },
//         Semicolon => match b {
//             Semicolon => true,
//             _ => false
//         },
//         LParen => match b {
//             LParen => true,
//             _ => false
//         },
//         RParen => match b {
//             RParen => true,
//             _ => false
//         },
//         LBracket => match b {
//             LBracket => true,
//             _ => false
//         },
//         RBracket => match b {
//             RBracket => true,
//             _ => false
//         },
//         Whitespace => match b {
//             Whitespace => true,
//             _ => false
//         }
//     }
// }

fn lex(src: &[u8]) -> LinkedList<LexToken> {
    let mut tokens: LinkedList<LexToken> = LinkedList::new();
    let mut buffer: Vec<u8> = Vec::new();
    let mut first_char_letter = false;

    let mut single_chars: HashMap<u8, LexToken> = HashMap::new();
    single_chars.insert(0x09, LexToken::Whitespace);
    single_chars.insert(0x0A, LexToken::Whitespace);
    single_chars.insert(0x0D, LexToken::Whitespace);
    single_chars.insert(0x20, LexToken::Whitespace);
    single_chars.insert(0x28, LexToken::LParen);
    single_chars.insert(0x29, LexToken::RParen);
    single_chars.insert(0x3B, LexToken::Semicolon);
    single_chars.insert(0x7B, LexToken::LBracket);
    single_chars.insert(0x7D, LexToken::RBracket);

    for c in src.iter() {
        match single_chars.get(c) {
            Some(single_char_token) => {
                if buffer.len() > 0 {
                    let buf_copy = buffer.clone();
                    let s = {std::str::from_utf8(&buf_copy).unwrap()};
                    if first_char_letter {
                        match s {
                            "int" => tokens.push_back(LexToken::Keyword(LexTokenKeyword::Int)),
                            "return" => tokens.push_back(LexToken::Keyword(LexTokenKeyword::Return)),
                            _ => tokens.push_back(LexToken::Identifier(String::from(s))),
                        }
                    } else {
                        if let Ok(n) = i32::from_str(s) {
                            tokens.push_back(LexToken::Integer(n));
                        } else {
                            panic!("could not parse integer: {}", s)
                        }
                    }
                    buffer = Vec::new();
                    first_char_letter = false;
                }
                match single_char_token {
                    &LexToken::Whitespace => (),
                    &LexToken::LParen => tokens.push_back(LexToken::LParen),
                    &LexToken::RParen => tokens.push_back(LexToken::RParen),
                    &LexToken::Semicolon => tokens.push_back(LexToken::Semicolon),
                    &LexToken::LBracket => tokens.push_back(LexToken::LBracket),
                    &LexToken::RBracket => tokens.push_back(LexToken::RBracket),
                    _ => panic!("single char token not implemented for {}", single_char_token),
                }
            },
            None => {
                if (0x41 <= *c && *c <= 0x5A) || (0x61 <= *c && *c <= 0x7A) {
                    if buffer.len() == 0 {
                        first_char_letter = true;
                    }
                    buffer.push(*c);
                } else if 0x30 <= *c && *c <= 0x39 {
                    buffer.push(*c);
                } else {
                    tokens.push_back(LexToken::Unknown(*c));
                }
            },
        }
    }
    return tokens;
}

fn usage_info() {
    let args: Vec<String> = env::args().collect();
    let exe_filename: String = args[0].clone();
    println!("Usage:");
    println!("{} src.c [-o dest.s]", exe_filename);
    println!("  Compile C file into assembly");
    println!("  src.c:  Source C file");
    println!("  dest.c: Destination assembly file");
    println!("");
    println!("{} -h", exe_filename);
    println!("  Display this message");
    exit(0)
}

pub struct Config {
    src_filename: String,
    dest_filename: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
        // expecting: cc src.c -o dest.s
        // if: cc src.c, then: cc src.c -o src.s

        match args.len() {
            0 => Err("how did you get here?"),
            1 => {
                println!("Error: no input file provided");
                usage_info();
                Err("") // program should exit before here, but this makes the compiler happy
            },
            2 => {
                let src_filename = args[1].clone();
                if src_filename == "-h" {
                    usage_info()
                }
                let dest_filename = {
                    let src_path = Path::new(&src_filename);
                    src_path.with_extension("s").to_string_lossy().into_owned()
                };
                Ok(Config { src_filename, dest_filename })
            },
            4 => {
                let src_filename = args[1].clone();
                let flag = args[2].clone();
                if flag != "-o" {
                    return Err("unexpected flag in second argument")
                }
                let dest_filename = args[3].clone();
                Ok(Config { src_filename, dest_filename })
            },
            _ => {
                println!("Error: unexpected number of arguments");
                usage_info();
                Err("") // program should exit before here, but this makes the compiler happy
            },
        }
    }
}
