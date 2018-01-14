use std::env;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::error::Error;
use std::str::FromStr;
use std::process::exit;
use std::collections::HashMap;
use std::collections::LinkedList;

pub fn run(config: Config) -> Result<(), Box<Error>> {
    let mut f = File::open(config.src_filename)?;

    let mut contents = String::new();
    f.read_to_string(&mut contents)?;

    let characters = (&contents).as_bytes();
    let tokens = lex(characters);
    for t in tokens.iter() {
        println!("{}", t);
    }

    Ok(())
}

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
