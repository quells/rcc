use std::clone::Clone;
use std::collections::HashMap;
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
    // let asm = generate(ast);
    // println!("{}", asm);
    //
    // println!("Writing code to {}", config.dest_filename);
    // let mut df = File::create(config.dest_filename)?;
    // df.write((&asm).as_bytes())?;

    Ok(())
}

fn _generate_unop(op: ParseUnOp) -> String {
    match op {
        ParseUnOp::Negation => format!("  neg     %eax\n"),
        ParseUnOp::BitwiseComplement => format!("  not     %eax\n"),
        ParseUnOp::LogicalNegation => format!("  cmpl    $0, %eax\n  movl    $0, %eax\n  sete    %al\n"),
    }
}

fn _generate_expression(expression: ParseExp) -> String {
    match expression {
        _ => panic!("not implemented")
        // ParseExp::Int(i) => {
        //     format!("  movl    ${}, %eax\n", i)
        // },
        // ParseExp::UnOp(op, operand) => {
        //     let operand_asm = _generate_expression(*operand);
        //     let op_asm = _generate_unop(op);
        //     format!("{}{}", operand_asm, op_asm)
        // }
    }
}

fn _generate_statement(statement: ParseStatement) -> String {
    match statement {
        ParseStatement::Return(exp) => {
            let exp_asm = _generate_expression(exp);
            format!("{}  ret\n", exp_asm)
        },
    }
}

fn _generate_function(function: ParseFunction) -> String {
    match function {
        ParseFunction::IntVoid(id, stmt) => {
            let stmt_asm = _generate_statement(stmt);
            format!("_{}\n_{}:\n{}", id, id, stmt_asm)
        },
    }
}

fn _generate_program(program: ParseProgram) -> String {
    let f_asm = match program {
        ParseProgram::Function(f) => _generate_function(f),
    };
    format!("  .global {}", f_asm)
}

fn generate(program: ParseProgram) -> String {
    _generate_program(program)
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
    Term(Box<ParseTerm>),
    BinOp(Box<ParseTerm>, ParseBinOp, Box<ParseTerm>),
}
impl fmt::Display for ParseExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseExp::Term(ref t) => write!(f, "{}", t),
            &ParseExp::BinOp(ref a, ref op, ref b) => write!(f, "({}{}{})", a, op, b),
        }
    }
}

enum ParseTerm {
    Factor(Box<ParseFactor>),
    BinOp(Box<ParseFactor>, ParseBinOp, Box<ParseFactor>),
}
impl fmt::Display for ParseTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseTerm::Factor(ref x) => write!(f, "{}", x),
            &ParseTerm::BinOp(ref a, ref op, ref b) => write!(f, "({}{}{})", a, op, b),
        }
    }
}

enum ParseFactor {
    Exp(Box<ParseExp>),
    UnOp(ParseUnOp, Box<ParseFactor>),
    Int(i32),
}
impl fmt::Display for ParseFactor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseFactor::Exp(ref e) => write!(f, "{}", e),
            &ParseFactor::UnOp(ref op, ref x) => write!(f, "{}{}", op, x),
            &ParseFactor::Int(ref i) => write!(f, "{}", i),
        }
    }
}

enum ParseUnOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}
impl fmt::Display for ParseUnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseUnOp::Negation => write!(f, "-"),
            &ParseUnOp::BitwiseComplement => write!(f, "~"),
            &ParseUnOp::LogicalNegation => write!(f, "!"),
        }
    }
}

enum ParseBinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}
impl fmt::Display for ParseBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseBinOp::Addition => write!(f, "+"),
            &ParseBinOp::Subtraction => write!(f, "-"),
            &ParseBinOp::Multiplication => write!(f, "*"),
            &ParseBinOp::Division => write!(f, "/"),
        }
    }
}

fn _eat(mut tokens: Vec<LexToken>, _expect: LexToken) -> Result<Vec<LexToken>, &'static str> {
    let next_token = tokens.remove(0);
    if matches!(next_token, _expect) {
        return Ok(tokens)
    } else {
        return Err("found unexpected token")
    }
}

fn _parse_factor(mut tokens: Vec<LexToken>) -> Result<(ParseFactor, Vec<LexToken>), &'static str> {
    let first_token = tokens.remove(0);
    match first_token {
        LexToken::LParen => {
            match _parse_exp(tokens) {
                Ok((inner, remaining_tokens)) => {
                    let mut copy_tokens = remaining_tokens.clone();
                    let next_token = copy_tokens.remove(0);
                    match next_token {
                        LexToken::RParen => Ok((ParseFactor::Exp(Box::new(inner)), copy_tokens)),
                        _ => Err("could not parse factor"),
                    }
                },
                Err(e) => Err(e),
            }
        },
        LexToken::Exclamation => {
            match _parse_factor(tokens) {
                Ok((operand, remaining_tokens)) => Ok((ParseFactor::UnOp(ParseUnOp::LogicalNegation, Box::new(operand)), remaining_tokens)),
                Err(e) => Err(e),
            }
        },
        LexToken::Tilde => {
            match _parse_factor(tokens) {
                Ok((operand, remaining_tokens)) => Ok((ParseFactor::UnOp(ParseUnOp::BitwiseComplement, Box::new(operand)), remaining_tokens)),
                Err(e) => Err(e),
            }
        },
        LexToken::Minus => {
            match _parse_factor(tokens) {
                Ok((operand, remaining_tokens)) => Ok((ParseFactor::UnOp(ParseUnOp::Negation, Box::new(operand)), remaining_tokens)),
                Err(e) => Err(e),
            }
        },
        LexToken::Integer(i) => Ok((ParseFactor::Int(i), tokens)),
        _ => Err("could not parse factor"),
    }
}

fn _parse_term(tokens: Vec<LexToken>) -> Result<(ParseTerm, Vec<LexToken>), &'static str> {
    match _parse_factor(tokens) {
        Ok((_lhs, mut remaining_tokens)) => {
            let mut lhs = Box::new(_lhs);
            loop {
                let mut copy_remaining_tokens = remaining_tokens.clone();
                match remaining_tokens.get(0) {
                    Some(next_token) => {
                        match *next_token {
                            LexToken::Star => {
                                let _ = copy_remaining_tokens.remove(0); // star
                                match _parse_factor(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let mult = Box::new(ParseTerm::BinOp(lhs, ParseBinOp::Multiplication, Box::new(rhs)));
                                        lhs = Box::new(ParseFactor::Exp(Box::new(ParseExp::Term(mult))));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("{}", e);
                                        break
                                    }
                                }
                            },
                            LexToken::Slash => {
                                let _ = copy_remaining_tokens.remove(0); // slash
                                match _parse_factor(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let div = Box::new(ParseTerm::BinOp(lhs, ParseBinOp::Division, Box::new(rhs)));
                                        lhs = Box::new(ParseFactor::Exp(Box::new(ParseExp::Term(div))));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("{}", e);
                                        break
                                    }
                                }
                            },
                            _ => break,
                        }
                    },
                    None => break,
                };
                remaining_tokens = copy_remaining_tokens
            };
            Ok((ParseTerm::Factor(lhs), remaining_tokens))
        },
        Err(e) => Err(e)
    }
}

fn _parse_exp(tokens: Vec<LexToken>) -> Result<(ParseExp, Vec<LexToken>), &'static str> {
    match _parse_term(tokens) {
        Ok((_lhs, mut remaining_tokens)) => {
            let mut lhs = Box::new(_lhs);
            loop {
                let mut copy_remaining_tokens = remaining_tokens.clone();
                match remaining_tokens.get(0) {
                    Some(next_token) => {
                        match *next_token {
                            LexToken::Plus => {
                                let _ = copy_remaining_tokens.remove(0); // plus
                                match _parse_term(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let __lhs = ParseFactor::Exp(Box::new(ParseExp::Term(lhs)));
                                        let __rhs = ParseFactor::Exp(Box::new(ParseExp::Term(Box::new(rhs))));
                                        lhs = Box::new(ParseTerm::BinOp(Box::new(__lhs), ParseBinOp::Addition, Box::new(__rhs)));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("{}", e);
                                        break
                                    }
                                }
                            },
                            LexToken::Minus => {
                                let _ = copy_remaining_tokens.remove(0); // minus
                                match _parse_term(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let __lhs = ParseFactor::Exp(Box::new(ParseExp::Term(lhs)));
                                        let __rhs = ParseFactor::Exp(Box::new(ParseExp::Term(Box::new(rhs))));
                                        lhs = Box::new(ParseTerm::BinOp(Box::new(__lhs), ParseBinOp::Subtraction, Box::new(__rhs)));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("{}", e);
                                        break
                                    }
                                }
                            },
                            _ => break,
                        }
                    },
                    None => break,
                };
                remaining_tokens = copy_remaining_tokens;
            };
            Ok((ParseExp::Term(lhs), remaining_tokens))
        },
        Err(e) => Err(e)
    }
}

fn _parse_statement(tokens: Vec<LexToken>) -> Result<(ParseStatement, Vec<LexToken>), &'static str> {
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

fn _parse_function(tokens: Vec<LexToken>) -> Result<(ParseFunction, Vec<LexToken>), &'static str> {
    match _eat(tokens, LexToken::Keyword(LexTokenKeyword::Int)) {
        Ok(mut remaining_tokens) => {
            let fn_id = remaining_tokens.remove(0);
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
        Err(e) => Err(e)
    }
}

fn _parse_program(tokens: Vec<LexToken>) -> Result<(ParseProgram, Vec<LexToken>), &'static str> {
    match _parse_function(tokens) {
        Ok((function, remaining)) => Ok((ParseProgram::Function(function), remaining)),
        Err(err) => Err(err),
    }
}

fn parse(tokens: Vec<LexToken>) -> Result<ParseProgram, &'static str> {
    match _parse_program(tokens) {
        Ok((program, remaining)) => {
            match remaining.len() {
                0 => Ok(program),
                _ => {
                    for t in remaining {
                        println!("{}", t)
                    }
                    Err("unexpected tokens found after parsing program")
                },
            }
        },
        Err(err) => Err(err),
    }
}

#[derive(Copy, Clone, PartialEq)]
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

#[derive(Clone, PartialEq)]
enum LexToken {
    Unknown(u8),
    Keyword(LexTokenKeyword),
    Identifier(String),
    Integer(i32),
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Tilde,
    Exclamation,
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
            &LexToken::Plus => write!(f, "<PLUS>"),
            &LexToken::Minus => write!(f, "<MINUS>"),
            &LexToken::Star => write!(f, "<STAR>"),
            &LexToken::Slash => write!(f, "<SLASH>"),
            &LexToken::Tilde => write!(f, "<TILDE>"),
            &LexToken::Exclamation => write!(f, "<EXCLAMATION>"),
            &LexToken::LParen => write!(f, "<LPAREN>"),
            &LexToken::RParen => write!(f, "<RPAREN>"),
            &LexToken::LBracket => write!(f, "<LBRACKET>"),
            &LexToken::RBracket => write!(f, "<RBRACKET>"),
            &LexToken::Whitespace => write!(f, "<WHITESPACE>"),
        }
    }
}

fn lex(src: &[u8]) -> Vec<LexToken> {
    let mut tokens: Vec<LexToken> = Vec::new();
    let mut buffer: Vec<u8> = Vec::new();
    let mut first_char_letter = false;

    let mut single_chars: HashMap<u8, LexToken> = HashMap::new();
    single_chars.insert(0x09, LexToken::Whitespace);
    single_chars.insert(0x0A, LexToken::Whitespace);
    single_chars.insert(0x0D, LexToken::Whitespace);
    single_chars.insert(0x20, LexToken::Whitespace);
    single_chars.insert(0x21, LexToken::Exclamation);
    single_chars.insert(0x28, LexToken::LParen);
    single_chars.insert(0x29, LexToken::RParen);
    single_chars.insert(0x2A, LexToken::Star);
    single_chars.insert(0x2B, LexToken::Plus);
    single_chars.insert(0x2D, LexToken::Minus);
    single_chars.insert(0x2F, LexToken::Slash);
    single_chars.insert(0x3B, LexToken::Semicolon);
    single_chars.insert(0x7B, LexToken::LBracket);
    single_chars.insert(0x7D, LexToken::RBracket);
    single_chars.insert(0x7E, LexToken::Tilde);

    for c in src.iter() {
        match single_chars.get(c) {
            Some(single_char_token) => {
                if buffer.len() > 0 {
                    let buf_copy = buffer.clone();
                    let s = {std::str::from_utf8(&buf_copy).unwrap()};
                    if first_char_letter {
                        match s {
                            "int" => tokens.push(LexToken::Keyword(LexTokenKeyword::Int)),
                            "return" => tokens.push(LexToken::Keyword(LexTokenKeyword::Return)),
                            _ => tokens.push(LexToken::Identifier(String::from(s))),
                        }
                    } else {
                        if let Ok(n) = i32::from_str(s) {
                            tokens.push(LexToken::Integer(n));
                        } else {
                            panic!("could not tokenize integer: {}", s)
                        }
                    }
                    buffer = Vec::new();
                    first_char_letter = false;
                }
                match single_char_token {
                    &LexToken::Whitespace => (),
                    _ => tokens.push((*single_char_token).clone()),
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
                    tokens.push(LexToken::Unknown(*c));
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
