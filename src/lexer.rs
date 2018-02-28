use std::fmt;
use std::str;
use std::str::FromStr;
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq)]
pub enum LexTokenKeyword {
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
pub enum LexToken {
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

pub fn lex(src: &[u8]) -> Vec<LexToken> {
    let mut tokens: Vec<LexToken> = Vec::new();
    let mut buffer: Vec<u8> = Vec::new();
    let mut first_char_letter = false;

    let mut single_chars: HashMap<u8, LexToken> = HashMap::new();
    single_chars.insert(0x09, LexToken::Whitespace); // horizontal tab
    single_chars.insert(0x0A, LexToken::Whitespace); // new line
    single_chars.insert(0x0D, LexToken::Whitespace); // carriage return
    single_chars.insert(0x20, LexToken::Whitespace); // space
    single_chars.insert(b'!', LexToken::Exclamation);
    single_chars.insert(b'(', LexToken::LParen);
    single_chars.insert(b')', LexToken::RParen);
    single_chars.insert(b'*', LexToken::Star);
    single_chars.insert(b'+', LexToken::Plus);
    single_chars.insert(b'-', LexToken::Minus);
    single_chars.insert(b'/', LexToken::Slash);
    single_chars.insert(b';', LexToken::Semicolon);
    single_chars.insert(b'[', LexToken::LBracket);
    single_chars.insert(b']', LexToken::RBracket);
    single_chars.insert(b'~', LexToken::Tilde);

    for c in src.iter() {
        match single_chars.get(c) {
            Some(single_char_token) => {
                if buffer.len() > 0 {
                    let buf_copy = buffer.clone();
                    let s = {str::from_utf8(&buf_copy).unwrap()};
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
                if (b'A' <= *c && *c <= b'Z') || (b'a' <= *c && *c <= b'z') {
                    if buffer.len() == 0 {
                        first_char_letter = true;
                    }
                    buffer.push(*c);
                } else if b'0' <= *c && *c <= b'9' {
                    buffer.push(*c);
                } else {
                    tokens.push(LexToken::Unknown(*c));
                }
            },
        }
    }
    return tokens;
}
