use std::fmt;
use std::str;
use std::str::FromStr;
use std::collections::HashMap;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum KeywordToken {
    Int,
    Return,
}
impl fmt::Debug for KeywordToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &KeywordToken::Int => write!(f, "<INT>"),
            &KeywordToken::Return => write!(f, "<RETURN>"),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Token {
    Unknown(u8),
    Keyword(KeywordToken),
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
    LBrace,
    RBrace,
    Whitespace,
}
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Unknown(ref c) => write!(f, "<UNKNOWN: {}>", c),
            Token::Keyword(ref keyword) => write!(f, "<KEYWORD: {:?}>", keyword),
            Token::Identifier(ref id) => write!(f, "<ID: {}>", id),
            Token::Integer(ref i) => write!(f, "<INT: {}>", i),
            Token::Semicolon => write!(f, "<SEMICOLON>"),
            Token::Plus => write!(f, "<PLUS>"),
            Token::Minus => write!(f, "<MINUS>"),
            Token::Star => write!(f, "<STAR>"),
            Token::Slash => write!(f, "<SLASH>"),
            Token::Tilde => write!(f, "<TILDE>"),
            Token::Exclamation => write!(f, "<EXCLAMATION>"),
            Token::LParen => write!(f, "<LPAREN>"),
            Token::RParen => write!(f, "<RPAREN>"),
            Token::LBrace => write!(f, "<LBrace>"),
            Token::RBrace => write!(f, "<RBrace>"),
            Token::Whitespace => write!(f, "<WHITESPACE>"),
        }
    }
}

pub fn lex(src: &[u8]) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut buffer: Vec<u8> = Vec::new();
    let mut first_char_letter = false;

    let mut single_chars: HashMap<u8, Token> = HashMap::new();
    single_chars.insert(0x09, Token::Whitespace); // horizontal tab
    single_chars.insert(0x0A, Token::Whitespace); // new line
    single_chars.insert(0x0D, Token::Whitespace); // carriage return
    single_chars.insert(0x20, Token::Whitespace); // space
    single_chars.insert(b'!', Token::Exclamation);
    single_chars.insert(b'(', Token::LParen);
    single_chars.insert(b')', Token::RParen);
    single_chars.insert(b'*', Token::Star);
    single_chars.insert(b'+', Token::Plus);
    single_chars.insert(b'-', Token::Minus);
    single_chars.insert(b'/', Token::Slash);
    single_chars.insert(b';', Token::Semicolon);
    single_chars.insert(b'{', Token::LBrace);
    single_chars.insert(b'}', Token::RBrace);
    single_chars.insert(b'~', Token::Tilde);

    for c in src.iter() {
        match single_chars.get(c) {
            Some(single_char_token) => {
                if buffer.len() > 0 {
                    let buf_copy = buffer.clone();
                    let s = {str::from_utf8(&buf_copy).unwrap()};
                    if first_char_letter {
                        match s {
                            "int" => tokens.push(Token::Keyword(KeywordToken::Int)),
                            "return" => tokens.push(Token::Keyword(KeywordToken::Return)),
                            _ => tokens.push(Token::Identifier(String::from(s))),
                        }
                    } else {
                        if let Ok(n) = i32::from_str(s) {
                            tokens.push(Token::Integer(n));
                        } else {
                            panic!("could not tokenize integer: {}", s)
                        }
                    }
                    buffer = Vec::new();
                    first_char_letter = false;
                }
                match single_char_token {
                    &Token::Whitespace => (),
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
                    tokens.push(Token::Unknown(*c));
                }
            },
        }
    }
    return tokens;
}
