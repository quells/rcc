use std::fmt;
use lexer::{LexToken,LexTokenKeyword};

// https://stackoverflow.com/questions/25576748/how-to-compare-enum-without-pattern-matching
macro_rules! matches(
    ($e:expr, $p:pat) => {
        match $e {
            $p => true,
            _ => false
        }
    }
);

pub enum ParseProgram {
    Function(ParseFunction),
}
impl fmt::Debug for ParseProgram {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseProgram::Function(ref func) => write!(f, "{:?}", func),
        }
    }
}

pub enum ParseFunction {
    IntVoid(String, ParseStatement), // identifier, statement
}
impl fmt::Debug for ParseFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseFunction::IntVoid(ref id, ref s) => write!(f, "int {}:<{:?}>", id, s),
        }
    }
}

pub enum ParseStatement {
    Return(ParseExp),
}
impl fmt::Debug for ParseStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseStatement::Return(ref e) => write!(f, "return {:?}", e),
        }
    }
}

pub enum ParseExp {
    Term(Box<ParseTerm>),
    BinOp(Box<ParseTerm>, ParseBinOp, Box<ParseTerm>),
}
impl fmt::Debug for ParseExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseExp::Term(ref t) => write!(f, "{:?}", t),
            &ParseExp::BinOp(ref a, ref op, ref b) => write!(f, "exp({:?}{:?}{:?})", a, op, b),
        }
    }
}

pub enum ParseTerm {
    Factor(Box<ParseFactor>),
    BinOp(Box<ParseFactor>, ParseBinOp, Box<ParseFactor>),
}
impl fmt::Debug for ParseTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseTerm::Factor(ref x) => write!(f, "{:?}", x),
            &ParseTerm::BinOp(ref a, ref op, ref b) => write!(f, "term({:?}{:?}{:?})", a, op, b),
        }
    }
}

pub enum ParseFactor {
    Exp(Box<ParseExp>),
    UnOp(ParseUnOp, Box<ParseFactor>),
    Int(i32),
}
impl fmt::Debug for ParseFactor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseFactor::Exp(ref e) => write!(f, "{:?}", e),
            &ParseFactor::UnOp(ref op, ref x) => write!(f, "{:?}{:?}", op, x),
            &ParseFactor::Int(ref i) => write!(f, "{}", i),
        }
    }
}

pub enum ParseUnOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}
impl fmt::Debug for ParseUnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseUnOp::Negation => write!(f, "-"),
            &ParseUnOp::BitwiseComplement => write!(f, "~"),
            &ParseUnOp::LogicalNegation => write!(f, "!"),
        }
    }
}

pub enum ParseBinOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}
impl fmt::Debug for ParseBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ParseBinOp::Addition => write!(f, "+"),
            &ParseBinOp::Subtraction => write!(f, "-"),
            &ParseBinOp::Multiplication => write!(f, "*"),
            &ParseBinOp::Division => write!(f, "/"),
        }
    }
}

fn _eat(mut tokens: Vec<LexToken>, _expect: LexToken) -> Result<Vec<LexToken>, String> {
    let next_token = tokens.remove(0);
    let debug_token = next_token.clone();
    if matches!(next_token, _expect) {
        return Ok(tokens)
    } else {
        return Err(format!("found unexpected token: {:?}", debug_token))
    }
}

fn _parse_factor(mut tokens: Vec<LexToken>) -> Result<(ParseFactor, Vec<LexToken>), String> {
    let first_token = tokens.remove(0);
    match first_token {
        LexToken::LParen => {
            match _parse_exp(tokens) {
                Ok((inner, remaining_tokens)) => {
                    let mut copy_tokens = remaining_tokens.clone();
                    let next_token = copy_tokens.remove(0);
                    match next_token {
                        LexToken::RParen => Ok((ParseFactor::Exp(Box::new(inner)), copy_tokens)),
                        _ => Err(format!("could not parse factor. expected <RParen>, found {:?}", next_token)),
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
        _ => Err(format!("could not parse factor. expected one of `(!~-`, found {:?}", first_token)),
    }
}

fn _parse_term(tokens: Vec<LexToken>) -> Result<(ParseTerm, Vec<LexToken>), String> {
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
                                        println!("Error matching *: {}", e);
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
                                        println!("Error matching /: {}", e);
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

fn _parse_exp(tokens: Vec<LexToken>) -> Result<(ParseExp, Vec<LexToken>), String> {
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
                                        let __rhs = ParseTerm::Factor(Box::new(ParseFactor::Exp(Box::new(ParseExp::Term(Box::new(rhs))))));
                                        let add = Box::new(ParseExp::BinOp(lhs, ParseBinOp::Addition, Box::new(__rhs)));
                                        lhs = Box::new(ParseTerm::Factor(Box::new(ParseFactor::Exp(add))));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("Error matching +: {}", e);
                                        break
                                    }
                                }
                            },
                            LexToken::Minus => {
                                let _ = copy_remaining_tokens.remove(0); // minus
                                match _parse_term(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let __rhs = ParseTerm::Factor(Box::new(ParseFactor::Exp(Box::new(ParseExp::Term(Box::new(rhs))))));
                                        let add = Box::new(ParseExp::BinOp(lhs, ParseBinOp::Subtraction, Box::new(__rhs)));
                                        lhs = Box::new(ParseTerm::Factor(Box::new(ParseFactor::Exp(add))));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("Error matching -: {}", e);
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

fn _parse_statement(tokens: Vec<LexToken>) -> Result<(ParseStatement, Vec<LexToken>), String> {
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

fn _parse_function(tokens: Vec<LexToken>) -> Result<(ParseFunction, Vec<LexToken>), String> {
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
                                    match _eat(remaining_tokens, LexToken::LBrace) {
                                        Ok(remaining_tokens) => {
                                            // eventually: parse list of statements
                                            match _parse_statement(remaining_tokens) {
                                                Ok((parsed_statement, remaining_tokens)) => {
                                                    match _eat(remaining_tokens, LexToken::RBrace) {
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
                _ => Err("missing expected identifier token".to_string())
            }
        },
        Err(e) => Err(e)
    }
}

fn _parse_program(tokens: Vec<LexToken>) -> Result<(ParseProgram, Vec<LexToken>), String> {
    match _parse_function(tokens) {
        Ok((function, remaining)) => Ok((ParseProgram::Function(function), remaining)),
        Err(err) => Err(err),
    }
}

pub fn parse(tokens: Vec<LexToken>) -> Result<ParseProgram, String> {
    match _parse_program(tokens) {
        Ok((program, remaining)) => {
            match remaining.len() {
                0 => Ok(program),
                _ => {
                    Err(format!("unexpected tokens found after parsing program: {:?}", remaining))
                },
            }
        },
        Err(err) => Err(err),
    }
}
