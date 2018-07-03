use std::fmt;
use lexer::{Token, KeywordToken};

pub enum Program {
    Function(Function),
}
impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Program::Function(ref func) => write!(f, "{:?}", func),
        }
    }
}

pub enum Function {
    IntVoid(String, Statement), // identifier, statement
}
impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Function::IntVoid(ref id, ref s) => write!(f, "int {}:<{:?}>", id, s),
        }
    }
}

pub enum Statement {
    Return(Expr),
}
impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Statement::Return(ref e) => write!(f, "return {:?}", e),
        }
    }
}

pub enum Expr {
    Term(Box<Term>),
    BinOp(Box<Term>, BinaryOp, Box<Term>),
}
impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Expr::Term(ref t) => write!(f, "{:?}", t),
            &Expr::BinOp(ref a, ref op, ref b) => write!(f, "expr({:?}{:?}{:?})", a, op, b),
        }
    }
}

pub enum Term {
    Factor(Box<Factor>),
    BinOp(Box<Factor>, BinaryOp, Box<Factor>),
}
impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Term::Factor(ref x) => write!(f, "{:?}", x),
            &Term::BinOp(ref a, ref op, ref b) => write!(f, "term({:?}{:?}{:?})", a, op, b),
        }
    }
}

pub enum Factor {
    Expr(Box<Expr>),
    UnOp(UnaryOp, Box<Factor>),
    Int(i32),
}
impl fmt::Debug for Factor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Factor::Expr(ref e) => write!(f, "{:?}", e),
            &Factor::UnOp(ref op, ref x) => write!(f, "{:?}{:?}", op, x),
            &Factor::Int(ref i) => write!(f, "{}", i),
        }
    }
}

pub enum UnaryOp {
    Negation,
    BitwiseComplement,
    LogicalNegation,
}
impl fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &UnaryOp::Negation => write!(f, "-"),
            &UnaryOp::BitwiseComplement => write!(f, "~"),
            &UnaryOp::LogicalNegation => write!(f, "!"),
        }
    }
}

pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}
impl fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &BinaryOp::Addition => write!(f, "+"),
            &BinaryOp::Subtraction => write!(f, "-"),
            &BinaryOp::Multiplication => write!(f, "*"),
            &BinaryOp::Division => write!(f, "/"),
        }
    }
}

fn _eat(mut tokens: Vec<Token>, _expect: Token) -> Result<Vec<Token>, String> {
    let next_token = tokens.remove(0);
    let debug_token = next_token.clone();
    if next_token == _expect {
        return Ok(tokens)
    } else {
        return Err(format!("found unexpected token: {:?}", debug_token))
    }
}

fn _parse_factor(tokens: Vec<Token>) -> Result<(Factor, Vec<Token>), String> {
    match tokens.split_first() {
        Some((first_token, rest)) => {
            match first_token {
                Token::LParen => {
                    match _parse_expr(rest.to_vec()) {
                        Ok((inner, remaining_tokens)) => {
                            let mut copy_tokens = remaining_tokens.clone();
                            let next_token = copy_tokens.remove(0);
                            match next_token {
                                Token::RParen => Ok((Factor::Expr(Box::new(inner)), copy_tokens)),
                                _ => Err(format!("could not parse factor. expected <RParen>, found {:?}", next_token)),
                            }
                        },
                        Err(e) => Err(e),
                    }
                },
                Token::Exclamation => {
                    match _parse_factor(rest.to_vec()) {
                        Ok((operand, remaining_tokens)) => Ok((Factor::UnOp(UnaryOp::LogicalNegation, Box::new(operand)), remaining_tokens)),
                        Err(e) => Err(e),
                    }
                },
                Token::Tilde => {
                    match _parse_factor(rest.to_vec()) {
                        Ok((operand, remaining_tokens)) => Ok((Factor::UnOp(UnaryOp::BitwiseComplement, Box::new(operand)), remaining_tokens)),
                        Err(e) => Err(e),
                    }
                },
                Token::Minus => {
                    match _parse_factor(rest.to_vec()) {
                        Ok((operand, remaining_tokens)) => Ok((Factor::UnOp(UnaryOp::Negation, Box::new(operand)), remaining_tokens)),
                        Err(e) => Err(e),
                    }
                },
                Token::Integer(i) => Ok((Factor::Int(*i), rest.to_vec())),
                _ => Err(format!("could not parse factor. expected one of `(!~-`, found {:?}", first_token)),
            }
        },
        None => Err(format!("expected one of (!~-0123456789, found none")),
    }
}

fn _parse_term(tokens: Vec<Token>) -> Result<(Term, Vec<Token>), String> {
    match _parse_factor(tokens) {
        Ok((_lhs, mut remaining_tokens)) => {
            let mut lhs = Box::new(_lhs);
            loop {
                let mut copy_remaining_tokens = remaining_tokens.clone();
                match remaining_tokens.get(0) {
                    Some(next_token) => {
                        match *next_token {
                            Token::Star => {
                                let _ = copy_remaining_tokens.remove(0); // star
                                match _parse_factor(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let mult = Box::new(Term::BinOp(lhs, BinaryOp::Multiplication, Box::new(rhs)));
                                        lhs = Box::new(Factor::Expr(Box::new(Expr::Term(mult))));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("Error matching *: {}", e);
                                        break
                                    }
                                }
                            },
                            Token::Slash => {
                                let _ = copy_remaining_tokens.remove(0); // slash
                                match _parse_factor(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let div = Box::new(Term::BinOp(lhs, BinaryOp::Division, Box::new(rhs)));
                                        lhs = Box::new(Factor::Expr(Box::new(Expr::Term(div))));
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
            Ok((Term::Factor(lhs), remaining_tokens))
        },
        Err(e) => Err(e)
    }
}

fn _parse_expr(tokens: Vec<Token>) -> Result<(Expr, Vec<Token>), String> {
    match _parse_term(tokens) {
        Ok((_lhs, mut remaining_tokens)) => {
            let mut lhs = Box::new(_lhs);
            loop {
                let mut copy_remaining_tokens = remaining_tokens.clone();
                match remaining_tokens.get(0) {
                    Some(next_token) => {
                        match *next_token {
                            Token::Plus => {
                                let _ = copy_remaining_tokens.remove(0); // plus
                                match _parse_term(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let __rhs = Term::Factor(Box::new(Factor::Expr(Box::new(Expr::Term(Box::new(rhs))))));
                                        let add = Box::new(Expr::BinOp(lhs, BinaryOp::Addition, Box::new(__rhs)));
                                        lhs = Box::new(Term::Factor(Box::new(Factor::Expr(add))));
                                        copy_remaining_tokens = __remaining_tokens;
                                    },
                                    Err(e) => {
                                        println!("Error matching +: {}", e);
                                        break
                                    }
                                }
                            },
                            Token::Minus => {
                                let _ = copy_remaining_tokens.remove(0); // minus
                                match _parse_term(copy_remaining_tokens) {
                                    Ok((rhs, mut __remaining_tokens)) => {
                                        let __rhs = Term::Factor(Box::new(Factor::Expr(Box::new(Expr::Term(Box::new(rhs))))));
                                        let add = Box::new(Expr::BinOp(lhs, BinaryOp::Subtraction, Box::new(__rhs)));
                                        lhs = Box::new(Term::Factor(Box::new(Factor::Expr(add))));
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
            Ok((Expr::Term(lhs), remaining_tokens))
        },
        Err(e) => Err(e)
    }
}

fn _parse_statement(tokens: Vec<Token>) -> Result<(Statement, Vec<Token>), String> {
    // eventually: parse assignment
    match _eat(tokens, Token::Keyword(KeywordToken::Return)) {
        Ok(remaining_tokens) => {
            match _parse_expr(remaining_tokens) {
                Ok((parsed_exp, remaining_tokens)) => {
                    match _eat(remaining_tokens, Token::Semicolon) {
                        Ok(remaining_tokens) => Ok((Statement::Return(parsed_exp), remaining_tokens)),
                        Err(e) => Err(e)
                    }
                },
                Err(e) => Err(e)
            }
        },
        Err(e) => Err(e)
    }
}

fn _parse_function(tokens: Vec<Token>) -> Result<(Function, Vec<Token>), String> {
    match _eat(tokens, Token::Keyword(KeywordToken::Int)) {
        Ok(mut remaining_tokens) => {
            let fn_id = remaining_tokens.remove(0);
            match fn_id {
                Token::Identifier(id) => {
                    match _eat(remaining_tokens, Token::LParen) {
                        Ok(remaining_tokens) => {
                            // eventually: parse function paramenters
                            match _eat(remaining_tokens, Token::RParen) {
                                Ok(remaining_tokens) => {
                                    match _eat(remaining_tokens, Token::LBrace) {
                                        Ok(remaining_tokens) => {
                                            // eventually: parse list of statements
                                            match _parse_statement(remaining_tokens) {
                                                Ok((parsed_statement, remaining_tokens)) => {
                                                    match _eat(remaining_tokens, Token::RBrace) {
                                                        Ok(remaining_tokens) => Ok((Function::IntVoid(id, parsed_statement), remaining_tokens)),
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

fn _parse_program(tokens: Vec<Token>) -> Result<(Program, Vec<Token>), String> {
    match _parse_function(tokens) {
        Ok((function, remaining)) => Ok((Program::Function(function), remaining)),
        Err(err) => Err(err),
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
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
