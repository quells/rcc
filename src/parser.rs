use std::fmt;
use lexer::{Token, KeywordToken};

const END_OF_TOKENS: &'static str = "unexpected end of token stream";

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
    IntVoid(String, Vec<Statement>), // identifier, statement
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

fn _eat(tokens: &[Token], _expect: Token) -> Result<&[Token], String> {
    match tokens.split_first() {
        Some((next, rest)) => {
            match next {
                _expect => Ok(rest),
                _ => Err(format!("found unexpected token: {:?}", next)),
            }
        },
        None => Err(format!("{}", END_OF_TOKENS)),
    }
}

fn _parse_factor(tokens: &[Token]) -> Result<(Factor, &[Token]), String> {
    match tokens.split_first() {
        Some((first_token, rest)) => {
            match first_token {
                Token::LParen => {
                    match _parse_expr(rest) {
                        Ok((inner, remaining_tokens)) => {
                            match remaining_tokens.split_first() {
                                Some((next, rest)) => {
                                    match next {
                                        Token::RParen => Ok((Factor::Expr(Box::new(inner)), rest)),
                                        _ => Err(format!("could not parse factor. expected <RParen>, found {:?}", next)),
                                    }
                                },
                                None => Err(format!("{}", END_OF_TOKENS)),
                            }
                        },
                        Err(e) => Err(e),
                    }
                },
                Token::Exclamation => {
                    match _parse_factor(rest) {
                        Ok((operand, remaining_tokens)) => Ok((Factor::UnOp(UnaryOp::LogicalNegation, Box::new(operand)), remaining_tokens)),
                        Err(e) => Err(e),
                    }
                },
                Token::Tilde => {
                    match _parse_factor(rest) {
                        Ok((operand, remaining_tokens)) => Ok((Factor::UnOp(UnaryOp::BitwiseComplement, Box::new(operand)), remaining_tokens)),
                        Err(e) => Err(e),
                    }
                },
                Token::Minus => {
                    match _parse_factor(rest) {
                        Ok((operand, remaining_tokens)) => Ok((Factor::UnOp(UnaryOp::Negation, Box::new(operand)), remaining_tokens)),
                        Err(e) => Err(e),
                    }
                },
                Token::Integer(i) => Ok((Factor::Int(*i), rest)),
                _ => Err(format!("could not parse factor. expected one of `(!~-`, found {:?}", first_token)),
            }
        },
        None => Err(format!("expected one of (!~-0123456789, found none")),
    }
}

fn _parse_term(tokens: &[Token]) -> Result<(Term, &[Token]), String> {
    match _parse_factor(tokens) {
        Ok((lhs, rest)) => {
            let mut lhs = Box::new(lhs);
            loop {
                match rest.split_first() {
                    Some((next, rest)) => {
                        match next {
                            Token::Star => return Err(format!("temp 178")),
                            Token::Slash => return Err(format!("temp 179")),
                            _ => break,
                        }
                    },
                    None => break,
                }
            }
            Ok((Term::Factor(lhs), rest))
        },
        Err(e) => Err(e),
    }
    // match _parse_factor(tokens) {
    //     Ok((_lhs, mut remaining_tokens)) => {
    //         let mut lhs = Box::new(_lhs);
    //         loop {
    //             let mut copy_remaining_tokens = remaining_tokens.clone();
    //             match remaining_tokens.get(0) {
    //                 Some(next_token) => {
    //                     match *next_token {
    //                         Token::Star => {
    //                             let _ = copy_remaining_tokens.remove(0); // star
    //                             match _parse_factor(copy_remaining_tokens) {
    //                                 Ok((rhs, mut __remaining_tokens)) => {
    //                                     let mult = Box::new(Term::BinOp(lhs, BinaryOp::Multiplication, Box::new(rhs)));
    //                                     lhs = Box::new(Factor::Expr(Box::new(Expr::Term(mult))));
    //                                     copy_remaining_tokens = __remaining_tokens;
    //                                 },
    //                                 Err(e) => {
    //                                     println!("Error matching *: {}", e);
    //                                     break
    //                                 }
    //                             }
    //                         },
    //                         Token::Slash => {
    //                             let _ = copy_remaining_tokens.remove(0); // slash
    //                             match _parse_factor(copy_remaining_tokens) {
    //                                 Ok((rhs, mut __remaining_tokens)) => {
    //                                     let div = Box::new(Term::BinOp(lhs, BinaryOp::Division, Box::new(rhs)));
    //                                     lhs = Box::new(Factor::Expr(Box::new(Expr::Term(div))));
    //                                     copy_remaining_tokens = __remaining_tokens;
    //                                 },
    //                                 Err(e) => {
    //                                     println!("Error matching /: {}", e);
    //                                     break
    //                                 }
    //                             }
    //                         },
    //                         _ => break,
    //                     }
    //                 },
    //                 None => break,
    //             };
    //             remaining_tokens = copy_remaining_tokens
    //         };
    //         Ok((Term::Factor(lhs), remaining_tokens))
    //     },
    //     Err(e) => Err(e)
    // }
}

fn _parse_expr(tokens: &[Token]) -> Result<(Expr, &[Token]), String> {
    match _parse_term(tokens) {
        Ok((lhs, mut outer)) => {
            let mut lhs = Box::new(lhs);
            loop {
                match outer.split_first() {
                    Some((next, rest)) => {
                        match next {
                            Token::Plus => {
                                match _parse_term(rest) {
                                    Ok((rhs, rest)) => {
                                        let __rhs = Term::Factor(Box::new(Factor::Expr(Box::new(Expr::Term(Box::new(rhs))))));
                                        let add = Box::new(Expr::BinOp(lhs, BinaryOp::Addition, Box::new(__rhs)));
                                        lhs = Box::new(Term::Factor(Box::new(Factor::Expr(add))));
                                        outer = rest;
                                    },
                                    Err(e) => return Err(format!("error matching +: {}", e)),
                                }
                            },
                            Token::Minus => {
                                match _parse_term(rest) {
                                    Ok((rhs, rest)) => {
                                        let __rhs = Term::Factor(Box::new(Factor::Expr(Box::new(Expr::Term(Box::new(rhs))))));
                                        let sub = Box::new(Expr::BinOp(lhs, BinaryOp::Subtraction, Box::new(__rhs)));
                                        lhs = Box::new(Term::Factor(Box::new(Factor::Expr(sub))));
                                        outer = rest;
                                    },
                                    Err(e) => return Err(format!("error matching -: {}", e)),
                                }
                            },
                            _ => break,
                        }
                    },
                    None => break,
                }
            }
            Ok((Expr::Term(lhs), outer))
        },
        Err(e) => Err(e),
    }
}

fn _parse_statement(tokens: &[Token]) -> Result<(Statement, &[Token]), String> {
    // eventually: parse assignment
    match _eat(tokens, Token::Keyword(KeywordToken::Return)) {
        Ok(rest) => {
            match _parse_expr(rest) {
                Ok((parsed_exp, rest)) => {
                    match _eat(rest, Token::Semicolon) {
                        Ok(rest) => Ok((Statement::Return(parsed_exp), rest)),
                        Err(e) => Err(e)
                    }
                },
                Err(e) => Err(e)
            }
        },
        Err(e) => Err(e)
    }
}

fn _parse_statement_list(tokens: &[Token]) -> (Vec<Statement>, &[Token]) {
    let mut statements = Vec::new();
    let mut outer = tokens;
    loop {
        match _parse_statement(outer) {
            Ok((statement, rest)) => {
                statements.push(statement);
                outer = rest;
            },
            Err(_) => break,
        }
    }
    (statements, outer)
}

fn _parse_function_parameters(tokens: &[Token]) -> Result<((), &[Token]), String> {
    Ok(((), tokens))
}

fn _parse_function(tokens: &[Token]) -> Result<(Function, &[Token]), String> {
    match _eat(tokens, Token::Keyword(KeywordToken::Int)) {
        Ok(rest) => {
            match rest.split_first() {
                Some((fn_id, rest)) => {
                    match fn_id {
                        Token::Identifier(id) => {
                            match _eat(rest, Token::LParen) {
                                Ok(rest) => {
                                    match _parse_function_parameters(rest) {
                                        // eventually: deal with function parameters
                                        Ok((_, rest)) => {
                                            match _eat(rest, Token::RParen) {
                                                Ok(rest) => {
                                                    match _eat(rest, Token::LBrace) {
                                                        Ok(rest) => {
                                                            let (parsed_statements, rest) = _parse_statement_list(rest);
                                                            match _eat(rest, Token::RBrace) {
                                                                Ok(rest) => {
                                                                    let parsed_function = Function::IntVoid(id.to_string(), parsed_statements);
                                                                    Ok((parsed_function, rest))
                                                                },
                                                                Err(e) => Err(e),
                                                            }
                                                        },
                                                        Err(e) => Err(e),
                                                    }
                                                },
                                                Err(e) => Err(e),
                                            }
                                        },
                                        Err(e) => Err(e),
                                    }
                                },
                                Err(e) => Err(e),
                            }
                        },
                        _ => Err(format!("missing expected identifier token")),
                    }
                },
                None => Err(format!("{}", END_OF_TOKENS)),
            }
        },
        Err(e) => Err(e)
    }
}

fn _parse_program(tokens: &[Token]) -> Result<(Program, &[Token]), String> {
    match _parse_function(tokens) {
        Ok((function, remaining)) => Ok((Program::Function(function), remaining)),
        Err(err) => Err(err),
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, String> {
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
