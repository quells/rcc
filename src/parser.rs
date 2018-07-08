use std::fmt;
use lexer::{DebugToken, Token, KeywordToken};

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
    IntVoid(String, Vec<Statement>), // identifier, statements
}
impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Function::IntVoid(ref id, ref s) => write!(f, "int {}:<{:?}>", id, s),
        }
    }
}

pub enum Statement {
    Return(LogicalOrExpr),
}
impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Statement::Return(ref e) => write!(f, "return {:?}", e),
        }
    }
}

trait RecursiveHierarchy<P> {
    fn wrap_in_parent(&self) -> P;
}

#[derive(Clone)]
pub enum LogicalOrExpr {
    LogicalAndExpr(Box<LogicalAndExpr>),
    BinOp(Box<LogicalAndExpr>, BinaryOp, Box<LogicalAndExpr>),
}
impl fmt::Debug for LogicalOrExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LogicalOrExpr::LogicalAndExpr(ref t) => write!(f, "{:?}", t),
            &LogicalOrExpr::BinOp(ref a, ref op, ref b) => write!(f, "expr({:?}{:?}{:?})", a, op, b),
        }
    }
}
impl RecursiveHierarchy<Factor> for LogicalOrExpr {
    fn wrap_in_parent(&self) -> Factor {
        Factor::LogicalOrExpr(Box::new(self.clone()))
    }
}
impl LogicalOrExpr {
    fn wrap_in_child(&self) -> LogicalAndExpr {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Clone)]
pub enum LogicalAndExpr {
    EqualityExpr(Box<EqualityExpr>),
    BinOp(Box<EqualityExpr>, BinaryOp, Box<EqualityExpr>),
}
impl fmt::Debug for LogicalAndExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &LogicalAndExpr::EqualityExpr(ref t) => write!(f, "{:?}", t),
            &LogicalAndExpr::BinOp(ref a, ref op, ref b) => write!(f, "expr({:?}{:?}{:?})", a, op, b),
        }
    }
}
impl RecursiveHierarchy<LogicalOrExpr> for LogicalAndExpr {
    fn wrap_in_parent(&self) -> LogicalOrExpr {
        LogicalOrExpr::LogicalAndExpr(Box::new(self.clone()))
    }
}
impl LogicalAndExpr {
    fn wrap_in_child(&self) -> EqualityExpr {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Clone)]
pub enum EqualityExpr {
    RelationalExpr(Box<RelationalExpr>),
    BinOp(Box<RelationalExpr>, BinaryOp, Box<RelationalExpr>),
}
impl fmt::Debug for EqualityExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &EqualityExpr::RelationalExpr(ref t) => write!(f, "{:?}", t),
            &EqualityExpr::BinOp(ref a, ref op, ref b) => write!(f, "expr({:?}{:?}{:?})", a, op, b),
        }
    }
}
impl RecursiveHierarchy<LogicalAndExpr> for EqualityExpr {
    fn wrap_in_parent(&self) -> LogicalAndExpr {
        LogicalAndExpr::EqualityExpr(Box::new(self.clone()))
    }
}
impl EqualityExpr {
    fn wrap_in_child(&self) -> RelationalExpr {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Clone)]
pub enum RelationalExpr {
    AdditiveExpr(Box<AdditiveExpr>),
    BinOp(Box<AdditiveExpr>, BinaryOp, Box<AdditiveExpr>),
}
impl fmt::Debug for RelationalExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RelationalExpr::AdditiveExpr(ref t) => write!(f, "{:?}", t),
            &RelationalExpr::BinOp(ref a, ref op, ref b) => write!(f, "expr({:?}{:?}{:?})", a, op, b),
        }
    }
}
impl RecursiveHierarchy<EqualityExpr> for RelationalExpr {
    fn wrap_in_parent(&self) -> EqualityExpr {
        EqualityExpr::RelationalExpr(Box::new(self.clone()))
    }
}
impl RelationalExpr {
    fn wrap_in_child(&self) -> AdditiveExpr {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Clone)]
pub enum AdditiveExpr {
    Term(Box<Term>),
    BinOp(Box<Term>, BinaryOp, Box<Term>),
}
impl fmt::Debug for AdditiveExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AdditiveExpr::Term(ref t) => write!(f, "{:?}", t),
            &AdditiveExpr::BinOp(ref a, ref op, ref b) => write!(f, "expr({:?}{:?}{:?})", a, op, b),
        }
    }
}
impl RecursiveHierarchy<RelationalExpr> for AdditiveExpr {
    fn wrap_in_parent(&self) -> RelationalExpr {
        RelationalExpr::AdditiveExpr(Box::new(self.clone()))
    }
}
impl AdditiveExpr {
    fn wrap_in_child(&self) -> Term {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Clone)]
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
impl RecursiveHierarchy<AdditiveExpr> for Term {
    fn wrap_in_parent(&self) -> AdditiveExpr {
        AdditiveExpr::Term(Box::new(self.clone()))
    }
}
impl Term {
    fn wrap_in_child(&self) -> Factor {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Clone)]
pub enum Factor {
    LogicalOrExpr(Box<LogicalOrExpr>),
    UnOp(UnaryOp, Box<Factor>),
    Int(i32),
}
impl fmt::Debug for Factor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Factor::LogicalOrExpr(ref e) => write!(f, "{:?}", e),
            &Factor::UnOp(ref op, ref x) => write!(f, "{:?}{:?}", op, x),
            &Factor::Int(ref i) => write!(f, "{}", i),
        }
    }
}
impl RecursiveHierarchy<Term> for Factor {
    fn wrap_in_parent(&self) -> Term {
        Term::Factor(Box::new(self.clone()))
    }
}
impl Factor {
    fn wrap_in_child(&self) -> LogicalOrExpr {
        self.wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent().wrap_in_parent()
    }
}

#[derive(Copy, Clone)]
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

#[derive(Copy, Clone)]
pub enum BinaryOp {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    LogicalAnd,
    LogicalOr,
    Equals,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
impl fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &BinaryOp::Addition => write!(f, "+"),
            &BinaryOp::Subtraction => write!(f, "-"),
            &BinaryOp::Multiplication => write!(f, "*"),
            &BinaryOp::Division => write!(f, "/"),
            &BinaryOp::LogicalAnd => write!(f, "&&"),
            &BinaryOp::LogicalOr => write!(f, "||"),
            &BinaryOp::Equals => write!(f, "=="),
            &BinaryOp::NotEqual => write!(f, "!="),
            &BinaryOp::LessThan => write!(f, "<"),
            &BinaryOp::LessThanOrEqual => write!(f, "<="),
            &BinaryOp::GreaterThan => write!(f, ">"),
            &BinaryOp::GreaterThanOrEqual => write!(f, ">="),
        }
    }
}

fn eat(tokens: &[DebugToken], expect: Token) -> Result<&[DebugToken], String> {
    match tokens.split_first() {
        Some((next, rest)) => {
            if next.token == expect {
                Ok(rest)
            } else {
                Err(format!("expected {:?}, found `{:?}` near l{} c{}", expect, next.token, next.line, next.character))
            }
        },
        None => Err(format!("{}", END_OF_TOKENS)),
    }
}

fn parse_factor(tokens: &[DebugToken]) -> Result<(Factor, &[DebugToken]), String> {
    match tokens.split_first() {
        Some((first_token, rest)) => {
            match first_token.token {
                Token::LParen => {
                    match parse_logical_or_expr(rest) {
                        Ok((inner, remaining_tokens)) => {
                            match remaining_tokens.split_first() {
                                Some((next, rest)) => {
                                    match next.token {
                                        Token::RParen => Ok((inner.wrap_in_parent(), rest)),
                                        _ => Err(format!("could not parse factor. expected <RParen>, found {:?} near l{} c{}", next.token, next.line, next.character)),
                                    }
                                },
                                None => Err(format!("{}", END_OF_TOKENS)),
                            }
                        },
                        Err(e) => Err(e),
                    }
                },
                Token::Exclamation => {
                    match parse_factor(rest) {
                        Ok((operand, rest)) => {
                            let op = UnaryOp::LogicalNegation;
                            Ok((Factor::UnOp(op, Box::new(operand)), rest))
                        },
                        Err(e) => Err(e),
                    }
                },
                Token::Tilde => {
                    match parse_factor(rest) {
                        Ok((operand, rest)) => {
                            let op = UnaryOp::BitwiseComplement;
                            Ok((Factor::UnOp(op, Box::new(operand)), rest))
                        },
                        Err(e) => Err(e),
                    }
                },
                Token::Minus => {
                    match parse_factor(rest) {
                        Ok((operand, rest)) => {
                            let op = UnaryOp::Negation;
                            Ok((Factor::UnOp(op, Box::new(operand)), rest))
                        },
                        Err(e) => Err(e),
                    }
                },
                Token::Integer(i) => Ok((Factor::Int(i), rest)),
                _ => Err(format!("could not parse factor. expected one of `(!~-`, found {:?} near l{} c{}", first_token.token, first_token.line, first_token.character)),
            }
        },
        None => Err(format!("expected one of (!~-0123456789, found none")),
    }
}

fn parse_term(tokens: &[DebugToken]) -> Result<(Term, &[DebugToken]), String> {
    match parse_factor(tokens) {
        Ok((lhs, mut outer)) => {
            let mut lhs = Box::new(lhs);
            loop {
                match outer.split_first() {
                    Some((next, rest)) => {
                        match next.token {
                            Token::Star => {
                                match parse_factor(rest) {
                                    Ok((rhs, rest)) => {
                                        let mult = Term::BinOp(lhs, BinaryOp::Multiplication, Box::new(rhs));
                                        lhs = Box::new(mult.wrap_in_parent().wrap_in_parent()
                                            .wrap_in_parent().wrap_in_parent()
                                            .wrap_in_parent().wrap_in_parent());
                                        outer = rest;
                                    },
                                    Err(e) => return Err(format!("error matching * operation: {}", e)),
                                }
                            },
                            Token::Slash => {
                                match parse_factor(rest) {
                                    Ok((rhs, rest)) => {
                                        let div = Term::BinOp(lhs, BinaryOp::Division, Box::new(rhs));
                                        lhs = Box::new(div.wrap_in_parent().wrap_in_parent()
                                            .wrap_in_parent().wrap_in_parent()
                                            .wrap_in_parent().wrap_in_parent());
                                        outer = rest;
                                    },
                                    Err(e) => return Err(format!("error matching / operation: {}", e)),
                                }
                            },
                            _ => break,
                        }
                    },
                    None => break,
                }
            }
            Ok((Term::Factor(lhs), outer))
        },
        Err(e) => Err(e),
    }
}

fn parse_additive_expr(tokens: &[DebugToken]) -> Result<(AdditiveExpr, &[DebugToken]), String> {
    match parse_term(tokens) {
        Ok((lhs, mut outer)) => {
            let mut lhs = Box::new(lhs);
            loop {
                match outer.split_first() {
                    Some((next, rest)) => {
                        match next.token {
                            Token::Plus => {
                                match parse_term(rest) {
                                    Ok((_rhs, rest)) => {
                                        let rhs = Box::new(_rhs);
                                        let add = AdditiveExpr::BinOp(lhs, BinaryOp::Addition, rhs);
                                        lhs = Box::new(add.wrap_in_child());
                                        outer = rest;
                                    },
                                    Err(e) => return Err(format!("error matching + operation: {}", e)),
                                }
                            },
                            Token::Minus => {
                                match parse_term(rest) {
                                    Ok((_rhs, rest)) => {
                                        let rhs = Box::new(_rhs);
                                        let sub = AdditiveExpr::BinOp(lhs, BinaryOp::Subtraction, rhs);
                                        lhs = Box::new(sub.wrap_in_child());
                                        outer = rest;
                                    },
                                    Err(e) => return Err(format!("error matching - operation: {}", e)),
                                }
                            },
                            _ => break,
                        }
                    },
                    None => break,
                }
            }
            Ok((AdditiveExpr::Term(lhs), outer))
        },
        Err(e) => Err(e),
    }
}

fn parse_relational_expr(tokens: &[DebugToken]) -> Result<(RelationalExpr, &[DebugToken]), String> {
    Err("relational expr here".to_string())
}

fn parse_equality_expr(tokens: &[DebugToken]) -> Result<(EqualityExpr, &[DebugToken]), String> {
    Err("equality expr here".to_string())
}

fn parse_logical_and_expr(tokens: &[DebugToken]) -> Result<(LogicalAndExpr, &[DebugToken]), String> {
    Err("logical and expr here".to_string())
}

fn parse_logical_or_expr(tokens: &[DebugToken]) -> Result<(LogicalOrExpr, &[DebugToken]), String> {
    match parse_logical_and_expr(tokens) {
        Ok((lhs, mut outer)) => {
            let mut lhs = Box::new(lhs);
            loop {
                match eat(outer, Token::Pipe) {
                    Ok(rest) => {
                        match eat(rest, Token::Pipe) {
                            Ok(rest) => {
                                match parse_logical_and_expr(rest) {
                                    Ok((_rhs, rest)) => {
                                        let rhs = Box::new(_rhs);
                                        let or = LogicalOrExpr::BinOp(lhs.clone(), BinaryOp::LogicalOr, rhs);
                                        lhs = Box::new(or.wrap_in_child());
                                        outer = rest;
                                    },
                                    Err(e) => return Err(e),
                                }
                            },
                            Err(_) => break,
                        }
                    },
                    Err(_) => break,
                }
            }
            Ok((LogicalOrExpr::LogicalAndExpr(lhs.clone()), outer))
        },
        Err(e) => Err(e),
    }
}

fn parse_statement(tokens: &[DebugToken]) -> Result<(Statement, &[DebugToken]), String> {
    // eventually: parse assignment
    match eat(tokens, Token::Keyword(KeywordToken::Return)) {
        Ok(rest) => {
            match parse_logical_or_expr(rest) {
                Ok((parsed_exp, rest)) => {
                    match eat(rest, Token::Semicolon) {
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

fn parse_statement_list(tokens: &[DebugToken]) -> Result<(Vec<Statement>, &[DebugToken]), String> {
    let mut statements = Vec::new();
    let mut outer = tokens;
    loop {
        match parse_statement(outer) {
            Ok((statement, rest)) => {
                statements.push(statement);
                outer = rest;
            },
            Err(e) => {
                match outer.split_first() {
                    Some((next, rest)) => {
                        match next.token {
                            Token::RBrace => break,
                            _ => {
                                return Err(e);
                            },
                        }
                    },
                    None => {
                        return Err(END_OF_TOKENS.to_string());
                    },
                }
            },
        }
    }
    Ok((statements, outer))
}

fn parse_function_parameters(tokens: &[DebugToken]) -> Result<((), &[DebugToken]), String> {
    Ok(((), tokens))
}

fn parse_function(tokens: &[DebugToken]) -> Result<(Function, &[DebugToken]), String> {
    match eat(tokens, Token::Keyword(KeywordToken::Int)) {
        Ok(rest) => {
            match rest.split_first() {
                Some((fn_id, rest)) => {
                    match fn_id.token {
                        Token::Identifier(ref id) => {
                            match eat(rest, Token::LParen) {
                                Ok(rest) => {
                                    match parse_function_parameters(rest) {
                                        // eventually: deal with function parameters
                                        Ok((_, rest)) => {
                                            match eat(rest, Token::RParen) {
                                                Ok(rest) => {
                                                    match eat(rest, Token::LBrace) {
                                                        Ok(rest) => {
                                                            match parse_statement_list(rest) {
                                                                Ok((parsed_statements, rest)) => {
                                                                    match eat(rest, Token::RBrace) {
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
                                Err(e) => Err(e),
                            }
                        },
                        _ => Err(format!("expected identifier token, found {:?} near l{} c{}", fn_id.token, fn_id.line, fn_id.character)),
                    }
                },
                None => Err(format!("{}", END_OF_TOKENS)),
            }
        },
        Err(e) => Err(e)
    }
}

fn parse_program(tokens: &[DebugToken]) -> Result<(Program, &[DebugToken]), String> {
    match parse_function(tokens) {
        Ok((function, remaining)) => Ok((Program::Function(function), remaining)),
        Err(err) => Err(err),
    }
}

pub fn parse(tokens: &[DebugToken]) -> Result<Program, String> {
    match parse_program(tokens) {
        Ok((program, remaining)) => {
            match remaining.len() {
                0 => Ok(program),
                _ => Err(format!("unexpected tokens found after parsing program: {:?}", remaining)),
            }
        },
        Err(err) => Err(err),
    }
}
