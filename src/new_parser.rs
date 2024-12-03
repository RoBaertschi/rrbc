use std::{collections::HashMap, mem::Discriminant};

use thiserror::Error;

use crate::{
    ast,
    lexer::{Lexer, LexerError, TokenKind},
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Assignment,
    Conditional,
    Or,
    And,
    BitwiseOr,
    Xor,
    BitwiseAnd,
    Equal,
    Ordered, // < <= > >=
    Shift,
    Sum,
    Product,
    Prefix,
}

impl Precedence {
    pub fn from_token(token: &TokenKind) -> Option<Self> {
        Some(match token {
            TokenKind::Plus => Self::Sum,
            TokenKind::Minus => Self::Sum,
            TokenKind::Asterisk => Self::Product,
            TokenKind::Slash => Self::Product,
            TokenKind::Percent => Self::Product,
            TokenKind::BitwiseAnd => Self::BitwiseAnd,
            TokenKind::BitwiseOr => Self::BitwiseOr,
            TokenKind::Xor => Self::Xor,
            TokenKind::ShiftLeft | TokenKind::ShiftRight => Self::Shift,
            TokenKind::Or => Self::Or,
            TokenKind::And => Self::And,
            TokenKind::Equal | TokenKind::NotEqual => Self::Equal,
            TokenKind::LessThan
            | TokenKind::LessOrEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterOrEqual => Self::Ordered,
            TokenKind::Assign
            | TokenKind::PlusAssign
            | TokenKind::MinusAssign
            | TokenKind::AsteriskAssign
            | TokenKind::SlashAssign
            | TokenKind::PercentAssign
            | TokenKind::BitwiseAndAssign
            | TokenKind::BitwiseOrAssign
            | TokenKind::BitwiseXorAssign
            | TokenKind::BitwiseShiftLeftAssign
            | TokenKind::BitwiseShiftRightAssign => Self::Assignment,
            TokenKind::QuestionMark => Self::Conditional,
            _ => return None,
        })
    }
}

#[derive(Error, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParserError {
    #[error("{0}")]
    LexerError(#[from] LexerError),
    #[error("Unexpected Token, expected: \"{expected:?}\", actual: \"{actual:?}\"")]
    UnexpectedToken {
        expected: TokenKind,
        actual: TokenKind,
    },
    #[error("Expected an '=' or ';' after a variable declaration. Got \"{0:?}\"")]
    DeclarationExpectedAssignOrSemicolon(TokenKind),
    #[error("Could not find a prefix function for \"{0:?}\".")]
    NoPrefixFunction(TokenKind),
}

type PrefixFunction = fn(&mut Parser) -> ast::Expression;
type PostfixFunction = fn(&mut Parser, ast::Expression) -> ast::Expression;
type InfixFunction = fn(&mut Parser, ast::Expression) -> ast::Expression;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: TokenKind,
    peek_token: TokenKind,

    prefix_functions: HashMap<Discriminant<TokenKind>, PrefixFunction>,
    postfix_functions: HashMap<Discriminant<TokenKind>, PostfixFunction>,
    infix_functions: HashMap<Discriminant<TokenKind>, InfixFunction>,
}
