use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

use thiserror::Error;

use crate::{
    ast::{self, BinaryOperator, BlockItem, Expression},
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

#[derive(Error, Debug)]
pub enum ParserError {
    /// 1. Is the expected token
    /// 2. is the received token
    /// Also, don't forget to not print the values, only the type should be printed.
    #[error("Unexpected Token, expected: \"{expected:?}\", actual: \"{actual:?}\"")]
    UnexpectedToken {
        expected: TokenKind,
        actual: TokenKind,
    },
    #[error("{0:?}")]
    LexerError(#[from] LexerError),
    #[error("Could not parse a declaration, offending token: {0:?}. parse_declaration got called on a invalid token for a declaration.")]
    CouldNotParseDeclaration(TokenKind),
    #[error("Expected an '=' or ';' after a variable declaration. Got \"{0:?}\"")]
    DeclarationExpectedAssignOrSemicolon(TokenKind),
    #[error("Could not find a prefix function for \"{0:?}\".")]
    NoPrefixFunction(TokenKind),
    #[error("Could not find a postfix function for \"{0:?}\".")]
    NoPostfixOperatorFound(TokenKind),
    #[error("Could not find unary operator for \"{0:?}\". This indicates a missuse of parse_unary or a missing branch in parse_unary.")]
    NoUnaryOperatorFound(TokenKind),
    #[error("Could not find binary operator for \"{0:?}\". This indicates a missuse of parse_binary or a missing branch in parse_binary.")]
    NoBinaryOperatorFound(TokenKind),
}

type PrefixFunction = fn(&mut Parser) -> Result<ast::Expression, ParserError>;
type PostfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;
type InfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: TokenKind,
    peek_token: TokenKind,
    prefix_functions: HashMap<Discriminant<TokenKind>, PrefixFunction>,
    postfix_functions: HashMap<Discriminant<TokenKind>, PostfixFunction>,
    infix_functions: HashMap<Discriminant<TokenKind>, InfixFunction>,
}

impl Parser {
    pub fn try_build(lexer: Lexer) -> Result<Self, LexerError> {
        let mut parser = Self {
            lexer,
            cur_token: TokenKind::Eof,
            peek_token: TokenKind::Eof,
            prefix_functions: HashMap::new(),
            postfix_functions: HashMap::new(),
            infix_functions: HashMap::new(),
        };

        // Prefix
        parser.register_prefix(
            mem::discriminant(&TokenKind::Constant(1)),
            Self::parse_constant,
        );
        parser.register_prefix(mem::discriminant(&TokenKind::Minus), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&TokenKind::Tilde), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&TokenKind::Not), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&TokenKind::Increment), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&TokenKind::Decrement), Self::parse_unary);
        parser.register_prefix(
            mem::discriminant(&TokenKind::OpenParen),
            Self::parse_grouped_expression,
        );
        parser.register_prefix(
            mem::discriminant(&TokenKind::Identifier(String::new())),
            Self::parse_identifier,
        );

        // Infix
        parser.register_infix(
            mem::discriminant(&TokenKind::Plus),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Minus),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Asterisk),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Slash),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Percent),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::ShiftLeft),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::ShiftRight),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseAnd),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Xor),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseOr),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Or),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::And),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::LessThan),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::LessOrEqual),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::GreaterThan),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::GreaterOrEqual),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Equal),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::NotEqual),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::Assign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::PlusAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::MinusAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::AsteriskAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::SlashAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::PercentAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseAndAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseOrAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseXorAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseShiftLeftAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::BitwiseShiftRightAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&TokenKind::QuestionMark),
            Self::parse_conditional,
        );

        parser.register_postfix(
            mem::discriminant(&TokenKind::Increment),
            Self::parse_postfix,
        );
        parser.register_postfix(
            mem::discriminant(&TokenKind::Decrement),
            Self::parse_postfix,
        );

        parser.next_token()?;
        parser.next_token()?;

        Ok(parser)
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParserError> {
        let func = self.parse_function_definition()?;

        self.expect_peek(TokenKind::Eof)?;

        Ok(ast::Program {
            function_definition: func,
        })
    }

    fn parse_expression_try_postfix(
        &mut self,
        mut left_exp: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        while self
            .postfix_functions
            .contains_key(&mem::discriminant(&self.peek_token))
        {
            self.next_token()?;
            let postfix = self
                .postfix_functions
                .get(&mem::discriminant(&self.cur_token));

            if let Some(postfix) = postfix {
                left_exp = postfix(self, left_exp)?;
            }
        }
        return Ok(left_exp);
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParserError> {
        let prefix = self
            .prefix_functions
            .get(&mem::discriminant(&self.cur_token));

        match prefix {
            Some(prefix) => {
                let mut left_exp = prefix(self)?;

                while !self.peek_token_is(&TokenKind::Semicolon)
                    && precedence < self.peek_precedence()
                {
                    if self
                        .infix_functions
                        .contains_key(&mem::discriminant(&self.peek_token))
                    {
                        self.next_token()?;
                        left_exp = self
                            .infix_functions
                            .get(&mem::discriminant(&self.cur_token))
                            .unwrap()(self, left_exp)?;
                    } else {
                        return self.parse_expression_try_postfix(left_exp);
                    }
                }

                self.parse_expression_try_postfix(left_exp)
            }
            None => Err(ParserError::NoPrefixFunction(self.cur_token.clone())),
        }
    }

    fn parse_binary_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        let bin = match &self.cur_token {
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Subtract,
            TokenKind::Asterisk => BinaryOperator::Multiply,
            TokenKind::Slash => BinaryOperator::Divide,
            TokenKind::Percent => BinaryOperator::Reminder,
            TokenKind::ShiftLeft => BinaryOperator::ShiftLeft,
            TokenKind::ShiftRight => BinaryOperator::ShiftRight,
            TokenKind::BitwiseAnd => BinaryOperator::BitwiseAnd,
            TokenKind::Xor => BinaryOperator::Xor,
            TokenKind::BitwiseOr => BinaryOperator::BitwiseOr,
            TokenKind::Equal => BinaryOperator::Equal,
            TokenKind::NotEqual => BinaryOperator::NotEqual,
            TokenKind::And => BinaryOperator::And,
            TokenKind::Or => BinaryOperator::Or,
            TokenKind::LessThan => BinaryOperator::LessThan,
            TokenKind::LessOrEqual => BinaryOperator::LessOrEqual,
            TokenKind::GreaterThan => BinaryOperator::GreaterThan,
            TokenKind::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
            tok => return Err(ParserError::NoBinaryOperatorFound(tok.clone())),
        };

        let precedence = self.cur_precedence();
        self.next_token()?;
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Binary {
            op: bin,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    fn parse_assignment_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        let op: ast::AssignmentOperator = match self.cur_token {
            TokenKind::Assign => ast::AssignmentOperator::None,
            TokenKind::PlusAssign => ast::AssignmentOperator::Add,
            TokenKind::MinusAssign => ast::AssignmentOperator::Subtract,
            TokenKind::AsteriskAssign => ast::AssignmentOperator::Multiply,
            TokenKind::SlashAssign => ast::AssignmentOperator::Divide,
            TokenKind::PercentAssign => ast::AssignmentOperator::Reminder,
            TokenKind::BitwiseAndAssign => ast::AssignmentOperator::BitwiseAnd,
            TokenKind::BitwiseOrAssign => ast::AssignmentOperator::BitwiseOr,
            TokenKind::BitwiseXorAssign => ast::AssignmentOperator::BitwiseXor,
            TokenKind::BitwiseShiftLeftAssign => ast::AssignmentOperator::ShiftLeft,
            TokenKind::BitwiseShiftRightAssign => ast::AssignmentOperator::ShiftRight,
            _ => unreachable!(
                "Wrong token passed to parse_assignment_expression, should not happen."
            ),
        };

        self.next_token()?;
        let right = self.parse_expression(Precedence::Lowest)?;
        Ok(Expression::Assignment {
            op,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    fn parse_conditional(&mut self, left: ast::Expression) -> Result<ast::Expression, ParserError> {
        self.next_token()?;
        let then = Box::new(self.parse_expression(Precedence::Lowest)?);
        self.expect_peek(TokenKind::Colon)?;
        self.next_token()?;
        let r#else = Box::new(self.parse_expression(Precedence::Assignment)?);
        Ok(Expression::Conditional {
            condition: Box::new(left),
            then,
            r#else,
        })
    }

    fn parse_constant(&mut self) -> Result<ast::Expression, ParserError> {
        if let TokenKind::Constant(val) = &self.cur_token {
            return Ok(ast::Expression::Constant(*val));
        }

        Err(ParserError::UnexpectedToken {
            expected: TokenKind::Constant(0),
            actual: self.cur_token.clone(),
        })
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, ParserError> {
        if let TokenKind::Identifier(val) = self.cur_token.clone() {
            return Ok(ast::Expression::Var(val));
        }

        Err(ParserError::UnexpectedToken {
            expected: TokenKind::Identifier(String::new()),
            actual: self.cur_token.clone(),
        })
    }

    fn parse_unary(&mut self) -> Result<ast::Expression, ParserError> {
        match &self.cur_token {
            TokenKind::Minus => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Negate,
                    expression: Box::new(expr),
                })
            }
            TokenKind::Tilde => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Complement,
                    expression: Box::new(expr),
                })
            }
            TokenKind::Not => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Not,
                    expression: Box::new(expr),
                })
            }
            TokenKind::Increment => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Increment,
                    expression: Box::new(expr),
                })
            }
            TokenKind::Decrement => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Decrement,
                    expression: Box::new(expr),
                })
            }
            _ => Err(ParserError::NoUnaryOperatorFound(self.cur_token.clone())),
        }
    }

    fn parse_postfix(&mut self, lhs: ast::Expression) -> Result<ast::Expression, ParserError> {
        match &self.cur_token {
            TokenKind::Increment => Ok(ast::Expression::Postfix(
                ast::PostfixOperator::Increment,
                Box::new(lhs),
            )),
            TokenKind::Decrement => Ok(ast::Expression::Postfix(
                ast::PostfixOperator::Decrement,
                Box::new(lhs),
            )),
            _ => Err(ParserError::NoPostfixOperatorFound(self.cur_token.clone())),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::CloseParen)?;

        Ok(expr)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match &self.cur_token {
            TokenKind::KWReturn => self.parse_return_statement(),
            TokenKind::KWIf => self.parse_if_statement(),
            TokenKind::KWGoto => self.parse_goto_statement(),
            TokenKind::Semicolon => Ok(ast::Statement::Null),
            TokenKind::Identifier(ident) if self.peek_token_is(&TokenKind::Colon) => {
                self.parse_label_statement(ident.clone())
            }
            _ => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(TokenKind::Semicolon)?;
                Ok(ast::Statement::Expression(expr))
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::Semicolon)?;

        Ok(ast::Statement::Return(expr))
    }

    fn parse_goto_statement(&mut self) -> Result<ast::Statement, ParserError> {
        if let TokenKind::Identifier(label_name) = self.peek_token.clone() {
            self.next_token()?;
            self.expect_peek(TokenKind::Semicolon)?;
            Ok(ast::Statement::Goto(label_name.clone()))
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier(String::new()),
                actual: self.peek_token.clone(),
            })
        }
    }
    fn parse_label_statement(&mut self, identifier: String) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::Colon)?;
        self.next_token()?;
        Ok(ast::Statement::Label(
            identifier,
            Box::new(self.parse_statement()?),
        ))
    }

    fn parse_if_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::OpenParen)?;
        self.next_token()?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::CloseParen)?;
        self.next_token()?;
        let stmt = self.parse_statement()?;

        let else_stmt = match self.peek_token {
            TokenKind::KWElse => {
                self.next_token()?;
                self.next_token()?;
                Some(Box::new(self.parse_statement()?))
            }
            _ => None,
        };

        Ok(ast::Statement::If {
            condition,
            then: Box::new(stmt),
            r#else: else_stmt,
        })
    }

    fn parse_declaration(&mut self) -> Result<ast::Declaration, ParserError> {
        match &self.cur_token {
            TokenKind::KWInt => {
                self.next_token()?;

                if let TokenKind::Identifier(ident) = self.cur_token.clone() {
                    self.next_token()?;
                    match &self.cur_token {
                        TokenKind::Semicolon => Ok(ast::Declaration {
                            name: ident,
                            exp: None,
                        }),
                        TokenKind::Assign => {
                            self.next_token()?;
                            let expr = self.parse_expression(self.cur_precedence())?;
                            self.expect_peek(TokenKind::Semicolon)?;
                            Ok(ast::Declaration {
                                name: ident,
                                exp: Some(expr),
                            })
                        }
                        tok => Err(ParserError::DeclarationExpectedAssignOrSemicolon(
                            tok.clone(),
                        )),
                    }
                } else {
                    Err(ParserError::UnexpectedToken {
                        expected: TokenKind::Identifier(String::new()),
                        actual: self.cur_token.clone(),
                    })
                }
            }
            tok => Err(ParserError::CouldNotParseDeclaration(tok.clone())),
        }
    }

    fn parse_block_item(&mut self) -> Result<ast::BlockItem, ParserError> {
        self.next_token()?;
        match &self.cur_token {
            TokenKind::KWInt => Ok(ast::BlockItem::D(self.parse_declaration()?)),
            _ => Ok(ast::BlockItem::S(self.parse_statement()?)),
        }
    }

    fn parse_function_definition(&mut self) -> Result<ast::FunctionDefinition, ParserError> {
        self.expect(TokenKind::KWInt)?;
        if let TokenKind::Identifier(name) = self.peek_token.clone() {
            self.next_token()?;
            self.expect_peek(TokenKind::OpenParen)?;
            self.expect_peek(TokenKind::KWVoid)?;
            self.expect_peek(TokenKind::CloseParen)?;
            self.expect_peek(TokenKind::OpenBrace)?;

            let mut body: Vec<BlockItem> = vec![];
            while !self.peek_token_is(&TokenKind::CloseBrace) {
                body.push(self.parse_block_item()?);
            }
            self.next_token()?; // Eat CloseBrace

            Ok(ast::FunctionDefinition {
                name: name.to_string(),
                body,
            })
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier("expected".to_owned()),
                actual: self.cur_token.clone(),
            })
        }
    }

    fn next_token(&mut self) -> Result<(), LexerError> {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token()?;

        Ok(())
    }

    fn register_prefix(&mut self, token: Discriminant<TokenKind>, func: PrefixFunction) {
        self.prefix_functions.insert(token, func);
    }

    fn register_postfix(&mut self, token: Discriminant<TokenKind>, func: PostfixFunction) {
        self.postfix_functions.insert(token, func);
    }

    fn register_infix(&mut self, token: Discriminant<TokenKind>, func: InfixFunction) {
        self.infix_functions.insert(token, func);
    }

    /// Expects current token to, but does not advance
    fn expect(&mut self, expected: TokenKind) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.cur_token) {
            return Ok(());
        }
        Err(ParserError::UnexpectedToken {
            expected,
            actual: self.cur_token.clone(),
        })
    }

    fn expect_peek(&mut self, expected: TokenKind) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.peek_token) {
            self.next_token()?;
            return Ok(());
        }
        Err(ParserError::UnexpectedToken {
            expected,
            actual: self.cur_token.clone(),
        })
    }

    fn peek_token_is(&self, token: &TokenKind) -> bool {
        mem::discriminant(&self.peek_token) == mem::discriminant(token)
    }

    //fn cur_token_is(&self, token: &Token) -> bool {
    //    mem::discriminant(&self.cur_token) == mem::discriminant(token)
    //}

    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(&self.peek_token).unwrap_or(Precedence::Lowest)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(&self.cur_token).unwrap_or(Precedence::Lowest)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_declaration() {
        let lexer = Lexer::new("int main(void) { int a = hello; int b; }".to_owned());

        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser.parse_program().expect("should successfully parse");

        assert_eq!(
            program.function_definition.body,
            vec![
                ast::BlockItem::D(ast::Declaration {
                    name: "a".to_owned(),
                    exp: Some(ast::Expression::Var("hello".to_owned()))
                }),
                ast::BlockItem::D(ast::Declaration {
                    name: "b".to_owned(),
                    exp: None,
                }),
            ]
        );
    }

    #[test]
    fn test_precedence1() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return -2 * 2;
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(Expression::Constant(2))
                    },),
                    rhs: Box::new(Expression::Constant(2))
                }
            ))]
        );
    }

    #[test]
    fn test_precedence2() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return -2 * (~2);
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(Expression::Constant(2))
                    },),
                    rhs: Box::new(Expression::Unary {
                        op: ast::UnaryOperator::Complement,
                        expression: Box::new(Expression::Constant(2))
                    })
                }
            ))]
        );
    }

    #[test]
    fn test_precedence3() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return -2 * (~2 * 4);
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(Expression::Constant(2))
                    },),
                    rhs: Box::new(Expression::Binary {
                        op: BinaryOperator::Multiply,
                        lhs: Box::new(Expression::Unary {
                            op: ast::UnaryOperator::Complement,
                            expression: Box::new(Expression::Constant(2))
                        }),
                        rhs: Box::new(Expression::Constant(4))
                    })
                }
            ))]
        );
    }

    #[test]
    fn test_precedence4() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return (3-2) * (~2);
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(Expression::Binary {
                        op: BinaryOperator::Subtract,
                        lhs: Box::new(Expression::Constant(3)),
                        rhs: Box::new(Expression::Constant(2))
                    },),
                    rhs: Box::new(Expression::Unary {
                        op: ast::UnaryOperator::Complement,
                        expression: Box::new(Expression::Constant(2))
                    })
                }
            ))]
        );
    }
    #[test]
    fn test_bitwise_prec() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return 40 << 4 + 12 >> 1;
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        let expected_result = vec![ast::BlockItem::S(ast::Statement::Return(
            Expression::Binary {
                op: BinaryOperator::ShiftRight,
                lhs: Box::new(Expression::Binary {
                    op: BinaryOperator::ShiftLeft,
                    lhs: Box::new(Expression::Constant(40)),
                    rhs: Box::new(Expression::Binary {
                        op: BinaryOperator::Add,
                        lhs: Box::new(Expression::Constant(4)),
                        rhs: Box::new(Expression::Constant(12)),
                    }),
                }),
                rhs: Box::new(Expression::Constant(1)),
            },
        ))];

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(program.function_definition.body, expected_result);
    }

    #[test]
    fn test_bitwise_prec2() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return 1 << 2 & 3 ^ 2 | 3;
        }
        "
            .to_owned(),
        );
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        let expected_result = vec![ast::BlockItem::S(ast::Statement::Return(
            Expression::Binary {
                op: BinaryOperator::BitwiseOr,
                lhs: Box::new(Expression::Binary {
                    op: BinaryOperator::Xor,
                    lhs: Box::new(Expression::Binary {
                        op: BinaryOperator::BitwiseAnd,
                        lhs: Box::new(Expression::Binary {
                            op: BinaryOperator::ShiftLeft,
                            lhs: Box::new(Expression::Constant(1)),
                            rhs: Box::new(Expression::Constant(2)),
                        }),
                        rhs: Box::new(Expression::Constant(3)),
                    }),
                    rhs: Box::new(Expression::Constant(2)),
                }),
                rhs: Box::new(Expression::Constant(3)),
            },
        ))];

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(program.function_definition.body, expected_result);
    }
}
