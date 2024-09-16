use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

use crate::{
    ast::{self, BinaryOperator, Expression},
    lexer::{Lexer, LexerError, Token},
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
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
    pub fn from_token(token: &Token) -> Option<Self> {
        Some(match token {
            Token::Plus => Self::Sum,
            Token::Minus => Self::Sum,
            Token::Asterisk => Self::Product,
            Token::Slash => Self::Product,
            Token::Percent => Self::Product,
            Token::BitwiseAnd => Self::BitwiseAnd,
            Token::BitwiseOr => Self::BitwiseOr,
            Token::Xor => Self::Xor,
            Token::ShiftLeft | Token::ShiftRight => Self::Shift,
            Token::Or => Self::Or,
            Token::And => Self::And,
            Token::Equal | Token::NotEqual => Self::Equal,
            Token::LessThan | Token::LessOrEqual | Token::GreaterThan | Token::GreaterOrEqual => {
                Self::Ordered
            }
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum ParserError {
    /// 1. Is the expected token, 2. is the received token.
    /// Also, don't forget to not print the values, only the type should be printed.
    UnexpectedToken {
        expected: Token,
        actual: Token,
    },
    LexerError(LexerError),
    CouldNotParseStatement,
    NoPrefixFunction(Token),
    NoUnaryOperatorFound(Token),
    NoBinaryOperatorFound(Token),
}

impl From<LexerError> for ParserError {
    fn from(value: LexerError) -> Self {
        Self::LexerError(value)
    }
}

type PrefixFunction = fn(&mut Parser) -> Result<ast::Expression, ParserError>;
type InfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    prefix_functions: HashMap<Discriminant<Token>, PrefixFunction>,
    infix_functions: HashMap<Discriminant<Token>, InfixFunction>,
}

impl Parser {
    pub fn try_build(lexer: Lexer) -> Result<Self, LexerError> {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            prefix_functions: HashMap::new(),
            infix_functions: HashMap::new(),
        };

        // Prefix
        parser.register_prefix(mem::discriminant(&Token::Constant(1)), Self::parse_constant);
        parser.register_prefix(mem::discriminant(&Token::Minus), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&Token::Tilde), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&Token::Not), Self::parse_unary);
        parser.register_prefix(
            mem::discriminant(&Token::OpenParen),
            Self::parse_grouped_expression,
        );

        // Infix
        parser.register_infix(
            mem::discriminant(&Token::Plus),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::Minus),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::Asterisk),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::Slash),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::Percent),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::ShiftLeft),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::ShiftRight),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseAnd),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::Xor),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseOr),
            Self::parse_binary_expression,
        );
        parser.register_infix(mem::discriminant(&Token::Or), Self::parse_binary_expression);
        parser.register_infix(
            mem::discriminant(&Token::And),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::LessThan),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::LessOrEqual),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::GreaterThan),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::GreaterOrEqual),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::Equal),
            Self::parse_binary_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::NotEqual),
            Self::parse_binary_expression,
        );

        parser.next_token()?;
        parser.next_token()?;

        Ok(parser)
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParserError> {
        let func = self.parse_function_definition()?;

        self.expect_peek(Token::Eof)?;

        Ok(ast::Program {
            function_definition: func,
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParserError> {
        let prefix = self
            .prefix_functions
            .get(&mem::discriminant(&self.cur_token));

        match prefix {
            Some(prefix) => {
                let mut left_exp = prefix(self)?;

                while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence()
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
                        return Ok(left_exp);
                    }
                }

                Ok(left_exp)
            }
            None => Err(ParserError::NoPrefixFunction(self.cur_token.clone())),
        }
    }

    fn parse_binary_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        let bin = match &self.cur_token {
            Token::Plus => BinaryOperator::Add,
            Token::Minus => BinaryOperator::Subtract,
            Token::Asterisk => BinaryOperator::Multiply,
            Token::Slash => BinaryOperator::Divide,
            Token::Percent => BinaryOperator::Reminder,
            Token::ShiftLeft => BinaryOperator::ShiftLeft,
            Token::ShiftRight => BinaryOperator::ShiftRight,
            Token::BitwiseAnd => BinaryOperator::BitwiseAnd,
            Token::Xor => BinaryOperator::Xor,
            Token::BitwiseOr => BinaryOperator::BitwiseOr,
            Token::Equal => BinaryOperator::Equal,
            Token::NotEqual => BinaryOperator::NotEqual,
            Token::And => BinaryOperator::And,
            Token::Or => BinaryOperator::Or,
            Token::LessThan => BinaryOperator::LessThan,
            Token::LessOrEqual => BinaryOperator::LessOrEqual,
            Token::GreaterThan => BinaryOperator::GreaterThan,
            Token::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
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

    fn parse_constant(&mut self) -> Result<ast::Expression, ParserError> {
        if let Token::Constant(val) = &self.cur_token {
            return Ok(ast::Expression::Constant(*val));
        }

        Err(ParserError::UnexpectedToken {
            expected: Token::Constant(0),
            actual: self.cur_token.clone(),
        })
    }

    fn parse_unary(&mut self) -> Result<ast::Expression, ParserError> {
        match &self.cur_token {
            Token::Minus => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary(
                    ast::UnaryOperator::Negate,
                    Box::new(expr),
                ))
            }
            Token::Tilde => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary(
                    ast::UnaryOperator::Complement,
                    Box::new(expr),
                ))
            }
            Token::Not => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary(
                    ast::UnaryOperator::Not,
                    Box::new(expr),
                ))
            }
            _ => Err(ParserError::NoUnaryOperatorFound(self.cur_token.clone())),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::CloseParen)?;

        Ok(expr)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match self.cur_token {
            Token::KWReturn => self.parse_return_statement(),
            _ => Err(ParserError::CouldNotParseStatement),
        }
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Semicolon)?;

        Ok(ast::Statement::Return(expr))
    }

    fn parse_function_definition(&mut self) -> Result<ast::FunctionDefinition, ParserError> {
        self.expect(Token::KWInt)?;
        if let Token::Identifier(name) = self.peek_token.clone() {
            self.next_token()?;
            self.expect_peek(Token::OpenParen)?;
            self.expect_peek(Token::KWVoid)?;
            self.expect_peek(Token::CloseParen)?;
            self.expect_peek(Token::OpenBrace)?;
            self.next_token()?;
            let statement: ast::Statement = self.parse_statement()?;
            self.expect_peek(Token::CloseBrace)?;

            Ok(ast::FunctionDefinition {
                name: name.to_string(),
                body: statement,
            })
        } else {
            Err(ParserError::UnexpectedToken {
                expected: Token::Identifier("expected".to_owned()),
                actual: self.cur_token.clone(),
            })
        }
    }

    fn next_token(&mut self) -> Result<(), LexerError> {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token()?;

        Ok(())
    }

    fn register_prefix(&mut self, token: Discriminant<Token>, func: PrefixFunction) {
        self.prefix_functions.insert(token, func);
    }

    fn register_infix(&mut self, token: Discriminant<Token>, func: InfixFunction) {
        self.infix_functions.insert(token, func);
    }

    /// Expects current token to, but does not advance
    fn expect(&mut self, expected: Token) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.cur_token) {
            return Ok(());
        }
        return Err(ParserError::UnexpectedToken {
            expected,
            actual: self.cur_token.clone(),
        });
    }

    fn expect_peek(&mut self, expected: Token) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.peek_token) {
            self.next_token()?;
            return Ok(());
        }
        return Err(ParserError::UnexpectedToken {
            expected,
            actual: self.cur_token.clone(),
        });
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        mem::discriminant(&self.peek_token) == mem::discriminant(token)
    }

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
            ast::Statement::Return(Expression::Binary {
                op: BinaryOperator::Multiply,
                lhs: Box::new(Expression::Unary(
                    ast::UnaryOperator::Negate,
                    Box::new(Expression::Constant(2))
                ),),
                rhs: Box::new(Expression::Constant(2))
            })
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
            ast::Statement::Return(Expression::Binary {
                op: BinaryOperator::Multiply,
                lhs: Box::new(Expression::Unary(
                    ast::UnaryOperator::Negate,
                    Box::new(Expression::Constant(2))
                ),),
                rhs: Box::new(Expression::Unary(
                    ast::UnaryOperator::Complement,
                    Box::new(Expression::Constant(2))
                ))
            })
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
            ast::Statement::Return(Expression::Binary {
                op: BinaryOperator::Multiply,
                lhs: Box::new(Expression::Unary(
                    ast::UnaryOperator::Negate,
                    Box::new(Expression::Constant(2))
                ),),
                rhs: Box::new(Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(Expression::Unary(
                        ast::UnaryOperator::Complement,
                        Box::new(Expression::Constant(2))
                    )),
                    rhs: Box::new(Expression::Constant(4))
                })
            })
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
            ast::Statement::Return(Expression::Binary {
                op: BinaryOperator::Multiply,
                lhs: Box::new(Expression::Binary {
                    op: BinaryOperator::Subtract,
                    lhs: Box::new(Expression::Constant(3)),
                    rhs: Box::new(Expression::Constant(2))
                },),
                rhs: Box::new(Expression::Unary(
                    ast::UnaryOperator::Complement,
                    Box::new(Expression::Constant(2))
                ))
            })
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

        let expected_result = ast::Statement::Return(Expression::Binary {
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
        });

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

        let expected_result = ast::Statement::Return(Expression::Binary {
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
        });

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(program.function_definition.body, expected_result);
    }
}
