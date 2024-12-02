use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

use thiserror::Error;

use crate::{
    ast::{self, BinaryOperator, BlockItem, Expression},
    lexer::{Lexer, LexerError, Token},
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Assignment,
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
            Token::Assign
            | Token::PlusAssign
            | Token::MinusAssign
            | Token::AsteriskAssign
            | Token::SlashAssign
            | Token::PercentAssign
            | Token::BitwiseAndAssign
            | Token::BitwiseOrAssign
            | Token::BitwiseXorAssign
            | Token::BitwiseShiftLeftAssign
            | Token::BitwiseShiftRightAssign => Self::Assignment,
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
    UnexpectedToken { expected: Token, actual: Token },
    #[error("{0:?}")]
    LexerError(#[from] LexerError),
    #[error("Could not parse a declaration, offending token: {0:?}. parse_declaration got called on a invalid token for a declaration.")]
    CouldNotParseDeclaration(Token),
    #[error("Expected an '=' or ';' after a variable declaration. Got \"{0:?}\"")]
    DeclarationExpectedAssignOrSemicolon(Token),
    #[error("Could not find a prefix function for \"{0:?}\".")]
    NoPrefixFunction(Token),
    #[error("Could not find a postfix function for \"{0:?}\".")]
    NoPostfixOperatorFound(Token),
    #[error("Could not find unary operator for \"{0:?}\". This indicates a missuse of parse_unary or a missing branch in parse_unary.")]
    NoUnaryOperatorFound(Token),
    #[error("Could not find binary operator for \"{0:?}\". This indicates a missuse of parse_binary or a missing branch in parse_binary.")]
    NoBinaryOperatorFound(Token),
}

type PrefixFunction = fn(&mut Parser) -> Result<ast::Expression, ParserError>;
type PostfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;
type InfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    prefix_functions: HashMap<Discriminant<Token>, PrefixFunction>,
    postfix_functions: HashMap<Discriminant<Token>, PostfixFunction>,
    infix_functions: HashMap<Discriminant<Token>, InfixFunction>,
}

impl Parser {
    pub fn try_build(lexer: Lexer) -> Result<Self, LexerError> {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            prefix_functions: HashMap::new(),
            postfix_functions: HashMap::new(),
            infix_functions: HashMap::new(),
        };

        // Prefix
        parser.register_prefix(mem::discriminant(&Token::Constant(1)), Self::parse_constant);
        parser.register_prefix(mem::discriminant(&Token::Minus), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&Token::Tilde), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&Token::Not), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&Token::Increment), Self::parse_unary);
        parser.register_prefix(mem::discriminant(&Token::Decrement), Self::parse_unary);
        parser.register_prefix(
            mem::discriminant(&Token::OpenParen),
            Self::parse_grouped_expression,
        );
        parser.register_prefix(
            mem::discriminant(&Token::Identifier(String::new())),
            Self::parse_identifier,
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
        parser.register_infix(
            mem::discriminant(&Token::Assign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::PlusAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::MinusAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::AsteriskAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::SlashAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::PercentAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseAndAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseOrAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseXorAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseShiftLeftAssign),
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            mem::discriminant(&Token::BitwiseShiftRightAssign),
            Self::parse_assignment_expression,
        );

        parser.register_postfix(mem::discriminant(&Token::Increment), Self::parse_postfix);
        parser.register_postfix(mem::discriminant(&Token::Decrement), Self::parse_postfix);

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

    fn parse_expression_try_postfix(
        &mut self,
        mut left_exp: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        if self
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

            return Ok(left_exp);
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

    fn parse_assignment_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        let op: ast::AssignmentOperator = match self.cur_token {
            Token::Assign => ast::AssignmentOperator::None,
            Token::PlusAssign => ast::AssignmentOperator::Add,
            Token::MinusAssign => ast::AssignmentOperator::Subtract,
            Token::AsteriskAssign => ast::AssignmentOperator::Multiply,
            Token::SlashAssign => ast::AssignmentOperator::Divide,
            Token::PercentAssign => ast::AssignmentOperator::Reminder,
            Token::BitwiseAndAssign => ast::AssignmentOperator::BitwiseAnd,
            Token::BitwiseOrAssign => ast::AssignmentOperator::BitwiseOr,
            Token::BitwiseXorAssign => ast::AssignmentOperator::BitwiseXor,
            Token::BitwiseShiftLeftAssign => ast::AssignmentOperator::ShiftLeft,
            Token::BitwiseShiftRightAssign => ast::AssignmentOperator::ShiftRight,
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

    fn parse_constant(&mut self) -> Result<ast::Expression, ParserError> {
        if let Token::Constant(val) = &self.cur_token {
            return Ok(ast::Expression::Constant(*val));
        }

        Err(ParserError::UnexpectedToken {
            expected: Token::Constant(0),
            actual: self.cur_token.clone(),
        })
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, ParserError> {
        if let Token::Identifier(val) = self.cur_token.clone() {
            return Ok(ast::Expression::Var(val));
        }

        Err(ParserError::UnexpectedToken {
            expected: Token::Identifier(String::new()),
            actual: self.cur_token.clone(),
        })
    }

    fn parse_unary(&mut self) -> Result<ast::Expression, ParserError> {
        match &self.cur_token {
            Token::Minus => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Negate,
                    expression: Box::new(expr),
                })
            }
            Token::Tilde => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Complement,
                    expression: Box::new(expr),
                })
            }
            Token::Not => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Not,
                    expression: Box::new(expr),
                })
            }
            Token::Increment => {
                self.next_token()?;
                let expr = self.parse_expression(Precedence::Prefix)?;
                Ok(ast::Expression::Unary {
                    op: ast::UnaryOperator::Increment,
                    expression: Box::new(expr),
                })
            }
            Token::Decrement => {
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
            Token::Increment => Ok(ast::Expression::Postfix(
                ast::PostfixOperator::Increment,
                Box::new(lhs),
            )),
            Token::Decrement => Ok(ast::Expression::Postfix(
                ast::PostfixOperator::Decrement,
                Box::new(lhs),
            )),
            _ => Err(ParserError::NoPostfixOperatorFound(self.cur_token.clone())),
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
            Token::Semicolon => Ok(ast::Statement::Null),
            _ => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(Token::Semicolon)?;
                Ok(ast::Statement::Expression(expr))
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::Semicolon)?;

        Ok(ast::Statement::Return(expr))
    }

    fn parse_declaration(&mut self) -> Result<ast::Declaration, ParserError> {
        match &self.cur_token {
            Token::KWInt => {
                self.next_token()?;

                if let Token::Identifier(ident) = self.cur_token.clone() {
                    self.next_token()?;
                    match &self.cur_token {
                        Token::Semicolon => Ok(ast::Declaration {
                            name: ident,
                            exp: None,
                        }),
                        Token::Assign => {
                            self.next_token()?;
                            let expr = self.parse_expression(self.cur_precedence())?;
                            self.expect_peek(Token::Semicolon)?;
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
                        expected: Token::Identifier(String::new()),
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
            Token::KWInt => Ok(ast::BlockItem::D(self.parse_declaration()?)),
            _ => Ok(ast::BlockItem::S(self.parse_statement()?)),
        }
    }

    fn parse_function_definition(&mut self) -> Result<ast::FunctionDefinition, ParserError> {
        self.expect(Token::KWInt)?;
        if let Token::Identifier(name) = self.peek_token.clone() {
            self.next_token()?;
            self.expect_peek(Token::OpenParen)?;
            self.expect_peek(Token::KWVoid)?;
            self.expect_peek(Token::CloseParen)?;
            self.expect_peek(Token::OpenBrace)?;

            let mut body: Vec<BlockItem> = vec![];
            while !self.peek_token_is(&Token::CloseBrace) {
                body.push(self.parse_block_item()?);
            }
            self.next_token()?; // Eat CloseBrace

            Ok(ast::FunctionDefinition {
                name: name.to_string(),
                body,
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

    fn register_postfix(&mut self, token: Discriminant<Token>, func: PostfixFunction) {
        self.postfix_functions.insert(token, func);
    }

    fn register_infix(&mut self, token: Discriminant<Token>, func: InfixFunction) {
        self.infix_functions.insert(token, func);
    }

    /// Expects current token to, but does not advance
    fn expect(&mut self, expected: Token) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.cur_token) {
            return Ok(());
        }
        Err(ParserError::UnexpectedToken {
            expected,
            actual: self.cur_token.clone(),
        })
    }

    fn expect_peek(&mut self, expected: Token) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.peek_token) {
            self.next_token()?;
            return Ok(());
        }
        Err(ParserError::UnexpectedToken {
            expected,
            actual: self.cur_token.clone(),
        })
    }

    fn peek_token_is(&self, token: &Token) -> bool {
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
