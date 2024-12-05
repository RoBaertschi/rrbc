use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

use thiserror::Error;

use crate::{
    ast::{self, BinaryOperator},
    lexer::{Lexer, LexerError, Loc, Token, TokenKind},
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

type PrefixFunction = fn(&mut Parser) -> Result<ast::Expression, ParserError>;
type PostfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;
type InfixFunction = fn(&mut Parser, ast::Expression) -> Result<ast::Expression, ParserError>;

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,

    prefix_functions: HashMap<Discriminant<TokenKind>, PrefixFunction>,
    postfix_functions: HashMap<Discriminant<TokenKind>, PostfixFunction>,
    infix_functions: HashMap<Discriminant<TokenKind>, InfixFunction>,
}

impl Parser {
    pub fn try_build(lexer: Lexer) -> Result<Self, ParserError> {
        let mut parser = Self {
            lexer,
            cur_token: Token {
                kind: TokenKind::Eof,
                loc: Loc { line: 0, column: 0 },
            },
            peek_token: Token {
                kind: TokenKind::Eof,
                loc: Loc { line: 0, column: 0 },
            },
            prefix_functions: HashMap::new(),
            postfix_functions: HashMap::new(),
            infix_functions: HashMap::new(),
        };
        // Prefix
        parser.register_prefix(&TokenKind::Constant(1), Self::parse_constant);
        parser.register_prefix(&TokenKind::Minus, Self::parse_unary);
        parser.register_prefix(&TokenKind::Tilde, Self::parse_unary);
        parser.register_prefix(&TokenKind::Not, Self::parse_unary);
        parser.register_prefix(&TokenKind::Increment, Self::parse_unary);
        parser.register_prefix(&TokenKind::Decrement, Self::parse_unary);
        parser.register_prefix(&TokenKind::OpenParen, Self::parse_grouped_expression);
        parser.register_prefix(
            &TokenKind::Identifier(String::new()),
            Self::parse_identifier,
        );

        // Infix
        parser.register_infix(&TokenKind::Plus, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Minus, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Asterisk, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Slash, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Percent, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::ShiftLeft, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::ShiftRight, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::BitwiseAnd, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Xor, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::BitwiseOr, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Or, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::And, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::LessThan, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::LessOrEqual, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::GreaterThan, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::GreaterOrEqual, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Equal, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::NotEqual, Self::parse_binary_expression);
        parser.register_infix(&TokenKind::Assign, Self::parse_assignment_expression);
        parser.register_infix(&TokenKind::PlusAssign, Self::parse_assignment_expression);
        parser.register_infix(&TokenKind::MinusAssign, Self::parse_assignment_expression);
        parser.register_infix(
            &TokenKind::AsteriskAssign,
            Self::parse_assignment_expression,
        );
        parser.register_infix(&TokenKind::SlashAssign, Self::parse_assignment_expression);
        parser.register_infix(&TokenKind::PercentAssign, Self::parse_assignment_expression);
        parser.register_infix(
            &TokenKind::BitwiseAndAssign,
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            &TokenKind::BitwiseOrAssign,
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            &TokenKind::BitwiseXorAssign,
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            &TokenKind::BitwiseShiftLeftAssign,
            Self::parse_assignment_expression,
        );
        parser.register_infix(
            &TokenKind::BitwiseShiftRightAssign,
            Self::parse_assignment_expression,
        );
        parser.register_infix(&TokenKind::QuestionMark, Self::parse_conditional);

        parser.register_postfix(&TokenKind::Increment, Self::parse_postfix);
        parser.register_postfix(&TokenKind::Decrement, Self::parse_postfix);

        parser.next_token()?;
        parser.next_token()?;

        Ok(parser)
    }

    fn next_token(&mut self) -> Result<(), LexerError> {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token()?);
        Ok(())
    }

    fn register_prefix(&mut self, token: &TokenKind, func: PrefixFunction) {
        self.prefix_functions.insert(mem::discriminant(token), func);
    }

    fn register_postfix(&mut self, token: &TokenKind, func: PostfixFunction) {
        self.postfix_functions
            .insert(mem::discriminant(token), func);
    }

    fn register_infix(&mut self, token: &TokenKind, func: InfixFunction) {
        self.infix_functions.insert(mem::discriminant(token), func);
    }

    fn expect(&mut self, expected: TokenKind) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.cur_token.kind) {
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken {
                expected,
                actual: self.cur_token.kind.clone(),
            })
        }
    }

    fn expect_peek(&mut self, expected: TokenKind) -> Result<(), ParserError> {
        if mem::discriminant(&expected) == mem::discriminant(&self.peek_token.kind) {
            self.next_token()?;
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken {
                expected,
                actual: self.cur_token.kind.clone(),
            })
        }
    }

    fn peek_token_is(&self, token: TokenKind) -> bool {
        mem::discriminant(&self.peek_token.kind) == mem::discriminant(&token)
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(&self.peek_token.kind).unwrap_or(Precedence::Lowest)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(&self.cur_token.kind).unwrap_or(Precedence::Lowest)
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParserError> {
        let function_definition = self.parse_function_definition()?;

        self.expect_peek(TokenKind::Eof)?;

        Ok(ast::Program {
            function_definition,
        })
    }

    fn parse_function_definition(&mut self) -> Result<ast::FunctionDefinition, ParserError> {
        self.expect(TokenKind::KWInt)?;
        if let TokenKind::Identifier(name) = self.peek_token.kind.clone() {
            self.next_token()?;
            self.expect_peek(TokenKind::OpenParen)?;
            self.expect_peek(TokenKind::KWVoid)?;
            self.expect_peek(TokenKind::CloseParen)?;
            self.expect_peek(TokenKind::OpenBrace)?;

            let block = self.parse_block()?;

            Ok(ast::FunctionDefinition {
                name: name.to_string(),
                body: block,
            })
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier("expected".to_owned()),
                actual: self.cur_token.kind.clone(),
            })
        }
    }

    fn parse_block_item(&mut self) -> Result<ast::BlockItem, ParserError> {
        match &self.cur_token.kind {
            TokenKind::KWInt => Ok(ast::BlockItem::D(self.parse_declaration()?)),
            _ => Ok(ast::BlockItem::S(self.parse_statement()?)),
        }
    }

    fn parse_block(&mut self) -> Result<ast::Block, ParserError> {
        let mut body: Vec<ast::BlockItem> = vec![];
        while !self.peek_token_is(TokenKind::CloseBrace) {
            self.next_token()?;
            body.push(self.parse_block_item()?);
        }
        self.next_token()?; // Eat CloseBrace
                            // }
                            // ^
        Ok(ast::Block(body))
    }

    fn parse_declaration(&mut self) -> Result<ast::Declaration, ParserError> {
        self.next_token()?;

        if let TokenKind::Identifier(ident) = self.cur_token.kind.clone() {
            self.next_token()?;
            match &self.cur_token.kind {
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
                actual: self.cur_token.kind.clone(),
            })
        }
    }

    // Statements

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match &self.cur_token.kind {
            TokenKind::KWReturn => self.parse_return_statement(),
            TokenKind::KWIf => self.parse_if_statement(),
            TokenKind::KWGoto => self.parse_goto_statement(),
            TokenKind::Semicolon => Ok(ast::Statement::Null),
            TokenKind::OpenBrace => self.parse_compound_statement(),
            TokenKind::Identifier(ident) if self.peek_token_is(TokenKind::Colon) => {
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
        if let TokenKind::Identifier(label_name) = self.peek_token.kind.clone() {
            self.next_token()?;
            self.expect_peek(TokenKind::Semicolon)?;
            Ok(ast::Statement::Goto(label_name))
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier(String::new()),
                actual: self.peek_token.kind.clone(),
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

    fn parse_compound_statement(&mut self) -> Result<ast::Statement, ParserError> {
        let block = self.parse_block()?;
        Ok(ast::Statement::Compound(block))
    }

    fn parse_if_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::OpenParen)?;
        self.next_token()?;
        let condition = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::CloseParen)?;
        self.next_token()?;
        let stmt = self.parse_statement()?;

        let else_stmt = match self.peek_token.kind {
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

    // Expressions

    fn parse_expression(&mut self, precedence: Precedence) -> Result<ast::Expression, ParserError> {
        let prefix = self
            .prefix_functions
            .get(&mem::discriminant(&self.cur_token.kind));

        match prefix {
            Some(prefix) => {
                let mut left_exp = prefix(self)?;

                while !self.peek_token_is(TokenKind::Semicolon)
                    && precedence < self.peek_precedence()
                {
                    if self
                        .infix_functions
                        .contains_key(&mem::discriminant(&self.peek_token.kind))
                    {
                        self.next_token()?;
                        left_exp = self
                            .infix_functions
                            .get(&mem::discriminant(&self.cur_token.kind))
                            .unwrap()(self, left_exp)?;
                    } else {
                        return self.parse_expression_postfix(left_exp);
                    }
                }

                self.parse_expression_postfix(left_exp)
            }
            None => Err(ParserError::NoPrefixFunction(self.cur_token.kind.clone())),
        }
    }

    fn parse_expression_postfix(
        &mut self,
        mut left_exp: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        while self
            .postfix_functions
            .contains_key(&mem::discriminant(&self.peek_token.kind))
        {
            self.next_token()?;
            let postfix = self
                .postfix_functions
                .get(&mem::discriminant(&self.cur_token.kind));

            if let Some(postfix) = postfix {
                left_exp = postfix(self, left_exp)?;
            }
        }
        return Ok(left_exp);
    }

    fn parse_binary_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        let bin = match &self.cur_token.kind {
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
            _ => {
                unreachable!("Wrong token passed to parse_binary_expression, should not happen. Offending Token: {:?}", &self.cur_token)
            }
        };

        let precedence = self.cur_precedence();
        self.next_token()?;
        let right = self.parse_expression(precedence)?;

        Ok(ast::Expression::Binary {
            op: bin,
            lhs: Box::new(left),
            rhs: Box::new(right),
        })
    }

    fn parse_assignment_expression(
        &mut self,
        left: ast::Expression,
    ) -> Result<ast::Expression, ParserError> {
        let op: ast::AssignmentOperator = match self.cur_token.kind {
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
        Ok(ast::Expression::Assignment {
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
        Ok(ast::Expression::Conditional {
            condition: Box::new(left),
            then,
            r#else,
        })
    }

    fn parse_constant(&mut self) -> Result<ast::Expression, ParserError> {
        if let TokenKind::Constant(val) = &self.cur_token.kind {
            return Ok(ast::Expression::Constant(*val));
        }

        Err(ParserError::UnexpectedToken {
            expected: TokenKind::Constant(0),
            actual: self.cur_token.kind.clone(),
        })
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, ParserError> {
        if let TokenKind::Identifier(val) = self.cur_token.kind.clone() {
            return Ok(ast::Expression::Var(val));
        }

        Err(ParserError::UnexpectedToken {
            expected: TokenKind::Identifier(String::new()),
            actual: self.cur_token.kind.clone(),
        })
    }

    fn parse_unary(&mut self) -> Result<ast::Expression, ParserError> {
        match &self.cur_token.kind {
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
            _ => unreachable!("Wrong token passed to parse_unary, should not happen."),
        }
    }

    fn parse_postfix(&mut self, lhs: ast::Expression) -> Result<ast::Expression, ParserError> {
        match &self.cur_token.kind {
            TokenKind::Increment => Ok(ast::Expression::Postfix(
                ast::PostfixOperator::Increment,
                Box::new(lhs),
            )),
            TokenKind::Decrement => Ok(ast::Expression::Postfix(
                ast::PostfixOperator::Decrement,
                Box::new(lhs),
            )),
            _ => unreachable!("Wrong token passed to parse_postfix, should not happen."),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression, ParserError> {
        self.next_token()?;

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(TokenKind::CloseParen)?;

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_declaration() {
        let input = "int main(void) { int a = hello; int b; }".to_owned();
        let lexer = Lexer::new(input);

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
        let input = r"
        int main(void) {
            return -2 * 2;
        }
        "
        .to_owned();
        let lexer = Lexer::new(input);
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                ast::Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(ast::Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(ast::Expression::Constant(2))
                    },),
                    rhs: Box::new(ast::Expression::Constant(2))
                }
            ))]
        );
    }

    #[test]
    fn test_precedence2() {
        let input = r#"
        int main(void) {
            return -2 * (~2);
        }
        "#
        .to_owned();
        let lexer = Lexer::new(input);
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                ast::Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(ast::Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(ast::Expression::Constant(2))
                    },),
                    rhs: Box::new(ast::Expression::Unary {
                        op: ast::UnaryOperator::Complement,
                        expression: Box::new(ast::Expression::Constant(2))
                    })
                }
            ))]
        );
    }

    #[test]
    fn test_precedence3() {
        let input = r"
        int main(void) {
            return -2 * (~2 * 4);
        }
        "
        .to_owned();
        let lexer = Lexer::new(input);
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                ast::Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(ast::Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(ast::Expression::Constant(2))
                    },),
                    rhs: Box::new(ast::Expression::Binary {
                        op: BinaryOperator::Multiply,
                        lhs: Box::new(ast::Expression::Unary {
                            op: ast::UnaryOperator::Complement,
                            expression: Box::new(ast::Expression::Constant(2))
                        }),
                        rhs: Box::new(ast::Expression::Constant(4))
                    })
                }
            ))]
        );
    }

    #[test]
    fn test_precedence4() {
        let input = r"
        int main(void) {
            return (3-2) * (~2);
        }
        "
        .to_owned();
        let lexer = Lexer::new(input);
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(
            program.function_definition.body,
            vec![ast::BlockItem::S(ast::Statement::Return(
                ast::Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(ast::Expression::Binary {
                        op: BinaryOperator::Subtract,
                        lhs: Box::new(ast::Expression::Constant(3)),
                        rhs: Box::new(ast::Expression::Constant(2))
                    },),
                    rhs: Box::new(ast::Expression::Unary {
                        op: ast::UnaryOperator::Complement,
                        expression: Box::new(ast::Expression::Constant(2))
                    })
                }
            ))]
        );
    }
    #[test]
    fn test_bitwise_prec() {
        let input = r"
        int main(void) {
            return 40 << 4 + 12 >> 1;
        }
        "
        .to_owned();
        let lexer = Lexer::new(input);
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        let expected_result = vec![ast::BlockItem::S(ast::Statement::Return(
            ast::Expression::Binary {
                op: BinaryOperator::ShiftRight,
                lhs: Box::new(ast::Expression::Binary {
                    op: BinaryOperator::ShiftLeft,
                    lhs: Box::new(ast::Expression::Constant(40)),
                    rhs: Box::new(ast::Expression::Binary {
                        op: BinaryOperator::Add,
                        lhs: Box::new(ast::Expression::Constant(4)),
                        rhs: Box::new(ast::Expression::Constant(12)),
                    }),
                }),
                rhs: Box::new(ast::Expression::Constant(1)),
            },
        ))];

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(program.function_definition.body, expected_result);
    }

    #[test]
    fn test_bitwise_prec2() {
        let input = r"
        int main(void) {
            return 1 << 2 & 3 ^ 2 | 3;
        }
        "
        .to_owned();
        let lexer = Lexer::new(input);
        let mut parser = Parser::try_build(lexer).expect("parser should be created successfully");

        let program = parser
            .parse_program()
            .expect("the program should be parsed successfully");

        let expected_result = vec![ast::BlockItem::S(ast::Statement::Return(
            ast::Expression::Binary {
                op: BinaryOperator::BitwiseOr,
                lhs: Box::new(ast::Expression::Binary {
                    op: BinaryOperator::Xor,
                    lhs: Box::new(ast::Expression::Binary {
                        op: BinaryOperator::BitwiseAnd,
                        lhs: Box::new(ast::Expression::Binary {
                            op: BinaryOperator::ShiftLeft,
                            lhs: Box::new(ast::Expression::Constant(1)),
                            rhs: Box::new(ast::Expression::Constant(2)),
                        }),
                        rhs: Box::new(ast::Expression::Constant(3)),
                    }),
                    rhs: Box::new(ast::Expression::Constant(2)),
                }),
                rhs: Box::new(ast::Expression::Constant(3)),
            },
        ))];

        assert_eq!(program.function_definition.name, "main".to_owned());
        assert_eq!(program.function_definition.body, expected_result);
    }
}
