use std::{
    collections::HashMap,
    mem::{self, Discriminant},
};

use crate::{
    ast::{self},
    lexer::{Lexer, LexerError, Token},
};

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

        parser.register_prefix(mem::discriminant(&Token::Constant(1)), Self::parse_constant);

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

    fn parse_expression(&mut self) -> Result<ast::Expression, ParserError> {
        let prefix = self
            .prefix_functions
            .get(&mem::discriminant(&self.cur_token));

        match prefix {
            Some(prefix) => {
                let mut left_exp = prefix(self)?;

                while !self.peek_token_is(&Token::Semicolon) {
                    if self
                        .infix_functions
                        .contains_key(&mem::discriminant(&self.cur_token))
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

    fn parse_constant(&mut self) -> Result<ast::Expression, ParserError> {
        if let Token::Constant(val) = &self.cur_token {
            return Ok(ast::Expression::Constant(*val));
        }

        Err(ParserError::UnexpectedToken {
            expected: Token::Constant(0),
            actual: self.cur_token.clone(),
        })
    }

    fn parse_statement(&mut self) -> Result<ast::Statement, ParserError> {
        match self.cur_token {
            Token::KWReturn => {
                self.next_token()?;

                let expr = self.parse_expression()?;

                self.expect_peek(Token::Semicolon)?;

                Ok(ast::Statement::Return(expr))
            }
            _ => Err(ParserError::CouldNotParseStatement),
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_function() {
        let lexer = Lexer::new(
            r"
        int main(void) {
            return 2;
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
            ast::Statement::Return(ast::Expression::Constant(2)),
        );
    }
}
