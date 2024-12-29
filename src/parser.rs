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
    #[error("Unexpected Token, expected one of: {expected:?}, actual: \"{actual:?}\"")]
    UnexpectedTokens {
        expected: Vec<TokenKind>,
        actual: TokenKind,
    },
    #[error("Expected an '=' or ';' after a variable declaration. Got \"{0:?}\"")]
    DeclarationExpectedAssignOrSemicolon(TokenKind),
    #[error("Expected a list of parameters, got \"{0:?}\"")]
    ExpectedParamList(TokenKind),
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
                actual: self.peek_token.kind.clone(),
            })
        }
    }

    fn peek_token_is(&self, token: TokenKind) -> bool {
        mem::discriminant(&self.peek_token.kind) == mem::discriminant(&token)
    }

    fn cur_token_is(&self, token: TokenKind) -> bool {
        mem::discriminant(&self.cur_token.kind) == mem::discriminant(&token)
    }

    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(&self.peek_token.kind).unwrap_or(Precedence::Lowest)
    }

    fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(&self.cur_token.kind).unwrap_or(Precedence::Lowest)
    }

    pub fn parse_program(&mut self) -> Result<ast::Program, ParserError> {
        let mut funcs = vec![];

        while !self.cur_token_is(TokenKind::Eof) {
            funcs.push(self.parse_function_declaration_full()?);
            self.next_token()?;
        }

        Ok(ast::Program {
            function_declrations: funcs,
        })
    }

    fn parse_function_declaration_full(&mut self) -> Result<ast::FunctionDeclaration, ParserError> {
        self.next_token()?;
        if let TokenKind::Identifier(name) = self.cur_token.kind.clone() {
            self.expect_peek(TokenKind::OpenParen)?;
            self.parse_function_declaration(name)
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier(String::new()),
                actual: self.cur_token.kind.clone(),
            })
        }
    }

    // Expects to be on (
    fn parse_function_declaration(
        &mut self,
        identifier: String,
    ) -> Result<ast::FunctionDeclaration, ParserError> {
        self.expect(TokenKind::OpenParen)?;
        self.next_token()?;

        let params = self.parse_param_list()?;

        self.next_token()?;

        let block = match &self.cur_token.kind {
            TokenKind::Semicolon => None,
            TokenKind::OpenBrace => Some(self.parse_block()?),
            tok => {
                return Err(ParserError::UnexpectedTokens {
                    expected: vec![TokenKind::Semicolon, TokenKind::OpenBrace],
                    actual: tok.clone(),
                })
            }
        };

        Ok(ast::FunctionDeclaration {
            name: identifier,
            body: block,
            params,
        })
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

    fn parse_param_list(&mut self) -> Result<Vec<ast::Identifier>, ParserError> {
        match &self.cur_token.kind {
            TokenKind::KWVoid => {
                self.expect_peek(TokenKind::CloseParen)?;
                Ok(vec![])
            }
            TokenKind::KWInt => {
                let mut params = vec![];
                while &self.cur_token.kind != &TokenKind::CloseParen {
                    self.expect_peek(TokenKind::Identifier(String::new()))?;
                    let ident = match &self.cur_token.kind {
                        TokenKind::Identifier(content) => content.clone(),
                        _ => unreachable!(),
                    };
                    params.push(ident);
                    // Comma or close paren
                    self.next_token()?;
                    match &self.cur_token.kind {
                        TokenKind::Comma => {
                            self.expect_peek(TokenKind::KWInt)?;
                        }
                        TokenKind::CloseParen => {}
                        tok => {
                            return Err(ParserError::UnexpectedTokens {
                                expected: vec![TokenKind::CloseParen, TokenKind::Comma],
                                actual: tok.clone(),
                            })
                        }
                    };
                }
                Ok(params)
            }
            tok => Err(ParserError::ExpectedParamList(tok.clone())),
        }
    }

    fn parse_variable_declaration_full(&mut self) -> Result<ast::VariableDeclaration, ParserError> {
        self.expect(TokenKind::KWInt)?;
        self.next_token()?;
        if let TokenKind::Identifier(ident) = self.cur_token.kind.clone() {
            self.next_token()?;
            self.parse_variable_declaration(ident)
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier(String::new()),
                actual: self.cur_token.kind.clone(),
            })
        }
    }

    fn parse_variable_declaration(
        &mut self,
        identifier: String,
    ) -> Result<ast::VariableDeclaration, ParserError> {
        match &self.cur_token.kind {
            TokenKind::Semicolon => Ok(ast::VariableDeclaration {
                name: identifier,
                exp: None,
            }),
            TokenKind::Assign => {
                self.next_token()?;
                let expr = self.parse_expression(self.cur_precedence())?;
                self.expect_peek(TokenKind::Semicolon)?;
                Ok(ast::VariableDeclaration {
                    name: identifier,
                    exp: Some(expr),
                })
            }
            tok => Err(ParserError::UnexpectedTokens {
                expected: vec![TokenKind::Semicolon, TokenKind::Assign],
                actual: tok.clone(),
            }),
        }
    }

    // WARN: This will parse a semicolon!
    fn parse_declaration(&mut self) -> Result<ast::Declaration, ParserError> {
        self.next_token()?;

        if let TokenKind::Identifier(ident) = self.cur_token.kind.clone() {
            self.next_token()?;
            match &self.cur_token.kind {
                TokenKind::Semicolon => Ok(ast::Declaration::VarDecl(ast::VariableDeclaration {
                    name: ident,
                    exp: None,
                })),
                TokenKind::Assign => {
                    self.next_token()?;
                    let expr = self.parse_expression(self.cur_precedence())?;
                    self.expect_peek(TokenKind::Semicolon)?;
                    Ok(ast::Declaration::VarDecl(ast::VariableDeclaration {
                        name: ident,
                        exp: Some(expr),
                    }))
                }
                TokenKind::OpenParen => self
                    .parse_function_declaration(ident)
                    .map(|ok| ast::Declaration::FunDecl(ok)),
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
            TokenKind::KWBreak => self.parse_break_statement(),
            TokenKind::KWCase => self.parse_case_statement(),
            TokenKind::KWDefault => self.parse_default_case_statement(),
            TokenKind::KWContinue => self.parse_continue_statement(),
            TokenKind::KWDo => self.parse_do_while_statement(),
            TokenKind::KWFor => self.parse_for_statement(),
            TokenKind::KWReturn => self.parse_return_statement(),
            TokenKind::KWSwitch => self.parse_switch_statement(),
            TokenKind::KWIf => self.parse_if_statement(),
            TokenKind::KWGoto => self.parse_goto_statement(),
            TokenKind::KWWhile => self.parse_while_statement(),

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

    fn parse_while_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::OpenParen)?;
        self.next_token()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::CloseParen)?;
        self.next_token()?;
        let stmt = self.parse_statement()?;
        Ok(ast::Statement::While {
            condition: expr,
            body: Box::new(stmt),
            label: None,
        })
    }

    fn parse_do_while_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.next_token()?;
        let stmt = self.parse_statement()?;
        self.expect_peek(TokenKind::KWWhile)?;
        self.expect_peek(TokenKind::OpenParen)?;
        self.next_token()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::CloseParen)?;
        self.expect_peek(TokenKind::Semicolon)?;
        Ok(ast::Statement::DoWhile {
            body: Box::new(stmt),
            condition: expr,
            label: None,
        })
    }

    fn parse_for_init(&mut self) -> Result<ast::ForInit, ParserError> {
        match self.cur_token.kind {
            TokenKind::Semicolon => Ok(ast::ForInit::None),
            TokenKind::KWInt => {
                let decl = self.parse_variable_declaration_full()?;
                Ok(ast::ForInit::InitDecl(decl))
            }
            _ => {
                let expr = self.parse_expression(Precedence::Lowest)?;
                self.expect_peek(TokenKind::Semicolon)?;
                Ok(ast::ForInit::InitExp(expr))
            }
        }
    }

    fn parse_for_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::OpenParen)?;
        self.next_token()?;
        let init = self.parse_for_init()?;
        self.next_token()?;
        let optional_condition = self.parse_optional_expression(TokenKind::Semicolon)?;
        self.next_token()?;
        let optional_end = self.parse_optional_expression(TokenKind::CloseParen)?;

        self.next_token()?;

        let body = Box::new(self.parse_statement()?);

        Ok(ast::Statement::For {
            init,
            condition: optional_condition,
            post: optional_end,
            body,
            label: None,
        })
    }

    fn parse_switch_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::OpenParen)?;
        self.next_token()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::CloseParen)?;
        self.next_token()?;
        let stmt = self.parse_statement()?;
        Ok(ast::Statement::Switch {
            expression: expr,
            body: Box::new(stmt),
            label: None,
            cases: None,
        })
    }

    fn parse_break_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::Semicolon)?;
        Ok(ast::Statement::Break(String::new()))
    }

    fn parse_continue_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::Semicolon)?;
        Ok(ast::Statement::Continue(String::new()))
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

    fn parse_default_case_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.expect_peek(TokenKind::Colon)?;
        self.next_token()?;
        Ok(ast::Statement::Default(
            Box::new(self.parse_statement()?),
            String::new(),
        ))
    }

    fn parse_case_statement(&mut self) -> Result<ast::Statement, ParserError> {
        self.next_token()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect_peek(TokenKind::Colon)?;
        self.next_token()?;
        Ok(ast::Statement::Case(
            expr,
            Box::new(self.parse_statement()?),
            String::new(),
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

    fn parse_optional_expression(
        &mut self,
        end: TokenKind,
    ) -> Result<Option<ast::Expression>, ParserError> {
        if self.cur_token_is(end.clone()) {
            Ok(None)
        } else {
            let expr = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(end)?;
            Ok(Some(expr))
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

    fn parse_argument_list(&mut self) -> Result<Vec<ast::Expression>, ParserError> {
        self.expect(TokenKind::OpenParen)?;

        let mut args = vec![];

        if !self.peek_token_is(TokenKind::CloseParen) {
            self.next_token()?;
            loop {
                args.push(self.parse_expression(Precedence::Lowest)?);

                if self.peek_token_is(TokenKind::CloseParen) {
                    self.next_token()?;
                    break Ok(args);
                }
                self.expect_peek(TokenKind::Comma)?;
                self.next_token()?;
            }
        } else {
            self.next_token()?;
            Ok(args)
        }
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, ParserError> {
        if let TokenKind::Identifier(val) = self.cur_token.kind.clone() {
            match &self.peek_token.kind {
                TokenKind::OpenParen => {
                    self.next_token()?;
                    let args = self.parse_argument_list()?;
                    Ok(ast::Expression::FunctionCall(val, args))
                }
                _ => Ok(ast::Expression::Var(val)),
            }
        } else {
            Err(ParserError::UnexpectedToken {
                expected: TokenKind::Identifier(String::new()),
                actual: self.cur_token.kind.clone(),
            })
        }
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
            program.function_declrations[0].body,
            Some(ast::Block(vec![
                ast::BlockItem::D(ast::Declaration::VarDecl(ast::VariableDeclaration {
                    name: "a".to_owned(),
                    exp: Some(ast::Expression::Var("hello".to_owned()))
                })),
                ast::BlockItem::D(ast::Declaration::VarDecl(ast::VariableDeclaration {
                    name: "b".to_owned(),
                    exp: None,
                })),
            ]))
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

        assert_eq!(program.function_declrations[0].name, "main".to_owned());
        assert_eq!(
            program.function_declrations[0].body,
            Some(ast::Block(vec![ast::BlockItem::S(ast::Statement::Return(
                ast::Expression::Binary {
                    op: BinaryOperator::Multiply,
                    lhs: Box::new(ast::Expression::Unary {
                        op: ast::UnaryOperator::Negate,
                        expression: Box::new(ast::Expression::Constant(2))
                    },),
                    rhs: Box::new(ast::Expression::Constant(2))
                }
            ))]))
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

        assert_eq!(program.function_declrations[0].name, "main".to_owned());
        assert_eq!(
            program.function_declrations[0].body,
            Some(ast::Block(vec![ast::BlockItem::S(ast::Statement::Return(
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
            ))]))
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

        assert_eq!(program.function_declrations[0].name, "main".to_owned());
        assert_eq!(
            program.function_declrations[0].body,
            Some(ast::Block(vec![ast::BlockItem::S(ast::Statement::Return(
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
            ))]))
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

        assert_eq!(program.function_declrations[0].name, "main".to_owned());
        assert_eq!(
            program.function_declrations[0].body,
            Some(ast::Block(vec![ast::BlockItem::S(ast::Statement::Return(
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
            ))]))
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

        let expected_result = ast::Block(vec![ast::BlockItem::S(ast::Statement::Return(
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
        ))]);

        assert_eq!(program.function_declrations[0].name, "main".to_owned());
        assert_eq!(program.function_declrations[0].body, Some(expected_result));
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

        let expected_result = ast::Block(vec![ast::BlockItem::S(ast::Statement::Return(
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
        ))]);

        assert_eq!(program.function_declrations[0].name, "main".to_owned());
        assert_eq!(program.function_declrations[0].body, Some(expected_result));
    }
}
