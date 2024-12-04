use std::vec::IntoIter;

use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum LexerError {
    #[error("The character '{0}' could not be represented")]
    UnknownCharacter(char),
    #[error("Could not convert the number \"{0}\" to a number.")]
    InvalidNumber(String),
    #[error("Identifiers can not start with numbers.")]
    IdentifierStartedWithNumber,
}

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone)]
pub enum TokenKind {
    Eof,
    Identifier(String),
    Constant(i32),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,

    // Ternary
    QuestionMark, // ?
    Colon,        // :

    // Operator
    Minus,          // -
    Plus,           // +
    Asterisk,       // *
    Slash,          // /
    Percent,        // %
    Tilde,          // ~
    BitwiseAnd,     // &
    BitwiseOr,      // |
    Xor,            // ^
    ShiftLeft,      // <<
    ShiftRight,     // >>
    Not,            // !
    And,            // &&
    Or,             // ||
    Equal,          // ==
    NotEqual,       // !=
    LessThan,       // <
    GreaterThan,    // >
    LessOrEqual,    // <=
    GreaterOrEqual, // >=

    // Compound Assignment
    Assign,                  // =
    PlusAssign,              // +=
    MinusAssign,             // -=
    AsteriskAssign,          // *=
    SlashAssign,             // /=
    PercentAssign,           // %=
    BitwiseAndAssign,        // &=
    BitwiseOrAssign,         // |=
    BitwiseXorAssign,        // ^=
    BitwiseShiftLeftAssign,  // <<=
    BitwiseShiftRightAssign, // >>=

    Decrement, // --
    Increment, // ++

    // Keywords
    KWInt,
    KWReturn,
    KWVoid,
    KWIf,
    KWElse,
    KWGoto,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
}

impl TokenKind {
    pub fn from_string(string: &str) -> Self {
        match string {
            "int" => Self::KWInt,
            "return" => Self::KWReturn,
            "void" => Self::KWVoid,
            "if" => Self::KWIf,
            "else" => Self::KWElse,
            "goto" => Self::KWGoto,
            _ => Self::Identifier(string.to_owned()),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    _input: String,
    chars: IntoIter<char>,
    loc: Loc,

    ch: char,
    peek_ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            chars: input.chars().collect::<Vec<_>>().into_iter(),
            _input: input,
            ch: '\0',
            peek_ch: '\0',

            loc: Loc { column: 0, line: 1 },
        };

        lexer.peek_ch = lexer.chars.next().unwrap_or('\0');
        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> char {
        self.peek_ch
    }

    fn is_digit(&self) -> bool {
        match self.ch {
            '0'..='9' => true,
            _ => false,
        }
    }

    fn is_valid_identifier_char(&self) -> bool {
        match self.ch {
            'a'..='z' => true,
            'A'..='Z' => true,
            '_' => true,
            _ => self.is_digit(),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\n' || self.ch == '\r' || self.ch == '\t' {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.ch == '\n' {
            self.loc.column = 0;
            self.loc.line += 1;
        }
        self.ch = self.peek_ch;
        self.peek_ch = self.chars.next().unwrap_or('\0');
        self.loc.column += 1;
    }

    fn read_constant(&mut self) -> Result<Token, LexerError> {
        let old_loc = self.loc;
        let mut string = String::new();

        while self.is_digit() {
            string.push(self.ch);
            self.read_char();
        }

        if self.is_valid_identifier_char() {
            return Err(LexerError::IdentifierStartedWithNumber);
        }

        let num: i32 = string
            .parse()
            .or(Err(LexerError::InvalidNumber(string.to_owned())))?;

        Ok(Token {
            kind: TokenKind::Constant(num),
            loc: old_loc,
        })
    }

    fn read_identifier(&mut self) -> Token {
        let old_loc = self.loc;
        let mut string = String::new();

        while self.is_valid_identifier_char() {
            string.push(self.ch);
            self.read_char();
        }
        Token {
            kind: TokenKind::from_string(&string),
            loc: old_loc,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();

        let old_loc = self.loc;

        let result = match self.ch {
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,
            ':' => TokenKind::Colon,
            '?' => TokenKind::QuestionMark,
            ';' => TokenKind::Semicolon,
            '~' => TokenKind::Tilde,
            '+' => match self.peek_char() {
                '+' => {
                    self.read_char();
                    TokenKind::Increment
                }
                '=' => {
                    self.read_char();
                    TokenKind::PlusAssign
                }
                _ => TokenKind::Plus,
            },
            '-' => match self.peek_char() {
                '-' => {
                    self.read_char();
                    TokenKind::Decrement
                }
                '=' => {
                    self.read_char();
                    TokenKind::MinusAssign
                }
                _ => TokenKind::Minus,
            },
            '*' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenKind::AsteriskAssign
                }
                _ => TokenKind::Asterisk,
            },
            '/' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenKind::SlashAssign
                }
                _ => TokenKind::Slash,
            },
            '%' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenKind::PercentAssign
                }
                _ => TokenKind::Percent,
            },
            '|' => match self.peek_char() {
                '|' => {
                    self.read_char();
                    TokenKind::Or
                }
                '=' => {
                    self.read_char();
                    TokenKind::BitwiseOrAssign
                }
                _ => TokenKind::BitwiseOr,
            },
            '&' => match self.peek_char() {
                '&' => {
                    self.read_char();
                    TokenKind::And
                }
                '=' => {
                    self.read_char();
                    TokenKind::BitwiseAndAssign
                }
                _ => TokenKind::BitwiseAnd,
            },
            '^' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenKind::BitwiseXorAssign
                }
                _ => TokenKind::Xor,
            },
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenKind::NotEqual
                }
                _ => TokenKind::Not,
            },
            '<' => match self.peek_char() {
                '<' => {
                    self.read_char();
                    if self.peek_char() == '=' {
                        self.read_char();
                        TokenKind::BitwiseShiftLeftAssign
                    } else {
                        TokenKind::ShiftLeft
                    }
                }
                '=' => {
                    self.read_char();
                    TokenKind::LessOrEqual
                }
                _ => TokenKind::LessThan,
            },
            '>' => match self.peek_char() {
                '>' => {
                    self.read_char();
                    if self.peek_char() == '=' {
                        self.read_char();
                        TokenKind::BitwiseShiftRightAssign
                    } else {
                        TokenKind::ShiftRight
                    }
                }
                '=' => {
                    self.read_char();
                    TokenKind::GreaterOrEqual
                }
                _ => TokenKind::GreaterThan,
            },
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    TokenKind::Equal
                } else {
                    TokenKind::Assign
                }
            }
            '\0' => {
                return Ok(Token {
                    kind: TokenKind::Eof,
                    loc: self.loc,
                })
            }
            _ => {
                if self.is_digit() {
                    return Ok(self.read_constant()?);
                } else if self.is_valid_identifier_char() {
                    return Ok(self.read_identifier());
                }

                return Err(LexerError::UnknownCharacter(char::from(self.ch)));
            }
        };

        self.read_char();
        Ok(Token {
            kind: result,
            loc: old_loc,
        })
    }
}

impl<'a> Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();

        if let Ok(ref tok) = token {
            if let TokenKind::Eof = tok.kind {
                return None;
            }
        }

        return Some(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r"
            int main(void) {
                return 2;
            }
            "
        .to_owned();
        let mut lexer = Lexer::new(input);
        let expected: Vec<_> = vec![
            TokenKind::KWInt,
            TokenKind::Identifier("main".to_owned()),
            TokenKind::OpenParen,
            TokenKind::KWVoid,
            TokenKind::CloseParen,
            TokenKind::OpenBrace,
            TokenKind::KWReturn,
            TokenKind::Constant(2),
            TokenKind::Semicolon,
            TokenKind::CloseBrace,
            TokenKind::Eof,
        ];

        for expected_token in expected {
            let token = lexer.next_token().expect("should return token");

            assert_eq!(expected_token, token.kind);
        }
    }

    #[test]
    fn test_all_compound_assignment() {
        let input = r"
        += -= *= /= %= &= |= ^= <<= >>= ++ --
        "
        .to_owned();
        let mut lexer = Lexer::new(input);

        let expected: Vec<_> = vec![
            TokenKind::PlusAssign,
            TokenKind::MinusAssign,
            TokenKind::AsteriskAssign,
            TokenKind::SlashAssign,
            TokenKind::PercentAssign,
            TokenKind::BitwiseAndAssign,
            TokenKind::BitwiseOrAssign,
            TokenKind::BitwiseXorAssign,
            TokenKind::BitwiseShiftLeftAssign,
            TokenKind::BitwiseShiftRightAssign,
            TokenKind::Increment,
            TokenKind::Decrement,
        ];

        for expected_token in expected {
            let token = lexer.next_token().expect("should return token");

            assert_eq!(expected_token, token.kind);
        }
    }

    #[test]
    #[should_panic]
    fn test_backslash() {
        let input = "\\".to_owned();
        let mut lexer = Lexer::new(input);

        let token = lexer.next_token().expect("Should fail");

        println!("{:?}", lexer);
        println!("token: {:?}", token);
        println!("{:?} {}", "\\", '\\' as u8);
    }
}
