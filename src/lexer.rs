#[derive(Debug)]
pub enum LexerError {
    UnknownCharacter(char),
    InvalidNumber(String),
    IdentifierStartedWithNumber,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone)]
pub enum Token {
    Eof,
    Identifier(String),
    Constant(i32),
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,

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
}

impl Token {
    pub fn from_string(string: &str) -> Self {
        match string {
            "int" => Self::KWInt,
            "return" => Self::KWReturn,
            "void" => Self::KWVoid,
            _ => Self::Identifier(string.to_owned()),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,

    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            pos: 0,
            read_pos: 0,
            ch: 0,
        };

        lexer.read_char();
        lexer
    }

    fn peek_char(&self) -> u8 {
        self.input.as_bytes()[self.read_pos]
    }

    fn is_digit(&self) -> bool {
        match self.ch {
            b'0'..=b'9' => true,
            _ => false,
        }
    }

    fn is_valid_identifier_char(&self) -> bool {
        match self.ch {
            b'a'..=b'z' => true,
            b'A'..=b'Z' => true,
            b'_' => true,
            _ => self.is_digit(),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\n' || self.ch == b'\r' || self.ch == b'\t' {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.input.len() <= self.read_pos {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_pos];
        }

        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn read_constant(&mut self) -> Result<i32, LexerError> {
        let old_pos = self.pos;

        while self.is_digit() {
            self.read_char();
        }

        if self.is_valid_identifier_char() {
            return Err(LexerError::IdentifierStartedWithNumber);
        }

        let string = &self.input[old_pos..self.pos];

        let num: i32 = string
            .parse()
            .or(Err(LexerError::InvalidNumber(string.to_owned())))?;

        Ok(num)
    }

    fn read_identifier(&mut self) -> Token {
        let old_pos = self.pos;
        while self.is_valid_identifier_char() {
            self.read_char();
        }

        let string = &self.input[old_pos..self.pos];

        Token::from_string(string)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace();
        let result = match self.ch {
            b'(' => Token::OpenParen,
            b')' => Token::CloseParen,
            b'{' => Token::OpenBrace,
            b'}' => Token::CloseBrace,
            b';' => Token::Semicolon,
            b'~' => Token::Tilde,
            b'+' => match self.peek_char() {
                b'+' => {
                    self.read_char();
                    Token::Increment
                }
                b'=' => {
                    self.read_char();
                    Token::PlusAssign
                }
                _ => Token::Plus,
            },
            b'-' => match self.peek_char() {
                b'-' => {
                    self.read_char();
                    Token::Decrement
                }
                b'=' => {
                    self.read_char();
                    Token::MinusAssign
                }
                _ => Token::Minus,
            },
            b'*' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::AsteriskAssign
                }
                _ => Token::Asterisk,
            },
            b'/' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::SlashAssign
                }
                _ => Token::Slash,
            },
            b'%' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::PercentAssign
                }
                _ => Token::Percent,
            },
            b'|' => match self.peek_char() {
                b'|' => {
                    self.read_char();
                    Token::Or
                }
                b'=' => {
                    self.read_char();
                    Token::BitwiseOrAssign
                }
                _ => Token::BitwiseOr,
            },
            b'&' => match self.peek_char() {
                b'&' => {
                    self.read_char();
                    Token::And
                }
                b'=' => {
                    self.read_char();
                    Token::BitwiseAndAssign
                }
                _ => Token::BitwiseAnd,
            },
            b'^' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::BitwiseXorAssign
                }
                _ => Token::Xor,
            },
            b'!' => match self.peek_char() {
                b'=' => {
                    self.read_char();
                    Token::NotEqual
                }
                _ => Token::Not,
            },
            b'<' => match self.peek_char() {
                b'<' => {
                    self.read_char();
                    if self.peek_char() == b'=' {
                        self.read_char();
                        Token::BitwiseShiftLeftAssign
                    } else {
                        Token::ShiftLeft
                    }
                }
                b'=' => {
                    self.read_char();
                    Token::LessOrEqual
                }
                _ => Token::LessThan,
            },
            b'>' => match self.peek_char() {
                b'>' => {
                    self.read_char();
                    if self.peek_char() == b'=' {
                        self.read_char();
                        Token::BitwiseShiftRightAssign
                    } else {
                        Token::ShiftRight
                    }
                }
                b'=' => {
                    self.read_char();
                    Token::GreaterOrEqual
                }
                _ => Token::GreaterThan,
            },
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            0 => return Ok(Token::Eof),
            _ => {
                if self.is_digit() {
                    return Ok(Token::Constant(self.read_constant()?));
                } else if self.is_valid_identifier_char() {
                    return Ok(self.read_identifier());
                }

                return Err(LexerError::UnknownCharacter(char::from(self.ch)));
            }
        };

        self.read_char();
        Ok(result)
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();

        if let Ok(Token::Eof) = token {
            return None;
        }

        return Some(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let mut lexer = Lexer::new(
            r"
            int main(void) {
                return 2;
            }
        "
            .to_owned(),
        );
        let expected: Vec<_> = vec![
            Token::KWInt,
            Token::Identifier("main".to_owned()),
            Token::OpenParen,
            Token::KWVoid,
            Token::CloseParen,
            Token::OpenBrace,
            Token::KWReturn,
            Token::Constant(2),
            Token::Semicolon,
            Token::CloseBrace,
            Token::Eof,
        ];

        for expected_token in expected {
            let token = lexer.next_token().expect("should return token");

            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn test_all_compound_assignment() {
        let mut lexer = Lexer::new(
            r"
        += -= *= /= %= &= |= ^= <<= >>= ++ --
        "
            .to_owned(),
        );

        let expected: Vec<_> = vec![
            Token::PlusAssign,
            Token::MinusAssign,
            Token::AsteriskAssign,
            Token::SlashAssign,
            Token::PercentAssign,
            Token::BitwiseAndAssign,
            Token::BitwiseOrAssign,
            Token::BitwiseXorAssign,
            Token::BitwiseShiftLeftAssign,
            Token::BitwiseShiftRightAssign,
            Token::Increment,
            Token::Decrement,
        ];

        for expected_token in expected {
            let token = lexer.next_token().expect("should return token");

            assert_eq!(expected_token, token);
        }
    }

    #[test]
    #[should_panic]
    fn test_backslash() {
        let mut lexer = Lexer::new("\\".to_owned());

        let token = lexer.next_token().expect("Should fail");

        println!("{:?}", lexer);
        println!("token: {:?}", token);
        println!("{:?} {}", "\\", '\\' as u8);
    }
}
