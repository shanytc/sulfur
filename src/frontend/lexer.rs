use super::token::Token;

#[derive(Debug, Clone)]
pub struct Lexer {
    pub(crate) input: Vec<char>,
    pub(crate) pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(), pos: 0
        }
    }

    fn peek(&self) -> char {
        self.input.get(self.pos).cloned().unwrap_or('\0')
    }
    fn peek_next(&self) -> char {
        self.input.get(self.pos + 1).cloned().unwrap_or('\0')
    }

    fn next_char(&mut self) -> char {
        let ch = self.peek();
        self.pos += 1;
        ch
    }

    pub fn skip_whitespace_and_comments(&mut self){
        loop {
            while self.peek().is_whitespace() {
                self.next_char();
            }

            if self.peek() == '/' && self.peek_next() == '/' {
                self.next_char();
                self.next_char();

                // chew everything up until the end of the line
                while self.peek() != '\n' && self.peek() != '\0' {
                    self.next_char();
                }

                continue;
            }

            // we're done with whitespace and comments
            break;
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();

        match self.peek() {
            '(' => { self.next_char(); Token::LParen }
            ')' => { self.next_char(); Token::RParen }
            '{' => { self.next_char(); Token::LBrace }
            '}' => { self.next_char(); Token::RBrace }
            ';' => { self.next_char(); Token::SemiColon }
            ',' => { self.next_char(); Token::Comma }
            '+' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::PlusAssign
                } else {
                    Token::Plus
                }
            }
            '-' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::MinusAssign
                } else if self.peek() == '>' {
                    self.next_char();
                    Token::Arrow
                }
                else {
                    Token::Minus
                }
            }
            '*' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::StarAssign
                } else {
                    Token::Star
                }
            }
            '/' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::SlashAssign
                } else {
                    Token::Slash
                }
            }
            '%' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::PercentAssign
                } else {
                    Token::Percent
                }
            }
            '&' => {
                self.next_char();
                if self.peek() == '&' {
                    self.next_char();
                    Token::AndAnd
                } else {
                    panic!("Unexpected character: {}", self.peek());
                }
            }
            '|' => {
                self.next_char();
                if self.peek() == '|' {
                    self.next_char();
                    Token::OrOr
                } else {
                    panic!("Unexpected character: {}", self.peek());
                }
            }
            '"' => {
                self.next_char();                 // consume opening quote
                let mut s = String::new();

                loop {
                    match self.peek() {
                        '\0' => panic!("unterminated string literal"),
                        '"'  => { self.next_char(); break; }      // closing quote
                        '\\' => {                                // escape sequence
                            s.push(self.next_char());            // push the backslash
                            s.push(self.next_char());            // push the escaped char
                        }
                        _   => s.push(self.next_char()),
                    }
                }
                Token::StringLiteral(s)
            }
            c if c.is_ascii_digit() => {
                let mut num = String::new();
                while self.peek().is_ascii_digit() || self.peek() == '.' {
                    num.push(self.next_char());
                }
                Token::NumberLiteral(num)
            }
            '\0' => Token::EOF,
            c if c.is_alphabetic() || c == '_' => {
                let mut ident = String::new();
                while self.peek().is_alphanumeric() || self.peek() == '_' {
                    ident.push(self.next_char());
                }
                Token::Identifier(ident)
            }
            '<' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '=' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::EqualEqual
                } else {
                    Token::Assign
                }
            },
            '!' => {
                self.next_char();
                if self.peek() == '=' {
                    self.next_char();
                    Token::BangEqual
                } else {
                    panic!("Unexpected character: {}", self.peek());
                }
            }
            _ => {
                panic!("Unexpected character: {}", self.peek());
                // self.next_char();
                // Token::EOF
            }
        }
    }
}