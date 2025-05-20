
#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    LParen, RParen, LBrace, RBrace,
    Comma,
    Assign,     // '='
    SemiColon,  // ';'
    EOF,
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
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

    fn next_char(&mut self) -> char {
        let ch = self.peek();
        self.pos += 1;
        ch
    }

    pub fn next_token(&mut self) -> Token {
        // Skip whitespace
        while self.peek().is_whitespace() {
            self.next_char();
        }

       match self.peek() {
           '(' => { self.next_char(); Token::LParen }
           ')' => { self.next_char(); Token::RParen }
           '{' => { self.next_char(); Token::LBrace }
           '}' => { self.next_char(); Token::RBrace }
           '=' => { self.next_char(); Token::Assign }
           ';' => { self.next_char(); Token::SemiColon }
           ',' => { self.next_char(); Token::Comma }
           '"' => {
               // parse string literal
               self.next_char(); // consume the opening quote
               let mut s = String::new();
               while self.peek() != '"' && self.peek() != '\0' {
                   s.push(self.next_char());
               }
               if self.peek() == '"' {
                   self.next_char();
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
           c if c.is_alphabetic() => {
               let mut ident = String::new();
                while self.peek().is_alphanumeric() {
                     ident.push(self.next_char());
                }
                Token::Identifier(ident)
           }
           _ => {
               println!("Unexpected character: {}", self.peek());
               self.next_char();
               Token::EOF
           }
       }
    }
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum IRNode {
    Function { name: String, body: Vec<IRNode> },
    VarDecl { name: String, value: Literal },
    Print { message: String },
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        Parser { lexer, cur_token }
    }

    fn advance(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Vec<IRNode> {
        let mut nodes = Vec::new();
        while self.cur_token != Token::EOF {
            match &self.cur_token {
                Token::Identifier(name) => {
                    let func_name = name.clone();
                    nodes.push(self.parse_function(func_name));
                }
                _ => {
                    println!("Expected function name, found: {:?}", self.cur_token);
                    self.advance();
                }
            }
        }
        nodes
    }

    pub fn parse_function(&mut self, name: String) -> IRNode {
        self.advance();
        if self.cur_token == Token::LParen { self.advance(); }
        if self.cur_token == Token::RParen { self.advance(); }
        if self.cur_token == Token::LBrace { self.advance(); }
        let body = self.parse_block();
        if self.cur_token == Token::RBrace { self.advance(); }
        IRNode::Function { name, body }
    }

    pub fn parse_block(&mut self) -> Vec<IRNode> {
        let mut stmts = Vec::new();
        while self.cur_token != Token::RBrace && self.cur_token != Token::EOF {
            match &self.cur_token {
                Token::Identifier(name) => {
                    let func_name = name.clone();
                    match func_name.as_str() {
                        "print" => {
                            stmts.push(self.parse_print());
                        },
                        "let" => {
                            stmts.push(self.parse_let());
                        }
                        _ => {
                            println!("Unknown function: {}", func_name);
                            self.advance();
                        }
                    }
                }
                _ => {
                    // println!("Expected statement, found: {:?}", self.cur_token);
                    self.advance();
                }
            }
        }
        stmts
    }

    pub fn parse_let(&mut self) -> IRNode {
        self.advance(); // consume "let"

        // consume variable name
        let name = if let Token::Identifier(n) = &self.cur_token {
            let nn = n.clone();
            self.advance(); // consume the identifier
            nn
        } else {
            panic!("Expected identifier after let, found: {:?}", self.cur_token);
        };

        // consume '='
        if self.cur_token == Token::Assign { self.advance(); }

        // consume value
        let value = match &self.cur_token {
            Token::NumberLiteral(s) => {
                if s.contains('.'){
                    let f = s.parse().unwrap_or(0.0);
                    self.advance(); // consume the number literal
                    Literal::Float(f)
                } else {
                    let i = s.parse().unwrap_or(0);
                    self.advance(); // consume the number literal
                    Literal::Int(i)
                }
            },
            Token::StringLiteral(s) => {
                let ss = s.clone();
                self.advance(); // consume the string literal
                Literal::String(ss)
            }
            _ => {
                println!("Expected number or string literal after let, found: {:?}", self.cur_token);
                Literal::String(String::new())
            }
        };

        if self.cur_token == Token::SemiColon { self.advance(); }
        IRNode::VarDecl { name, value }
    }

    pub fn parse_print(&mut self) -> IRNode {
        self.advance(); // consume "print"
        if self.cur_token == Token::LParen { self.advance(); }
        let message = if let Token::StringLiteral(s) = &self.cur_token {
            let msg = s.clone();
            self.advance(); // consume the string literal
            msg
        } else {
            println!("Expected string literal after print, found: {:?}", self.cur_token);
            String::new()
        };

        if self.cur_token == Token::RParen { self.advance(); }
        if self.cur_token == Token::SemiColon { self.advance(); }
        IRNode::Print { message }
    }
}

pub enum Backend {
    JavaScript,
    MASM,
}

pub struct IRTranslator;

impl IRTranslator {
    pub fn translate(nodes: &[IRNode], backend: Backend) -> String {
       match backend {
           Backend::JavaScript => todo!(),
           Backend::MASM => {
               let mut out = String::new();

               // MASM boilerplate
               out.push_str(".686\n.model flat, c\n\n");
               out.push_str("extrn printf:PROC\n");
               out.push_str("includelib msvcrt.lib\n\n");

               // data section
               out.push_str(".data\n");
               for node in nodes {
                   if let IRNode::Function { name: _ , body } = node {
                       for stmt in body {
                           match stmt {
                               IRNode::Print { message } => {
                                   out.push_str(&format!("msg db \"{}\",0\n", message));
                               },
                               IRNode::VarDecl { name, value } => {
                                   match value {
                                       Literal::Int(i) => out.push_str(&format!("{} dq {},0\n", name, i)),
                                       Literal::Float(f) => out.push_str(&format!("{} dq {},0\n", name, f)),
                                       Literal::String(s) => out.push_str(&format!("{} db \"{}\",0\n", name, s)),
                                   }
                               }
                              _ => {
                                  panic!("Unsupported IR node: {:?}", stmt);
                              }
                           }
                       }
                   }
               }

               // code section
               out.push_str("\n.code\n");
               for node in nodes {
                   if let IRNode::Function { name , body: _ } = node {
                      out.push_str(&format!("{} proc\n", name)) ;
                       // assume ohly one print per function for now
                       out.push_str("    push offset msg\n");
                       out.push_str("    call printf\n");
                       out.push_str("    ret\n");
                       out.push_str(&format!("{} endp\n", name));
                   }
               }

               out.push_str("end main\n");
               out
           },
       }
    }
}

fn main() {
    let src = "main() { let x = 5; }";
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer);
    let ir = parser.parse_program();
    println!("{:?}", ir);

    let masm = IRTranslator::translate(&ir, Backend::MASM);
    println!("{}", masm);
}

