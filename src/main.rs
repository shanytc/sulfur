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
pub enum PrintArg {
    Literal(Literal),
    Variable(String),
}

#[derive(Debug)]
pub enum IRNode {
    Function { name: String, body: Vec<IRNode> },
    VarDecl { name: String, value: Literal },
    Print { args: Vec<PrintArg> },
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

        let mut args = Vec::new();

        loop {
            match &self.cur_token {
                Token::StringLiteral(s) => {
                    let ss = s.clone();
                    args.push(PrintArg::Literal(Literal::String(ss)));
                    self.advance(); // consume the string literal
                }
                Token::NumberLiteral(s) => {
                    if s.contains('.') {
                        let f = s.parse().unwrap_or(0.0);
                        args.push(PrintArg::Literal(Literal::Float(f)));
                    } else {
                        let i = s.parse().unwrap_or(0);
                        args.push(PrintArg::Literal(Literal::Int(i)));
                    }
                    self.advance(); // consume the number literal
                }
                Token::Identifier(name) => {
                    let nn = name.clone();
                    args.push(PrintArg::Variable(nn));
                    self.advance(); // consume the identifier
                }
                _ => {
                    println!("Expected arguments after print, found: {:?}", self.cur_token);
                    break;
                }
            }
            if self.cur_token == Token::Comma { self.advance(); continue; }
            break;
        }

        if self.cur_token == Token::RParen { self.advance(); }
        if self.cur_token == Token::SemiColon { self.advance(); }

        IRNode::Print {args}
    }
}

pub enum Backend {
    JavaScript,
    MASM,
}

pub struct IRTranslator;

impl IRTranslator {
    fn masm_encode_string(src: &str) -> String {
        let mut encoded = String::new();
        let mut chars = src.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' && matches!(chars.peek(), Some('n')) {
                chars.next();                       // consume the 'n'
                encoded.push_str("\", 13,10, \"");      // CR LF
            } else if ch == '"' {
                encoded.push_str("\"\"");           // escape quotes for MASM
            } else {
                encoded.push(ch);
            }
        }

        format!("\"{}\"", encoded.trim_end_matches(", 13,10, "))
    }

    pub fn translate(nodes: &[IRNode], backend: Backend) -> String {
       match backend {
           Backend::JavaScript => todo!(),
           Backend::MASM => {
               let mut out = String::new();

               // Determine if any print exists
                let has_print = nodes.iter().any(|n| if let IRNode::Function { body, .. } = n {
                    body.iter().any(|stmt| matches!(stmt, IRNode::Print { .. }))
                } else { false });

               // MASM boilerplate
               out.push_str(".386\n.model flat, c\n");
               out.push_str("option casemap:none\n\n");
               if has_print {
                   out.push_str("extrn printf:PROC\n");
                   out.push_str("includelib lib\\msvcrt.lib\n\n");
                }

               // collect variables
               let mut var_decls = Vec::new();
               for n in nodes {
                   if let IRNode::Function { name: _ , body } = n {
                       for stmt in body {
                           if let IRNode::VarDecl { name, value } = stmt {
                               var_decls.push((name.clone(), value));
                           }
                       }
                   }
               }

               // data section
               out.push_str(".data\n");
               for (name, value) in &var_decls {
                    match value {
                        Literal::Int(i) => out.push_str(&format!("{} dd {}\n", name, i)),
                        Literal::Float(f) => out.push_str(&format!("{} real4 {}\n", name, f)),
                        Literal::String(s) => {
                            // escape \n as 13, 10
                            out.push_str(&format!("{} db \"{}\",0\n", name, s))
                        },
                    }
                }

               // Compile format strings for print statements
               let mut fmt_map = Vec::new(); // (label, fmt)
               let mut fmt_count = 0;

               // Iterate through nodes to find print statements and build format strings
               for n in nodes {
                   if let IRNode::Function { body, .. } = n {
                       for stmt in body {
                           if let IRNode::Print { args } = stmt {
                               // Determine raw_fmt: first string literal or default "{}"
                               let raw_fmt = if let Some(PrintArg::Literal(Literal::String(s))) = args.iter().find(|a| matches!(a, PrintArg::Literal(Literal::String(_)))) {
                                   s.clone()
                               } else {
                                   "{}".to_string()
                               };
                               // Build fmt_str by interleaving parts and correct specifiers
                               let parts: Vec<&str> = raw_fmt.split("{}").collect();
                               let mut fmt_str = String::new();
                               // parameters correspond to args after the format literal
                               let params: Vec<&PrintArg> = if let PrintArg::Literal(Literal::String(_)) = &args[0] {
                                   args.iter().skip(1).collect()
                               } else {
                                   args.iter().collect()
                               };
                               for i in 0..parts.len() {
                                   fmt_str.push_str(parts[i]);
                                   if i < params.len() {
                                       let spec = match params[i] {
                                           PrintArg::Literal(Literal::Float(_)) => "%f",
                                           PrintArg::Literal(Literal::String(_)) => "%s",
                                           PrintArg::Literal(Literal::Int(_)) => "%d",
                                           PrintArg::Variable(name) => match var_decls.iter().find(|(n, _)| n == name) {
                                               Some((_, Literal::Int(_))) => "%d",
                                               Some((_, Literal::Float(_))) => "%f",
                                               Some((_, Literal::String(_))) => "%s",
                                               _ => panic!("Unknown variable type: {:?}", name),
                                           },
                                       };
                                       fmt_str.push_str(spec);
                                   }
                               }
                               let lbl = format!("fmt{}", fmt_count);
                               fmt_count += 1;
                               fmt_map.push((lbl.clone(), fmt_str));
                           }
                       }
                   }
               }

               // Add format strings to data section
               for (lbl, fmt) in &fmt_map {
                   out.push_str(&format!("{} db {},0\n", lbl, Self::masm_encode_string(fmt)));
               }

               // code section
               out.push_str("\n.code\n");
               let mut fmt_index = 0;
               for n in nodes {
                    if let IRNode::Function { name, body } = n {
                        out.push_str(&format!("{} proc\n", name));
                        for stmt in body {
                            // Handle Print statements
                            if let IRNode::Print { args } = stmt {
                                // push args in reverse order
                                let params: Vec<&PrintArg> = if let PrintArg::Literal(Literal::String(_)) = &args[0] {
                                    args.iter().skip(1).collect()
                                } else {
                                    args.iter().collect()
                                };
                                for param in params.iter().rev() {
                                    match param {
                                        PrintArg::Literal(Literal::Int(i)) => out.push_str(&format!("    push {}\n", i)),
                                        PrintArg::Literal(Literal::Float(f)) => out.push_str(&format!("    push {}\n", f)),
                                        PrintArg::Literal(Literal::String(_)) => {
                                            // string literal params are not pushed separately
                                        },
                                        PrintArg::Variable(name) => match var_decls.iter().find(|(n, _)| n == name) {
                                            Some((_, Literal::Int(_))) => out.push_str(&format!("    push dword ptr [{}]\n", name)),
                                            Some((_, Literal::Float(_))) => out.push_str(&format!("    push dword ptr [{}]\n", name)),
                                            Some((_, Literal::String(_))) => {
                                                out.push_str(&format!("    lea eax, {}\n", name));
                                                out.push_str("    push eax\n");
                                            },
                                            _  => {}
                                        },
                                    }
                                }
                                // push format label
                                let fmt_label = &fmt_map[fmt_index].0;
                                fmt_index += 1;
                                out.push_str(&format!("    push offset {}\n", fmt_label));
                                out.push_str("    call printf\n");
                                // clean the stack
                                let num_args = fmt_map[fmt_index - 1].1.matches('%').count();
                                out.push_str(&format!("    add esp, {}\n", (num_args+1) * 4));
                            }

                        }
                        out.push_str("    xor eax, eax\n");
                        out.push_str("    ret\n");
                        out.push_str(&format!("{} endp\n\n", name));
                    }
                }
                out.push_str("end main\n");
                out
           },
       }
    }
}

fn main() {
    let src = r#"
        main() {
            let x = "test";
            print("hello\nworld");
        }"#;
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer);
    let ir = parser.parse_program();
    println!("{:?}", ir);

    let masm = IRTranslator::translate(&ir, Backend::MASM);
    println!("{}", masm);
}

