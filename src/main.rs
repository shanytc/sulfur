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
                panic!("Unexpected character: {}", self.peek());
                // self.next_char();
                // Token::EOF
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
    Assign { name: String, value: Literal },
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
                Token::Identifier(id) if id == "print" => stmts.push(self.parse_print()),
                Token::Identifier(id) if id == "let" => stmts.push(self.parse_let()),
                // consume identifier = value
                Token::Identifier(id) => {
                    let var_name = id.clone();

                    // check if variable is declared, else panic!
                    if !stmts.iter().any(|n| matches!(n, IRNode::VarDecl { name, .. } if name == &var_name)) {
                        panic!("Variable '{}' hasn't been declared!", var_name);
                    }

                    self.advance();
                    if self.cur_token == Token::Assign {
                        stmts.push(self.parse_assign(var_name));
                    } else {
                        println!("Expected assignment after identifier '{}', found: {:?}", var_name, self.cur_token);
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
                Literal::Int(0)
            }
        };

        if self.cur_token == Token::SemiColon { self.advance(); }
        IRNode::VarDecl { name, value }
    }

    pub fn parse_literal(token: &Token) -> Literal {
        match token {
            Token::NumberLiteral(s) => {
                if s.contains('.') {
                    let f = s.parse().unwrap_or(0.0);
                    Literal::Float(f)
                } else {
                    let i = s.parse().unwrap_or(0);
                    Literal::Int(i)
                }
            }
            Token::StringLiteral(s) => Literal::String(s.clone()),
            _ => panic!("Expected a literal, found: {:?}", token),
        }
    }

    pub fn parse_assign(&mut self, name: String) -> IRNode {
       // here we want to parse assignment to a variable
        assert_eq!(self.cur_token, Token::Assign, "Expected assignment operator '='");
        self.advance(); // consume '='
        let value = Self::parse_literal(&self.cur_token);
        if self.cur_token == Token::SemiColon { self.advance(); }
        IRNode::Assign { name, value }
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
            }
            else if ch == '\\' && matches!(chars.peek(), Some('t')) {
                chars.next();                       // consume the 't'
                encoded.push_str("\", 9, \"");         // tab
            }
            else if ch == '\\' && matches!(chars.peek(), Some('r')) {
                chars.next();                       // consume the 'r'
                encoded.push_str("\", 13, \"");        // CR
            }
            else if ch == '\\' {
                encoded.push('\\');                 // just a backslash
            }
            else {
                encoded.push(ch);
            }
        }

        format!("\"{}\"", encoded.trim_end_matches(", 13,10, ")).replace("\"\",", "")
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
                        Literal::Float(f) => out.push_str(&format!("{} real8 {}\n", name, f)),
                        Literal::String(s) => {
                            // escape \n as 13, 10
                            out.push_str(&format!("{} db \"{}\",0\n", name, s))
                        },
                    }
                }

                // Compile format strings for print statements
                let mut fmt_map: Vec<(String, String)> = Vec::new(); // (label, fmt)
                let mut fmt_count = 0;

                // Iterate through nodes to find print statements and build format strings
                for n in nodes {
                    if let IRNode::Function { body, .. } = n {
                        for stmt in body {
                            if let IRNode::Print { args } = stmt {

                                // check if we have string only, variable or mixed args
                                let has_string = args.iter().any(|a| matches!(a, PrintArg::Literal(Literal::String(_))));
                                let has_variable = args.iter().any(|a| matches!(a, PrintArg::Variable(_)));
                                let has_number = args.iter().any(|a| matches!(a, PrintArg::Literal(Literal::Int(_)))) ||
                                                  args.iter().any(|a| matches!(a, PrintArg::Literal(Literal::Float(_))));
                                let mut fmt_str = String::new();

                                if !has_string && !has_variable && has_number {
                                    fmt_str.push_str("%d");
                                    fmt_map.push((format!("fmt{}", fmt_count), String::from("\"%d\"")));
                                    fmt_count += 1;
                                }

                                if has_string && !has_variable && !has_number {
                                    // get arg literal
                                    if let Some(PrintArg::Literal(Literal::String(name))) = args.first() {
                                        let new_name = Self::masm_encode_string(name.as_str());
                                        fmt_map.push((format!("fmt{}", fmt_count), new_name));
                                        fmt_count += 1;
                                        continue;
                                    }
                                }

                                if has_variable && !has_string && !has_number {
                                    // get arg variable
                                    if let Some(PrintArg::Variable(name)) = args.first() {
                                        match var_decls.iter().find(|(n, _)| n == name) {
                                            Some((_, Literal::Int(_))) => {
                                                fmt_map.push((format!("fmt{}", fmt_count), String::from("\"%d\"")));
                                            }
                                            Some((_, Literal::Float(_))) => {
                                                fmt_map.push((format!("fmt{}", fmt_count), String::from("\"%f\"")));
                                            }
                                            Some((_, Literal::String(_))) => {
                                                fmt_map.push((format!("fmt{}", fmt_count), String::from("\"%s\"")));
                                            }
                                            _ => panic!("Unknown variable type: {:?}", name),
                                        }
                                        fmt_count += 1;
                                        continue;
                                    }
                                }

                                if has_variable && (has_string || has_number) {
                                    // check if we have a string literal as first arg
                                    if let Some(PrintArg::Literal(Literal::String(name))) = args.first() {
                                        let new_name = Self::masm_encode_string(name.as_str()).trim().to_string();
                                        fmt_map.push((format!("fmt{}", fmt_count), new_name));
                                        fmt_count += 1;
                                    } else {
                                        panic!("String literal expected as first argument");
                                    }

                                    for arg in args.iter().skip(1) {
                                        match arg {
                                            PrintArg::Literal(Literal::Int(_)) => fmt_str.push_str("%d"),
                                            PrintArg::Literal(Literal::Float(_)) => fmt_str.push_str("%f"),
                                            PrintArg::Literal(Literal::String(_)) => fmt_str.push_str("%s"),
                                            PrintArg::Variable(name) => {
                                                if let Some((_, Literal::Int(_))) = var_decls.iter().find(|(n, _)| n == name) {
                                                    fmt_str.push_str("%d");
                                                } else if let Some((_, Literal::Float(_))) = var_decls.iter().find(|(n, _)| n == name) {
                                                    fmt_str.push_str("%f");
                                                } else if let Some((_, Literal::String(_))) = var_decls.iter().find(|(n, _)| n == name) {
                                                    fmt_str.push_str("%s");
                                                } else {
                                                    panic!("Unknown variable type: {:?}", name);
                                                }
                                            }
                                        }
                                        fmt_count += 1;
                                    }

                                }
                            }
                        }
                    }
                }

                // Add format strings to data section
                for (lbl, fmt) in &fmt_map {
                    out.push_str(&format!("{} db {},0\n", lbl, fmt));
                }

                // code section
                out.push_str("\n.code\n");
                let mut fmt_index = 0;
                for n in nodes {
                    if let IRNode::Function { name, body } = n {
                        out.push_str(&format!("{} proc\n", name));
                        for stmt in body {
                            if let IRNode::Assign { name, value } = stmt {
                               match value {
                                   Literal::Int(i) => out.push_str(&format!("    mov dword ptr [{}], {}\n", name, i)),
                                   Literal::Float(f) => {
                                       out.push_str(&format!("    mov dword ptr [{}+4], {}\n", name, f.to_bits() >> 32));
                                       out.push_str(&format!("    mov dword ptr [{}], {}\n", name, f.to_bits() & 0xFFFFFFFF));
                                   },
                                   Literal::String(s) => {
                                       let encoded = Self::masm_encode_string(s);
                                       out.push_str(&format!("    mov dword ptr [{}], offset {}\n", name, encoded));
                                   },
                               }
                            }

                            // Handle Print statements
                            if let IRNode::Print { args } = stmt {
                                let mut clean_float_stack = 0;
                                // push args in reverse order
                                let params: Vec<&PrintArg> = if let PrintArg::Literal(Literal::String(_)) = &args[0] {
                                    args.iter().skip(1).collect()
                                } else {
                                    args.iter().collect()
                                };

                                for param in params.iter().rev() {
                                    match param {
                                        PrintArg::Literal(Literal::Int(i)) => out.push_str(&format!("    push {}\n", i)),
                                        PrintArg::Literal(Literal::Float(_)) => panic!("real or BCD number not allowed as parameter"),
                                        PrintArg::Literal(Literal::String(_)) => {
                                            out.push_str("    push offset ");
                                        },
                                        PrintArg::Variable(name) => match var_decls.iter().find(|(n, _)| n == name) {
                                            Some((_, Literal::Int(_))) => out.push_str(&format!("    push dword ptr [{}]\n", name)),
                                            Some((_, Literal::Float(_))) => {
                                                clean_float_stack += 1;
                                                out.push_str(&format!("    push dword ptr [{}+4]\n", name));
                                                out.push_str(&format!("    push dword ptr [{}]\n", name));
                                            },
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

                                let num_args = fmt_map[fmt_index - 1].1.matches('%').count();
                                out.push_str(&format!("    add esp, {}\n", (num_args + 1 + clean_float_stack) * 4));
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
            let x;
            x = 5;
            print("val=%d", x);
        }"#;
    let lexer = Lexer::new(src);
    let mut parser = Parser::new(lexer);
    let ir = parser.parse_program();
    println!("----------------- IR -------------------");
    println!("{:?}", ir);
    println!("----------------------------------------");

    let masm = IRTranslator::translate(&ir, Backend::MASM);

    let output_file = "c:\\masm32\\test.asm";
    std::fs::write(output_file, &masm).expect("Unable to write file");

    println!("{}", masm);
}

