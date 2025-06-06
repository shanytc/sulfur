use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::process::{Command, Stdio};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    Plus,           // +
    PlusAssign,     // +=
    Minus,          // -
    MinusAssign,    // -=
    Star,           // *
    StarAssign,     // *=
    Slash,          // /
    SlashAssign,    // /=
    Percent,        // %
    PercentAssign, // %=
    LParen, RParen, LBrace, RBrace,
    Comma,
    Assign,     // '='
    SemiColon,  // ';'
    Less,          // <
    LessEqual,     // <=
    Greater,       // >
    GreaterEqual,  // >=
    EqualEqual,    // ==
    BangEqual,     // !=
    AndAnd,      // &&
    OrOr,       // ||
    Arrow,      // ->
    EOF,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),          //  5 or  3.14 or "abc"
    Variable(String),          //  x
    Binary {                   //  x + 4 or 7 + y or a + b
        left: Box<Expr>,
        op: Token,            // Token::Plus, Token::Minus, etc.
        right: Box<Expr>,
    },
    Unary {
        op: Token,            // Token::Star (pointers)
        expr: Box<Expr>,      // the expression to apply the unary operator to
    },
    Call {
        name: String,         // function name
        args: Vec<Expr>,      // arguments to the function
    }
}

#[derive(Debug, Clone)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

const VAR_NAME_EXTENSION: &str = "_var";

static RUNTIME_LOOKUP: &[(&str, &str)] = &[
    // C runtime
    ("printf", "msvcrt.lib"),
    ("scanf",  "msvcrt.lib"),
    ("malloc", "msvcrt.lib"),
    ("free",   "msvcrt.lib"),
    ("memcpy", "msvcrt.lib"),
    // Win32 kernel (examples)
    ("CreateFileA", "kernel32.lib"),
    ("CloseHandle", "kernel32.lib"),
];

fn lit_label(idx: usize) -> String {
    format!("lit{}", idx)
}

fn lib_for(sym: &str) -> &'static str {
    RUNTIME_LOOKUP
        .iter()
        .find(|(s, _)| *s == sym)
        .map(|(_, lib)| *lib)
        .unwrap_or("msvcrt.lib")           // sensible default
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

                // chew up everything until the end of the line
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

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum PrintArg {
    Literal(Literal),
    Variable(String),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String,
    Void // function returns nothing
}

#[derive(Debug)]
pub enum IRNode {
    Function {
        name: String,
        params: Vec<(String, Type)>,     // e.g [("x", Type::Int), ("y", Type::Float)]
        return_type: Type,              // e.g Type::Int
        body: Vec<IRNode>
    },
    VarDecl(Vec<(String, Option<Literal>)>),
    Assign { name: String, value: Expr },
    Print { args: Vec<PrintArg> },
    While {
        cond: Expr,
        body: Vec<IRNode>,
    },
    If {
        cond: Expr,
        then_branch: Vec<IRNode>,
        else_branch: Option<Vec<IRNode>>,
    },
    Call { // function call
        name: String, // function name
        args: Vec<Expr>, // arguments to the function
    },
    Return(Option<Expr>),
    Store {
        dst: Expr,   // address (*dst = value)
        value: Expr, // rhs value
    }
}

#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    constants: HashMap<String, Literal>,
    functions: HashMap<String, (Vec<(String, Type)>, Type)>, // function name -> (params, return type)
    current_func: Option<String>, // current function being parsed
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        Parser { lexer, cur_token, constants: HashMap::new(), functions: HashMap::new(), current_func: None }
    }

    fn advance(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    fn scoped(&self, base: &str) -> String {
        match self.current_func {
            Some(ref func_name) => format!("{}_{}{}", func_name, base, VAR_NAME_EXTENSION),
            None => format!("{}{}", base, VAR_NAME_EXTENSION), // no function context, just append _var
        }
    }

    pub fn parse_program(&mut self) -> Vec<IRNode> {
        let mut nodes = Vec::new();
        while self.cur_token != Token::EOF {
            match &self.cur_token {
                Token::Identifier(name) if name == "fn" => {
                    nodes.push(self.parse_function());
                }
                _ => {
                    println!("Expected function name, found: {:?}", self.cur_token);
                    self.advance();
                }
            }
        }
        nodes
    }

    pub fn parse_function(&mut self) -> IRNode {
        // current token is 'fn'
        self.advance();

        // function name must be an identifier
        let name = if let Token::Identifier(id) = &self.cur_token {
            let n = id.clone();
            self.advance(); // consume the identifier
            n
        } else {
            panic!("Expected function name after 'fn', found: {:?}", self.cur_token);
        };

        let mut params: Vec<(String, Type)> = Vec::new();

        if self.cur_token == Token::LParen {
            // eat the '('
            self.advance();

            if self.cur_token != Token::RParen {
               // parse params
                loop {
                   // must be an identifier
                    let param_name = if let Token::Identifier(id) = &self.cur_token {
                        let n = id.clone();
                        self.advance(); // consume the identifier
                        n
                    } else {
                        panic!("Expected parameter name, found: {:?}", self.cur_token);
                    };

                    // TODO: assume all parameters are Int for simplicity for now
                    let scoped = self.scoped(param_name.as_str());
                    self.constants.insert(scoped, Literal::Int(0)); // default to 0
                    params.push((param_name, Type::Int)); // default to Int type for simplicity

                    if self.cur_token == Token::Comma {
                        self.advance(); // consume the comma
                        continue;
                    }
                    break;
                }
            }

            // eat the ')'
            if self.cur_token == Token::RParen { self.advance(); } else {
                panic!("Expected ')' after function parameters, found: {:?}", self.cur_token);
            }
        } else {
            panic!("Expected '(' after function name '{}', found: {:?}", name, self.cur_token);
        }

        // optional '->' return type
        let return_type = if self.cur_token == Token::Arrow {
           self.advance(); // consume the '->'
            if let Token::Identifier(id) = &self.cur_token {
                let ret_type = match id.as_str() {
                    "int" => Type::Int,
                    "float" => Type::Float,
                    "string" => Type::String,
                    "void" => Type::Void,
                    _ => panic!("Unknown return type: {}", id),
                };
                self.advance(); // consume the return type identifier
                ret_type
            } else {
                panic!("Expected return type after '->', found: {:?}", self.cur_token);
            }
        } else {
            Type::Void // default to void if no return type specified
        };

        self.functions.insert(name.clone(), (params.clone(), return_type.clone()));

        if self.cur_token == Token::LBrace { self.advance(); } else {
            panic!("Expected '{{' after function parameters, found: {:?}", self.cur_token);
        }

        let prev_func = self.current_func.replace(name.clone());

        for (param_name, _) in &params {
            let scoped = self.scoped(param_name.as_str());
            self.constants.insert(scoped, Literal::Int(0)); // default to 0
        }

        let body = self.parse_block();

        self.current_func = prev_func; // restore previous function name

        if self.cur_token == Token::RBrace { self.advance(); } else {
            panic!("Expected '}}' after function body, found: {:?}", self.cur_token);
        }

        IRNode::Function { name, params, return_type, body }
    }

    pub fn parse_return(&mut self) -> IRNode {
        self.advance(); // consume "return"

        let expr = if self.cur_token != Token::SemiColon {
            let expr = self.parse_logic();
            Some(expr)
        } else {
            None
        };

        // consume the semicolon
        if self.cur_token == Token::SemiColon { self.advance(); } else {
            panic!("Expected ';' after return expression, found: {:?}", self.cur_token);
        }

        IRNode::Return(expr)
    }

    pub fn parse_logic(&mut self) -> Expr {
        let mut expr = self.parse_comparison();

        while matches!(
            self.cur_token, Token::AndAnd | Token::OrOr
        ){
            let op = self.cur_token.clone();
            self.advance();
            let rhs = self.parse_comparison();

            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(rhs),
            };
        }

        expr
    }

    pub fn parse_if(&mut self) -> IRNode {
        self.advance(); // consume "if"
        if self.cur_token == Token::LParen { self.advance(); }

        let cond = self.parse_logic();

        if self.cur_token == Token::RParen { self.advance(); }
        if self.cur_token == Token::LBrace { self.advance(); }

        let then_branch = self.parse_block(); // parse the "then" block
        if self.cur_token == Token::RBrace { self.advance(); }

        // optional "else" block
        if let Token::Identifier(id) = &self.cur_token {
            if id == "else" {
                self.advance(); // consume "else"
                if let Token::Identifier(next) = &self.cur_token {
                    if next == "if" {
                        // Don’t consume a `{` here—just let parse_if handle it
                        let nested_if = self.parse_if();
                        // Wrap it in a one‐element Vec so it fits the Vec<IRNode> type
                        return IRNode::If {
                            cond,
                            then_branch,
                            else_branch: Some(vec![nested_if]),
                        };
                    }
                }

                if self.cur_token == Token::LBrace {
                   self.advance();
                    let else_body = self.parse_block();
                    if self.cur_token == Token::RBrace { self.advance(); }
                    return IRNode::If {
                        cond,
                        then_branch,
                        else_branch: Some(else_body),
                    };
                }
            }
        }

        IRNode::If {
            cond,
            then_branch,
            else_branch: None,
        }
    }

    pub fn parse_while(&mut self) -> IRNode {
        self.advance();
        if self.cur_token == Token::LParen { self.advance(); }

        // parse the condition
        let cond = self.parse_logic();

        if self.cur_token == Token::RParen { self.advance(); }
        if self.cur_token == Token::LBrace { self.advance(); }

        // parse the body of the while loop
        let body = self.parse_block();

        if self.cur_token == Token::RBrace { self.advance(); }
        IRNode::While { cond, body }
    }

    pub fn parse_block(&mut self) -> Vec<IRNode> {
        let mut stmts = Vec::new();
        while self.cur_token != Token::RBrace && self.cur_token != Token::EOF {
            match &self.cur_token {
                Token::Identifier(id) if id == "print" => stmts.push(self.parse_print()),
                Token::Identifier(id) if id == "let" => {
                    let mut more = self.parse_let();
                    stmts.append(&mut more);
                },
                Token::Identifier(id) if id == "while" => stmts.push(self.parse_while()),
                Token::Identifier(id) if id == "if" => stmts.push(self.parse_if()),
                Token::Identifier(id) if id == "return" => stmts.push(self.parse_return()),
                // consume identifier = value
                Token::Identifier(id) => {
                    // check for function call
                    if let Token::Identifier(fname) = &self.cur_token {
                        let name = fname.clone();
                        if self.lexer.input.get(self.lexer.pos).cloned() == Some('(') {
                            // function call used as statement
                            let call_expr = self.parse_expression(); // consume function's args
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            } else {
                                println!("Expected ; after function call '{}', found: {:?}", name, self.cur_token);
                            }
                            // no IRNode variant for function calls. ignore.
                            if let Expr::Call {name, args} = call_expr {
                                stmts.push(IRNode::Call { name, args });
                            }
                            continue
                        }
                    }

                    // variable assignment
                    let var_name = self.scoped(id.clone().as_str()); // append _var to the variable name
                    if !self.constants.contains_key(&var_name) {
                        panic!("Variable '{}' hasn't been declared!", var_name);
                    }

                    self.advance();

                    match &self.cur_token {
                        Token::Assign => stmts.push(self.parse_assign(var_name)),
                        Token::PlusAssign => {
                            self.advance(); // consume the '+='
                            let expr = self.parse_logic();
                            let lhs_expr = Expr::Variable(var_name.clone()); // append _var to the variable name
                            let new_value = Expr::Binary {
                                left: Box::new(lhs_expr),
                                op: Token::Plus,
                                right: Box::new(expr),
                            };
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            }
                            stmts.push(IRNode::Assign {
                                name: var_name.clone(),
                                value: new_value,
                            });
                        },
                        Token::MinusAssign => {
                            self.advance(); // consume the '-='
                            let expr = self.parse_logic();
                            let lhs_expr = Expr::Variable(var_name.clone()); // append _var to the variable name
                            let new_value = Expr::Binary {
                                left: Box::new(lhs_expr),
                                op: Token::Minus,
                                right: Box::new(expr),
                            };
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            }
                            stmts.push(IRNode::Assign {
                                name: var_name.clone(),
                                value: new_value,
                            });
                        },
                        Token::StarAssign => {
                            self.advance(); // consume the '*='
                            let expr = self.parse_logic();
                            let lhs_expr = Expr::Variable(var_name.clone()); // append _var to the variable name
                            let new_value = Expr::Binary {
                                left: Box::new(lhs_expr),
                                op: Token::Star,
                                right: Box::new(expr),
                            };
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            }
                            stmts.push(IRNode::Assign {
                                name: var_name.clone(),
                                value: new_value,
                            });
                        },
                        Token::SlashAssign => {
                            self.advance(); // consume the '/='
                            let expr = self.parse_logic();
                            let lhs_expr = Expr::Variable(var_name.clone()); // append _var to the variable name
                            let new_value = Expr::Binary {
                                left: Box::new(lhs_expr),
                                op: Token::Slash,
                                right: Box::new(expr),
                            };
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            }
                            stmts.push(IRNode::Assign {
                                name: var_name.clone(),
                                value: new_value,
                            });
                        },
                        Token::PercentAssign => {
                            self.advance(); // consume the '%='
                            let expr = self.parse_logic();
                            let lhs_expr = Expr::Variable(var_name.clone()); // append _var to the variable name
                            let new_value = Expr::Binary {
                                left: Box::new(lhs_expr),
                                op: Token::Percent,
                                right: Box::new(expr),
                            };
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            }
                            stmts.push(IRNode::Assign {
                                name: var_name.clone(),
                                value: new_value,
                            });
                        },
                        Token::Star => {
                            self.advance(); // consume the '*'
                            let addr_expr = self.parse_factor();
                            assert_eq!(self.cur_token, Token::Assign, "Expected '=' after '*expression'");
                            self.advance(); // consume the '='
                            let rhs = self.parse_logic();
                            if self.cur_token == Token::SemiColon {
                                self.advance(); // consume the semicolon
                            }
                            stmts.push(IRNode::Store {
                                dst: addr_expr,
                                value: rhs,
                            });
                        }
                        _ => {
                            println!("Expected assignment after identifier '{}', found: {:?}", var_name, self.cur_token);
                        }
                    }
                }
                Token::Star => {
                    self.advance();                      // consume '*'
                    let addr_expr = self.parse_factor(); // address expression (e.g. Variable("main_a_var"))
                    assert_eq!(self.cur_token, Token::Assign,
                               "Expected '=' after pointer destination");
                    self.advance();                      // consume '='
                    let rhs = self.parse_logic();        // right-hand side
                    if self.cur_token == Token::SemiColon {
                        self.advance();                  // optional ';'
                    }
                    stmts.push(IRNode::Store { dst: addr_expr, value: rhs });
                }
                _ => {
                    // println!("Expected statement, found: {:?}", self.cur_token);
                    self.advance();
                }
            }
        }
        stmts
    }

    fn const_eval(&self, expr: &Expr) -> Literal {
        match expr {
            Expr::Literal(lit) => lit.clone(),

            Expr::Binary { left, op: Token::Plus, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a),   Literal::Int(b))   => Literal::Int(a + b),
                    (Literal::Float(a), Literal::Float(b)) => Literal::Float(a + b),
                    (Literal::Int(a),   Literal::Float(b)) => Literal::Float(a as f64 + b),
                    (Literal::Float(a), Literal::Int(b))   => Literal::Float(a + b as f64),
                    _ => panic!("Cannot add strings in a constant expression"),
                }
            }

            Expr::Binary { left, op: Token::Minus, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a),   Literal::Int(b))   => Literal::Int(a - b),
                    (Literal::Float(a), Literal::Float(b)) => Literal::Float(a - b),
                    (Literal::Int(a),   Literal::Float(b)) => Literal::Float(a as f64 - b),
                    (Literal::Float(a), Literal::Int(b))   => Literal::Float(a - b as f64),
                    _ => panic!("Cannot subtract strings in a constant expression"),
                }
            }

            Expr::Binary { left, op: Token::AndAnd, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a), Literal::Int(b)) => Literal::Int((a != 0 && b != 0) as i64),
                    (Literal::Float(a), Literal::Float(b)) => Literal::Int((a != 0.0 && b != 0.0) as i64),
                    (Literal::Int(a), Literal::Float(b)) => Literal::Int((a != 0 && b != 0.0) as i64),
                    (Literal::Float(a), Literal::Int(b)) => Literal::Int((a != 0.0 && b != 0) as i64),
                    _ => panic!("Cannot evaluate logical AND on non-numeric constants"),
                }
            }
            Expr::Binary { left, op: Token::OrOr, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a), Literal::Int(b)) => Literal::Int((a != 0 || b != 0) as i64),
                    (Literal::Float(a), Literal::Float(b)) => Literal::Int((a != 0.0 || b != 0.0) as i64),
                    (Literal::Int(a), Literal::Float(b)) => Literal::Int((a != 0 || b != 0.0) as i64),
                    (Literal::Float(a), Literal::Int(b)) => Literal::Int((a != 0.0 || b != 0) as i64),
                    _ => panic!("Cannot evaluate logical OR on non-numeric constants"),
                }
            }

            Expr::Binary { left, op: Token::Percent, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a), Literal::Int(b)) => {
                        if b == 0 { panic!("Division by zero in constant expression!"); }
                        Literal::Int(a % b)
                    }
                    (Literal::Float(a), Literal::Float(b)) => {
                        if b == 0.0 { panic!("Division by zero in constant expression!"); }
                        Literal::Float(a % b)
                    }
                    (Literal::Int(a), Literal::Float(b)) => {
                        if b == 0.0 { panic!("Division by zero in constant expression!"); }
                        Literal::Float(a as f64 % b)
                    }
                    (Literal::Float(a), Literal::Int(b)) => {
                        if b == 0 { panic!("Division by zero in constant expression!"); }
                        Literal::Float(a % b as f64)
                    }
                    _ => panic!("Cannot evaluate modulo on non-numeric constants"),
                }
            }

            Expr::Binary { left, op: Token::Star, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a),   Literal::Int(b))   => Literal::Int(a * b),
                    (Literal::Float(a), Literal::Float(b)) => Literal::Float(a * b),
                    (Literal::Int(a),   Literal::Float(b)) => Literal::Float(a as f64 * b),
                    (Literal::Float(a), Literal::Int(b))   => Literal::Float(a * b as f64),
                    _ => panic!("Cannot multiply strings in a constant expression"),
                }
            }

            Expr::Binary { left, op: Token::Slash, right } => {
                let l = self.const_eval(left);
                let r = self.const_eval(right);
                match (l, r) {
                    (Literal::Int(a),   Literal::Int(b))   => {
                        if b == 0 { panic!("Division by zero in constant expression!"); }
                        Literal::Int(a / b)
                    }
                    (Literal::Float(a), Literal::Float(b)) => {
                        if b == 0.0 { panic!("Division by zero in constant expression!"); }
                        Literal::Float(a / b)
                    }
                    (Literal::Int(a),   Literal::Float(b)) => {
                        if b == 0.0 { panic!("Division by zero in constant expression!"); }
                        Literal::Float(a as f64 / b)
                    }
                    (Literal::Float(a), Literal::Int(b))   => {
                        if b == 0 { panic!("Division by zero in constant expression!"); }
                        Literal::Float(a / b as f64)
                    }
                    _ => panic!("Cannot divide strings in a constant expression"),
                }
            }

            Expr::Variable(var_name) => {
                // let var_name = self.scoped(name.clone().as_str()); // append _var to the variable name
                self.constants
                    .get(var_name)
                    .cloned()
                    .unwrap_or_else(|| panic!("Variable '{}' is not declared!", var_name))
            }

            _ => panic!("Only constant numeric expressions are allowed in a let declaration!"),
        }
    }

    pub fn parse_comparison(&mut self) -> Expr {
        let mut expr = self.parse_expression();

        while matches!(
            self.cur_token,
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual | Token::EqualEqual | Token::BangEqual
        ) {
            let op = self.cur_token.clone();
            self.advance();
            let rhs = self.parse_expression(); // right operand

            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(rhs),
            };
        }
        expr
    }

    fn is_const_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Literal(_) => true,
            Expr::Binary { left, right, .. } => Self::is_const_expr(left) && Self::is_const_expr(right),
            _ => false, // function calls are not constant expressions
        }
    }

    pub fn parse_let(&mut self) -> Vec<IRNode> {
        self.advance(); // consume "let"
        // let mut declarations: Vec<(String, Option<Literal>)> = Vec::new();
        let mut declarations = Vec::new();
        let mut init_stmts = Vec::new();

        loop {
            let name = match &self.cur_token {
                Token::Identifier(n) => {
                    let n = n.clone();
                    self.advance(); // consume the identifier
                    self.scoped(n.as_str())
                },
                _ => panic!("Expected identifier after let, found: {:?}", self.cur_token),
            };

            if self.constants.contains_key(&name) {
                panic!("Variable '{}' is already declared!", name);
            }

            let mut lit_val: Option<Literal> = None;
            if self.cur_token == Token::Assign {
                self.advance(); // consume the '='
                let expr = self.parse_logic(); // parse the rhs expression

                if Self::is_const_expr(&expr) {
                    // compile time constant expression
                    let lit = self.const_eval(&expr);
                    self.constants.insert(name.clone(), lit.clone());
                    lit_val = Some(lit);
                } else {
                    init_stmts.push(IRNode::Assign { name: name.clone(), value: expr });
                    self.constants.insert(name.clone(), Literal::Int(0));
                }
            } else {
               self.constants.insert(name.clone(), Literal::Int(0)); // default to 0
            }

            declarations.push((name, lit_val));

            match &self.cur_token {
                Token::Comma => self.advance(), // consume the comma
                Token::SemiColon => {
                    self.advance(); // consume the semicolon
                    break; // end of declarations
                }
                _ => panic!("Expected comma or semicolon after variable declaration, found: {:?}", self.cur_token),
            }
        }

        // IRNode::VarDecl(declarations)
        let mut nodes = vec![IRNode::VarDecl(declarations)];
        nodes.extend(init_stmts);
        nodes
    }

    pub fn parse_factor(&mut self) -> Expr {
        // pointer dereference
        if self.cur_token == Token::Star {
            self.advance(); // consume '*'
            let inner = self.parse_factor();
            return Expr::Unary {
                op: Token::Star,
                expr: Box::new(inner),
            };
        }

        match &self.cur_token {
            Token::NumberLiteral(_) | Token::StringLiteral(_) => {
                let lit = Self::parse_literal(&self.cur_token);
                self.advance(); // consume the literal
                Expr::Literal(lit)
            }
            Token::Identifier(name) => {
                let base_name = name.clone();
                // peek and check if next token is LParen '(', parse a 'call'
                if self.lexer.input.get(self.lexer.pos).cloned() == Some('(') {
                    // function call
                    self.advance(); // consume function name
                    self.advance(); // consume '('

                    let mut args = Vec::new();
                    if self.cur_token != Token::RParen {
                        loop {
                           let expr = self.parse_logic();
                            args.push(expr);
                            if self.cur_token == Token::Comma {
                                self.advance(); // consume the comma
                                continue;
                            }
                            break; // end of arguments
                        }
                    }

                    if self.cur_token == Token::RParen {
                        self.advance(); // consume ')'
                    } else {
                        panic!("Expected ')' after function arguments, found: {:?}", self.cur_token);
                    }

                    Expr::Call {
                        name: base_name.clone(),
                        args,
                    }
                } else {
                    let var_name = self.scoped(base_name.clone().as_str());
                    self.advance(); // consume the identifier
                    Expr::Variable(var_name)
                }
            }
            Token::LParen => {
                self.advance();   // consume '('
                let inner = self.parse_logic();    // full expression inside
                assert_eq!(self.cur_token, Token::RParen,
                           "Expected ')' to close expression");
                self.advance();  // consume ')'
                inner
            }
            _ => panic!("Expected a factor, found: {:?}", self.cur_token),
        }
    }

    pub fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();
        while matches!(self.cur_token, Token::Star | Token::Slash | Token::Percent) {
            let op = self.cur_token.clone(); // save the operator
            self.advance(); // consume the operator
            let rhs = self.parse_factor();

            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(rhs),
            }
        }
        expr
    }

    pub fn parse_expression(&mut self) -> Expr {
        let mut expr = self.parse_term();

        // TODO: Handle binary operations like +, -, *, /
        while matches!(self.cur_token, Token::Plus | Token::Minus) {
            let op = self.cur_token.clone(); // save the '+', '-' operator
            self.advance(); // consume the operator '+'
            let rhs = self.parse_term();

            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(rhs),
            }
        }

        expr
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
        let value = self.parse_logic();
        if self.cur_token == Token::SemiColon { self.advance(); }
        IRNode::Assign { name, value }
    }

    pub fn parse_print(&mut self) -> IRNode {
        self.advance(); // consume "print"
        if self.cur_token == Token::LParen { self.advance(); }

        let mut args = Vec::new();
        let mut first = true;

        loop {
            // must be string literal
            if first {
                match &self.cur_token {
                    Token::StringLiteral(s) => {
                        args.push(PrintArg::Literal(Literal::String(s.clone())));
                        self.advance(); // consume the string literal
                    }
                    Token::Identifier(id) if {
                        let v = self.scoped(id);
                        self.constants.contains_key(&v)
                    } => {
                        let var_name = self.scoped(id); // append _var to the variable name
                        args.push(PrintArg::Variable(var_name.clone()));
                        self.advance(); // consume the identifier
                    }
                    _ => {
                        let expr = self.parse_logic();
                        args.push(PrintArg::Expr(expr));
                    }
                }

                first = false;
            } else {
                // any other ordinary expression
                let expr = self.parse_logic();
                args.push(PrintArg::Expr(expr));
            }

            // check for more args
            if self.cur_token == Token::Comma {
                self.advance(); // consume the comma
                continue;
            }
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
        let mut parts: Vec<String> = Vec::new();
        let mut seg   = String::new();
        let mut chars = src.chars().peekable();

        // helper to flush current text segment (if not empty)
        let flush = |seg: &mut String, parts: &mut Vec<String>| {
            if !seg.is_empty() {
                parts.push(format!("\"{}\"", seg.replace('"', "\"\"")));
                seg.clear();
            }
        };

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                match chars.peek() {
                    Some('n') => { chars.next(); flush(&mut seg, &mut parts); parts.push("13,10".into()); }
                    Some('r') => { chars.next(); flush(&mut seg, &mut parts); parts.push("13".into());    }
                    Some('t') => { chars.next(); flush(&mut seg, &mut parts); parts.push("9".into());     }
                    Some('\\') => { chars.next(); seg.push('\\'); }      // keep backslash
                    Some('"')  => { chars.next(); seg.push('"');  }      // keep quote
                    _ => seg.push('\\'),                                 // unknown escape: keep '\'
                }
            } else {
                seg.push(ch);
            }
        }
        flush(&mut seg, &mut parts);

        parts.join(",")
    }

    pub fn masm_generator(
        out: &mut String,
        expr: &Expr,
        var_decls: &Vec<(String, &Literal)>,
        functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
        lit_table: &mut HashMap<String, String>,
    ){
        match expr {
            Expr::Literal(Literal::Int(i)) => {
                out.push_str(&format!("    mov eax, {}\n",i));
            }
            Expr::Literal(Literal::String(s)) => {
                let lbl = Self::intern_literal(lit_table, s);
                out.push_str(&format!("    lea eax, {}\n", lbl));
            }
            Expr::Variable(name) => {
                let name = name.to_owned(); // append _var to the variable name
                if let Some((_, Literal::Int(_))) = var_decls.iter().find(|(n, _)| *n == name) {
                    out.push_str(&format!("    mov eax, dword ptr [{}]\n", name));
                } else if let Some((_, Literal::Float(_))) = var_decls.iter().find(|(n, _)| *n == name) {
                    out.push_str(&format!("    fld qword ptr [{}]\n", name)); // load float variable
                    out.push_str("    fistp dword ptr [eax]\n"); // convert to int and store in eax
                } else if let Some((_, Literal::String(_))) = var_decls.iter().find(|(n, _)| *n == name) {
                    out.push_str(&format!("    lea eax, {}\n", name)); // load address of string variable
                } else {
                    panic!("Variable '{}' not found in declarations", name);
                }
            }
            Expr::Binary { left, op , right } => {
                Self::masm_generator(out, left, var_decls, functions, lit_table); // generate code for the left expression
                out.push_str("    push eax\n"); // push a left result onto stack
                Self::masm_generator(out, right, var_decls, functions, lit_table); // generate code for right expression
                out.push_str("    pop ebx\n"); // pop left result into ebx

                match op {
                    Token::Plus => out.push_str("    add eax, ebx\n"), // add left and right results
                    Token::Minus => {
                        out.push_str("    sub ebx, eax\n");
                        out.push_str("    mov eax, ebx\n"); // subtract left from right
                    },
                    Token::Star => out.push_str("    imul eax, ebx\n"), // multiply left and right results
                    Token::Slash => {
                        // eax = left / right
                        out.push_str("    mov ecx, eax\n");         // save divisor (= right)
                        out.push_str("    mov eax, ebx\n");         // eax = dividend (= left)
                        out.push_str("    cdq\n");                  // sign-extend into edx
                        out.push_str("    idiv ecx\n");             // eax = quotient
                    },
                    Token::Percent => {
                        // eax = left % right
                        out.push_str("    mov ecx, eax\n");         // save divisor (= right)
                        out.push_str("    mov eax, ebx\n");         // eax = dividend (= left)
                        out.push_str("    cdq\n");                  // sign-extend into edx
                        out.push_str("    idiv ecx\n");             // edx = remainder
                        out.push_str("    mov eax, edx\n");         // result in eax
                    },
                    Token::Less
                    | Token::LessEqual
                    | Token:: Greater
                    | Token::GreaterEqual
                    | Token::EqualEqual
                    | Token::BangEqual => {
                        out.push_str("    cmp ebx, eax\n");
                        out.push_str("    mov eax, 0\n"); // default to false (0)
                        match op {
                            Token::Less => out.push_str("    setl al\n"), // set al to 1 if less
                            Token::LessEqual => out.push_str("    setle al\n"), // set al to 1 if less or equal
                            Token::Greater => out.push_str("    setg al\n"), // set al to 1 if greater
                            Token::GreaterEqual => out.push_str("    setge al\n"), // set al to 1 if greater or equal
                            Token::EqualEqual => out.push_str("    sete al\n"), // set al to 1 if equal
                            Token::BangEqual => out.push_str("    setne al\n"), // set al to 1 if not equal
                            _ => unreachable!("Unsupported comparison operator: {:?}", op)
                        }
                    },
                    Token::AndAnd => {
                        /* EBX = left, EAX = right */
                        out.push_str("    cmp ebx, 0\n");
                        out.push_str("    setne bl\n");       // bl = (left != 0)
                        out.push_str("    cmp eax, 0\n");
                        out.push_str("    setne al\n");       // al  = (right != 0)
                        out.push_str("    and al, bl\n");     // al = al & bl
                        out.push_str("    movzx eax, al\n");  // zero-extend to eax
                    },
                    Token::OrOr => {
                        /* EBX = left, EAX = right */
                        out.push_str("    cmp ebx, 0\n");
                        out.push_str("    setne bl\n");       // bl = (left != 0)
                        out.push_str("    cmp eax, 0\n");
                        out.push_str("    setne al\n");       // al  = (right != 0)
                        out.push_str("    or al, bl\n");      // al = al | bl
                        out.push_str("    movzx eax, al\n");  // zero-extend to eax
                    }
                    _ => unreachable!("Unsupported operator generation: {:?}", op)
                }
            }
            Expr::Unary { op: Token::Star, expr } => {
                // *(ptr +- idx)
                if let Expr::Binary { left, op, right } = &**expr {
                    if matches!(op, Token::Plus | Token::Minus) {
                        Self::masm_generator(out, left, var_decls, functions, lit_table); // evaluate pointer base
                        out.push_str("    push eax\n");
                        Self::masm_generator(out, right, var_decls, functions, lit_table); // evaluate index
                        out.push_str("    pop ebx\n"); // pop pointer base into EBX
                        Self::gen_ptr_add(out, op); // calculate pointer address in EAX
                        out.push_str("    mov eax, dword ptr [eax]\n"); // dereference pointer
                        return;
                    }
                }

                // fallback
                Self::masm_generator(out, expr, var_decls, functions, lit_table);
                out.push_str("    mov eax, dword ptr [eax]\n"); // dereference pointer
            }
            Expr::Call { name, args } => {
                // Evaluate args right-to-left, push them on the stack
                // We assume each argument fits in EAX after evaluating its expression.
                for arg in args.iter().rev() {
                    Self::masm_generator(out, arg, var_decls, functions, lit_table);
                    out.push_str("    push eax\n");
                }
                // Now call the function
                out.push_str(&format!("    call {}\n", name));
                // After call, the return value (if any) is in EAX.
                // The caller is responsible for cleaning up: add esp, <args.len()*4>
                let cleanup_bytes = args.len() * 4;
                if cleanup_bytes > 0 {
                    out.push_str(&format!("    add esp, {}\n", cleanup_bytes));
                }
                // At this point, EAX holds the returned integer (or 0 if void).
            }
            _ => panic!("Unsupported expression type for MASM generation: {:?}", expr)
        }
    }

    fn gen_label(counter: &mut usize) -> String {
        let lbl = format!("label_{}", *counter);
        *counter += 1;
        lbl
    }

    fn gen_ptr_add(out: &mut String, op: &Token) {
        out.push_str("    imul eax, 4\n");           // index *= 4   (sizeof i32)
        match op {
            Token::Plus  => out.push_str("    add eax, ebx\n"),
            Token::Minus => {
                out.push_str("    sub ebx, eax\n");
                out.push_str("    mov eax, ebx\n");
            }
            _ => unreachable!(),
        }
    }

    pub fn emit_stmt(
        out: &mut String,
        stmt: &IRNode,
        var_decls: &Vec<(String, &Literal)>,
        lbl_counter: &mut usize,
        tail: &String,
        functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
        lit_table: &mut HashMap<String, String>,
    ) {
        match stmt {
            IRNode::If {cond, then_branch, else_branch} => {
                let else_label = Self::gen_label(lbl_counter);
                let end_label = Self::gen_label(lbl_counter);

                // Evaluate condition
                Self::masm_generator(out, cond, var_decls, functions, lit_table);
                out.push_str("    cmp eax, 0\n");
                out.push_str(&format!("    je {}\n", else_label));

                // Then branch
                for inner in then_branch {
                    Self::emit_stmt(out, inner, var_decls, lbl_counter, tail, functions, lit_table);
                }
                out.push_str(&format!("    jmp {}\n", end_label));

                // Else branch
                out.push_str(&format!("{}:\n", else_label));
                if let Some(else_branch) = else_branch {
                    for inner in else_branch {
                        Self::emit_stmt(out, inner, var_decls, lbl_counter, tail, functions, lit_table);
                    }
                }
                out.push_str(&format!("{}:\n", end_label));
            }

            IRNode::While { cond, body } => {
                let start_label = Self::gen_label(lbl_counter);
                let body_end_label = Self::gen_label(lbl_counter); // End of loop body
                let exit_label = Self::gen_label(lbl_counter);

                out.push_str(&format!("{}:\n", start_label));
                IRTranslator::masm_generator(out, cond, var_decls, functions, lit_table);
                out.push_str("    cmp eax, 0\n");
                out.push_str(&format!("    je {}\n", exit_label));

                for inner in body {
                    Self::emit_stmt(out, inner, var_decls, lbl_counter, &body_end_label, functions, lit_table);
                }

                out.push_str(&format!("{}:\n", body_end_label));
                out.push_str(&format!("    jmp {}\n", start_label));
                out.push_str(&format!("{}:\n", exit_label));
            }

            IRNode::Assign { name, value } => {
                // let's add labels
                Self::masm_generator(out, value, var_decls, functions, lit_table);
                out.push_str(&format!("    mov dword ptr [{}], eax\n", name));
            }

            // Handle Print statements
            IRNode::Print { args } => {
                let mut pushes = 0;
                // declare literals
                let mut fmt_builder = String::new();

                // push args in reverse order
                let original_fmt = args.first();
                let args: Vec<&PrintArg> = if let PrintArg::Literal(Literal::String(_)) = &args[0] {
                    args.iter().skip(1).collect()
                } else {
                    args.iter().collect()
                };

                for param in args.iter().rev() {
                    match param {
                        PrintArg::Literal(Literal::Int(i)) => {
                            out.push_str(&format!("    push {}\n", i));
                            fmt_builder.push_str("%d");
                            pushes += 1;
                        },
                        PrintArg::Literal(Literal::Float(f)) => {
                            let lbl = IRTranslator::intern_literal(lit_table, &format!("{}", f));
                            out.push_str(&format!("    fld  qword ptr [{}]\n", lbl)); // …optional
                            out.push_str(&format!("    push dword ptr [{}+4]\n", lbl));
                            out.push_str(&format!("    push dword ptr [{}]\n",   lbl));
                            fmt_builder.push_str("%f");
                            pushes += 2;
                        }
                        PrintArg::Literal(Literal::String(s)) => {
                            let lbl = IRTranslator::intern_literal(lit_table, s);
                            out.push_str(&format!("    push offset {}\n", lbl));
                            fmt_builder.push_str("%s");
                            pushes += 1;
                        },
                        PrintArg::Variable(name) => match var_decls.iter().find(|(n, _)| n == name) {
                            Some((_, Literal::Int(_))) => {
                                out.push_str(&format!("    push dword ptr [{}]\n", name));
                                fmt_builder.push_str("%d");
                                pushes += 1;
                            },
                            Some((_, Literal::Float(_))) => {
                                out.push_str(&format!("    push dword ptr [{}+4]\n", name));
                                out.push_str(&format!("    push dword ptr [{}]\n", name));
                                fmt_builder.push_str("%f");
                                pushes += 2;
                            },
                            Some((_, Literal::String(_))) => {
                                out.push_str(&format!("    lea eax, {}\n", name));
                                out.push_str("    push eax\n");
                                fmt_builder.push_str("%s");
                                pushes += 1;
                            },
                            _  => {
                                panic!("Variable '{}' not found in declarations", name);
                            }
                        },
                        PrintArg::Expr(e) => {
                            Self::masm_generator(out, e, var_decls, functions, lit_table);
                            out.push_str("    push eax\n");
                            fmt_builder.push_str("%d");
                            pushes += 1;
                        },
                    }
                }

                let (_, enc_fmt) = match original_fmt {
                    // user gave a literal → raw is *exactly* that string
                    Some(PrintArg::Literal(Literal::String(s))) => {
                        let enc = IRTranslator::masm_encode_string(s);  // encode once
                        (s.clone(), enc)
                    }

                    // no literal → we built an automatic "%d%f..." in fmt_text;
                    // it still needs encoding for MASM.
                    Some(_) => {
                        let raw = format!("{fmt_builder}\\n");
                        let enc = IRTranslator::masm_encode_string(&raw);
                        (raw, enc)
                    }

                    None => panic!("print() called with no arguments"),
                };

                // Re-use existing literal label if present
                let fmt_lbl = IRTranslator::intern_literal(lit_table, &enc_fmt);
                out.push_str(&format!("    push offset {fmt_lbl}\n"));
                pushes += 1;

                out.push_str("    call printf\n");
                out.push_str(&format!("    add esp, {}\n", pushes * 4));
            }
            IRNode::Store { dst, value} => {
                // rhs => edx
                Self::masm_generator(out, value, var_decls, functions, lit_table);
                out.push_str("    mov edx, eax\n"); // store value in edx

                // address => eax
                if let Expr::Binary { left, op, right } = dst {
                   if matches!(op, Token::Plus | Token::Minus) {
                       Self::masm_generator(out, left, var_decls, functions, lit_table);
                       out.push_str("    push eax\n"); // save pointer base
                       Self::masm_generator(out, right, var_decls, functions, lit_table);
                       out.push_str("    pop ebx\n"); // pop pointer base into EBX
                       Self::gen_ptr_add(out, op); // calculate pointer address in EAX
                   } else {
                          Self::masm_generator(out, dst, var_decls, functions, lit_table);
                   }
                } else {
                    Self::masm_generator(out, dst, var_decls, functions, lit_table);
                }

                out.push_str("    mov dword ptr [eax], edx\n"); // store value in address
            }
            IRNode::Call { name, args } => {
                // generate code for function call
                for arg in args {
                    Self::masm_generator(out, arg, var_decls, functions, lit_table);
                    out.push_str("    push eax\n"); // push argument onto stack
                }
                out.push_str(&format!("    call {}\n", name));
                let num_args = args.len();
                out.push_str(&format!("    add esp, {}\n", num_args * 4)); // clean up stack
            }
            IRNode::Return(opt_expr) => {
                if let Some(expr) = opt_expr {
                    Self::masm_generator(out, expr, var_decls, functions, lit_table);
                } else {
                    out.push_str("    xor eax, eax\n"); // return 0 if no expression
                }
                out.push_str(&format!("    jmp {}\n", tail)); // jump to the end of the function
            }
            _ => {}
        }
    }


    fn collect_libs(
        stmt: &IRNode,
        defined: &HashMap<String, (Vec<(String, Type)>, Type)>,
        libs: &mut Vec<String>,         // preserves order for the final MASM file
        extrns: &mut HashSet<String>,   // tracks *symbols* already declared
    ) {
        fn walk_expr(
            e: &Expr,
            defined: &HashMap<String, (Vec<(String, Type)>, Type)>,
            libs: &mut Vec<String>,
            extrns: &mut HashSet<String>,
        ) {
            match e {
                Expr::Call { name, args } => {
                    if !defined.contains_key(name) {
                        // 1️⃣  make sure EXTRN appears once per symbol
                        extrns.insert(name.clone());

                        // 2️⃣  make sure the corresponding library line appears once
                        let lib = lib_for(name);
                        if !libs.contains(&lib.to_string()) {
                            libs.push(lib.to_string());
                        }
                    }
                    // recurse into arguments
                    for a in args {
                        walk_expr(a, defined, libs, extrns);
                    }
                }

                Expr::Unary  { expr, .. }        => walk_expr(expr, defined, libs, extrns),
                Expr::Binary { left, right, .. } => {
                    walk_expr(left,  defined, libs, extrns);
                    walk_expr(right, defined, libs, extrns);
                }
                Expr::Literal(_) | Expr::Variable(_) => {}
            }
        }

        match stmt {
            // nodes that hold expressions
            IRNode::Assign { value, .. } |
            IRNode::Return(Some(value))  => walk_expr(value, defined, libs, extrns),

            IRNode::Print { args }       => {
                extrns.insert("printf".into());

                let lib = lib_for("printf"); // usually "msvcrt.lib"
                if !libs.contains(&lib.to_string()) {
                    libs.push(lib.to_string());
                }

                for p in args {
                    if let PrintArg::Expr(e) = p {
                        walk_expr(e, defined, libs, extrns);
                    }
                }
            }

            IRNode::Store { dst, value } => {
                walk_expr(dst,   defined, libs, extrns);
                walk_expr(value, defined, libs, extrns);
            }

            IRNode::Call { name, args }  => {
                let dummy = Expr::Call { name: name.clone(), args: args.clone() };
                walk_expr(&dummy, defined, libs, extrns);
            }

            // control-flow recursion
            IRNode::If { cond, then_branch, else_branch } => {
                walk_expr(cond, defined, libs, extrns);
                for n in then_branch { Self::collect_libs(n, defined, libs, extrns); }
                if let Some(eb) = else_branch {
                    for n in eb { Self::collect_libs(n, defined, libs, extrns); }
                }
            }
            IRNode::While { cond, body } => {
                walk_expr(cond, defined, libs, extrns);
                for n in body { Self::collect_libs(n, defined, libs, extrns); }
            }

            // everything else has no external calls
            IRNode::VarDecl(_)
            | IRNode::Return(None)
            | IRNode::Function { .. } => {}
        }
    }


    fn scan_expr(expr: &Expr, pool: &mut HashMap<String, String>) {
        match expr {
            Expr::Literal(Literal::String(s)) => {
                Self::intern_literal(pool, s);
            }
            Expr::Binary { left, right, .. } => {
                Self::scan_expr(left, pool);
                Self::scan_expr(right, pool);
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    Self::scan_expr(arg, pool);
                }
            }
            Expr::Literal(_) | Expr::Variable(_) => {
                // no literals to collect in these nodes
            }
            Expr::Unary { expr, .. } => {
                Self::scan_expr(expr, pool);
            }
        }
    }

    fn intern_literal(pool: &mut HashMap<String, String>, s: &str) -> String {
        let idx = pool.len();
        pool.entry(s.to_owned())
            .or_insert_with(|| lit_label(idx))
            .clone()
    }

    fn collect_literals(stmt: &IRNode, pool: &mut HashMap<String, String>) {
        match stmt {
            IRNode::Print { args} => {
                if let Some(PrintArg::Literal(Literal::String(s))) = args.first() {
                    let var = Self::masm_encode_string(s.clone().as_str());
                    Self::intern_literal(pool, &var); // add to the pool → lit1, lit2, …
                } else {
                    let mut fmt = String::new();
                    for p in args {
                        match p {
                            PrintArg::Literal(Literal::Float(_))          => fmt.push_str("%f"),
                            PrintArg::Literal(Literal::String(_))         => fmt.push_str("%s"),
                            // Int literals, variables and expressions all use %d
                            _                                             => fmt.push_str("%d"),
                        }
                    }
                    fmt.push_str("\\n");          // trailing CR/LF just like emit_stmt
                    let enc = Self::masm_encode_string(&fmt);
                    Self::intern_literal(pool, &enc); // add to the pool → lit1, lit2, …
                }
            }
            IRNode::Assign {value, ..} | IRNode::Return(Some(value)) => Self::scan_expr(value, pool),
            IRNode::If { cond, then_branch, else_branch } => {
                Self::scan_expr(cond, pool);
                for inner in then_branch {
                    Self::collect_literals(inner, pool);
                }
                if let Some(else_branch) = else_branch {
                    for inner in else_branch {
                        Self::collect_literals(inner, pool);
                    }
                }
            },
            IRNode::While { cond, body} => {
                Self::scan_expr(cond, pool);
                for inner in body {
                    Self::collect_literals(inner, pool);
                }
            }
            IRNode::Call { args, ..} => {
                for arg in args {
                    Self::scan_expr(arg, pool);
                }
            },
            IRNode::VarDecl(_) | IRNode::Function {..} => {
                // no literals to collect in these nodes
            }
            _ => {}
        }
    }

    fn collect_vars<'a>(stmt: &'a IRNode, out: &mut Vec<(String, &'a Literal)>) {
        match stmt {
            IRNode::VarDecl(decls) => {
                for (name, value) in decls {
                    // If the value is None, we assume it's 0
                    let lit = value.as_ref().unwrap_or(&Literal::Int(0));
                    out.push((name.clone(), lit));
                }
            }
            IRNode::If { cond: _, then_branch, else_branch } => {
                for s in then_branch { Self::collect_vars(s, out); }
                if let Some(else_branch) = else_branch {
                    for s in else_branch { Self::collect_vars(s, out); }
                }
            }

            IRNode::While { body, ..} => {
                for s in body {
                    Self::collect_vars(s, out);
                }
            }
            _ => {}
        }
    }

    pub fn translate(
        nodes: &[IRNode],
        functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
        backend: Backend) -> (String, Vec<String>) {
        match backend {
            Backend::JavaScript => todo!(),
            Backend::MASM => {
                let mut libraries = Vec::new();
                let mut externs = HashSet::new();
                let mut out = String::new();
                let mut lit_table: HashMap<String, String> = HashMap::new();

                // MASM boilerplate
                out.push_str(".386\n.model flat, c\n");
                out.push_str("option casemap:none\n\n");

                // declare all external functions
                nodes.iter().for_each(|n| {
                    if let IRNode::Function { name: _, params: _, return_type: _, body} = n {
                        for stmt in body {
                            Self::collect_libs(stmt, functions, &mut libraries, &mut externs);
                        }
                    }
                });

                for s in &externs {
                    out.push_str(&format!("extrn {s}:PROC\n"));
                }

                if !externs.is_empty() {
                    out.push('\n');
                }

                // all includelib lines in first-seen order
                for l in &libraries {
                    out.push_str(&format!("includelib lib\\{l}\n"));
                }
                if !libraries.is_empty() {
                    out.push('\n');
                }

                // collect variables
                let mut var_decls = Vec::new();
                for n in nodes {
                    if let IRNode::Function { body, params, name: func_name, .. } = n {
                        // parameters
                        for (_name, _) in params {
                            let v = format!("{func_name}_{_name}{VAR_NAME_EXTENSION}");
                            var_decls.push((v, &Literal::Int(0)));
                        }
                        // every VarDecl anywhere in the body
                        for stmt in body {
                            Self::collect_vars(stmt, &mut var_decls);
                        }
                    }
                }

                // data section
                out.push_str(".data\n");

                // declare variables
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

                // collect literals from all functions
                for n in nodes {
                    if let IRNode::Function { name: _, params: _, return_type: _, body } = n {
                        for stmt in body {
                            Self::collect_literals(stmt, &mut lit_table);
                        }
                    }
                }

                // declare literals
                for (src, lbl) in &lit_table {
                    out.push_str(&format!("{} db {},0\n", lbl, src));
                }

                // code section
                out.push_str("\n.code\n");

                let mut lbl_counter = 0;

                for n in nodes {
                    if let IRNode::Function { name: func_name, params, return_type, body } = n {
                        // Generate function header (prologue)
                        out.push_str(&format!("{} proc\n", func_name));
                        out.push_str("    push ebp\n");
                        out.push_str("    mov ebp, esp\n");

                        // copy each parameter to local variable
                        for (idx, (_name, _ptype)) in params.iter().enumerate() {
                            let var_name = format!("{}_{}{VAR_NAME_EXTENSION}",func_name, _name.clone()); // append _var to the variable name
                            out.push_str(&format!("    mov eax, dword ptr [ebp+{}]\n", 8 + idx * 4));
                            out.push_str(&format!("    mov dword ptr [{}], eax\n", var_name));
                        }

                        // set up a "return label" for returns
                        let func_tail = Self::gen_label(&mut lbl_counter);

                        for stmt in body.iter() {
                            Self::emit_stmt(&mut out, stmt, &var_decls, &mut lbl_counter, &func_tail, functions, &mut lit_table);
                        }

                        // remove 'jmp func_tail' if it is the last statement
                        {
                            let needle = format!("    jmp {}\n", func_tail);
                            if out.ends_with(&needle) {
                                let new_len = out.len() - needle.len();
                                out.truncate(new_len); // remove the last jump to the tail
                            }
                        }

                        out.push_str(&format!("{}:\n", func_tail));

                        // Generate function epilogue
                        // If the return type is void, we don't need to return anything
                        if matches!(return_type, Type::Void) {
                            out.push_str("    xor eax, eax\n");
                        }
                        out.push_str("    mov esp, ebp\n");
                        out.push_str("    pop ebp\n");
                        out.push_str("    ret\n");
                        out.push_str(&format!("{} endp\n\n", func_name));
                    }
                }
                out.push_str("end main\n");
                (out, libraries)
            },
        }
    }
}

fn main() {
    // open file called main.sf
    let src = std::fs::read_to_string("src/main.sf")
        .expect("Failed to read source file");
    let lexer = Lexer::new(src.as_str());
    let mut parser = Parser::new(lexer);
    let ir = parser.parse_program();
    println!("--------------- IR OUTPUT ---------------");
    println!("{:?}", ir);
    println!("----------------------------------------");

    let (masm, libs) = IRTranslator::translate(&ir, &parser.functions, Backend::MASM);

    let masm32 = "c:\\masm32\\bin\\ml.exe";
    let masm32linker = "c:\\masm32\\bin\\link.exe";
    let output_file = "c:\\masm32\\test.asm";
    let obj_path = Path::new(output_file).with_extension("obj");   // C:\...\test.obj
    let exe_path = Path::new(output_file).with_extension("exe");   // C:\...\test.obj
    let work_dir = Path::new(output_file).parent().unwrap();                     // directory to work in
    std::fs::write(output_file, &masm).expect("Unable to write file");


    // compile and run the MASM32 from c:\masm32\bin\ml.exe
    println!("----------------- MASM OUTPUT ------------------");
    println!("{}", masm);
    println!("-------------------------------------------------");

    if true {
        println!("\nMASM code generated and saved to: {}", output_file);
        // assemble, link the output file
        let status = std::process::Command::new(masm32)
            .arg("/c")
            .arg("/coff")
            .arg(format!("/Fo{}", obj_path.display()))   // <── put OBJ here
            .arg(output_file)
            .stdout(Stdio::null())        // hide normal output
            .stderr(Stdio::null())
            .status()
            .expect("Failed to execute assembler");

        if status.success() {
            println!("Assembly successful!");
            let libs = libs.iter()
                .map(|lib| format!("c:\\masm32\\lib\\{}", lib))
                .collect::<Vec<_>>();
            // link the object file to create an executable
            let link_status = std::process::Command::new(masm32linker)
                .arg(obj_path.as_os_str())
                .args(&libs)   // <── provides _printf
                .arg("/SUBSYSTEM:CONSOLE")
                .arg("/ENTRY:main")             // <── bypass the CRT startup
                .args(["/OPT:REF", "/OPT:ICF"]) // <── for debugging
                .arg(format!("/OUT:{}", exe_path.display())) // <── put EXE here
                .stdout(Stdio::null())        // hide normal output
                .stderr(Stdio::null())
                .status()
                .expect("Failed to execute linker");

            if link_status.success() {
                println!("Linking successful! Executable created: test.exe");

                println!("\n----------------- RUN OUTPUT ------------------");
                let run = Command::new(&exe_path)
                    .current_dir(work_dir)                 // optional; same dir as exe
                    .output()                              // captures stdout + stderr
                    .expect("failed to launch executable");
                print!("{}", String::from_utf8_lossy(&run.stdout));
                println!("\n---------------- {} ---------------------", run.status);
            } else {
                println!("Linking failed!");
            }
        } else {
            println!("Assembly failed!");
        }
    }
}

