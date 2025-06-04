use std::collections::HashMap;
use std::path::Path;
use std::process::{Command, Stdio};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(String),
    Plus,          // +
    PlusAssign,  // +=
    Minus,         // -
    MinusAssign, // -=
    Star,          // *
    StarAssign,  // *=
    Slash,         // /
    SlashAssign, // /=
    Percent ,      // %
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

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),          //  5 or  3.14 or "abc"
    Variable(String),          //  x
    Binary {                   //  x + 4 or 7 + y or a + b
        left: Box<Expr>,
        op: Token,            // only Token::Plus, Token::Minus, etc.
        right: Box<Expr>,
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

fn lit_label(idx: usize) -> String {
    format!("lit{}", idx)
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
                Token::Identifier(id) if id == "let" => stmts.push(self.parse_let()),
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
                    let base_name = id.clone();
                    let var_name = self.scoped(id.clone().as_str()); // append _var to the variable name
                    if !self.constants.contains_key(&var_name) {
                        panic!("Variable '{}' hasn't been declared!", var_name);
                    }

                    self.advance();
                    // if self.cur_token == Token::Assign {
                    //     stmts.push(self.parse_assign(var_name));
                    // } else {
                    //     println!("Expected assignment after identifier '{}', found: {:?}", var_name, self.cur_token);
                    // }
                    match &self.cur_token {
                        Token::Assign => stmts.push(self.parse_assign(var_name)),
                        Token::PlusAssign => {
                            self.advance(); // consume the '+='
                            let expr = self.parse_logic();
                            let lhs_expr = Expr::Variable(base_name); // append _var to the variable name
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
                            let lhs_expr = Expr::Variable(base_name); // append _var to the variable name
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
                            let lhs_expr = Expr::Variable(base_name); // append _var to the variable name
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
                            let lhs_expr = Expr::Variable(base_name); // append _var to the variable name
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
                            let lhs_expr = Expr::Variable(base_name); // append _var to the variable name
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
                        _ => {
                            println!("Expected assignment after identifier '{}', found: {:?}", var_name, self.cur_token);
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

            Expr::Variable(name) => {
                let var_name = self.scoped(name.clone().as_str()); // append _var to the variable name
                self.constants
                    .get(&var_name)
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

    pub fn parse_let(&mut self) -> IRNode {
        self.advance(); // consume "let"
        let mut declarations: Vec<(String, Option<Literal>)> = Vec::new();

        loop {
            let name = match &self.cur_token {
                Token::Identifier(n) => {
                    let n = n.clone();
                    self.advance(); // consume the identifier
                    self.scoped(n.as_str())
                },
                _ => panic!("Expected identifier after let, found: {:?}", self.cur_token),
            };

            let mut value: Option<Literal> = None;
            if self.cur_token == Token::Assign {
                self.advance();  // consume the '='

                // strings: keep the old direct path
                if let Token::StringLiteral(_) = &self.cur_token {
                    value = Some(Self::parse_literal(&self.cur_token));
                    self.advance();                 // eat the string token
                } else {
                    // anything else: parse expression then fold
                    let expr = self.parse_logic();      // returns Expr
                    value = Some(self.const_eval(&expr));   // → Literal
                    self.constants.insert(name.clone(), value.clone().unwrap());
                }
            } else {
                // if no value is assigned, we just declare the variable without initialization
                self.constants.insert(name.clone(), Literal::Int(0)); // default to 0
            }

            declarations.push((name, value));

            match &self.cur_token {
                Token::Comma => self.advance(), // consume the comma
                Token::SemiColon => {
                    self.advance(); // consume the semicolon
                    break; // end of declarations
                }
                _ => panic!("Expected comma or semicolon after variable declaration, found: {:?}", self.cur_token),
            }
        }

        IRNode::VarDecl(declarations)
    }

    pub fn parse_factor(&mut self) -> Expr {
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

                    if self. cur_token == Token::RParen {
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
                if let Token::StringLiteral(s) = &self.cur_token {
                    args.push(PrintArg::Literal(Literal::String(s.clone())));
                    self.advance(); // consume the string literal
                    first = false;
                }else if let Token::Identifier(id) = &self.cur_token {
                    // if the first argument is an identifier, it must be a variable
                    let var_name = self.scoped(id.clone().as_str()); // append _var to the variable name
                    if self.constants.contains_key(&var_name) {
                        args.push(PrintArg::Variable(var_name));
                    } else {
                        panic!("Variable '{}' is not declared!", var_name);
                    }
                } else {
                    panic!("Expected string literal as first argument to print, found: {:?}", self.cur_token);
                }
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

                out.push_str(&format!("    lea eax, {}\n", lbl));            }
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

    pub fn emit_stmt(
        out: &mut String,
        stmt: &IRNode,
        var_decls: &Vec<(String, &Literal)>,
        fmt_map: &mut Vec<(String, String)>,
        fmt_index: &mut usize,
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
                    Self::emit_stmt(out, inner, var_decls, fmt_map, fmt_index, lbl_counter, tail, functions, lit_table);
                }
                out.push_str(&format!("    jmp {}\n", end_label));

                // Else branch
                out.push_str(&format!("{}:\n", else_label));
                if let Some(else_branch) = else_branch {
                    for inner in else_branch {
                        Self::emit_stmt(out, inner, var_decls, fmt_map, fmt_index, lbl_counter, tail, functions, lit_table);
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
                    Self::emit_stmt(out, inner, var_decls, fmt_map, fmt_index, lbl_counter, &body_end_label, functions, lit_table);
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
                // let entry = Self::gen_label(lbl_counter);
                // out.push_str(&format!("{}:\n", entry));
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
                        PrintArg::Expr(e) => {
                            Self::masm_generator(out, e, var_decls, functions, lit_table);
                            out.push_str("    push eax\n");
                        },
                    }
                }
                // push format label
                let fmt_label = &fmt_map[*fmt_index].0;
                *fmt_index += 1;
                out.push_str(&format!("    push offset {}\n", fmt_label));
                out.push_str("    call printf\n");

                let num_args = fmt_map[*fmt_index - 1].1.matches('%').count();
                out.push_str(&format!("    add esp, {}\n", (num_args + 1 + clean_float_stack) * 4));
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

    fn collect_declarations(
        stmt: &IRNode,
        var_decls: &Vec<(String, &Literal)>,
        fmt_map: &mut Vec<(String, String)>,
        fmt_count: &mut usize,
    ) {
       match stmt {
           IRNode::If { cond: _, then_branch, else_branch } => {
               for inner in then_branch {
                   Self::collect_declarations(inner, var_decls, fmt_map, fmt_count);
               }
               if let Some(else_branch) = else_branch {
                   for inner in else_branch {
                       Self::collect_declarations(inner, var_decls, fmt_map, fmt_count);
                   }
               }
           }
           IRNode::While { cond: _, body } => {
                for inner_stmt in body {
                    Self::collect_declarations(inner_stmt, var_decls, fmt_map, fmt_count);
                }
           }
           IRNode::Print { args } => {
               // check if we have string only, variable or mixed args
               let has_string = args.iter().any(|a| matches!(a, PrintArg::Literal(Literal::String(_))));
               let has_variable = args.iter().any(|a| matches!(a, PrintArg::Variable(_)));
               let has_number = args.iter().any(|a| matches!(a, PrintArg::Literal(Literal::Int(_)))) ||
               args.iter().any(|a| matches!(a, PrintArg::Literal(Literal::Float(_))));
               let mut fmt_str = String::new();

               if !has_string && !has_variable && has_number {
                   fmt_str.push_str("%d");
                   fmt_map.push((format!("fmt{}", fmt_count), String::from("\"%d\"")));
                   *fmt_count += 1;
               }

               if has_string && !has_variable && !has_number {
                   // get arg literal
                   if let Some(PrintArg::Literal(Literal::String(name))) = args.first() {
                       let new_name = Self::masm_encode_string(name.as_str());
                       fmt_map.push((format!("fmt{}", fmt_count), new_name));
                       *fmt_count += 1;
                       return;
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
                       *fmt_count += 1;
                       return;
                   }
               }

               if has_variable && (has_string || has_number) {
                   // check if we have a string literal as first arg
                   if let Some(PrintArg::Literal(Literal::String(name))) = args.first() {
                       // check if placeholders (like %d, %s) exists, else set a warning
                       let new_name = Self::masm_encode_string(name.as_str()).trim().to_string();
                       fmt_map.push((format!("fmt{}", fmt_count), new_name));
                       *fmt_count += 1;
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
                           PrintArg::Expr(e) => {
                               // handle expressions
                               if let Expr::Literal(Literal::Int(_)) = e {
                                   fmt_str.push_str("%d");
                               } else if let Expr::Literal(Literal::Float(_)) = e {
                                   fmt_str.push_str("%f");
                               } else if let Expr::Literal(Literal::String(_)) = e {
                                   fmt_str.push_str("%s");
                               } else if let Expr::Binary { left: _, op: Token::Plus, right: _} = e {
                                   // handle binary expressions
                                   fmt_str.push_str("%d");
                               }else {
                                   panic!("Unsupported expression type in print: {:?}", e);
                               }
                           }
                       }
                       *fmt_count += 1;
                   }
               }
           }
           _ => {}
        }
    }

    fn collect_libs(out: &mut String ,stmt: &IRNode, libs: &mut Vec<String>) {
        match stmt{
            IRNode::If { cond: _, then_branch, else_branch } => {
                for inner in then_branch {
                    Self::collect_libs(out, inner, libs);
                }
                if let Some(else_branch) = else_branch {
                    for inner in else_branch {
                        Self::collect_libs(out, inner, libs);
                    }
                }
            }
            IRNode::While { cond: _, body } => {
                for inner_stmt in body {
                    Self::collect_libs(out, inner_stmt, libs);
                }
            }
            IRNode::Print {..} => {
                if !libs.contains(&"msvcrt.lib".to_string()) {
                    out.push_str("extrn printf:PROC\n");
                    out.push_str("includelib lib\\msvcrt.lib\n\n");
                    libs.push("msvcrt.lib".to_string());
                }
            }
            _ => {}
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
                for a in args {
                    if let PrintArg::Literal(Literal::String(s)) = a {
                        Self::intern_literal(pool, s);
                    }
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

    pub fn translate(
        nodes: &[IRNode],
        functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
        backend: Backend) -> (String, Vec<String>) {
        match backend {
            Backend::JavaScript => todo!(),
            Backend::MASM => {
                let mut libraries = Vec::new();
                let mut out = String::new();
                let mut lit_table: HashMap<String, String> = HashMap::new();

                // MASM boilerplate
                out.push_str(".386\n.model flat, c\n");
                out.push_str("option casemap:none\n\n");

                // declare all external functions
                nodes.iter().for_each(|n| {
                    if let IRNode::Function { name: _, params: _, return_type: _, body} = n {
                        for stmt in body {
                            Self::collect_libs(
                                &mut out, stmt, &mut libraries
                            )
                        }
                    }
                });

                // collect variables
                let mut var_decls = Vec::new();
                for n in nodes {
                    if let IRNode::Function { name: func_name, params, return_type: _, body } = n {
                        for stmt in body {
                            if let IRNode::VarDecl(decls) = stmt {
                                for (name, value) in decls {
                                    // If the value is None, we assume it's 0
                                    let lit = value.as_ref().unwrap_or(&Literal::Int(0));
                                    var_decls.push((name.clone(), lit));
                                }
                            }
                        }

                        for (_name, _) in params {
                            let var_name = format!("{}_{}{VAR_NAME_EXTENSION}", func_name, _name); // append _var to the variable name
                            var_decls.push((var_name.clone(), &Literal::Int(0))); // default to 0
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
                            Self::collect_declarations(stmt, &var_decls, &mut fmt_map, &mut fmt_count);
                        }
                    }
                }

                // Add format strings to data section
                for (lbl, fmt) in &fmt_map {
                    out.push_str(&format!("{} db {},0\n", lbl, fmt));
                }

                for n in nodes {
                    if let IRNode::Function { body, .. } = n {
                        for stmt in body {
                            Self::collect_literals(stmt, &mut lit_table);
                        }
                    }
                }

                for (src,lbl) in lit_table.iter().map(|(s,l)| (s,l)).collect::<Vec<_>>() {
                    out.push_str(&format!("{} db {},0\n", lbl, IRTranslator::masm_encode_string(src)));
                }

                // code section
                out.push_str("\n.code\n");

                let mut fmt_index = 0;
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
                            Self::emit_stmt(&mut out, stmt, &var_decls, &mut fmt_map, &mut fmt_index, &mut lbl_counter, &func_tail, functions, &mut lit_table);
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

