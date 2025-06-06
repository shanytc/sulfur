use std::collections::HashMap;
use crate::shared::VAR_NAME_EXTENSION;
use super::{
    token::Token,
    lexer::Lexer,
    ast::{Expr, PrintArg, Literal, Type},
    ir::IRNode
};

#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    constants: HashMap<String, Literal>,
    pub functions: HashMap<String, (Vec<(String, Type)>, Type)>, // function name -> (params, return type)
    current_func: Option<String>, // the current function being parsed
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
                    params.push((param_name, Type::Int)); // default to an Int type for simplicity

                    if self.cur_token == Token::Comma {
                        self.advance(); // consume the comma
                        continue;
                    }
                    break;
                }
            }

            // eat the ')' character.
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

        self.current_func = prev_func; // restore the previous function name

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
                    if let Token::Identifier(func_name) = &self.cur_token {
                        let name = func_name.clone();
                        if self.lexer.input.get(self.lexer.pos).cloned() == Some('(') {
                            // function call used as a statement
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
                // peek and check if the next token is LParen '(', parse a 'call'
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