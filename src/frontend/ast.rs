use super::token::Token;

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),           //  5 or 3.14 or "abc"
    Variable(String),           //  x
    Binary {                    //  x + 4 or 7 + y or a + b
        left: Box<Expr>,
        op: Token,              // Token::Plus, Token::Minus, etc.
        right: Box<Expr>,
    },
    Unary {
        op: Token,              // Token::Star (pointers)
        expr: Box<Expr>,        // the expression to apply the unary operator to
    },
    Call {
        name: String,           // function name
        args: Vec<Expr>,        // arguments to the function
    }
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
