use super::{ast::{Expr, Literal, PrintArg, Type}};
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