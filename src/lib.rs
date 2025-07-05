//! Public entry-points for use by the CLI **and** the test-harness.

pub mod frontend;      // Lexer / Parser / IR definitions (exact code you already have)
pub mod backend;
mod shared;

use frontend::{Lexer, Parser};
use backend::{ir_translator::IRTranslator as IR, Backend};

/// Parse Sulfur source and return `(masm_source, libraries)`
pub fn compile_to_masm(src: &str) -> (String, Vec<String>) {
    let lexer  = Lexer::new(src);
    let mut p  = Parser::new(lexer);
    let ir     = p.parse_program(); // build IR
    IR::translate(&ir, &p.functions, &p.ptr_vars, Backend::MASM32)
}