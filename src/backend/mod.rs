pub mod ir_translator;
mod masm32;

pub use ir_translator::{IRTranslator, Backend};
pub use masm32::{generate};