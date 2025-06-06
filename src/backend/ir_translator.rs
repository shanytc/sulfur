use std::collections::{HashMap};
use crate::backend::generate;
use crate::frontend::{IRNode, Type};

pub enum Backend {
    JavaScript,
    MASM32,
}

pub struct IRTranslator;

impl IRTranslator {
    pub fn translate(
        nodes: &[IRNode],
        functions: &HashMap<String, (Vec<(String, Type)>, Type)>,
        backend: Backend) -> (String, Vec<String>) {
        match backend {
            Backend::JavaScript => todo!(),
            Backend::MASM32 => generate(nodes, functions),
        }
    }
}