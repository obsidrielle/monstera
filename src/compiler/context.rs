use crate::checker::Substitution;
use crate::compiler::{LoopContext, Variable};
use inkwell::values::{FunctionValue, InstructionValue, PointerValue};
use std::collections::HashMap;

pub(crate) struct SemanticContext<'ctx> {
    symbols: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    loop_context: Vec<LoopContext<'ctx>>,
    substitution: Substitution,
}

impl<'ctx> SemanticContext<'ctx> {
    pub fn new(substitution: Substitution) -> Self {
        Self {
            symbols: HashMap::new(),
            functions: HashMap::new(),
            current_function: None,
            loop_context: Vec::new(),
            substitution,
        }
    }

    pub fn current_function(&self) -> Option<FunctionValue<'ctx>> {
        self.current_function
    }

    pub fn variable_addr(&self, name: &str) -> PointerValue<'ctx> {
        self.symbols[name].addr
    }

    pub fn variable(&self, name: &str) -> Variable<'ctx> {
        self.symbols[name]
    }
}