use crate::checker::Substitution;
use crate::compiler::{LoopContext, Variable};
use inkwell::values::FunctionValue;
use std::collections::HashMap;

struct SemanticContext<'ctx> {
    symbols: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    loop_context: Vec<LoopContext<'ctx>>,
    substitution: Substitution,
}
