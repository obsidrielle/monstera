use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, InstructionValue};

pub(crate) struct LLVMBackend<'ctx> {
    pub context: &'ctx inkwell::context::Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
}

impl<'ctx> LLVMBackend<'ctx> {
    pub fn new(context: &'ctx inkwell::context::Context) -> Self {
        let module = context.create_module("main");
        Self {
            context,
            builder: context.create_builder(),
            module
        }
    }

    pub fn current_basic_block(&self) -> BasicBlock<'ctx> {
        self.builder.get_insert_block().unwrap()
    }

    pub fn terminator(&self) -> Option<InstructionValue<'ctx>> {
        self
            .current_basic_block()
            .get_terminator()
    }
}