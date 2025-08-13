use inkwell::builder::Builder;
use inkwell::module::Module;

struct LLVMBackend<'ctx> {
    context: &'ctx inkwell::context::Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
}
