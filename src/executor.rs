use crate::compiler::Compiler;
use inkwell::OptimizationLevel;
use inkwell::execution_engine::JitFunction;

type MainFunc = unsafe extern "C" fn() -> i32;

pub(crate) struct Executor;

impl Executor {
    pub(crate) fn exec_in_memory(compiler: &Compiler) -> anyhow::Result<()> {
        let engine = compiler
            .module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        compiler.module.print_to_stderr();
        unsafe {
            let main_jit_fn: JitFunction<MainFunc> = engine.get_function("main")?;
            println!("exit code: {}", main_jit_fn.call());
        }

        Ok(())
    }
}
