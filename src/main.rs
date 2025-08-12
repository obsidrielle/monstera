use miette::{Diagnostic, Report};
use pest::Parser;
use crate::checker::TypeChecker;
use crate::error::build_diagnostic;
use crate::parser::ast::parse_program;
use crate::parser::Rule;

mod parser;
mod compiler;
mod executor;
mod checker;
mod error;



fn main() -> miette::Result<()> {
    let source_code = r#"
            fn add(a: i64, b: i64) -> i64 {
                return a + b;
            }

            fn main() -> i32 {
                let t = add(1, 2, 3);
                return t;
            }
        "#;
    let mut result = crate::parser::MonsteraParser::parse(Rule::program, source_code).unwrap();

    let mut checker = TypeChecker::new(source_code.to_string());
    let mut program = parse_program(result.next().unwrap());

    checker.infer_program(&mut program)
        .map_err(|err| build_diagnostic(err, source_code))?;

    let mut substitution = checker.solve_bounds();
    substitution.print_all_substitution();
    /*let context = inkwell::context::Context::create();
    let mut compiler = Compiler::new(&context, substitution);
    compiler.compile_program(program);

    Executor::exec_in_memory(&compiler).unwrap();*/
    Ok(())
}
