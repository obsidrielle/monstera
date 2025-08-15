use miette::Report;
use crate::checker::TypeChecker;
use crate::compiler::Compiler;
use crate::error::build_diagnostic;
use crate::executor::Executor;
use crate::parser::Rule;
use crate::parser::ast::parse_program;
use pest::Parser;

mod checker;
mod compiler;
mod error;
mod executor;
mod parser;

fn main() -> miette::Result<()> {
    let source_code = r#"
            fn add(a: i64, b: i64) -> i64 {
                return a + b;
            }

            fn main() -> i32 {
                loop {
                    let t = 1;
                    if t == 1 {
                        break;
                    }
                }
                return 0;
            }
        "#;
    let mut result = crate::parser::MonsteraParser::parse(Rule::program, source_code).unwrap();

    let mut checker = TypeChecker::new();
    let program = parse_program(result.next().unwrap());

    println!("{:?}", program);
    checker
        .infer_program(&program)
        .map_err(|err| build_diagnostic(err, source_code))?;

    let substitution = checker.solve_bounds();
    let substitution = match substitution {
        Ok(x) => x,
        Err(e) => {
            for error in e {
                eprintln!("{:?}", Report::new(build_diagnostic(error, source_code)));
            }
            return Ok(());
        }
    };
    println!("{:#?}", substitution);

    let context = inkwell::context::Context::create();
    let mut compiler = Compiler::new(&context, substitution);
    compiler.compile_program(&program);

    Executor::exec_in_memory(&compiler).unwrap();
    Ok(())
}
