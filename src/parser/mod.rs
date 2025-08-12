use std::sync::OnceLock;
use pest::pratt_parser::{Op, PrattParser};
use pest::pratt_parser::Assoc::Left;

pub(crate) mod ast;
mod ir;

#[derive(pest_derive::Parser)]
#[grammar = "monstera.pest"]
pub(crate) struct MonsteraParser;

static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();

pub(crate) fn pratt_parser<'a>() -> &'a PrattParser<Rule> {
    PRATT_PARSER
        .get_or_init(|| {
            PrattParser::new()
                .op(Op::infix(Rule::add, Left) | Op::infix(Rule::subtract, Left))
                .op(Op::infix(Rule::multiply, Left) | Op::infix(Rule::divide, Left))
        })
}

#[cfg(test)]
mod tests {
    use pest::Parser;
    use crate::checker::TypeChecker;
    use crate::parser::{MonsteraParser, Rule};
    use crate::parser::ast::parse_program;
    use crate::compiler::Compiler;
    use crate::error::CompileError;
    use crate::executor::Executor;

    #[test]
    fn test_basic() -> Result<(), CompileError> {
        let source_code = r#"
            fn add(a: i32, b: i32) -> i64 {
                return a + b;
            }

            fn main() -> i32 {
                let i = 5;
                let x = 2 * i;
                let z = x / i;
                let p = add(x, z);
                return z + x;
            }
        "#;
        let mut result = MonsteraParser::parse(Rule::program, source_code).unwrap();

        let mut checker = TypeChecker::new(source_code.to_string());
        let mut program = parse_program(result.next().unwrap());

        if let Err(e) = checker.infer_program(&mut program) {
            println!("{:?}", e);
        }

        let mut substitution = checker.solve_bounds();
        substitution.print_all_substitution();
        /*let context = inkwell::context::Context::create();
        let mut compiler = Compiler::new(&context, substitution);
        compiler.compile_program(program);

        Executor::exec_in_memory(&compiler).unwrap();*/
        Ok(())
    }
}