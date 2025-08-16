use pest::pratt_parser::Assoc::Left;
use pest::pratt_parser::{Op, PrattParser};
use std::sync::OnceLock;

pub(crate) mod ast;
mod ir;

#[derive(pest_derive::Parser)]
#[grammar = "monstera.pest"]
pub(crate) struct MonsteraParser;

static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();

pub(crate) fn pratt_parser() -> &'static PrattParser<Rule> {
    PRATT_PARSER.get_or_init(|| {
        PrattParser::new()
            .op(Op::infix(Rule::or, Left))
            .op(Op::infix(Rule::and, Left))
            .op(Op::infix(Rule::equal, Left)
                | Op::infix(Rule::not_equal, Left)
                | Op::infix(Rule::greater, Left)
                | Op::infix(Rule::greater_or_equal, Left)
                | Op::infix(Rule::less, Left)
                | Op::infix(Rule::less_or_equal, Left))
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::subtract, Left))
            .op(Op::infix(Rule::multiply, Left) | Op::infix(Rule::divide, Left))
    })
}
