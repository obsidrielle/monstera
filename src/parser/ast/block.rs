use pest::iterators::Pair;
use crate::parser::ast::Spanned;
use crate::parser::ast::statement::{parse_statement, Statement};
use crate::parser::Rule;

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Spanned<Statement>>,
}

pub(crate) fn parse_block(pair: Pair<Rule>) -> Spanned<Block> {
    let span = pair.as_span();
    let statements = pair.into_inner()
        .map(|pair| parse_statement(pair))
        .collect();
    Spanned::new(Block {
        statements,
    }, span.into())
}
