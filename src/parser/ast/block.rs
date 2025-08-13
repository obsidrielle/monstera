use pest::iterators::Pair;
use crate::parser::ast::{parse_expression, Either, Expression, Span, Spanned};
use crate::parser::ast::statement::{parse_statement, Statement};
use crate::parser::Rule;

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Either<Spanned<Statement>, Spanned<Block>>>,
}

pub(crate) fn parse_block(pair: Pair<Rule>) -> Spanned<Block> {
    let span = pair.as_span();
    let statements = pair.into_inner()
        .map(|pair| {
            match pair.as_rule() {
                Rule::statement => Either::Left(parse_statement(pair)),
                Rule::block => Either::Right(parse_block(pair)),
                _ => unreachable!(),
            }
        })
        .collect();
    Spanned::new(Block {
        statements,
    }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct IfBlock {
    pub(crate) condition: Spanned<Expression>,
    pub(crate) block: Spanned<Block>,
}

pub(crate) fn parse_if_block(pair: Pair<Rule>) -> Spanned<IfBlock> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    
    Spanned::new(IfBlock {
        condition: parse_expression(next_pair!(pairs)),
        block: parse_block(next_pair!(pairs)),
    }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct ElseIfBlock {
    pub(crate) condition: Spanned<Expression>,
    pub(crate) block: Spanned<Block>,
}

pub(crate) fn parse_else_if_block(pair: Pair<Rule>) -> Spanned<ElseIfBlock> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    Spanned::new(ElseIfBlock {
        condition: parse_expression(next_pair!(pairs)),
        block: parse_block(next_pair!(pairs)),
    }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct ElseBlock {
    pub(crate) block: Spanned<Block>,
}

pub(crate) fn parse_else_block(pair: Pair<Rule>) -> Spanned<ElseBlock> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    
    Spanned::new(ElseBlock {
        block: parse_block(next_pair!(pairs)),
    }, span.into())
}