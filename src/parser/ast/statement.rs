use pest::iterators::Pair;
use crate::parser::ast::{parse_spanned_t, Spanned};
use crate::parser::ast::expr::{parse_expression, Expression};
use crate::parser::ast::types::MaybeNull;
use crate::parser::Rule;

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Expression(Spanned<Expression>),
    Assign(Spanned<AssignStatement>),
    Return(Spanned<ReturnStatement>),
}

pub(crate) fn parse_statement(pair: Pair<Rule>) -> Spanned<Statement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    Spanned::new(match pair.as_rule() {
        Rule::assign_statement => Statement::Assign(parse_assign_statement(pair)),
        Rule::return_statement => Statement::Return(parse_return_statement(pair)),
        Rule::expression => Statement::Expression(parse_expression(pair)),
        _ => unreachable!(),
    }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct AssignStatement {
    pub(crate) identifier: Spanned<String>,
    pub(crate) typ: MaybeNull,
    pub(crate) expression: Spanned<Expression>,
}

pub(crate) fn parse_assign_statement(pair: Pair<Rule>) -> Spanned<AssignStatement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    Spanned::new(AssignStatement {
        identifier: parse_spanned_t(next_pair!(pairs)),
        typ: MaybeNull::alloc_unknown(),
        expression: parse_expression(next_pair!(pairs)),
    }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct ReturnStatement {
    pub(crate) expression: Spanned<Expression>,
    pub(crate) typ: MaybeNull,
}

pub(crate) fn parse_return_statement(pair: Pair<Rule>) -> Spanned<ReturnStatement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    Spanned::new(ReturnStatement {
        expression: parse_expression(next_pair!(pairs)),
        typ: MaybeNull::alloc_unknown(),
    }, span.into())
}
