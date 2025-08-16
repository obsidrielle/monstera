use pest::iterators::Pair;
use crate::parser::ast::{parse_expression, parse_spanned_t, Expression, Spanned};
use crate::parser::Rule;

#[derive(Debug, Clone)]
pub enum Array {
    List(Spanned<ListExpression>),
    Repeat(Spanned<RepeatExpression>),
}

pub fn parse_array(pair: Pair<Rule>) -> Spanned<Array> {
    let span = pair.as_span().into();
    let mut pairs = pair.into_inner();

    let pair = next_pair!(pairs);
    let array = match pair.as_rule() {
        Rule::list_expression => Array::List(parse_list_expression(pair)),
        Rule::repeat_expression => Array::Repeat(parse_repeat_expression(pair)),
        _ => unreachable!(),
    };
    Spanned::new(array, span)
}

#[derive(Debug, Clone)]
pub struct ListExpression {
    pub expressions: Vec<Spanned<Expression>>,
}

pub fn parse_list_expression(pair: Pair<Rule>) -> Spanned<ListExpression> {
    let span = pair.as_span().into();
    let expressions = pair
        .into_inner()
        .map(|pair| parse_expression(pair))
        .collect();
    Spanned::new(ListExpression { expressions }, span)
}

#[derive(Debug, Clone)]
pub struct RepeatExpression {
    pub expression: Spanned<Expression>,
    pub length: Spanned<u64>,
}

pub fn parse_repeat_expression(pair: Pair<Rule>) -> Spanned<RepeatExpression> {
    let span = pair.as_span().into();
    let mut pairs = pair.into_inner();
    Spanned::new(RepeatExpression {
        expression: parse_expression(next_pair!(pairs)),
        length: parse_spanned_t(next_pair!(pairs)),
    }, span)
}