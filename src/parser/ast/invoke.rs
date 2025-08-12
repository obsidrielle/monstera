use pest::iterators::Pair;
use crate::parser::ast::{parse_spanned_t, Spanned};
use crate::parser::ast::expr::{parse_expression, Expression};
use crate::parser::Rule;

#[derive(Debug, Clone)]
pub(crate) struct Invoke {
    pub identifier: Spanned<String>,
    pub param_list: Spanned<InvokeParamList>,
}

pub(crate) fn parse_invoke(pair: Pair<Rule>) -> Spanned<Invoke> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    Spanned::new(Invoke {
        identifier: parse_spanned_t(next_pair!(pairs)),
        param_list: parse_invoke_param_list(next_pair!(pairs)),
    }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct InvokeParamList {
    pub expressions: Vec<Spanned<Expression>>,
}

fn parse_invoke_param_list(pair: Pair<Rule>) -> Spanned<InvokeParamList> {
    let span = pair.as_span();
    let expressions = pair.into_inner()
        .map(|pair| parse_expression(pair))
        .collect();
    Spanned::new(InvokeParamList {
        expressions,
    }, span.into())
}