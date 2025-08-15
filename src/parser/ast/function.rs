use crate::parser::Rule;
use crate::parser::ast::block::{Block, parse_block};
use crate::parser::ast::types::{MaybeNull, parse_maybe_null};
use crate::parser::ast::{Spanned, parse_spanned_t};
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub(crate) struct Param {
    pub(crate) name: Spanned<String>,
    pub(crate) typ: Spanned<MaybeNull>,
}

pub(crate) fn parse_param(pair: Pair<Rule>) -> Spanned<Param> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    Spanned::new(
        Param {
            name: parse_spanned_t(next_pair!(pairs)),
            typ: parse_maybe_null(next_pair!(pairs)),
        },
        span.into(),
    )
}

#[derive(Debug, Clone)]
pub(crate) struct ParamList {
    pub(crate) params: Vec<Spanned<Param>>,
}

pub(crate) fn parse_param_list(pair: Pair<Rule>) -> Spanned<ParamList> {
    let span = pair.as_span();
    let params = pair.into_inner().map(|pair| parse_param(pair)).collect();
    Spanned::new(ParamList { params }, span.into())
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) name: Spanned<String>,
    pub(crate) param_list: Spanned<ParamList>,
    pub(crate) return_type: Spanned<MaybeNull>,
    pub(crate) block: Spanned<Block>,
}

impl Function {
    pub fn get_params(&self) -> &Vec<Spanned<Param>> {
        &self.param_list.params
    }
}

pub(crate) fn parse_function(pair: Pair<Rule>) -> Spanned<Function> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    let name = parse_spanned_t(next_pair!(pairs));
    let param_list = parse_param_list(next_pair!(pairs));
    let return_type = parse_maybe_null(next_pair!(pairs));
    let block = parse_block(next_pair!(pairs));

    Spanned::new(
        Function {
            name,
            param_list,
            return_type,
            block,
        },
        span.into(),
    )
}
