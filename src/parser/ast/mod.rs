macro_rules! next_pair {
    ($e:ident) => {
        $e.next().unwrap()
    };
}

mod block;
mod expr;
mod function;
mod invoke;
mod op;
mod statement;
mod types;

pub(crate) use block::*;
pub(crate) use expr::*;
pub(crate) use function::*;
pub(crate) use invoke::*;
pub(crate) use op::*;
pub(crate) use statement::*;
pub(crate) use types::*;

use crate::parser::Rule;
use miette::SourceSpan;
use pest::iterators::Pair;
use std::fmt::{Debug, Display};
use std::ops::{Deref, DerefMut};
use std::str::FromStr;

pub(crate) fn parse_spanned_t<T: FromStr>(pair: Pair<Rule>) -> Spanned<T>
where
    <T as FromStr>::Err: Debug,
{
    let span = pair.as_span();
    Spanned::new(pair.as_str().parse::<T>().unwrap(), span.into())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub(crate) struct Span {
    pub start: usize,
    pub end: usize,
}

impl Into<miette::SourceSpan> for Span {
    fn into(self) -> SourceSpan {
        (self.start, self.end - self.start).into()
    }
}

impl Span {
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
    pub fn to(self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Default, Ord, PartialOrd)]
pub(crate) struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Spanned { node, span }
    }

    pub fn into_inner(self) -> T {
        self.node
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn empty(node: T) -> Self {
        Spanned {
            node,
            span: Span::empty(),
        }
    }
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(value: pest::Span<'a>) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Program {
    pub(crate) functions: Vec<Spanned<Function>>,
}

pub(crate) fn parse_program(pair: Pair<Rule>) -> Program {
    let functions = pair
        .into_inner()
        .filter(|pair| pair.as_rule() != Rule::EOI)
        .map(|pair| parse_function(pair))
        .collect();
    Program { functions }
}
