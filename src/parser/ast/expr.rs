use crate::parser::ast::invoke::{Invoke, parse_invoke};
use crate::parser::ast::op::{BinaryOp, parse_binary_op};
use crate::parser::ast::types::MaybeNull;
use crate::parser::ast::{Spanned, parse_spanned_t, TypeKind};
use crate::parser::{Rule, pratt_parser};
use pest::iterators::Pair;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    NumberLiteral {
        value: Spanned<i64>,
        typ: MaybeNull,
    },
    BooleanLiteral(Spanned<bool>),
    Variable {
        name: Spanned<String>,
        typ: MaybeNull,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Spanned<Expression>>,
        rhs: Box<Spanned<Expression>>,
    },
    Invoke(Spanned<Invoke>),
}

pub(crate) fn parse_expression(pair: Pair<Rule>) -> Spanned<Expression> {
    let pairs = pair.into_inner();
    pratt_parser()
        .map_primary(|primary| {
            let span = primary.as_span();
            match primary.as_rule() {
                Rule::number => Spanned::new(
                    Expression::NumberLiteral {
                        value: parse_spanned_t(primary),
                        typ: MaybeNull::alloc_unknown(TypeKind::Int),
                    },
                    span.into(),
                ),
                Rule::boolean => Spanned::new(
                    Expression::BooleanLiteral(parse_spanned_t(primary)),
                    span.into(),
                ),
                Rule::identifier => Spanned::new(
                    Expression::Variable {
                        name: parse_spanned_t(primary),
                        typ: MaybeNull::alloc_unknown(TypeKind::Int),
                    },
                    span.into(),
                ),
                Rule::expression => parse_expression(primary),
                Rule::invoke => {
                    Spanned::new(Expression::Invoke(parse_invoke(primary)), span.into())
                }
                _ => unreachable!(),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let span = lhs.span().to(rhs.span());
            Spanned::new(
                Expression::BinaryOp {
                    op: parse_binary_op(op),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
                span,
            )
        })
        .parse(pairs)
}
