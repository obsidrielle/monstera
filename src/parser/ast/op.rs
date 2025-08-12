use std::fmt::{Display, Formatter};
use pest::iterators::Pair;
use crate::parser::Rule;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum BinaryOp {
    Add, Subtract, Multiply, Divide,
    Equal, NotEqual, Greater, Less, GreaterOrEqual,
    LessOrEqual,
    And, Or,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl AsRef<str> for BinaryOp {
    fn as_ref(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Equal => "==",
            Self::NotEqual => "!=",
            Self::Greater => ">",
            Self::Less => "<",
            Self::GreaterOrEqual => ">=",
            Self::LessOrEqual => "<=",
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

pub(crate) fn parse_binary_op(pair: Pair<Rule>) -> BinaryOp {
    match pair.as_rule() {
        Rule::add => BinaryOp::Add,
        Rule::subtract => BinaryOp::Subtract,
        Rule::multiply => BinaryOp::Multiply,
        Rule::divide => BinaryOp::Divide,
        _ => todo!(),
    }
}