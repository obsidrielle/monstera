use crate::parser::Rule;
use pest::iterators::Pair;
use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterOrEqual,
    LessOrEqual,
    And,
    Or,
}

impl BinaryOp {
    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Add | Self::Subtract | Self::Multiply | Self::Divide)
    }

    pub fn is_compare(&self) -> bool {
        matches!(self, Self::Equal | Self::NotEqual |
            Self::Greater | Self::GreaterOrEqual |
            Self::Less | Self::LessOrEqual |
            Self::And | Self::Or)
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
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
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

pub(crate) fn parse_binary_op(pair: Pair<Rule>) -> BinaryOp {
    match pair.as_rule() {
        Rule::add => BinaryOp::Add,
        Rule::subtract => BinaryOp::Subtract,
        Rule::multiply => BinaryOp::Multiply,
        Rule::divide => BinaryOp::Divide,
        Rule::equal => BinaryOp::Equal,
        Rule::not_equal => BinaryOp::NotEqual,
        Rule::greater => BinaryOp::Greater,
        Rule::greater_or_equal => BinaryOp::GreaterOrEqual,
        Rule::less => BinaryOp::Less,
        Rule::less_or_equal => BinaryOp::LessOrEqual,
        Rule::and => BinaryOp::And,
        Rule::or => BinaryOp::Or,
        _ => todo!(),
    }
}
