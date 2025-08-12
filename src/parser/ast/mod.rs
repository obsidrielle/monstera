use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::str::FromStr;
use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;
use pest::iterators::Pair;
use pest::pratt_parser::PrattParserMap;
use strum_macros::{Display, EnumString};
use crate::checker::alloc_type_idx;
use crate::parser::{pratt_parser, Rule, PRATT_PARSER};

macro_rules! next_pair {
    ($e:ident) => {
        $e.next().unwrap()
    };
}

#[derive(Debug)]
pub(crate) struct Program {
    pub(crate) functions: Vec<Function>,
}

pub(crate) fn parse_program(pair: Pair<Rule>) -> Program {
    let functions = pair.into_inner()
        .filter(|pair| pair.as_rule() != Rule::EOI)
        .map(|pair| parse_function(pair))
        .collect();
    Program {
        functions,
    }
}

#[derive(Debug)]
pub(crate) struct Param {
    pub(crate) name: String,
    pub(crate) typ: MaybeNull,
}

pub(crate) fn parse_param(pair: Pair<Rule>) -> Param {
    let mut pairs = pair.into_inner();
    Param {
        name: next_pair!(pairs).as_str().to_string(),
        typ: parse_maybe_null(next_pair!(pairs)),
    }
}

#[derive(Debug)]
pub(crate) struct ParamList {
    pub(crate) params: Vec<Param>,
}

pub(crate) fn parse_param_list(pair: Pair<Rule>) -> ParamList {
    let params = pair.into_inner()
        .map(|pair| parse_param(pair))
        .collect();
    ParamList {
        params,
    }
}

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) name: String,
    pub(crate) param_list: ParamList,
    pub(crate) return_type: MaybeNull,
    pub(crate) block: Block,
}

pub(crate) fn parse_function(pair: Pair<Rule>) -> Function {
    let mut pairs = pair.into_inner();

    let name = next_pair!(pairs).as_str().into();
    let param_list = parse_param_list(next_pair!(pairs));
    let return_type = parse_maybe_null(next_pair!(pairs));
    let block = parse_block(next_pair!(pairs));

    Function {
        name,
        param_list,
        return_type,
        block,
    }
}

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

#[derive(Debug)]
pub(crate) enum Expression {
    NumberLiteral {
        value: i64,
        typ: MaybeNull,
    },
    BooleanLiteral(bool),
    Variable {
        name: String,
        typ: MaybeNull,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    }
}

pub(crate) fn parse_expression(pair: Pair<Rule>) -> Expression {
    let pairs = pair.into_inner();
    pratt_parser()
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Expression::NumberLiteral {
                value: primary.as_str().parse::<i64>().unwrap(),
                typ: MaybeNull::alloc_unknown(),
            },
            Rule::boolean => Expression::BooleanLiteral(primary.as_str().parse::<bool>().unwrap()),
            Rule::identifier => Expression::Variable {
                name: primary.as_str().to_string(),
                typ: MaybeNull::unknown(alloc_type_idx()),
            },
            Rule::expression => parse_expression(primary),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| {
            Expression::BinaryOp {
                op: parse_binary_op(op),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
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

#[derive(Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    Assign(AssignStatement),
    Return(ReturnStatement),
}

pub(crate) fn parse_statement(pair: Pair<Rule>) -> Statement {
    let mut pairs = pair.into_inner();

    let pair = pairs.next().unwrap();

    match pair.as_rule() {
        Rule::assign_statement => Statement::Assign(parse_assign_statement(pair)),
        Rule::return_statement => Statement::Return(parse_return_statement(pair)),
        Rule::expression => Statement::Expression(parse_expression(pair)),
        _ => unreachable!(),
    }
}

#[derive(Debug)]
pub(crate) struct AssignStatement {
    pub(crate) identifier: String,
    pub(crate) typ: MaybeNull,
    pub(crate) expression: Expression,
}

pub(crate) fn parse_assign_statement(pair: Pair<Rule>) -> AssignStatement {
    let mut pairs = pair.into_inner();
    AssignStatement {
        identifier: pairs.next().unwrap().as_str().into(),
        typ: MaybeNull::alloc_unknown(),
        expression: parse_expression(pairs.next().unwrap()),
    }
}

#[derive(Debug)]
pub(crate) struct ReturnStatement {
    pub(crate) expression: Expression,
    pub(crate) typ: MaybeNull,
}

pub(crate) fn parse_return_statement(pair: Pair<Rule>) -> ReturnStatement {
    let mut pairs = pair.into_inner();
    ReturnStatement {
        expression: parse_expression(next_pair!(pairs)),
        typ: MaybeNull::alloc_unknown(),
    }
}

#[derive(Debug)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Statement>,
}

pub(crate) fn parse_block(pair: Pair<Rule>) -> Block {
    let statements = pair.into_inner()
        .map(|pair| parse_statement(pair))
        .collect();
    Block {
        statements,
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) enum DType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Void,
    Unknown(u64),
}

impl DType {
    pub(crate) fn is_number(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 |
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => true,
            _ => false,
        }
    }

    pub(crate) fn is_primitive(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 |
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::Void => true,
            _ => false,
        }
    }

    pub(crate) fn is_unknown(&self) -> bool {
        match self {
            Self::Unknown(_) => true,
            _ => false,
        }
    }
}

impl Display for DType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Unknown(_) => write!(f, "<unknown>"),
        }
    }
}

impl FromStr for DType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(DType::Bool),
            "i8" => Ok(DType::I8),
            "i16" => Ok(DType::I16),
            "i32" => Ok(DType::I32),
            "i64" => Ok(DType::I64),
            "u8" => Ok(DType::U8),
            "u16" => Ok(DType::U16),
            "u32" => Ok(DType::U32),
            "u64" => Ok(DType::U64),
            "void" => Ok(DType::Void),
            _ => Err(format!("Unknown dtype {}", s)),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct MaybeNull {
    pub typ: DType,
    pub maybe_null: bool,
}

impl Deref for MaybeNull {
    type Target = DType;
    fn deref(&self) -> &Self::Target {
        &self.typ
    }
}

impl Display for MaybeNull {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", if self.maybe_null { "?" } else { "" }, self.typ)
    }
}

impl MaybeNull {
    pub(crate) fn maybe_null(typ: DType) -> Self {
        Self {
            typ,
            maybe_null: true,
        }
    }

    pub(crate) fn nonnull(typ: DType) -> Self {
        Self {
            typ,
            maybe_null: false,
        }
    }

    pub(crate) fn unknown(idx: u64) -> Self {
        Self {
            typ: DType::Unknown(idx),
            maybe_null: false,
        }
    }

    pub(crate) fn is_maybe_null(&self) -> bool {
        self.maybe_null
    }

    pub(crate) fn alloc_unknown() -> Self {
        Self::unknown(alloc_type_idx())
    }

    pub(crate) fn type_idx(&self) -> u64 {
        match self.typ {
            DType::Unknown(idx) => idx,
            _ => 0,
        }
    }
}

impl Into<DType> for MaybeNull {
    fn into(self) -> DType {
        self.typ
    }
}

pub(crate) fn parse_maybe_null(pair: Pair<Rule>) -> MaybeNull {
    let mut pairs = pair.into_inner();
    let pair_str = next_pair!(pairs).as_str();

    pair_str
        .strip_prefix("?")
        .and_then(|s| Some(MaybeNull::maybe_null(s.parse().unwrap())))
        .unwrap_or_else(|| MaybeNull::nonnull(pair_str.parse().unwrap()))
}

impl Default for DType {
    fn default() -> Self {
        Self::I32
    }
}