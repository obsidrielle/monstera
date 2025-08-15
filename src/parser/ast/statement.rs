use crate::parser::Rule;
use crate::parser::ast::expr::{Expression, parse_expression};
use crate::parser::ast::types::MaybeNull;
use crate::parser::ast::{Block, ElseBlock, ElseIfBlock, IfBlock, Spanned, parse_block, parse_else_block, parse_else_if_block, parse_if_block, parse_spanned_t, TypeKind};
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Expression(Spanned<Expression>),
    Assign(Spanned<AssignStatement>),
    If(Spanned<IfStatement>),
    Loop(Spanned<LoopStatement>),
    Control(Spanned<ControlStatement>),
    Return(Spanned<ReturnStatement>),
}

pub(crate) fn parse_statement(pair: Pair<Rule>) -> Spanned<Statement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();

    println!("{:#?}", pair.as_rule());

    Spanned::new(
        match pair.as_rule() {
            Rule::assign_statement => Statement::Assign(parse_assign_statement(pair)),
            Rule::return_statement => Statement::Return(parse_return_statement(pair)),
            Rule::expression => Statement::Expression(parse_expression(pair)),
            Rule::if_statement => Statement::If(parse_if_statement(pair)),
            Rule::loop_statement => Statement::Loop(parse_loop_statement(pair)),
            Rule::control_statement => Statement::Control(parse_control_statement(pair)),
            _ => unreachable!(),
        },
        span.into(),
    )
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
    Spanned::new(
        AssignStatement {
            identifier: parse_spanned_t(next_pair!(pairs)),
            typ: MaybeNull::alloc_unknown(TypeKind::Int),
            expression: parse_expression(next_pair!(pairs)),
        },
        span.into(),
    )
}

#[derive(Debug, Clone)]
pub(crate) struct ReturnStatement {
    pub(crate) expression: Spanned<Expression>,
    pub(crate) typ: MaybeNull,
}

pub(crate) fn parse_return_statement(pair: Pair<Rule>) -> Spanned<ReturnStatement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    Spanned::new(
        ReturnStatement {
            expression: parse_expression(next_pair!(pairs)),
            typ: MaybeNull::alloc_unknown(TypeKind::Int),
        },
        span.into(),
    )
}

#[derive(Debug, Clone)]
pub(crate) struct IfStatement {
    pub(crate) if_block: Spanned<IfBlock>,
    pub(crate) else_if_block: Vec<Spanned<ElseIfBlock>>,
    pub(crate) else_block: Option<Spanned<ElseBlock>>,
}

pub(crate) fn parse_if_statement(pair: Pair<Rule>) -> Spanned<IfStatement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    let if_block = parse_if_block(next_pair!(pairs));
    let mut else_if_block = Vec::new();
    let mut else_block = None;

    while let Some(pair) = pairs.next() {
        match pair.as_rule() {
            Rule::else_if_block => else_if_block.push(parse_else_if_block(pair)),
            Rule::else_block => else_block = Some(parse_else_block(pair)),
            _ => unreachable!(),
        }
    }

    Spanned::new(
        IfStatement {
            if_block,
            else_if_block,
            else_block,
        },
        span.into(),
    )
}

#[derive(Debug, Clone)]
pub(crate) struct LoopStatement {
    pub(crate) block: Spanned<Block>,
}

pub(crate) fn parse_loop_statement(pair: Pair<Rule>) -> Spanned<LoopStatement> {
    let span = pair.as_span();
    Spanned::new(
        LoopStatement {
            block: parse_block(pair),
        },
        span.into(),
    )
}

#[derive(Debug, Clone)]
pub(crate) enum ControlStatement {
    Break,
    Continue,
}

pub(crate) fn parse_control_statement(pair: Pair<Rule>) -> Spanned<ControlStatement> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();

    let control = match next_pair!(pairs).as_rule() {
        Rule::control_break => ControlStatement::Break,
        Rule::control_continue => ControlStatement::Continue,
        _ => unreachable!(),
    };
    Spanned::new(control, span.into())
}
