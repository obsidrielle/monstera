mod graph;
mod solver;
pub use solver::*;

use crate::error::SemanticError;
use crate::parser::ast::{BinaryOp, Block, DType, Expression, Function, IfStatement, Invoke, MaybeNull, Program, Span, Spanned, Statement, TypeKind};
use either::Either;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};

static TYPE_COUNTER: AtomicU64 = AtomicU64::new(1);

pub fn alloc_type_idx() -> u64 {
    TYPE_COUNTER.fetch_add(1, Ordering::Relaxed)
}

macro_rules! both_known {
    ($a:expr, $b:expr) => {
        !$a.is_unknown() && !$b.is_unknown()
    };
}

macro_rules! both_unknown {
    ($a:expr, $b:expr) => {
        $a.is_unknown() && $b.is_unknown()
    };
}

pub(crate) struct TypeChecker {
    symbols: HashMap<String, Spanned<MaybeNull>>,
    return_type: Spanned<MaybeNull>,
    bounds: BTreeSet<TypeBound>,
    functions: HashMap<String, Spanned<Function>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
enum TypeBound {
    Equals(Spanned<MaybeNull>, Spanned<MaybeNull>),
}

impl TypeChecker {
    pub(crate) fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            return_type: Spanned::new(MaybeNull::unknown(0, TypeKind::Int), Span::empty()),
            bounds: BTreeSet::new(),
            functions: HashMap::new(),
        }
    }

    fn get_variable_type(
        &self,
        name: &str,
        span: Span,
    ) -> Result<Spanned<MaybeNull>, SemanticError> {
        self.symbols
            .get(name)
            .copied()
            .ok_or(SemanticError::UndefinedVariable {
                name: Spanned::new(name.to_string(), span),
            })
    }

    fn get_function_define(
        &self,
        name: &str,
        span: Span,
    ) -> Result<&Spanned<Function>, SemanticError> {
        self.functions
            .get(name)
            .ok_or(SemanticError::UndefinedFunction {
                name: Spanned::new(name.to_string(), span),
            })
    }

    pub(crate) fn infer_program(&mut self, program: &Program) -> Result<(), SemanticError> {
        for function in &program.functions {
            self.infer_function(function)?;
        }
        Ok(())
    }

    fn infer_function(&mut self, function: &Spanned<Function>) -> Result<(), SemanticError> {
        if function.return_type.is_primitive() && function.return_type.is_maybe_null() {
            return Err(SemanticError::MaybeNullOfPrimitiveType {
                typ: Spanned::new(
                    MaybeNull::nonnull(function.return_type.typ),
                    function.return_type.span,
                ),
            });
        }

        self.return_type = function.return_type;
        self.functions
            .insert((*function.name).clone(), function.clone());

        for param in &function.param_list.params {
            self.symbols.insert((*param.name).clone(), param.typ);
        }

        self.infer_block(&function.block)?;
        self.symbols.clear();
        Ok(())
    }

    fn infer_block(&mut self, block: &Spanned<Block>) -> Result<(), SemanticError> {
        let old_symbols = self.symbols.clone();

        for either in &block.statements {
            match either {
                Either::Left(statement) => self.infer_statement(statement)?,
                Either::Right(block) => self.infer_block(block)?,
            }
        }
        self.symbols = old_symbols;
        Ok(())
    }

    fn infer_statement(&mut self, statement: &Spanned<Statement>) -> Result<(), SemanticError> {
        match &statement.node {
            Statement::Expression(expression) => {
                let _ = self.infer_expression(expression)?;
            }
            Statement::Return(Spanned { node, .. }) => match &*node.expression {
                Expression::NumberLiteral { value, typ } => {
                    let span = node.expression.span;
                    self.add_bound(
                        TypeBound::Equals(
                            Spanned::new(node.typ, span),
                            Spanned::new(*typ, span),
                        )
                    );
                    self.add_bound(
                        TypeBound::Equals(
                            Spanned::new(*typ, span),
                            self.return_type,
                        )
                    );
                }
                Expression::BooleanLiteral(_) => {
                    self.add_bound(
                        TypeBound::Equals(
                            Spanned::new(node.typ, node.expression.span),
                            Spanned::empty(MaybeNull::nonnull(DType::Bool))
                        )
                    );
                    self.add_bound(
                        TypeBound::Equals(
                            self.return_type,
                            Spanned::new(node.typ, node.expression.span)
                        )
                    );
                }
                Expression::Variable { name, typ } => {
                    let defined = self.get_variable_type(name.as_str(), name.span)?;
                    let span = node.expression.span();
                    self.add_bound(TypeBound::Equals(defined, Spanned::new(*typ, span)));
                    self.add_bound(TypeBound::Equals(defined, Spanned::new(node.typ, span)));
                    self.add_bound(
                        TypeBound::Equals(
                            defined,
                            self.return_type,
                        )
                    );
                }
                Expression::BinaryOp { .. } => {
                    let typ = self.infer_expression(&node.expression)?;
                    if !typ.is_unknown() && typ != self.return_type.node {
                        return Err(SemanticError::ReturnTypeMismatch {
                            expected: self.return_type,
                            found: Spanned::new(typ, node.expression.span),
                        });
                    }

                    let span = node.expression.span;
                    self.add_bound(
                        TypeBound::Equals(
                            Spanned::new(typ, span),
                            Spanned::new(node.typ, span),
                        )
                    );
                    self.add_bound(
                        TypeBound::Equals(
                            Spanned::new(typ, span),
                            self.return_type,
                        )
                    );
                }
                Expression::Invoke(invoke) => {
                    // we can ignore this returned value because we needn't it
                    let _ = self.infer_invoke_expression(invoke);
                }
                Expression::Array(array) => {}
            },
            Statement::Assign(assign) => {
                let typ = self.infer_expression(&assign.expression)?;

                // an assign statement may contain type annotation
                if both_known!(typ, assign.typ) && typ != *assign.typ {
                    return Err(SemanticError::TypeMismatch {
                        expected: assign.typ,
                        found: Spanned::new(typ, assign.expression.span),
                    })
                }

                self.add_bound(
                    TypeBound::Equals(
                        Spanned::new(*assign.typ, assign.span),
                        Spanned::new(typ, assign.expression.span),
                    )
                );

                let typ = if !assign.typ.is_unknown() { assign.typ } else { Spanned::empty(typ) };
                self.symbols.insert(
                    (*assign.identifier).clone(),
                    typ,
                );
            }
            Statement::If(stat) => {
                let IfStatement {
                    if_block,
                    else_if_block,
                    else_block,
                } = &**stat;

                self.infer_expression(&if_block.condition)?;
                self.infer_block(&if_block.block)?;

                for branch in else_if_block {
                    self.infer_expression(&branch.condition)?;
                    self.infer_block(&branch.block)?;
                }
                if let Some(block) = else_block {
                    self.infer_block(&block.block)?;
                }
            }
            Statement::Control(_) => {}
            Statement::Loop(stat) => self.infer_block(&stat.block)?,
            _ => unreachable!(),
        }
        Ok(())
    }

    fn infer_binary_op_expression(
        &mut self,
        op: &BinaryOp,
        lhs: &Spanned<Expression>,
        rhs: &Spanned<Expression>,
    ) -> Result<MaybeNull, SemanticError> {
        let lhs_typ = self.infer_expression(lhs)?;
        let rhs_typ = self.infer_expression(rhs)?;

        if both_known!(lhs_typ, rhs_typ) && lhs_typ != rhs_typ {
            return Err(SemanticError::InvalidBinaryOp {
                lhs_typ: Spanned::new(lhs_typ, lhs.span),
                rhs_typ: Spanned::new(rhs_typ, rhs.span),
            });
        }

        self.add_bound(
            TypeBound::Equals(
                Spanned::new(lhs_typ, lhs.span),
                Spanned::new(rhs_typ, rhs.span),
            )
        );

        if op.is_arithmetic() {
            return Ok(lhs_typ)
        }
        Ok(MaybeNull::nonnull(DType::Bool))
    }

    fn infer_invoke_expression(
        &mut self,
        invoke: &Spanned<Invoke>,
    ) -> Result<MaybeNull, SemanticError> {
        let name = &invoke.identifier;
        let function = self.get_function_define(name, invoke.identifier.span)?;
        let return_type = *function.return_type;

        let formal_params = &function.param_list.params;
        let actual_args = &invoke.param_list.expressions;

        if formal_params.len() != actual_args.len() {
            return Err(SemanticError::ArityMismatch {
                name: name.clone(),
                expected: Spanned::new(formal_params.len(), function.param_list.span),
                found: Spanned::new(actual_args.len(), invoke.param_list.span),
            });
        }

        let mut tasks = Vec::new();
        for (index, param) in function.param_list.params.iter().enumerate() {
            tasks.push((index, param.typ));
        }

        for (index, param_typ) in tasks {
            let invoke_expr = &invoke.param_list.expressions[index];
            let expr_typ = self.infer_expression(invoke_expr)?;
            self.add_bound(
                TypeBound::Equals(
                    Spanned::new(expr_typ, invoke_expr.span),
                    param_typ
                )
            );
        }

        Ok(return_type)
    }

    fn infer_expression(
        &mut self,
        expression: &Spanned<Expression>,
    ) -> Result<MaybeNull, SemanticError> {
        match &expression.node {
            Expression::NumberLiteral { typ, .. } => Ok(*typ),
            Expression::BooleanLiteral(_) => Ok(MaybeNull::nonnull(DType::Bool)),
            Expression::Variable { name, typ } => {
                let defined = self.get_variable_type(name, name.span)?;
                self.add_bound(
                    TypeBound::Equals(
                        Spanned::new(*typ, expression.span),
                        defined,
                    )
                );
                Ok(*defined)
            }
            Expression::BinaryOp { lhs, rhs, op } => self.infer_binary_op_expression(op, lhs, rhs),
            Expression::Invoke(invoke) => self.infer_invoke_expression(invoke),
            Expression::Array(array) => todo!(),
        }
    }

    fn add_bound(&mut self, bound: TypeBound) {
        self.bounds.insert(bound);
    }

    pub(crate) fn print_bound(&self) {
        println!("{:#?}", self.bounds)
    }

    pub fn solve_bounds(&self) -> Result<Substitution, Vec<SemanticError>> {
        let bounds = self.bounds
            .iter()
            .copied()
            .collect::<Vec<TypeBound>>();
        BoundSolver::new(&bounds).solve()
    }
}