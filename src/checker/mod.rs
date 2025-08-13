use std::cell::{OnceCell, RefCell};
use std::collections::{HashMap, HashSet};
use std::iter::zip;
use std::os::linux::raw::stat;
use std::sync::{Mutex, OnceLock};
use std::sync::atomic::{AtomicU64, Ordering};
use miette::Report;
use crate::error::{SemanticError};
use crate::parser::ast::{Block, DType, Either, Expression, Function, MaybeNull, Program, Span, Spanned, Statement};

static TYPE_COUNTER: AtomicU64 = AtomicU64::new(1);

pub fn alloc_type_idx() -> u64 {
    TYPE_COUNTER.fetch_add(1, Ordering::Relaxed)
}

macro_rules! both_known {
    ($a:expr, $b:expr) => {
        !$a.is_unknown() && !$b.is_unknown()
    };
}

pub(crate) struct TypeChecker {
    symbols: HashMap<String, Spanned<MaybeNull>>,
    return_type: Spanned<MaybeNull>,
    bounds: HashSet<TypeBound>,
    functions: HashMap<String, Spanned<Function>>,
    src: String,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
enum TypeBound {
    Equals(MaybeNull, MaybeNull),
}

impl TypeChecker {
    pub(crate) fn new(src: String) -> Self {
        Self {
            symbols: HashMap::new(),
            return_type: Spanned::new(MaybeNull::unknown(0), Span::empty()),
            bounds: HashSet::new(),
            functions: HashMap::new(),
            src,
        }
    }

    fn get_variable_type(&self, name: &str, span: Span) -> Result<Spanned<MaybeNull>, SemanticError> {
        self.symbols
            .get(name)
            .and_then(|typ| Some(typ.clone()))
            .ok_or(SemanticError::UndefinedVariable {
                name: Spanned::new(name.to_string(), span),
            })
    }

    fn get_function_define(&self, name: &str, span: Span) -> Result<&Spanned<Function>, SemanticError> {
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
                typ: Spanned::new(MaybeNull::nonnull(function.return_type.typ), function.return_type.span),
            });
        }

        self.return_type = function.return_type;
        self.functions.insert((*function.name).clone(), function.clone());

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
            Statement::Return(Spanned { node, span }) => {
                match &*node.expression {
                    Expression::NumberLiteral {
                        value, typ
                    } => {
                        self.add_bound(TypeBound::Equals(node.typ, *typ));
                    }
                    Expression::BooleanLiteral(_) => {
                        self.add_bound(TypeBound::Equals(node.typ, MaybeNull::nonnull(DType::Bool)));
                    }
                    Expression::Variable {
                        name, typ,
                    } => {
                        let defined = self.get_variable_type(name.as_str(), name.span)?;
                        self.add_bound(TypeBound::Equals(*defined, *typ));
                        self.add_bound(TypeBound::Equals(*defined, node.typ))
                    },
                    Expression::BinaryOp {
                        op, lhs, rhs
                    } => {
                        let typ = self.infer_expression(&node.expression)?;

                        if !typ.is_unknown() && typ != self.return_type.node {
                            return Err(SemanticError::ReturnTypeMismatch {
                                expected: self.return_type,
                                found: Spanned::new(typ, node.expression.span),
                            })
                        }
                        self.add_bound(TypeBound::Equals(typ, node.typ));
                        self.add_bound(TypeBound::Equals(typ, *self.return_type));
                    }
                    Expression::Invoke(invoke) => {
                        let function = self.get_function_define(&invoke.identifier, invoke.identifier.span)?;
                        if function.return_type.node != self.return_type.node {
                            return Err(SemanticError::ReturnTypeMismatch {
                                expected: self.return_type,
                                found: function.return_type,
                            })
                        }
                    }
                }
            }
            Statement::Assign(assign) => {
                let typ = self.infer_expression(&assign.expression)?;
                self.add_bound(TypeBound::Equals(assign.typ, typ));
                self.symbols.insert((*assign.identifier).clone(), Spanned::new(typ, Span::empty()));
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn infer_expression(&mut self, expression: &Spanned<Expression>) -> Result<MaybeNull, SemanticError> {
        match &expression.node {
            Expression::NumberLiteral {
                typ, ..
            } => Ok(*typ),
            Expression::BooleanLiteral(_) => Ok(MaybeNull::nonnull(DType::Bool)),
            Expression::Variable { name, typ } => {
                let defined= self.get_variable_type(name, name.span)?;
                Ok(defined.node)
            }
            Expression::BinaryOp {
                op, lhs, rhs
            } => {
                let lhs_typ = self.infer_expression(lhs)?;
                let rhs_typ = self.infer_expression(rhs)?;

                if both_known!(lhs_typ, rhs_typ) && lhs_typ != rhs_typ {
                    return Err(SemanticError::InvalidBinaryOp {
                        lhs_typ: Spanned::new(lhs_typ, lhs.span),
                        rhs_typ: Spanned::new(rhs_typ, rhs.span),
                    });
                }

                self.add_bound(TypeBound::Equals(lhs_typ, rhs_typ));
                Ok(lhs_typ)
            }
            Expression::Invoke(invoke) => {
                let name = invoke.identifier.clone();
                let function = self.get_function_define(&name, invoke.identifier.span)?;
                let param_list = &function.param_list;
                let return_type = function.return_type;

                let expected_len = param_list.params.len();
                let actual_len = invoke.param_list.expressions.len();

                if expected_len != actual_len {
                    return Err(SemanticError::ArityMismatch {
                        name: name.clone(),
                        expected: Spanned::new(expected_len, param_list.span),
                        found: Spanned::new(actual_len, invoke.param_list.span),
                    });
                }

                let mut tasks = Vec::new();
                for (index, param) in function.param_list.params.iter().enumerate() {
                    tasks.push((index, param.typ));
                }

                for (index, param_typ) in tasks {
                    let invoke_expr = &invoke.param_list.expressions[index];
                    let expr_typ = self.infer_expression(invoke_expr)?;
                    self.add_bound(TypeBound::Equals(expr_typ, param_typ.node));
                }
                Ok(return_type.node)
            }
        }
    }

    fn add_bound(&mut self, bound: TypeBound) {
        self.bounds.insert(bound);
    }

    pub(crate) fn print_bound(&self) {
        println!("{:#?}", self.bounds)
    }

    pub fn solve_bounds(&self) -> Substitution {
        let context = BoundSolveContext::new(&self.bounds);
        context.solve()
    }
}

struct UnionFind {
    parents: HashMap<u64, u64>,
}

impl UnionFind {
    pub fn new() -> Self {
        Self {
            parents: HashMap::new(),
        }
    }

    pub fn union(&mut self, a: u64, b: u64) {
        self.ensure_exists_parent(b);

        let a_parent = self.find(a);
        let b_parent = self.find(b);
        if a_parent != b_parent {
            self.parents.insert(a_parent, b_parent);
        }
    }

    pub fn find(&mut self, a: u64) -> u64 {
        self.ensure_exists_parent(a);
        let a_parent = self.parents[&a];
        if a_parent != a {
            let b = self.find(a_parent);
            self.parents.insert(a, b);
            return b;
        }
        a
    }

    fn ensure_exists_parent(&mut self, a: u64) {
        self.parents
            .entry(a)
            .or_insert(a);
    }

    pub fn be_parent(&mut self, a: u64) {
        self.parents.insert(a, a);
    }
}

pub(crate) struct Substitution {
    union_find: UnionFind,
    symbols: Vec<u64>,
    known: HashMap<u64, DType>,
}

impl Substitution {
    fn new(
        union_find: UnionFind,
        known: HashMap<u64, DType>,
        symbols: HashSet<u64>,
    ) -> Self {
        Self {
            union_find,
            known,
            symbols: symbols.into_iter().collect(),
        }
    }

    pub fn find(&mut self, idx: u64) -> DType {
        let parent = self.union_find.find(idx);
        self.known[&parent]
    }

    pub fn print_all_substitution(&mut self) {
        println!("{:#?}", self.symbols
            .clone()
            .into_iter()
            .map(|e| (e, self.find(e)))
            .collect::<HashMap<_, _>>())
    }
}

struct BoundSolveContext<'a> {
    bounds: &'a HashSet<TypeBound>,
}

impl<'a> BoundSolveContext<'a> {
    fn new(bounds: &'a HashSet<TypeBound>) -> Self {
        Self { bounds }
    }

    fn solve(&self) -> Substitution {
        let mut symbols = HashSet::new();
        let mut substitution = HashMap::new();
        let mut union_find = UnionFind::new();

        for value in self.bounds {
            match value {
                TypeBound::Equals(a, b) => {
                    let (mut a, mut b) = (a.clone(), b.clone());
                    if b.is_unknown() {
                        std::mem::swap(&mut a, &mut b);
                    }
                    match (a.typ, b.typ) {
                        (DType::Unknown(x), DType::Unknown(y)) => {
                            symbols.insert(x);
                            symbols.insert(y);

                            if substitution.contains_key(&y) {
                                union_find.union(x, y);
                                continue;
                            }
                            union_find.union(y, x);
                        }
                        (DType::Unknown(x), typ) => {
                            symbols.insert(x);
                            substitution.insert(x, typ);

                            let x_parent = union_find.find(x);
                            union_find.be_parent(x);
                            union_find.union(x_parent, x);
                        }
                        _ => {}
                    }
                }
            }
        }

        let sub = Substitution::new(union_find, substitution, symbols);
        sub
    }
}

#[cfg(test)]
mod tests {

}