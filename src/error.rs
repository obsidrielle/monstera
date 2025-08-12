use inkwell::llvm_sys::LLVMYieldCallback;
use miette::{Diagnostic, Report, SourceSpan};
use thiserror::Error;
use crate::parser::ast::{BinaryOp, MaybeNull, Span, Spanned};

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch {
        expected: MaybeNull,
        found: MaybeNull,
    },

    UndefinedVariable {
        name: Spanned<String>,
    },

    UndefinedFunction {
        name: Spanned<String>,
    },

    NotAFunction(String),

    DuplicateDefinition(String),

    ArityMismatch {
        name: Spanned<String>,
        expected: Spanned<usize>,
        found: Spanned<usize>,
    },

    InvalidOperator {
        op: String,
        typ: MaybeNull,
    },

    InvalidBinaryOp {
        lhs_typ: Spanned<MaybeNull>,
        rhs_typ: Spanned<MaybeNull>,
    },

    ReturnTypeMismatch {
        expected: Spanned<MaybeNull>,
        found: Spanned<MaybeNull>,
    },

    ReturnValueFromUnitFunction,

    InvalidMainSignature,

    MaybeNullOfPrimitiveType {
        typ: Spanned<MaybeNull>,
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("Return type mismatch")]
#[diagnostic(code(sematic::return_type_mismatch))]
pub struct ReturnMismatchDiag {
    #[source_code]
    src: String,
    #[label("...as specified here.")]
    expected_span: SourceSpan,
    expected: MaybeNull,
    #[label("This expression has type `i32`, but the function is declared to return `i64`...")]
    found_span: SourceSpan,
    found: MaybeNull,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Arity mismatch")]
#[diagnostic(code(sematic::arity_mismatch))]
#[help("")]
pub struct ArityMismatchDiag {
    #[source_code]
    src: String,
    #[label("The arguments list is defined here")]
    expected_span: SourceSpan,
    expected: usize,
    #[label("This function `{name}` expected `{expected}` arguments, but `{found}` were given")]
    found_span: SourceSpan,
    found: usize,
    name: String,
}
fn build_return_type_mismatch_diag((expected, found): (Spanned<MaybeNull>, Spanned<MaybeNull>), src: &str) -> ReturnMismatchDiag {
    ReturnMismatchDiag {
        src: src.to_string(),
        expected_span: expected.span.into(),
        expected: *expected,
        found_span: found.span.into(),
        found: *found,
    }
}

fn build_arity_mismatch_diag(name: String, (expected, found): (Spanned<usize>, Spanned<usize>), src: &str) -> ArityMismatchDiag {
    ArityMismatchDiag {
        src: src.to_string(),
        expected_span: expected.span.into(),
        expected: *expected,
        found_span: found.span.into(),
        found: *found,
        name,
    }
}

#[derive(Debug, Error, Diagnostic)]
pub enum SemanticErrorReport {
    #[error(transparent)]
    #[diagnostic(transparent)]
    ReturnTypeMismatch(ReturnMismatchDiag),
}

#[derive(Debug, Error, Diagnostic)]
#[error(transparent)]
#[diagnostic(transparent)]
pub struct DiagnosticReport(#[from] Box<dyn Diagnostic + Send + Sync>);
pub fn build_diagnostic(error: SemanticError, src: &str) -> DiagnosticReport {
    let diagnostic: Box<dyn Diagnostic + Send + Sync> = match error {
        SemanticError::ReturnTypeMismatch { expected, found } =>
            Box::new(build_return_type_mismatch_diag((expected, found), src)),
        SemanticError::ArityMismatch { name, expected, found } =>
            Box::new(build_arity_mismatch_diag(name.to_string(), (expected, found), src)),
        _ => unreachable!(),
    };
    DiagnosticReport(diagnostic)
}