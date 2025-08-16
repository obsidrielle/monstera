use crate::parser::ast::{MaybeNull, Spanned};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch {
        expected: Spanned<MaybeNull>,
        found: Spanned<MaybeNull>,
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
    },
}

#[derive(Debug, Error, Diagnostic)]
#[error("Return type mismatch")]
#[diagnostic(code(semantic::return_type_mismatch))]
pub struct ReturnMismatchDiag {
    #[source_code]
    src: String,
    #[label("but the function is declared to return `{expected}`, as specified here")]
    expected_span: SourceSpan,
    expected: MaybeNull,
    #[label("The actual type is `{found}`")]
    found_span: SourceSpan,
    found: MaybeNull,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Arity mismatch")]
#[diagnostic(code(semantic::arity_mismatch))]
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

#[derive(Debug, Error, Diagnostic)]
#[error("Undefined variable")]
#[diagnostic(code(semantic::undefined_variable))]
pub struct UndefinedVariableDiag {
    #[source_code]
    src: String,
    #[label("This variable `{variable}` is undefined")]
    span: SourceSpan,
    variable: String,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Type mismatch")]
#[diagnostic(code(semantic::type_mismatch))]
pub struct TypeMismatchDiag {
    #[source_code]
    src: String,
    #[label("This part expected type `{expected}`")]
    expected_span: SourceSpan,
    expected: MaybeNull,
    #[label("but this type isn't equal to that, which is `{found}`")]
    found_span: SourceSpan,
    found: MaybeNull,
}

#[derive(Debug, Error, Diagnostic)]
#[error("Invalid binary operator")]
#[diagnostic(code(semantic::invalid_binary_operator))]
pub struct InvalidBinaryOpDiag {
    #[source_code]
    src: String,
    #[label("left expression has type `{lhs}`")]
    lhs_span: SourceSpan,
    lhs: MaybeNull,
    #[label("but right expression has a different typ `{rhs}`")]
    rhs_span: SourceSpan,
    rhs: MaybeNull,
}

pub(crate) fn build_undefined_variable_diag(
    variable: Spanned<String>,
    src: &str,
) -> UndefinedVariableDiag {
    UndefinedVariableDiag {
        src: src.to_string(),
        span: variable.span.into(),
        variable: variable.to_string(),
    }
}

pub(crate) fn build_return_type_mismatch_diag(
    (expected, found): (Spanned<MaybeNull>, Spanned<MaybeNull>),
    src: &str,
) -> ReturnMismatchDiag {
    ReturnMismatchDiag {
        src: src.to_string(),
        expected_span: expected.span.into(),
        expected: *expected,
        found_span: found.span.into(),
        found: *found,
    }
}

pub(crate) fn build_arity_mismatch_diag(
    name: String,
    (expected, found): (Spanned<usize>, Spanned<usize>),
    src: &str,
) -> ArityMismatchDiag {
    ArityMismatchDiag {
        src: src.to_string(),
        expected_span: expected.span.into(),
        expected: *expected,
        found_span: found.span.into(),
        found: *found,
        name,
    }
}

pub(crate) fn build_type_mismatch(
    (expected, found): (Spanned<MaybeNull>, Spanned<MaybeNull>),
    src: &str,
) -> TypeMismatchDiag {
    TypeMismatchDiag {
        src: src.to_string(),
        expected_span: expected.span.into(),
        expected: *expected,
        found_span: found.span.into(),
        found: *found,
    }
}

pub(crate) fn build_invalid_binary_op(
    (lhs, rhs): (Spanned<MaybeNull>, Spanned<MaybeNull>),
    src: &str,
) -> InvalidBinaryOpDiag {
    InvalidBinaryOpDiag {
        src: src.to_string(),
        lhs_span: lhs.span.into(),
        lhs: *lhs,
        rhs_span: rhs.span.into(),
        rhs: *rhs,
    }
}