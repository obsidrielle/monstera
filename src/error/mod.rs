mod semantic;

pub use semantic::*;

use crate::error::semantic::{build_arity_mismatch_diag, build_return_type_mismatch_diag};
use miette::Diagnostic;
use thiserror::Error;


pub(crate) enum CompilerError {}

#[derive(Debug, Error, Diagnostic)]
#[error(transparent)]
#[diagnostic(transparent)]
pub struct DiagnosticReport(#[from] Box<dyn Diagnostic + Send + Sync>);

pub fn build_diagnostic(error: SemanticError, src: &str) -> DiagnosticReport {
    let diagnostic: Box<dyn Diagnostic + Send + Sync> = match error {
        SemanticError::ReturnTypeMismatch { expected, found } => {
            Box::new(build_return_type_mismatch_diag((expected, found), src))
        }
        SemanticError::ArityMismatch {
            name,
            expected,
            found,
        } => Box::new(build_arity_mismatch_diag(
            name.to_string(),
            (expected, found),
            src,
        )),
        SemanticError::UndefinedVariable { name } => {
            Box::new(build_undefined_variable_diag(name, src))
        }
        SemanticError::TypeMismatch { expected, found } => {
            Box::new(build_type_mismatch((expected, found), src))
        }
        _ => unreachable!(),
    };
    DiagnosticReport(diagnostic)
}
