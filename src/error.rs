use inkwell::llvm_sys::LLVMYieldCallback;
use thiserror::Error;
use crate::parser::ast::{BinaryOp, MaybeNull};

#[derive(Error, Debug)]
pub(crate) enum CompileError {
    #[error(transparent)]
    Semantic(#[from] SemanticError),
}

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Type Mismatch: expected type `{expected}`, but found type `{found}`")]
    TypeMismatch {
        expected: MaybeNull,
        found: MaybeNull,
    },

    #[error("Undefined Variable: `{0}` is not defined in this scope")]
    UndefinedVariable(String),

    #[error("Undefined Function: function `{0}` is not defined")]
    UndefinedFunction(String),

    #[error("'{0}' is not a function, it cannot be called")]
    NotAFunction(String),

    #[error("Duplicate Definition: `{0}` is already defined in this scope")]
    DuplicateDefinition(String),

    #[error("Wrong Number of Arguments: function `{name}` expected {expected} arguments, but {found} were provided")]
    ArityMismatch {
        name: String,
        expected: usize,
        found: usize,
    },

    #[error("Invalid Operator: cannot apply operator `{op}` to type `{typ}`")]
    InvalidOperator {
        op: String,
        typ: MaybeNull,
    },

    #[error("Invalid Binary Operation: cannot apply operator `{op}` between types `{lhs_typ}` and `{rhs_typ}`")]
    InvalidBinaryOp {
        op: BinaryOp,
        lhs_typ: MaybeNull,
        rhs_typ: MaybeNull,
    },

    #[error("Return type mismatch: function expects to return `{expected}`, but found `{found}`")]
    ReturnTypeMismatch {
        expected: MaybeNull,
        found: MaybeNull,
    },

    #[error("Cannot return a value from a function that returns () (unit type)")]
    ReturnValueFromUnitFunction,

    #[error("Main function `main` must have no arguments and return type `i32`")]
    InvalidMainSignature,

    #[error("A primitive type `{typ}` cannot be nullable")]
    MaybeNullOfPrimitiveType {
        typ: MaybeNull,
    }
}