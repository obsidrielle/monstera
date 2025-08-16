use std::fmt::{Display, Formatter};
use std::str::FromStr;
use crate::parser::ast::{DType, MaybeNull, Type};

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct ArrayType {
    typ: MaybeNull,
    length: u64,
}

impl Type for ArrayType {
    fn type_idx(&self) -> u64 {
        self.typ.type_idx() + 10
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}; {}]", self.typ, self.length)
    }
}

impl FromStr for ArrayType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s
            .strip_prefix("[")
            .ok_or_else(|| "array type should start with `[`".to_string())?
            .strip_suffix("]")
            .ok_or_else(|| "array type should end with `]`".to_string())?;

        let (typ, u64) = trimmed
            .split_once(";")
            .map(|(typ, u64)| (typ.parse::<DType>().unwrap().into(), u64.parse().unwrap()))
            .ok_or_else(|| "Failed to extract type or length")?;

        Ok(Self { typ, length: u64 })
    }
}

