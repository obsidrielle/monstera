use std::fmt::{Display, Formatter};
use std::str::FromStr;
use crate::parser::ast::Type;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) enum IntType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

impl Type for IntType {
    fn type_idx(&self) -> u64 {
        match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 3,
            Self::I64 => 4,
            Self::U8 => 5,
            Self::U16 => 6,
            Self::U32 => 7,
            Self::U64 => 8,
        }
    }
}

impl FromStr for IntType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "u8" => Ok(Self::U8),
            "u16" => Ok(Self::U16),
            "u32" => Ok(Self::U32),
            "u64" => Ok(Self::U64),
            _ => Err(format!("Unknown type: {s}")),
        }
    }
}
impl Display for IntType {
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
        }
    }
}