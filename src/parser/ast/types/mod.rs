mod array;
mod int;
mod struct_type;
mod arena;

use std::collections::HashMap;
pub use array::*;
pub use int::*;
pub use struct_type::*;

use crate::checker::alloc_type_idx;
use crate::parser::Rule;
use crate::parser::ast::Spanned;
use pest::iterators::Pair;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::str::FromStr;
use lazy_static::lazy_static;
use crate::parser::ast::types::arena::{type_arena, CompositeType};

pub trait Type {
    fn type_idx(&self) -> u64;
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Hash)]
pub(crate) enum DType {
    Bool,
    Int(IntType),
    Array(u64),
    Struct(u64),
    Void,
    Unknown {
        idx: u64,
        kind: TypeKind,
    }
}

#[derive(Debug, PartialOrd, Ord, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) enum TypeKind {
    Int,
}

impl TypeKind {
    pub fn default_type(&self) -> MaybeNull {
        
        match self {
            Self::Int => MaybeNull::nonnull(DType::Int(IntType::I32)),
        }
    }
}

impl Type for DType {
    fn type_idx(&self) -> u64 {
        match self {
            Self::Bool => 9,
            Self::Void => 10,
            Self::Array(array_id) => *array_id,
            Self::Struct(struct_id) => *struct_id,
            Self::Int(int) => int.type_idx(),
            Self::Unknown { idx, .. } => *idx + 50,
        }
    }
}

impl DType {
    pub(crate) fn is_number(&self) -> bool {
        matches!(self, Self::Int(_))
    }

    pub(crate) fn is_primitive(&self) -> bool {
        matches!(self, Self::Int(_) | Self::Bool | Self::Void)
    }

    pub(crate) fn is_compositive(&self) -> bool {
        matches!(self, Self::Array(_) | Self::Struct(_))
    }

    pub(crate) fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown { .. })
    }
}
impl Display for DType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{}", int),
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            _ => write!(f, "<unknown>"),
        }
    }
}

impl FromStr for DType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => return Ok(DType::Bool),
            "void" => return Ok(DType::Void),
            "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" => return Ok(DType::Int(s.parse::<IntType>()?)),
            _ => {}
        }
        
        if let Ok(array) = s.parse::<ArrayType>() {
            let mut lock = type_arena().lock().unwrap();
            let index = lock.add_type(CompositeType::Array(array));
            return Ok(DType::Array(index));
        }
        
        // a struct with type index 0 means it is undefined 
        let lock = type_arena().lock().unwrap();
        Ok(DType::Struct(lock.index(s)
            .unwrap_or(0)))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default, Ord, PartialOrd)]
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

    pub(crate) fn unknown(idx: u64, kind: TypeKind) -> Self {
        Self {
            typ: DType::Unknown {
                idx,
                kind,
            },
            maybe_null: false,
        }
    }

    pub(crate) fn is_maybe_null(&self) -> bool {
        self.maybe_null
    }

    pub(crate) fn alloc_unknown(kind: TypeKind) -> Self {
        MaybeNull::unknown(alloc_type_idx(), kind)
    }
}

impl Type for MaybeNull {
    fn type_idx(&self) -> u64 {
        self.typ.type_idx()
    }
}

impl From<DType> for MaybeNull {
    fn from(value: DType) -> Self {
        Self {
            typ: value,
            maybe_null: false,
        }
    }
}

pub(crate) fn parse_maybe_null(pair: Pair<Rule>) -> Spanned<MaybeNull> {
    let span = pair.as_span();
    let mut pairs = pair.into_inner();
    let pair_str = next_pair!(pairs).as_str();

    pair_str
        .strip_prefix("?")
        .map(|s| Spanned::new(
            MaybeNull::maybe_null(s.parse().unwrap()),
            span.into(),
        ))
        .unwrap_or_else(|| Spanned::new(MaybeNull::nonnull(pair_str.parse().unwrap()), span.into()))
}

impl Default for DType {
    fn default() -> Self {
        Self::Int(IntType::I32)
    }
}