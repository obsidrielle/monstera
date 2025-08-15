use crate::checker::alloc_type_idx;
use crate::parser::Rule;
use crate::parser::ast::Spanned;
use pest::iterators::Pair;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::str::FromStr;

pub trait Type {
    fn type_idx(&self) -> u64;
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) enum DType {
    Bool,
    Int(IntType),
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
            Self::Int(int) => int.type_idx(),
            Self::Unknown { idx, .. } => *idx + 50,
        }
    }
}

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

impl DType {
    pub(crate) fn is_number(&self) -> bool {
        matches!(self, Self::Int(_))
    }

    pub(crate) fn is_primitive(&self) -> bool {
        matches!(self, Self::Int(_) | Self::Bool | Self::Void)
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
            "bool" => Ok(DType::Bool),
            "void" => Ok(DType::Void),
            _ => Ok(DType::Int(s.parse::<IntType>()?)),
        }
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

impl Into<DType> for MaybeNull {
    fn into(self) -> DType {
        self.typ
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
