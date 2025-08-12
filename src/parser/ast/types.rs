use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::str::FromStr;
use pest::iterators::Pair;
use crate::checker::alloc_type_idx;
use crate::parser::ast::Spanned;
use crate::parser::Rule;

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Hash)]
pub(crate) enum DType {
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Void,
    Unknown(u64),
}

impl DType {
    pub(crate) fn is_number(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 |
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => true,
            _ => false,
        }
    }

    pub(crate) fn is_primitive(&self) -> bool {
        match self {
            Self::I8 | Self::I16 | Self::I32 | Self::I64 |
            Self::U8 | Self::U16 | Self::U32 | Self::U64 | Self::Void => true,
            _ => false,
        }
    }

    pub(crate) fn is_unknown(&self) -> bool {
        match self {
            Self::Unknown(_) => true,
            _ => false,
        }
    }
}
impl Display for DType {
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
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Unknown(_) => write!(f, "<unknown>"),
        }
    }
}

impl FromStr for DType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "bool" => Ok(DType::Bool),
            "i8" => Ok(DType::I8),
            "i16" => Ok(DType::I16),
            "i32" => Ok(DType::I32),
            "i64" => Ok(DType::I64),
            "u8" => Ok(DType::U8),
            "u16" => Ok(DType::U16),
            "u32" => Ok(DType::U32),
            "u64" => Ok(DType::U64),
            "void" => Ok(DType::Void),
            _ => Err(format!("Unknown dtype {}", s)),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

    pub(crate) fn unknown(idx: u64) -> Self {
        Self {
            typ: DType::Unknown(idx),
            maybe_null: false,
        }
    }

    pub(crate) fn is_maybe_null(&self) -> bool {
        self.maybe_null
    }

    pub(crate) fn alloc_unknown() -> Self {
        Self::unknown(alloc_type_idx())
    }

    pub(crate) fn type_idx(&self) -> u64 {
        match self.typ {
            DType::Unknown(idx) => idx,
            _ => 0,
        }
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
        .and_then(|s| Some(Spanned::new(MaybeNull::maybe_null(s.parse().unwrap()), span.into())))
        .unwrap_or_else(|| Spanned::new(MaybeNull::nonnull(pair_str.parse().unwrap()), span.into()))
}

impl Default for DType {
    fn default() -> Self {
        Self::I32
    }
}
