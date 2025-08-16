use std::fmt::{Display, Formatter};
use std::str::FromStr;
use crate::parser::ast::MaybeNull;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub members: Vec<(String, MaybeNull)>,
}

impl Display for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;
        for (name, typ) in &self.members {
            writeln!(f, "\t{}: {}", name, typ)?;
        }
        write!(f, "}}")
    }
}