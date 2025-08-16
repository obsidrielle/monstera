use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};
use std::sync::atomic::{AtomicU64, Ordering};
use crate::parser::ast::{ArrayType, StructType};
use crate::parser::ast::types::arena::CompositeType::Struct;

static TYPE_ARENA: OnceLock<Mutex<Arena>> = OnceLock::new();

pub fn type_arena<'a>() -> &'static Mutex<Arena> {
    TYPE_ARENA
        .get_or_init(|| Mutex::new(Arena::new()))
}

#[derive(Debug)]
pub struct Arena {
    counter: AtomicU64,
    types: Vec<CompositeType>,
    index: HashMap<String, u64>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum CompositeType {
    Array(ArrayType),
    Struct(StructType),
}

impl CompositeType {
    pub fn name(&self) -> String {
        match self {
            Self::Array(array) => format!("{array}"),
            Self::Struct(struct_) => struct_.name.clone(),
        }
    }
}

impl Arena {
    pub fn new() -> Self {
        Self {
            counter: AtomicU64::new(1),
            types: vec![Struct(StructType {
                name: "Placeholder".into(),
                members: Vec::new(),
            })],
            index: HashMap::new(),
        }
    }

    pub fn add_type(&mut self, typ: CompositeType) -> u64 {
        let typ_name = typ.name();
        if let Some(index) = self.index.get(&typ_name) {
            return *index;
        }

        let index = self.counter.fetch_add(1, Ordering::Relaxed);
        self.types.push(typ.clone());
        self.index.insert(typ_name, index);
        index
    }

    pub fn type_ref(&self, index: u64) -> &CompositeType {
        &self.types[index as usize]
    }

    pub fn index(&self, name: &str) -> Option<u64> {
        self.index.get(name).copied()
    }

    pub fn type_ref_by_name(&self, name: &str) -> Option<&CompositeType> {
        self.index.get(name).map(|index| &self.types[*index as usize])
    }
}