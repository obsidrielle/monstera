use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::ops::Sub;
use crate::checker::TypeBound;
use crate::checker::graph::{Graph, Undirected};
use crate::error::SemanticError;
use crate::parser::ast::{DType, MaybeNull, Spanned, Type, TypeKind};

pub(crate) struct BoundSolver<'a> {
    bounds: &'a Vec<TypeBound>,
}

impl<'a> BoundSolver<'a> {
    pub fn new(bounds: &'a Vec<TypeBound>) -> Self {
        Self { bounds }
    }

    fn handle_partial_known(
        &self,
        unknown: &Spanned<MaybeNull>,
        known: &Spanned<MaybeNull>,
        known_vec: &mut HashSet<u64>,
        graph: &mut Graph<u64, Spanned<MaybeNull>, Undirected>) {
        known_vec.insert(known.type_idx());
        graph.add_vertex(known.type_idx(), *known);
        graph.add_vertex(unknown.type_idx(), *unknown);
        graph.add_edge(unknown.type_idx(), known.type_idx());
    }

    fn create_graph(&self) -> (Graph<u64, Spanned<MaybeNull>, Undirected>, Vec<u64>) {
        let mut graph = Graph::new();
        let mut known = HashSet::new();

        for bound in self.bounds {
            let TypeBound::Equals(lhs, rhs) = bound;

            match (lhs.is_unknown(), rhs.is_unknown()) {
                (true, false) => self.handle_partial_known(lhs, rhs, &mut known, &mut graph),
                (false, true) => self.handle_partial_known(rhs, lhs, &mut known, &mut graph),
                (true, true) => {
                    let lhs_idx = lhs.type_idx();
                    let rhs_idx = rhs.type_idx();

                    graph.add_vertex(lhs_idx, *lhs);
                    graph.add_vertex(rhs_idx, *rhs);
                    graph.add_edge(lhs_idx, rhs_idx);
                }
                _ => {},
            };
        }

        (graph, known.into_iter().collect())
    }


    fn collect_free_types(
        &self,
        graph: &Graph<u64, Spanned<MaybeNull>, Undirected>,
        inner: &mut HashMap<u64, Spanned<MaybeNull>>,
    ) {
        let free_variables = graph.vertex
            .values()
            .filter_map(|v| {
                if inner.contains_key(&v.name) {
                    return None
                }
                match **v.payload {
                    DType::Unknown { idx, kind } => {
                        match kind {
                            k @ TypeKind::Int => Some((v.name, Spanned::empty(k.default_type()))),
                        }
                    }
                    _ => None,
                }
            })
            .collect::<HashMap<_, _>>();
        inner.extend(free_variables);
    }

    pub fn solve(&self) -> Result<Substitution, Vec<SemanticError>> {
        let (graph, known) = self.create_graph();

        println!("{:#?}", graph);
        println!("{:#?}", known);

        let mut errors = Vec::new();
        let mut inner = known
            .iter()
            .map(|e| (*e, graph.vertex_ref(e).unwrap().payload))
            .collect::<HashMap<_, _>>();

        // current vertex's type must be known.
        // this bfs function will help us walk this graph.
        graph.bfs(|vertex| {
            let vertex_type = inner
                .get(&vertex.name)
                .copied()
                .unwrap();

            for &neighbor_idx in &vertex.edges {
                let neighbor = &graph.vertex_ref(&neighbor_idx).unwrap();

                // if a neighbor's type is known, it must be equal to current vertex's type.
                // otherwise, it should be an error.
                if !neighbor.payload.is_unknown() && neighbor.payload != vertex_type {
                    errors.push(SemanticError::TypeMismatch {
                        expected: vertex.payload,
                        found: neighbor.payload,
                    });
                    return;
                }

                // finally, spread the type to the neighbors.
                inner
                    .entry(neighbor_idx)
                    .or_insert(vertex_type);
            }
        }, known);
        self.collect_free_types(&graph, &mut inner);


        if errors.is_empty() {
            return Ok(Substitution {
                inner,
            })
        }
        Err(errors)
    }
}

pub(crate) struct Substitution {
    inner: HashMap<u64, Spanned<MaybeNull>>,
}

impl Debug for Substitution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner
            .iter()
            .try_for_each(|(k, v)| writeln!(f, "{:?} -> {:?}", k, v))?;
        Ok(())
    }
}

impl Substitution {
    pub fn find(&self, idx: u64) -> MaybeNull {
        println!("idx: {idx}");
        *self.inner.get(&idx).copied().unwrap()
    }
}