use crate::checker::graph::private::Sealed;
use std::collections::{HashMap, VecDeque};
use std::env::VarError;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

#[derive(Debug)]
pub(crate) struct Vertex<V, P> {
    pub name: V,
    pub payload: P,
    pub edges: Vec<V>,
}

pub(crate) struct Graph<V, P, Type> {
    pub vertex: HashMap<V, Vertex<V, P>>,
    _marker: std::marker::PhantomData<Type>,
}

impl<V, P, Type> Debug for Graph<V, P, Type>
where
    V: Debug + Eq + Hash + Clone,
    P: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("Graph");

        writeln!(f, " {{")?;
        for vertex in self.vertex.values() {
            writeln!(
                f,
                "    Vertex<name: {:?}, payload: {:?}> -> edges: {:?}",
                vertex.name, vertex.payload, vertex.edges
            )?;
        }
        writeln!(f, "}}")?;

        Ok(())
    }
}

impl<V, P, Type> Graph<V, P, Type> {
    pub fn new() -> Self {
        Self {
            vertex: HashMap::new(),
            _marker: std::marker::PhantomData,
        }
    }
}

pub trait GraphBehavior<V, P>: private::Sealed
where
    V: Eq + Hash + Clone,
    P: Default,
{
    fn ensure_vertex_exists(vertex: &mut HashMap<V, Vertex<V, P>>, src: V) -> &mut Vertex<V, P> {
        vertex
            .entry(src.clone())
            .or_insert_with(|| Vertex {
            name: src,
            payload: P::default(),
            edges: Vec::new(),
        })
    }

    fn add_edge(vertex: &mut HashMap<V, Vertex<V, P>>, src: V, dst: V);

    fn add_vertex(vertex: &mut HashMap<V, Vertex<V, P>>, name: V, payload: P) {
        vertex
            .entry(name.clone())
            .or_insert_with(|| Vertex {
                name,
                payload,
                edges: Vec::new(),
            },
        );
    }

    fn vertex_mut<'a>(
        vertex: &'a mut HashMap<V, Vertex<V, P>>,
        name: &V,
    ) -> Option<&'a mut Vertex<V, P>> {
        vertex.get_mut(name)
    }

    fn vertex_ref<'a> (
        vertex: &'a HashMap<V, Vertex<V, P>>,
        name: &V,
    ) -> Option<&'a Vertex<V, P>> {
        vertex.get(name)
    }
}

mod private {
    pub trait Sealed {}
}

#[derive(Debug)]
pub struct Undirected;

#[derive(Debug)]
pub struct Directed;

impl Sealed for Undirected {}
impl Sealed for Directed {}

impl<V, P> GraphBehavior<V, P> for Undirected
where
    V: Eq + Hash + Clone,
    P: Default,
{
    fn add_edge(vertex: &mut HashMap<V, Vertex<V, P>>, src: V, dst: V) {
        Self::ensure_vertex_exists(vertex, src.clone())
            .edges
            .push(dst.clone());
        Self::ensure_vertex_exists(vertex, dst).edges.push(src);
    }
}

impl<V, P> GraphBehavior<V, P> for Directed
where
    V: Eq + Hash + Clone,
    P: Default,
{
    fn add_edge(vertex: &mut HashMap<V, Vertex<V, P>>, src: V, dst: V) {
        todo!("Implement add_edge for Directed graphs")
    }
}

impl<V, P, T> Graph<V, P, T>
where
    V: Eq + Hash + Clone,
    P: Default,
    T: GraphBehavior<V, P>,
{
    pub fn add_edge(&mut self, src: V, dst: V) {
        T::add_edge(&mut self.vertex, src, dst);
    }

    pub fn add_vertex(&mut self, name: V, payload: P) {
        T::add_vertex(&mut self.vertex, name, payload);
    }

    pub fn vertex_ref(&self, name: &V) -> Option<&Vertex<V, P>> {
        T::vertex_ref(&self.vertex, name)
    }

    pub fn vertex_mut(&mut self, name: &V) -> Option<&mut Vertex<V, P>> {
        T::vertex_mut(&mut self.vertex, name)
    }
}

impl<V, P, T> Graph<V, P, T>
where
    V: Eq + Hash + Clone,
{
    pub fn bfs<F>(&self, mut f: F, src: Vec<V>)
    where
        F: FnMut(&Vertex<V, P>),
    {
        let mut queue = VecDeque::from(src);
        let mut visited = HashMap::new();

        while let Some(src) = queue.pop_front() {
            visited.insert(src.clone(), true);

            let vertex = self.vertex.get(&src).unwrap();
            f(vertex);

            for vertex in &vertex.edges {
                if !visited.contains_key(vertex) {
                    visited.insert(vertex.clone(), true);
                    queue.push_back(vertex.clone());
                }
            }
        }
    }
}