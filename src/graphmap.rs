#![doc = " `GraphMap<N, E, Ty>` is a graph datastructure where node values are mapping"]
#![doc = " keys."]
use crate::graph::node_index;
use crate::graph::Graph;
use crate::visit;
use crate::IntoWeightedEdge;
use crate::{Directed, Direction, EdgeType, Incoming, Outgoing, Undirected};
use indexmap::map::Keys;
use indexmap::map::{Iter as IndexMapIter, IterMut as IndexMapIterMut};
use indexmap::IndexMap;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt;
use std::hash::{self, Hash};
use std::iter::FromIterator;
use std::iter::{Cloned, DoubleEndedIterator};
use std::marker::PhantomData;
use std::mem;
use std::ops::{Deref, Index, IndexMut};
use std::slice::Iter;
#[doc = " A `GraphMap` with undirected edges."]
#[doc = ""]
#[doc = " For example, an edge between *1* and *2* is equivalent to an edge between"]
#[doc = " *2* and *1*."]
pub type UnGraphMap<N, E> = GraphMap<N, E, Undirected>;
#[doc = " A `GraphMap` with directed edges."]
#[doc = ""]
#[doc = " For example, an edge from *1* to *2* is distinct from an edge from *2* to"]
#[doc = " *1*."]
pub type DiGraphMap<N, E> = GraphMap<N, E, Directed>;
#[doc = " `GraphMap<N, E, Ty>` is a graph datastructure using an associative array"]
#[doc = " of its node weights `N`."]
#[doc = ""]
#[doc = " It uses an combined adjacency list and sparse adjacency matrix"]
#[doc = " representation, using **O(|V| + |E|)** space, and allows testing for edge"]
#[doc = " existence in constant time."]
#[doc = ""]
#[doc = " `GraphMap` is parameterized over:"]
#[doc = ""]
#[doc = " - Associated data `N` for nodes and `E` for edges, called *weights*."]
#[doc = " - The node weight `N` must implement `Copy` and will be used as node"]
#[doc = " identifier, duplicated into several places in the data structure."]
#[doc = " It must be suitable as a hash table key (implementing `Eq + Hash`)."]
#[doc = " The node type must also implement `Ord` so that the implementation can"]
#[doc = " order the pair (`a`, `b`) for an edge connecting any two nodes `a` and `b`."]
#[doc = " - `E` can be of arbitrary type."]
#[doc = " - Edge type `Ty` that determines whether the graph edges are directed or"]
#[doc = " undirected."]
#[doc = ""]
#[doc = " You can use the type aliases `UnGraphMap` and `DiGraphMap` for convenience."]
#[doc = ""]
#[doc = " `GraphMap` does not allow parallel edges, but self loops are allowed."]
#[doc = ""]
#[doc = " Depends on crate feature `graphmap` (default)."]
#[derive(Clone)]
pub struct GraphMap<N, E, Ty> {
    nodes: IndexMap<N, Vec<(N, CompactDirection)>>,
    edges: IndexMap<(N, N), E>,
    ty: PhantomData<Ty>,
}
impl<N: Eq + Hash + fmt::Debug, E: fmt::Debug, Ty: EdgeType> fmt::Debug for GraphMap<N, E, Ty> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.nodes.fmt(f)
    }
}
#[doc = " A trait group for `GraphMap`'s node identifier."]
pub trait NodeTrait: Copy + Ord + Hash {}
impl<N> NodeTrait for N where N: Copy + Ord + Hash {}
#[derive(Copy, Clone, Debug, PartialEq)]
enum CompactDirection {
    Outgoing,
    Incoming,
}
impl CompactDirection {
    #[doc = " Return the opposite `CompactDirection`."]
    #[inline]
    pub fn opposite(self) -> CompactDirection {
        match self {
            CompactDirection::Outgoing => CompactDirection::Incoming,
            CompactDirection::Incoming => CompactDirection::Outgoing,
        }
    }
}
impl From<Direction> for CompactDirection {
    fn from(d: Direction) -> Self {
        match d {
            Outgoing => CompactDirection::Outgoing,
            Incoming => CompactDirection::Incoming,
        }
    }
}
impl From<CompactDirection> for Direction {
    fn from(d: CompactDirection) -> Self {
        match d {
            CompactDirection::Outgoing => Outgoing,
            CompactDirection::Incoming => Incoming,
        }
    }
}
impl PartialEq<Direction> for CompactDirection {
    fn eq(&self, rhs: &Direction) -> bool {
        (*self as usize) == (*rhs as usize)
    }
}
#[cfg(feature = "serde-1")]
impl<N, E, Ty> serde::Serialize for GraphMap<N, E, Ty>
where
    Ty: EdgeType,
    N: NodeTrait + serde::Serialize,
    E: serde::Serialize,
    GraphMap<N, E, Ty>: Clone,
{
    #[doc = " Serializes the given `GraphMap` into the same format as the standard"]
    #[doc = " `Graph`. Needs feature `serde-1`."]
    #[doc = ""]
    #[doc = " Note: the graph has to be `Clone` for this to work."]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let cloned_graph: GraphMap<N, E, Ty> = GraphMap::clone(self);
        let equivalent_graph: Graph<N, E, Ty, u32> = cloned_graph.into_graph();
        equivalent_graph.serialize(serializer)
    }
}
#[cfg(feature = "serde-1")]
impl<'de, N, E, Ty> serde::Deserialize<'de> for GraphMap<N, E, Ty>
where
    Ty: EdgeType,
    N: NodeTrait + serde::Deserialize<'de>,
    E: Clone + serde::Deserialize<'de>,
{
    #[doc = " Deserializes into a new `GraphMap` from the same format as the standard"]
    #[doc = " `Graph`. Needs feature `serde-1`."]
    #[doc = ""]
    #[doc = " **Warning**: When deseralizing a graph that was not originally a `GraphMap`,"]
    #[doc = " the restrictions from [`from_graph`](#method.from_graph) apply."]
    #[doc = ""]
    #[doc = " Note: The edge weights have to be `Clone` for this to work."]
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let equivalent_graph: Graph<N, E, Ty, u32> = Graph::deserialize(deserializer)?;
        Ok(GraphMap::from_graph(equivalent_graph))
    }
}
impl<N, E, Ty> GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    #[doc = " Create a new `GraphMap`"]
    pub fn new() -> Self {
        Self::default()
    }
    #[doc = " Create a new `GraphMap` with estimated capacity."]
    pub fn with_capacity(nodes: usize, edges: usize) -> Self {
        GraphMap {
            nodes: IndexMap::with_capacity(nodes),
            edges: IndexMap::with_capacity(edges),
            ty: PhantomData,
        }
    }
    #[doc = " Return the current node and edge capacity of the graph."]
    pub fn capacity(&self) -> (usize, usize) {
        (self.nodes.capacity(), self.edges.capacity())
    }
    #[doc = " Use their natural order to map the node pair (a, b) to a canonical edge id."]
    #[inline]
    fn edge_key(a: N, b: N) -> (N, N) {
        if Ty::is_directed() || a <= b {
            (a, b)
        } else {
            (b, a)
        }
    }
    #[doc = " Whether the graph has directed edges."]
    pub fn is_directed(&self) -> bool {
        Ty::is_directed()
    }
    #[doc = " Create a new `GraphMap` from an iterable of edges."]
    #[doc = ""]
    #[doc = " Node values are taken directly from the list."]
    #[doc = " Edge weights `E` may either be specified in the list,"]
    #[doc = " or they are filled with default values."]
    #[doc = ""]
    #[doc = " Nodes are inserted automatically to match the edges."]
    #[doc = ""]
    #[doc = " ```"]
    #[doc = " use petgraph::graphmap::UnGraphMap;"]
    #[doc = ""]
    #[doc = " // Create a new undirected GraphMap."]
    #[doc = " // Use a type hint to have `()` be the edge weight type."]
    #[doc = " let gr = UnGraphMap::<_, ()>::from_edges(&["]
    #[doc = "     (0, 1), (0, 2), (0, 3),"]
    #[doc = "     (1, 2), (1, 3),"]
    #[doc = "     (2, 3),"]
    #[doc = " ]);"]
    #[doc = " ```"]
    pub fn from_edges<I>(iterable: I) -> Self
    where
        I: IntoIterator,
        I::Item: IntoWeightedEdge<E, NodeId = N>,
    {
        Self::from_iter(iterable)
    }
    #[doc = " Return the number of nodes in the graph."]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
    #[doc = " Return the number of edges in the graph."]
    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }
    #[doc = " Remove all nodes and edges"]
    pub fn clear(&mut self) {
        self.nodes.clear();
        self.edges.clear();
    }
    #[doc = " Add node `n` to the graph."]
    pub fn add_node(&mut self, n: N) -> N {
        self.nodes.entry(n).or_insert(Vec::new());
        n
    }
    #[doc = " Return `true` if node `n` was removed."]
    #[doc = ""]
    #[doc = " Computes in **O(V)** time, due to the removal of edges with other nodes."]
    pub fn remove_node(&mut self, n: N) -> bool {
        let links = match self.nodes.swap_remove(&n) {
            None => return false,
            Some(sus) => sus,
        };
        for (succ, dir) in links {
            let edge = if dir == CompactDirection::Outgoing {
                Self::edge_key(n, succ)
            } else {
                Self::edge_key(succ, n)
            };
            self.remove_single_edge(&succ, &n, dir.opposite());
            self.edges.swap_remove(&edge);
        }
        true
    }
    #[doc = " Return `true` if the node is contained in the graph."]
    pub fn contains_node(&self, n: N) -> bool {
        self.nodes.contains_key(&n)
    }
    #[doc = " Add an edge connecting `a` and `b` to the graph, with associated"]
    #[doc = " data `weight`. For a directed graph, the edge is directed from `a`"]
    #[doc = " to `b`."]
    #[doc = ""]
    #[doc = " Inserts nodes `a` and/or `b` if they aren't already part of the graph."]
    #[doc = ""]
    #[doc = " Return `None` if the edge did not previously exist, otherwise,"]
    #[doc = " the associated data is updated and the old value is returned"]
    #[doc = " as `Some(old_weight)`."]
    #[doc = ""]
    #[doc = " ```"]
    #[doc = " // Create a GraphMap with directed edges, and add one edge to it"]
    #[doc = " use petgraph::graphmap::DiGraphMap;"]
    #[doc = ""]
    #[doc = " let mut g = DiGraphMap::new();"]
    #[doc = " g.add_edge(\"x\", \"y\", -1);"]
    #[doc = " assert_eq!(g.node_count(), 2);"]
    #[doc = " assert_eq!(g.edge_count(), 1);"]
    #[doc = " assert!(g.contains_edge(\"x\", \"y\"));"]
    #[doc = " assert!(!g.contains_edge(\"y\", \"x\"));"]
    #[doc = " ```"]
    pub fn add_edge(&mut self, a: N, b: N, weight: E) -> Option<E> {
        if let old @ Some(_) = self.edges.insert(Self::edge_key(a, b), weight) {
            old
        } else {
            self.nodes
                .entry(a)
                .or_insert_with(|| Vec::with_capacity(1))
                .push((b, CompactDirection::Outgoing));
            if a != b {
                self.nodes
                    .entry(b)
                    .or_insert_with(|| Vec::with_capacity(1))
                    .push((a, CompactDirection::Incoming));
            }
            None
        }
    }
    #[doc = " Remove edge relation from a to b"]
    #[doc = ""]
    #[doc = " Return `true` if it did exist."]
    fn remove_single_edge(&mut self, a: &N, b: &N, dir: CompactDirection) -> bool {
        match self.nodes.get_mut(a) {
            None => false,
            Some(sus) => {
                if Ty::is_directed() {
                    match sus.iter().position(|elt| elt == &(*b, dir)) {
                        Some(index) => {
                            sus.swap_remove(index);
                            true
                        }
                        None => false,
                    }
                } else {
                    match sus.iter().position(|elt| &elt.0 == b) {
                        Some(index) => {
                            sus.swap_remove(index);
                            true
                        }
                        None => false,
                    }
                }
            }
        }
    }
    #[doc = " Remove edge from `a` to `b` from the graph and return the edge weight."]
    #[doc = ""]
    #[doc = " Return `None` if the edge didn't exist."]
    #[doc = ""]
    #[doc = " ```"]
    #[doc = " // Create a GraphMap with undirected edges, and add and remove an edge."]
    #[doc = " use petgraph::graphmap::UnGraphMap;"]
    #[doc = ""]
    #[doc = " let mut g = UnGraphMap::new();"]
    #[doc = " g.add_edge(\"x\", \"y\", -1);"]
    #[doc = ""]
    #[doc = " let edge_data = g.remove_edge(\"y\", \"x\");"]
    #[doc = " assert_eq!(edge_data, Some(-1));"]
    #[doc = " assert_eq!(g.edge_count(), 0);"]
    #[doc = " ```"]
    pub fn remove_edge(&mut self, a: N, b: N) -> Option<E> {
        let exist1 = self.remove_single_edge(&a, &b, CompactDirection::Outgoing);
        let exist2 = if a != b {
            self.remove_single_edge(&b, &a, CompactDirection::Incoming)
        } else {
            exist1
        };
        let weight = self.edges.remove(&Self::edge_key(a, b));
        debug_assert!(exist1 == exist2 && exist1 == weight.is_some());
        weight
    }
    #[doc = " Return `true` if the edge connecting `a` with `b` is contained in the graph."]
    pub fn contains_edge(&self, a: N, b: N) -> bool {
        self.edges.contains_key(&Self::edge_key(a, b))
    }
    #[doc = " Return an iterator over the nodes of the graph."]
    #[doc = ""]
    #[doc = " Iterator element type is `N`."]
    pub fn nodes(&self) -> Nodes<N> {
        Nodes {
            iter: self.nodes.keys().cloned(),
        }
    }
    #[doc = " Return an iterator of all nodes with an edge starting from `a`."]
    #[doc = ""]
    #[doc = " - `Directed`: Outgoing edges from `a`."]
    #[doc = " - `Undirected`: All edges from or to `a`."]
    #[doc = ""]
    #[doc = " Produces an empty iterator if the node doesn't exist.<br>"]
    #[doc = " Iterator element type is `N`."]
    pub fn neighbors(&self, a: N) -> Neighbors<N, Ty> {
        Neighbors {
            iter: match self.nodes.get(&a) {
                Some(neigh) => neigh.iter(),
                None => [].iter(),
            },
            ty: self.ty,
        }
    }
    #[doc = " Return an iterator of all neighbors that have an edge between them and"]
    #[doc = " `a`, in the specified direction."]
    #[doc = " If the graph's edges are undirected, this is equivalent to *.neighbors(a)*."]
    #[doc = ""]
    #[doc = " - `Directed`, `Outgoing`: All edges from `a`."]
    #[doc = " - `Directed`, `Incoming`: All edges to `a`."]
    #[doc = " - `Undirected`: All edges from or to `a`."]
    #[doc = ""]
    #[doc = " Produces an empty iterator if the node doesn't exist.<br>"]
    #[doc = " Iterator element type is `N`."]
    pub fn neighbors_directed(&self, a: N, dir: Direction) -> NeighborsDirected<N, Ty> {
        NeighborsDirected {
            iter: match self.nodes.get(&a) {
                Some(neigh) => neigh.iter(),
                None => [].iter(),
            },
            start_node: a,
            dir,
            ty: self.ty,
        }
    }
    #[doc = " Return an iterator of target nodes with an edge starting from `a`,"]
    #[doc = " paired with their respective edge weights."]
    #[doc = ""]
    #[doc = " - `Directed`: Outgoing edges from `a`."]
    #[doc = " - `Undirected`: All edges from or to `a`."]
    #[doc = ""]
    #[doc = " Produces an empty iterator if the node doesn't exist.<br>"]
    #[doc = " Iterator element type is `(N, N, &E)`."]
    pub fn edges(&self, a: N) -> Edges<N, E, Ty> {
        Edges {
            from: a,
            iter: self.neighbors(a),
            edges: &self.edges,
        }
    }
    #[doc = " Return an iterator of target nodes with an edge starting from `a`,"]
    #[doc = " paired with their respective edge weights."]
    #[doc = ""]
    #[doc = " - `Directed`, `Outgoing`: All edges from `a`."]
    #[doc = " - `Directed`, `Incoming`: All edges to `a`."]
    #[doc = " - `Undirected`, `Outgoing`: All edges connected to `a`, with `a` being the source of each"]
    #[doc = "   edge."]
    #[doc = " - `Undirected`, `Incoming`: All edges connected to `a`, with `a` being the target of each"]
    #[doc = "   edge."]
    #[doc = ""]
    #[doc = " Produces an empty iterator if the node doesn't exist.<br>"]
    #[doc = " Iterator element type is `(N, N, &E)`."]
    pub fn edges_directed(&self, a: N, dir: Direction) -> EdgesDirected<N, E, Ty> {
        EdgesDirected {
            from: a,
            iter: self.neighbors_directed(a, dir),
            dir,
            edges: &self.edges,
        }
    }
    #[doc = " Return a reference to the edge weight connecting `a` with `b`, or"]
    #[doc = " `None` if the edge does not exist in the graph."]
    pub fn edge_weight(&self, a: N, b: N) -> Option<&E> {
        self.edges.get(&Self::edge_key(a, b))
    }
    #[doc = " Return a mutable reference to the edge weight connecting `a` with `b`, or"]
    #[doc = " `None` if the edge does not exist in the graph."]
    pub fn edge_weight_mut(&mut self, a: N, b: N) -> Option<&mut E> {
        self.edges.get_mut(&Self::edge_key(a, b))
    }
    #[doc = " Return an iterator over all edges of the graph with their weight in arbitrary order."]
    #[doc = ""]
    #[doc = " Iterator element type is `(N, N, &E)`"]
    pub fn all_edges(&self) -> AllEdges<N, E, Ty> {
        AllEdges {
            inner: self.edges.iter(),
            ty: self.ty,
        }
    }
    #[doc = " Return an iterator over all edges of the graph in arbitrary order, with a mutable reference"]
    #[doc = " to their weight."]
    #[doc = ""]
    #[doc = " Iterator element type is `(N, N, &mut E)`"]
    pub fn all_edges_mut(&mut self) -> AllEdgesMut<N, E, Ty> {
        AllEdgesMut {
            inner: self.edges.iter_mut(),
            ty: self.ty,
        }
    }
    #[doc = " Return a `Graph` that corresponds to this `GraphMap`."]
    #[doc = ""]
    #[doc = " 1. Note that node and edge indices in the `Graph` have nothing in common"]
    #[doc = "    with the `GraphMap`s node weights `N`. The node weights `N` are used as"]
    #[doc = "    node weights in the resulting `Graph`, too."]
    #[doc = " 2. Note that the index type is user-chosen."]
    #[doc = ""]
    #[doc = " Computes in **O(|V| + |E|)** time (average)."]
    #[doc = ""]
    #[doc = " **Panics** if the number of nodes or edges does not fit with"]
    #[doc = " the resulting graph's index type."]
    pub fn into_graph<Ix>(self) -> Graph<N, E, Ty, Ix>
    where
        Ix: crate::graph::IndexType,
    {
        let mut gr = Graph::with_capacity(self.node_count(), self.edge_count());
        for (&node, _) in &self.nodes {
            gr.add_node(node);
        }
        for ((a, b), edge_weight) in self.edges {
            let (ai, _, _) = self.nodes.get_full(&a).unwrap();
            let (bi, _, _) = self.nodes.get_full(&b).unwrap();
            gr.add_edge(node_index(ai), node_index(bi), edge_weight);
        }
        gr
    }
    #[doc = " Creates a `GraphMap` that corresponds to the given `Graph`."]
    #[doc = ""]
    #[doc = " **Warning**: Nodes with the same weight are merged and only the last parallel edge"]
    #[doc = " is kept. Node and edge indices of the `Graph` are lost. Only use this function"]
    #[doc = " if the node weights are distinct and there are no parallel edges."]
    #[doc = ""]
    #[doc = " Computes in **O(|V| + |E|)** time (average)."]
    pub fn from_graph<Ix>(graph: Graph<N, E, Ty, Ix>) -> Self
    where
        Ix: crate::graph::IndexType,
        E: Clone,
    {
        let mut new_graph: GraphMap<N, E, Ty> =
            GraphMap::with_capacity(graph.node_count(), graph.edge_count());
        for node in graph.raw_nodes() {
            new_graph.add_node(node.weight);
        }
        for edge in graph.edge_indices() {
            let (a, b) = graph.edge_endpoints(edge).unwrap();
            new_graph.add_edge(
                *graph.node_weight(a).unwrap(),
                *graph.node_weight(b).unwrap(),
                graph.edge_weight(edge).unwrap().clone(),
            );
        }
        new_graph
    }
}
#[doc = " Create a new `GraphMap` from an iterable of edges."]
impl<N, E, Ty, Item> FromIterator<Item> for GraphMap<N, E, Ty>
where
    Item: IntoWeightedEdge<E, NodeId = N>,
    N: NodeTrait,
    Ty: EdgeType,
{
    fn from_iter<I>(iterable: I) -> Self
    where
        I: IntoIterator<Item = Item>,
    {
        let iter = iterable.into_iter();
        let (low, _) = iter.size_hint();
        let mut g = Self::with_capacity(0, low);
        g.extend(iter);
        g
    }
}
#[doc = " Extend the graph from an iterable of edges."]
#[doc = ""]
#[doc = " Nodes are inserted automatically to match the edges."]
impl<N, E, Ty, Item> Extend<Item> for GraphMap<N, E, Ty>
where
    Item: IntoWeightedEdge<E, NodeId = N>,
    N: NodeTrait,
    Ty: EdgeType,
{
    fn extend<I>(&mut self, iterable: I)
    where
        I: IntoIterator<Item = Item>,
    {
        let iter = iterable.into_iter();
        let (low, _) = iter.size_hint();
        self.edges.reserve(low);
        for elt in iter {
            let (source, target, weight) = elt.into_weighted_edge();
            self.add_edge(source, target, weight);
        }
    }
}
iterator_wrap! { impl (Iterator DoubleEndedIterator ExactSizeIterator) for # [derive (Debug , Clone)] struct Nodes <'a , N > where { N : 'a + NodeTrait } item : N , iter : Cloned < Keys <'a , N , Vec < (N , CompactDirection) >>>, }
#[derive(Debug, Clone)]
pub struct Neighbors<'a, N, Ty = Undirected>
where
    N: 'a,
    Ty: EdgeType,
{
    iter: Iter<'a, (N, CompactDirection)>,
    ty: PhantomData<Ty>,
}
impl<'a, N, Ty> Iterator for Neighbors<'a, N, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type Item = N;
    fn next(&mut self) -> Option<N> {
        if Ty::is_directed() {
            (&mut self.iter)
                .filter_map(|&(n, dir)| if dir == Outgoing { Some(n) } else { None })
                .next()
        } else {
            self.iter.next().map(|&(n, _)| n)
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.iter.size_hint();
        if Ty::is_directed() {
            (0, upper)
        } else {
            (lower, upper)
        }
    }
}
#[derive(Debug, Clone)]
pub struct NeighborsDirected<'a, N, Ty>
where
    N: 'a,
    Ty: EdgeType,
{
    iter: Iter<'a, (N, CompactDirection)>,
    start_node: N,
    dir: Direction,
    ty: PhantomData<Ty>,
}
impl<'a, N, Ty> Iterator for NeighborsDirected<'a, N, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type Item = N;
    fn next(&mut self) -> Option<N> {
        if Ty::is_directed() {
            let self_dir = self.dir;
            let start_node = self.start_node;
            (&mut self.iter)
                .filter_map(move |&(n, dir)| {
                    if dir == self_dir || n == start_node {
                        Some(n)
                    } else {
                        None
                    }
                })
                .next()
        } else {
            self.iter.next().map(|&(n, _)| n)
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let (lower, upper) = self.iter.size_hint();
        if Ty::is_directed() {
            (0, upper)
        } else {
            (lower, upper)
        }
    }
}
#[derive(Debug, Clone)]
pub struct Edges<'a, N, E: 'a, Ty>
where
    N: 'a + NodeTrait,
    Ty: EdgeType,
{
    from: N,
    edges: &'a IndexMap<(N, N), E>,
    iter: Neighbors<'a, N, Ty>,
}
impl<'a, N, E, Ty> Iterator for Edges<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    type Item = (N, N, &'a E);
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|b| {
            let a = self.from;
            match self.edges.get(&GraphMap::<N, E, Ty>::edge_key(a, b)) {
                None => unreachable!(),
                Some(edge) => (a, b, edge),
            }
        })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
#[derive(Debug, Clone)]
pub struct EdgesDirected<'a, N, E: 'a, Ty>
where
    N: 'a + NodeTrait,
    Ty: EdgeType,
{
    from: N,
    dir: Direction,
    edges: &'a IndexMap<(N, N), E>,
    iter: NeighborsDirected<'a, N, Ty>,
}
impl<'a, N, E, Ty> Iterator for EdgesDirected<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    type Item = (N, N, &'a E);
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|mut b| {
            let mut a = self.from;
            if self.dir == Direction::Incoming {
                mem::swap(&mut a, &mut b);
            }
            match self.edges.get(&GraphMap::<N, E, Ty>::edge_key(a, b)) {
                None => unreachable!(),
                Some(edge) => (a, b, edge),
            }
        })
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
#[derive(Debug, Clone)]
pub struct AllEdges<'a, N, E: 'a, Ty>
where
    N: 'a + NodeTrait,
{
    inner: IndexMapIter<'a, (N, N), E>,
    ty: PhantomData<Ty>,
}
impl<'a, N, E, Ty> Iterator for AllEdges<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    type Item = (N, N, &'a E);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(&(a, b), v)| (a, b, v))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
    fn count(self) -> usize {
        self.inner.count()
    }
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.bar(n)
    }
    fn last(self) -> Option<Self::Item> {
        self.inner
            .last()
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
}
impl<'a, N, E, Ty> AllEdges<'a, N, E, Ty>
where
    E: 'a,
    N: 'a + NodeTrait,
    Ty: EdgeType,
{
    fn bar<'lt0>(&mut self, n: usize) -> Option<(N, N, &'lt0 E)>
    where
        'a: 'lt0,
    {
        self.inner
            .nth(n)
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
}
impl<'a, N, E, Ty> DoubleEndedIterator for AllEdges<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
}
pub struct AllEdgesMut<'a, N, E: 'a, Ty>
where
    N: 'a + NodeTrait,
{
    inner: IndexMapIterMut<'a, (N, N), E>,
    ty: PhantomData<Ty>,
}
impl<'a, N, E, Ty> Iterator for AllEdgesMut<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    type Item = (N, N, &'a mut E);
    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
    fn count(self) -> usize {
        self.inner.count()
    }
    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.inner
            .nth(n)
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
    fn last(self) -> Option<Self::Item> {
        self.inner
            .last()
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
}
impl<'a, N, E, Ty> DoubleEndedIterator for AllEdgesMut<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner
            .next_back()
            .map(|(&(n1, n2), weight)| (n1, n2, weight))
    }
}
#[doc = " Index `GraphMap` by node pairs to access edge weights."]
impl<N, E, Ty> Index<(N, N)> for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type Output = E;
    fn index(&self, index: (N, N)) -> &E {
        let index = Self::edge_key(index.0, index.1);
        self.edge_weight(index.0, index.1)
            .expect("GraphMap::index: no such edge")
    }
}
#[doc = " Index `GraphMap` by node pairs to access edge weights."]
impl<N, E, Ty> IndexMut<(N, N)> for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    fn index_mut(&mut self, index: (N, N)) -> &mut E {
        let index = Self::edge_key(index.0, index.1);
        self.edge_weight_mut(index.0, index.1)
            .expect("GraphMap::index: no such edge")
    }
}
#[doc = " Create a new empty `GraphMap`."]
impl<N, E, Ty> Default for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    fn default() -> Self {
        GraphMap::with_capacity(0, 0)
    }
}
#[doc = " A reference that is hashed and compared by its pointer value."]
#[doc = ""]
#[doc = " `Ptr` is used for certain configurations of `GraphMap`,"]
#[doc = " in particular in the combination where the node type for"]
#[doc = " `GraphMap` is something of type for example `Ptr(&Cell<T>)`,"]
#[doc = " with the `Cell<T>` being `TypedArena` allocated."]
pub struct Ptr<'b, T: 'b>(pub &'b T);
impl<'b, T> Copy for Ptr<'b, T> {}
impl<'b, T> Clone for Ptr<'b, T> {
    fn clone(&self) -> Self {
        *self
    }
}
fn ptr_eq<T>(a: *const T, b: *const T) -> bool {
    a == b
}
impl<'b, T> PartialEq for Ptr<'b, T> {
    #[doc = " Ptr compares by pointer equality, i.e if they point to the same value"]
    fn eq(&self, other: &Ptr<'b, T>) -> bool {
        ptr_eq(self.0, other.0)
    }
}
impl<'b, T> PartialOrd for Ptr<'b, T> {
    fn partial_cmp(&self, other: &Ptr<'b, T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<'b, T> Ord for Ptr<'b, T> {
    #[doc = " Ptr is ordered by pointer value, i.e. an arbitrary but stable and total order."]
    fn cmp(&self, other: &Ptr<'b, T>) -> Ordering {
        let a: *const T = self.0;
        let b: *const T = other.0;
        a.cmp(&b)
    }
}
impl<'b, T> Deref for Ptr<'b, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
    }
}
impl<'b, T> Eq for Ptr<'b, T> {}
impl<'b, T> Hash for Ptr<'b, T> {
    fn hash<H: hash::Hasher>(&self, st: &mut H) {
        let ptr = (self.0) as *const T;
        ptr.hash(st)
    }
}
impl<'b, T: fmt::Debug> fmt::Debug for Ptr<'b, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}
#[derive(Debug, Clone)]
pub struct NodeIdentifiers<'a, N, E: 'a, Ty>
where
    N: 'a + NodeTrait,
{
    iter: IndexMapIter<'a, N, Vec<(N, CompactDirection)>>,
    ty: PhantomData<Ty>,
    edge_ty: PhantomData<E>,
}
impl<'a, N, E, Ty> Iterator for NodeIdentifiers<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    type Item = N;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(&n, _)| n)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
#[derive(Debug, Clone)]
pub struct NodeReferences<'a, N, E: 'a, Ty>
where
    N: 'a + NodeTrait,
{
    iter: IndexMapIter<'a, N, Vec<(N, CompactDirection)>>,
    ty: PhantomData<Ty>,
    edge_ty: PhantomData<E>,
}
impl<'a, N, E, Ty> Iterator for NodeReferences<'a, N, E, Ty>
where
    N: 'a + NodeTrait,
    E: 'a,
    Ty: EdgeType,
{
    type Item = (N, &'a N);
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(n, _)| (*n, n))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.iter.size_hint()
    }
}
impl<N, E, Ty> visit::GraphBase for GraphMap<N, E, Ty>
where
    N: Copy + PartialEq,
{
    type NodeId = N;
    type EdgeId = (N, N);
}
impl<N, E, Ty> visit::Data for GraphMap<N, E, Ty>
where
    N: Copy + PartialEq,
    Ty: EdgeType,
{
    type NodeWeight = N;
    type EdgeWeight = E;
}
impl<N, E, Ty> visit::Visitable for GraphMap<N, E, Ty>
where
    N: Copy + Ord + Hash,
    Ty: EdgeType,
{
    type Map = HashSet<N>;
    fn visit_map(&self) -> HashSet<N> {
        HashSet::with_capacity(self.node_count())
    }
    fn reset_map(&self, map: &mut Self::Map) {
        map.clear();
    }
}
impl<N, E, Ty> visit::GraphProp for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type EdgeType = Ty;
}
impl<'a, N, E, Ty> visit::IntoNodeReferences for &'a GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type NodeRef = (N, &'a N);
    type NodeReferences = NodeReferences<'a, N, E, Ty>;
    fn node_references(self) -> Self::NodeReferences {
        NodeReferences {
            iter: self.nodes.iter(),
            ty: self.ty,
            edge_ty: PhantomData,
        }
    }
}
impl<'a, N, E: 'a, Ty> visit::IntoNodeIdentifiers for &'a GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type NodeIdentifiers = NodeIdentifiers<'a, N, E, Ty>;
    fn node_identifiers(self) -> Self::NodeIdentifiers {
        NodeIdentifiers {
            iter: self.nodes.iter(),
            ty: self.ty,
            edge_ty: PhantomData,
        }
    }
}
impl<N, E, Ty> visit::NodeCount for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    fn node_count(&self) -> usize {
        (*self).node_count()
    }
}
impl<N, E, Ty> visit::NodeIndexable for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    fn node_bound(&self) -> usize {
        self.node_count()
    }
    fn to_index(&self, ix: Self::NodeId) -> usize {
        let (i, _, _) = self.nodes.get_full(&ix).unwrap();
        i
    }
    fn from_index(&self, ix: usize) -> Self::NodeId {
        assert!(
            ix < self.nodes.len(),
            "The requested index {} is out-of-bounds.",
            ix
        );
        let (&key, _) = self.nodes.get_index(ix).unwrap();
        key
    }
}
impl<N, E, Ty> visit::NodeCompactIndexable for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
}
impl<'a, N: 'a, E, Ty> visit::IntoNeighbors for &'a GraphMap<N, E, Ty>
where
    N: Copy + Ord + Hash,
    Ty: EdgeType,
{
    type Neighbors = Neighbors<'a, N, Ty>;
    fn neighbors(self, n: Self::NodeId) -> Self::Neighbors {
        self.neighbors(n)
    }
}
impl<'a, N: 'a, E, Ty> visit::IntoNeighborsDirected for &'a GraphMap<N, E, Ty>
where
    N: Copy + Ord + Hash,
    Ty: EdgeType,
{
    type NeighborsDirected = NeighborsDirected<'a, N, Ty>;
    fn neighbors_directed(self, n: N, dir: Direction) -> Self::NeighborsDirected {
        self.neighbors_directed(n, dir)
    }
}
impl<N, E, Ty> visit::EdgeIndexable for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    fn edge_bound(&self) -> usize {
        self.edge_count()
    }
    fn to_index(&self, ix: Self::EdgeId) -> usize {
        let (i, _, _) = self.edges.get_full(&ix).unwrap();
        i
    }
    fn from_index(&self, ix: usize) -> Self::EdgeId {
        assert!(
            ix < self.edges.len(),
            "The requested index {} is out-of-bounds.",
            ix
        );
        let (&key, _) = self.edges.get_index(ix).unwrap();
        key
    }
}
impl<'a, N: 'a, E: 'a, Ty> visit::IntoEdges for &'a GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type Edges = Edges<'a, N, E, Ty>;
    fn edges(self, a: Self::NodeId) -> Self::Edges {
        self.edges(a)
    }
}
impl<'a, N: 'a, E: 'a, Ty> visit::IntoEdgesDirected for &'a GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type EdgesDirected = EdgesDirected<'a, N, E, Ty>;
    fn edges_directed(self, a: Self::NodeId, dir: Direction) -> Self::EdgesDirected {
        self.edges_directed(a, dir)
    }
}
impl<'a, N: 'a, E: 'a, Ty> visit::IntoEdgeReferences for &'a GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    type EdgeRef = (N, N, &'a E);
    type EdgeReferences = AllEdges<'a, N, E, Ty>;
    fn edge_references(self) -> Self::EdgeReferences {
        self.all_edges()
    }
}
impl<N, E, Ty> visit::EdgeCount for GraphMap<N, E, Ty>
where
    N: NodeTrait,
    Ty: EdgeType,
{
    #[inline]
    fn edge_count(&self) -> usize {
        self.edge_count()
    }
}
#[doc = " The `GraphMap` keeps an adjacency matrix internally."]
impl<N, E, Ty> visit::GetAdjacencyMatrix for GraphMap<N, E, Ty>
where
    N: Copy + Ord + Hash,
    Ty: EdgeType,
{
    type AdjMatrix = ();
    #[inline]
    fn adjacency_matrix(&self) {}
    #[inline]
    fn is_adjacent(&self, _: &(), a: N, b: N) -> bool {
        self.contains_edge(a, b)
    }
}
