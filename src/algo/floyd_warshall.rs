use crate::algo::{BoundedMeasure, NegativeCycle};
use crate::visit::{
    EdgeRef, GraphProp, IntoEdgeReferences, IntoNodeIdentifiers, NodeCompactIndexable,
};
use std::collections::HashMap;
use std::hash::Hash;
#[allow(clippy::type_complexity, clippy::needless_range_loop)]
#[doc = " \\[Generic\\] [Floyd–Warshall algorithm](https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm) is an algorithm for all pairs shortest path problem"]
#[doc = ""]
#[doc = " Compute shortest paths in a weighted graph with positive or negative edge weights (but with no negative cycles)"]
#[doc = ""]
#[doc = " # Arguments"]
#[doc = " * `graph`: graph with no negative cycle"]
#[doc = " * `edge_cost`: closure that returns cost of a particular edge"]
#[doc = ""]
#[doc = " # Returns"]
#[doc = " * `Ok`: (if graph contains no negative cycle) a hashmap containing all pairs shortest paths"]
#[doc = " * `Err`: if graph contains negative cycle."]
#[doc = ""]
#[doc = " # Examples"]
#[doc = " ```rust"]
#[doc = " use petgraph::{prelude::*, Graph, Directed};"]
#[doc = " use petgraph::algo::floyd_warshall;"]
#[doc = " use std::collections::HashMap;"]
#[doc = ""]
#[doc = " let mut graph: Graph<(), (), Directed> = Graph::new();"]
#[doc = " let a = graph.add_node(());"]
#[doc = " let b = graph.add_node(());"]
#[doc = " let c = graph.add_node(());"]
#[doc = " let d = graph.add_node(());"]
#[doc = ""]
#[doc = " graph.extend_with_edges(&["]
#[doc = "    (a, b),"]
#[doc = "    (a, c),"]
#[doc = "    (a, d),"]
#[doc = "    (b, c),"]
#[doc = "    (b, d),"]
#[doc = "    (c, d)"]
#[doc = " ]);"]
#[doc = ""]
#[doc = " let weight_map: HashMap<(NodeIndex, NodeIndex), i32> = ["]
#[doc = "    ((a, a), 0), ((a, b), 1), ((a, c), 4), ((a, d), 10),"]
#[doc = "    ((b, b), 0), ((b, c), 2), ((b, d), 2),"]
#[doc = "    ((c, c), 0), ((c, d), 2)"]
#[doc = " ].iter().cloned().collect();"]
#[doc = " //     ----- b --------"]
#[doc = " //    |      ^         | 2"]
#[doc = " //    |    1 |    4    v"]
#[doc = " //  2 |      a ------> c"]
#[doc = " //    |   10 |         | 2"]
#[doc = " //    |      v         v"]
#[doc = " //     --->  d <-------"]
#[doc = ""]
#[doc = " let inf = std::i32::MAX;"]
#[doc = " let expected_res: HashMap<(NodeIndex, NodeIndex), i32> = ["]
#[doc = "    ((a, a), 0), ((a, b), 1), ((a, c), 3), ((a, d), 3),"]
#[doc = "    ((b, a), inf), ((b, b), 0), ((b, c), 2), ((b, d), 2),"]
#[doc = "    ((c, a), inf), ((c, b), inf), ((c, c), 0), ((c, d), 2),"]
#[doc = "    ((d, a), inf), ((d, b), inf), ((d, c), inf), ((d, d), 0),"]
#[doc = " ].iter().cloned().collect();"]
#[doc = ""]
#[doc = ""]
#[doc = " let res = floyd_warshall(&graph, |edge| {"]
#[doc = "     if let Some(weight) = weight_map.get(&(edge.source(), edge.target())) {"]
#[doc = "         *weight"]
#[doc = "     } else {"]
#[doc = "         inf"]
#[doc = "     }"]
#[doc = " }).unwrap();"]
#[doc = ""]
#[doc = " let nodes = [a, b, c, d];"]
#[doc = " for node1 in &nodes {"]
#[doc = "     for node2 in &nodes {"]
#[doc = "         assert_eq!(res.get(&(*node1, *node2)).unwrap(), expected_res.get(&(*node1, *node2)).unwrap());"]
#[doc = "     }"]
#[doc = " }"]
#[doc = " ```"]
pub fn floyd_warshall<G, F, K>(
    graph: G,
    mut edge_cost: F,
) -> Result<HashMap<(G::NodeId, G::NodeId), K>, NegativeCycle>
where
    G: NodeCompactIndexable + IntoEdgeReferences + IntoNodeIdentifiers + GraphProp,
    G::NodeId: Eq + Hash,
    F: FnMut(G::EdgeRef) -> K,
    K: BoundedMeasure + Copy,
{
    let num_of_nodes = graph.node_count();
    let mut dist = vec![vec![K::max(); num_of_nodes]; num_of_nodes];
    for edge in graph.edge_references() {
        dist[graph.to_index(edge.source())][graph.to_index(edge.target())] = edge_cost(edge);
        if !graph.is_directed() {
            dist[graph.to_index(edge.target())][graph.to_index(edge.source())] = edge_cost(edge);
        }
    }
    for node in graph.node_identifiers() {
        dist[graph.to_index(node)][graph.to_index(node)] = K::default();
    }
    bar(&num_of_nodes, &mut dist);
    for i in 0..num_of_nodes {
        if dist[i][i] < K::default() {
            return Err(NegativeCycle(()));
        }
    }
    let mut distance_map: HashMap<(G::NodeId, G::NodeId), K> =
        HashMap::with_capacity(num_of_nodes * num_of_nodes);
    for i in 0..num_of_nodes {
        for j in 0..num_of_nodes {
            distance_map.insert((graph.from_index(i), graph.from_index(j)), dist[i][j]);
        }
    }
    Ok(distance_map)
}
fn bar<K>(num_of_nodes: &usize, dist: &mut Vec<Vec<K>>)
where
    K: BoundedMeasure + Copy,
{
    for k in 0..(*num_of_nodes) {
        for i in 0..(*num_of_nodes) {
            for j in 0..(*num_of_nodes) {
                let (result, overflow) = dist[i][k].overflowing_add(dist[k][j]);
                if !overflow && dist[i][j] > result {
                    dist[i][j] = result;
                }
            }
        }
    }
}
