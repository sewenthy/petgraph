use self::semantic::EdgeMatcher;
use self::semantic::NoSemanticMatch;
use self::semantic::NodeMatcher;
use self::state::Vf2State;
use crate::data::DataMap;
use crate::visit::EdgeCount;
use crate::visit::EdgeRef;
use crate::visit::GetAdjacencyMatrix;
use crate::visit::GraphBase;
use crate::visit::GraphProp;
use crate::visit::IntoEdgesDirected;
use crate::visit::IntoNeighborsDirected;
use crate::visit::NodeCompactIndexable;
use crate::{Incoming, Outgoing};
use std::convert::TryFrom;
mod state {
    use super::*;
    #[derive(Debug)]
    pub struct Vf2State<'a, G: GetAdjacencyMatrix> {
        #[doc = " A reference to the graph this state was built from."]
        pub graph: &'a G,
        #[doc = " The current mapping M(s) of nodes from G0 → G1 and G1 → G0,"]
        #[doc = " `usize::MAX` for no mapping."]
        pub mapping: Vec<usize>,
        #[doc = " out[i] is non-zero if i is in either M_0(s) or Tout_0(s)"]
        #[doc = " These are all the next vertices that are not mapped yet, but"]
        #[doc = " have an outgoing edge from the mapping."]
        out: Vec<usize>,
        #[doc = " ins[i] is non-zero if i is in either M_0(s) or Tin_0(s)"]
        #[doc = " These are all the incoming vertices, those not mapped yet, but"]
        #[doc = " have an edge from them into the mapping."]
        #[doc = " Unused if graph is undirected -- it's identical with out in that case."]
        ins: Vec<usize>,
        pub out_size: usize,
        pub ins_size: usize,
        pub adjacency_matrix: G::AdjMatrix,
        generation: usize,
    }
    impl<'a, G> Vf2State<'a, G>
    where
        G: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
    {
        pub fn new(g: &'a G) -> Self {
            let c0 = g.node_count();
            Vf2State {
                graph: g,
                mapping: vec![std::usize::MAX; c0],
                out: vec![0; c0],
                ins: vec![0; c0 * (g.is_directed() as usize)],
                out_size: 0,
                ins_size: 0,
                adjacency_matrix: g.adjacency_matrix(),
                generation: 0,
            }
        }
        #[doc = " Return **true** if we have a complete mapping"]
        pub fn is_complete(&self) -> bool {
            self.generation == self.mapping.len()
        }
        #[doc = " Add mapping **from** <-> **to** to the state."]
        pub fn push_mapping(&mut self, from: G::NodeId, to: usize) {
            self.generation += 1;
            self.mapping[self.graph.to_index(from)] = to;
            self.bar(&from);
            if self.graph.is_directed() {
                for ix in self.graph.neighbors_directed(from, Incoming) {
                    if self.ins[self.graph.to_index(ix)] == 0 {
                        self.ins[self.graph.to_index(ix)] = self.generation;
                        self.ins_size += 1;
                    }
                }
            }
        }
        fn bar<'lt0>(&mut self, from: &'lt0 <G as GraphBase>::NodeId) {
            for ix in self.graph.neighbors_directed((*from), Outgoing) {
                if self.out[self.graph.to_index(ix)] == 0 {
                    self.out[self.graph.to_index(ix)] = self.generation;
                    self.out_size += 1;
                }
            }
        }
        #[doc = " Restore the state to before the last added mapping"]
        pub fn pop_mapping(&mut self, from: G::NodeId) {
            self.mapping[self.graph.to_index(from)] = std::usize::MAX;
            for ix in self.graph.neighbors_directed(from, Outgoing) {
                if self.out[self.graph.to_index(ix)] == self.generation {
                    self.out[self.graph.to_index(ix)] = 0;
                    self.out_size -= 1;
                }
            }
            if self.graph.is_directed() {
                for ix in self.graph.neighbors_directed(from, Incoming) {
                    if self.ins[self.graph.to_index(ix)] == self.generation {
                        self.ins[self.graph.to_index(ix)] = 0;
                        self.ins_size -= 1;
                    }
                }
            }
            self.generation -= 1;
        }
        #[doc = " Find the next (least) node in the Tout set."]
        pub fn next_out_index(&self, from_index: usize) -> Option<usize> {
            self.out[from_index..]
                .iter()
                .enumerate()
                .find(move |&(index, &elt)| {
                    elt > 0 && self.mapping[from_index + index] == std::usize::MAX
                })
                .map(|(index, _)| index)
        }
        #[doc = " Find the next (least) node in the Tin set."]
        pub fn next_in_index(&self, from_index: usize) -> Option<usize> {
            if !self.graph.is_directed() {
                return None;
            }
            self.ins[from_index..]
                .iter()
                .enumerate()
                .find(move |&(index, &elt)| {
                    elt > 0 && self.mapping[from_index + index] == std::usize::MAX
                })
                .map(|(index, _)| index)
        }
        #[doc = " Find the next (least) node in the N - M set."]
        pub fn next_rest_index(&self, from_index: usize) -> Option<usize> {
            self.mapping[from_index..]
                .iter()
                .enumerate()
                .find(|&(_, &elt)| elt == std::usize::MAX)
                .map(|(index, _)| index)
        }
    }
}
mod semantic {
    use super::*;
    pub struct NoSemanticMatch;
    pub trait NodeMatcher<G0: GraphBase, G1: GraphBase> {
        fn enabled() -> bool;
        fn eq(&mut self, _g0: &G0, _g1: &G1, _n0: G0::NodeId, _n1: G1::NodeId) -> bool;
    }
    impl<G0: GraphBase, G1: GraphBase> NodeMatcher<G0, G1> for NoSemanticMatch {
        #[inline]
        fn enabled() -> bool {
            false
        }
        #[inline]
        fn eq(&mut self, _g0: &G0, _g1: &G1, _n0: G0::NodeId, _n1: G1::NodeId) -> bool {
            true
        }
    }
    impl<G0, G1, F> NodeMatcher<G0, G1> for F
    where
        G0: GraphBase + DataMap,
        G1: GraphBase + DataMap,
        F: FnMut(&G0::NodeWeight, &G1::NodeWeight) -> bool,
    {
        #[inline]
        fn enabled() -> bool {
            true
        }
        #[inline]
        fn eq(&mut self, g0: &G0, g1: &G1, n0: G0::NodeId, n1: G1::NodeId) -> bool {
            if let (Some(x), Some(y)) = (g0.node_weight(n0), g1.node_weight(n1)) {
                self(x, y)
            } else {
                false
            }
        }
    }
    pub trait EdgeMatcher<G0: GraphBase, G1: GraphBase> {
        fn enabled() -> bool;
        fn eq(
            &mut self,
            _g0: &G0,
            _g1: &G1,
            e0: (G0::NodeId, G0::NodeId),
            e1: (G1::NodeId, G1::NodeId),
        ) -> bool;
    }
    impl<G0: GraphBase, G1: GraphBase> EdgeMatcher<G0, G1> for NoSemanticMatch {
        #[inline]
        fn enabled() -> bool {
            false
        }
        #[inline]
        fn eq(
            &mut self,
            _g0: &G0,
            _g1: &G1,
            _e0: (G0::NodeId, G0::NodeId),
            _e1: (G1::NodeId, G1::NodeId),
        ) -> bool {
            true
        }
    }
    impl<G0, G1, F> EdgeMatcher<G0, G1> for F
    where
        G0: GraphBase + DataMap + IntoEdgesDirected,
        G1: GraphBase + DataMap + IntoEdgesDirected,
        F: FnMut(&G0::EdgeWeight, &G1::EdgeWeight) -> bool,
    {
        #[inline]
        fn enabled() -> bool {
            true
        }
        #[inline]
        fn eq(
            &mut self,
            g0: &G0,
            g1: &G1,
            e0: (G0::NodeId, G0::NodeId),
            e1: (G1::NodeId, G1::NodeId),
        ) -> bool {
            let w0 = g0
                .edges_directed(e0.0, Outgoing)
                .find(|edge| edge.target() == e0.1)
                .and_then(|edge| g0.edge_weight(edge.id()));
            let w1 = g1
                .edges_directed(e1.0, Outgoing)
                .find(|edge| edge.target() == e1.1)
                .and_then(|edge| g1.edge_weight(edge.id()));
            if let (Some(x), Some(y)) = (w0, w1) {
                self(x, y)
            } else {
                false
            }
        }
    }
}
mod matching {
    use super::*;
    #[derive(Copy, Clone, PartialEq, Debug)]
    enum OpenList {
        Out,
        In,
        Other,
    }
    #[derive(Clone, PartialEq, Debug)]
    enum Frame<G0, G1>
    where
        G0: GraphBase,
        G1: GraphBase,
    {
        Outer,
        Inner {
            nodes: (G0::NodeId, G1::NodeId),
            open_list: OpenList,
        },
        Unwind {
            nodes: (G0::NodeId, G1::NodeId),
            open_list: OpenList,
        },
    }
    fn is_feasible<G0, G1, NM, EM>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
        nodes: (G0::NodeId, G1::NodeId),
        node_match: &mut NM,
        edge_match: &mut EM,
    ) -> bool
    where
        G0: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
        G1: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
        NM: NodeMatcher<G0, G1>,
        EM: EdgeMatcher<G0, G1>,
    {
        macro_rules! field {
            ($ x : ident , 0) => {
                $x.0
            };
            ($ x : ident , 1) => {
                $x.1
            };
            ($ x : ident , 1 - 0) => {
                $x.1
            };
            ($ x : ident , 1 - 1) => {
                $x.0
            };
        }
        macro_rules! r_succ {
            ($ j : tt) => {{
                let mut succ_count = 0;
                for n_neigh in field!(st, $j)
                    .graph
                    .neighbors_directed(field!(nodes, $j), Outgoing)
                {
                    succ_count += 1;
                    let m_neigh = if field!(nodes, $j) != n_neigh {
                        field!(st, $j).mapping[field!(st, $j).graph.to_index(n_neigh)]
                    } else {
                        field!(st, 1 - $j).graph.to_index(field!(nodes, 1 - $j))
                    };
                    if m_neigh == std::usize::MAX {
                        continue;
                    }
                    let has_edge = field!(st, 1 - $j).graph.is_adjacent(
                        &field!(st, 1 - $j).adjacency_matrix,
                        field!(nodes, 1 - $j),
                        field!(st, 1 - $j).graph.from_index(m_neigh),
                    );
                    if !has_edge {
                        return false;
                    }
                }
                succ_count
            }};
        }
        macro_rules! r_pred {
            ($ j : tt) => {{
                let mut pred_count = 0;
                for n_neigh in field!(st, $j)
                    .graph
                    .neighbors_directed(field!(nodes, $j), Incoming)
                {
                    pred_count += 1;
                    let m_neigh = field!(st, $j).mapping[field!(st, $j).graph.to_index(n_neigh)];
                    if m_neigh == std::usize::MAX {
                        continue;
                    }
                    let has_edge = field!(st, 1 - $j).graph.is_adjacent(
                        &field!(st, 1 - $j).adjacency_matrix,
                        field!(st, 1 - $j).graph.from_index(m_neigh),
                        field!(nodes, 1 - $j),
                    );
                    if !has_edge {
                        return false;
                    }
                }
                pred_count
            }};
        }
        if r_succ!(0) > r_succ!(1) {
            return false;
        }
        if st.0.graph.is_directed() && r_pred!(0) > r_pred!(1) {
            return false;
        }
        if NM::enabled() && !node_match.eq(st.0.graph, st.1.graph, nodes.0, nodes.1) {
            return false;
        }
        if EM::enabled() {
            macro_rules! edge_feasibility {
                ($ j : tt) => {{
                    for n_neigh in field!(st, $j)
                        .graph
                        .neighbors_directed(field!(nodes, $j), Outgoing)
                    {
                        let m_neigh = if field!(nodes, $j) != n_neigh {
                            field!(st, $j).mapping[field!(st, $j).graph.to_index(n_neigh)]
                        } else {
                            field!(st, 1 - $j).graph.to_index(field!(nodes, 1 - $j))
                        };
                        if m_neigh == std::usize::MAX {
                            continue;
                        }
                        let e0 = (field!(nodes, $j), n_neigh);
                        let e1 = (
                            field!(nodes, 1 - $j),
                            field!(st, 1 - $j).graph.from_index(m_neigh),
                        );
                        let edges = (e0, e1);
                        if !edge_match.eq(
                            st.0.graph,
                            st.1.graph,
                            field!(edges, $j),
                            field!(edges, 1 - $j),
                        ) {
                            return false;
                        }
                    }
                    if field!(st, $j).graph.is_directed() {
                        for n_neigh in field!(st, $j)
                            .graph
                            .neighbors_directed(field!(nodes, $j), Incoming)
                        {
                            let m_neigh =
                                field!(st, $j).mapping[field!(st, $j).graph.to_index(n_neigh)];
                            if m_neigh == std::usize::MAX {
                                continue;
                            }
                            let e0 = (n_neigh, field!(nodes, $j));
                            let e1 = (
                                field!(st, 1 - $j).graph.from_index(m_neigh),
                                field!(nodes, 1 - $j),
                            );
                            let edges = (e0, e1);
                            if !edge_match.eq(
                                st.0.graph,
                                st.1.graph,
                                field!(edges, $j),
                                field!(edges, 1 - $j),
                            ) {
                                return false;
                            }
                        }
                    }
                }};
            }
            edge_feasibility!(0);
            edge_feasibility!(1);
        }
        true
    }
    fn next_candidate<G0, G1>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
    ) -> Option<(G0::NodeId, G1::NodeId, OpenList)>
    where
        G0: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
        G1: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
    {
        let mut from_index = None;
        let mut open_list = OpenList::Out;
        let mut to_index = st.1.next_out_index(0);
        if to_index.is_some() {
            from_index = st.0.next_out_index(0);
            open_list = OpenList::Out;
        }
        if to_index.is_none() || from_index.is_none() {
            to_index = st.1.next_in_index(0);
            if to_index.is_some() {
                from_index = st.0.next_in_index(0);
                open_list = OpenList::In;
            }
        }
        if to_index.is_none() || from_index.is_none() {
            to_index = st.1.next_rest_index(0);
            if to_index.is_some() {
                from_index = st.0.next_rest_index(0);
                open_list = OpenList::Other;
            }
        }
        match (from_index, to_index) {
            (Some(n), Some(m)) => Some((
                st.0.graph.from_index(n),
                st.1.graph.from_index(m),
                open_list,
            )),
            _ => None,
        }
    }
    fn next_from_ix<G0, G1>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
        nx: G0::NodeId,
        open_list: OpenList,
    ) -> Option<G0::NodeId>
    where
        G0: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
        G1: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
    {
        let start = st.0.graph.to_index(nx) + 1;
        let cand0 = match open_list {
            OpenList::Out => st.0.next_out_index(start),
            OpenList::In => st.0.next_in_index(start),
            OpenList::Other => st.0.next_rest_index(start),
        }
        .map(|c| c + start);
        match cand0 {
            None => None,
            Some(ix) => {
                debug_assert!(ix >= start);
                Some(st.0.graph.from_index(ix))
            }
        }
    }
    fn pop_state<G0, G1>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
        nodes: (G0::NodeId, G1::NodeId),
    ) where
        G0: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
        G1: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
    {
        st.0.pop_mapping(nodes.0);
        st.1.pop_mapping(nodes.1);
    }
    fn push_state<G0, G1>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
        nodes: (G0::NodeId, G1::NodeId),
    ) where
        G0: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
        G1: GetAdjacencyMatrix + GraphProp + NodeCompactIndexable + IntoNeighborsDirected,
    {
        st.0.push_mapping(nodes.0, st.1.graph.to_index(nodes.1));
        st.1.push_mapping(nodes.1, st.0.graph.to_index(nodes.0));
    }
    #[doc = " Return Some(bool) if isomorphism is decided, else None."]
    pub fn try_match<G0, G1, NM, EM>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
        node_match: &mut NM,
        edge_match: &mut EM,
        match_subgraph: bool,
    ) -> Option<bool>
    where
        G0: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        G1: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        NM: NodeMatcher<G0, G1>,
        EM: EdgeMatcher<G0, G1>,
    {
        let mut stack = vec![Frame::Outer];
        if isomorphisms(st, node_match, edge_match, match_subgraph, &mut stack).is_some() {
            Some(true)
        } else {
            None
        }
    }
    fn isomorphisms<G0, G1, NM, EM>(
        st: &mut (Vf2State<'_, G0>, Vf2State<'_, G1>),
        node_match: &mut NM,
        edge_match: &mut EM,
        match_subgraph: bool,
        stack: &mut Vec<Frame<G0, G1>>,
    ) -> Option<Vec<usize>>
    where
        G0: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        G1: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        NM: NodeMatcher<G0, G1>,
        EM: EdgeMatcher<G0, G1>,
    {
        if st.0.is_complete() {
            return Some(st.0.mapping.clone());
        }
        let mut result = None;
        while let Some(frame) = stack.pop() {
            match frame {
                Frame::Unwind { nodes, open_list } => {
                    pop_state(st, nodes);
                    match next_from_ix(st, nodes.0, open_list) {
                        None => continue,
                        Some(nx) => {
                            let f = Frame::Inner {
                                nodes: (nx, nodes.1),
                                open_list,
                            };
                            stack.push(f);
                        }
                    }
                }
                Frame::Outer => match next_candidate(st) {
                    None => continue,
                    Some((nx, mx, open_list)) => {
                        let f = Frame::Inner {
                            nodes: (nx, mx),
                            open_list,
                        };
                        stack.push(f);
                    }
                },
                Frame::Inner { nodes, open_list } => {
                    if is_feasible(st, nodes, node_match, edge_match) {
                        push_state(st, nodes);
                        if st.0.is_complete() {
                            result = Some(st.0.mapping.clone());
                        }
                        if (!match_subgraph
                            && st.0.out_size == st.1.out_size
                            && st.0.ins_size == st.1.ins_size)
                            || (match_subgraph
                                && st.0.out_size <= st.1.out_size
                                && st.0.ins_size <= st.1.ins_size)
                        {
                            let f0 = Frame::Unwind { nodes, open_list };
                            stack.push(f0);
                            stack.push(Frame::Outer);
                            continue;
                        }
                        pop_state(st, nodes);
                    }
                    match next_from_ix(st, nodes.0, open_list) {
                        None => continue,
                        Some(nx) => {
                            let f = Frame::Inner {
                                nodes: (nx, nodes.1),
                                open_list,
                            };
                            stack.push(f);
                        }
                    }
                }
            }
            if result.is_some() {
                return result;
            }
        }
        result
    }
    pub struct GraphMatcher<'a, 'b, 'c, G0, G1, NM, EM>
    where
        G0: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        G1: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        NM: NodeMatcher<G0, G1>,
        EM: EdgeMatcher<G0, G1>,
    {
        st: (Vf2State<'a, G0>, Vf2State<'b, G1>),
        node_match: &'c mut NM,
        edge_match: &'c mut EM,
        match_subgraph: bool,
        stack: Vec<Frame<G0, G1>>,
    }
    impl<'a, 'b, 'c, G0, G1, NM, EM> GraphMatcher<'a, 'b, 'c, G0, G1, NM, EM>
    where
        G0: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        G1: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        NM: NodeMatcher<G0, G1>,
        EM: EdgeMatcher<G0, G1>,
    {
        pub fn new(
            g0: &'a G0,
            g1: &'b G1,
            node_match: &'c mut NM,
            edge_match: &'c mut EM,
            match_subgraph: bool,
        ) -> Self {
            let stack = vec![Frame::Outer];
            Self {
                st: (Vf2State::new(g0), Vf2State::new(g1)),
                node_match,
                edge_match,
                match_subgraph,
                stack,
            }
        }
    }
    impl<'a, 'b, 'c, G0, G1, NM, EM> Iterator for GraphMatcher<'a, 'b, 'c, G0, G1, NM, EM>
    where
        G0: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        G1: NodeCompactIndexable
            + EdgeCount
            + GetAdjacencyMatrix
            + GraphProp
            + IntoNeighborsDirected,
        NM: NodeMatcher<G0, G1>,
        EM: EdgeMatcher<G0, G1>,
    {
        type Item = Vec<usize>;
        fn next(&mut self) -> Option<Self::Item> {
            isomorphisms(
                &mut self.st,
                self.node_match,
                self.edge_match,
                self.match_subgraph,
                &mut self.stack,
            )
        }
        fn size_hint(&self) -> (usize, Option<usize>) {
            let n = self.st.0.graph.node_count();
            let upper_bounds: Vec<Option<usize>> = vec![
                1u64,
                1,
                2,
                6,
                24,
                120,
                720,
                5040,
                40320,
                362880,
                3628800,
                39916800,
                479001600,
                6227020800,
                87178291200,
                1307674368000,
                20922789888000,
                355687428096000,
                6402373705728000,
                121645100408832000,
                2432902008176640000,
            ]
            .iter()
            .map(|n| usize::try_from(*n).ok())
            .collect();
            if n > upper_bounds.len() {
                return (0, None);
            }
            (0, upper_bounds[n])
        }
    }
}
#[doc = " \\[Generic\\] Return `true` if the graphs `g0` and `g1` are isomorphic."]
#[doc = ""]
#[doc = " Using the VF2 algorithm, only matching graph syntactically (graph"]
#[doc = " structure)."]
#[doc = ""]
#[doc = " The graphs should not be multigraphs."]
#[doc = ""]
#[doc = " **Reference**"]
#[doc = ""]
#[doc = " * Luigi P. Cordella, Pasquale Foggia, Carlo Sansone, Mario Vento;"]
#[doc = "   *A (Sub)Graph Isomorphism Algorithm for Matching Large Graphs*"]
pub fn is_isomorphic<G0, G1>(g0: G0, g1: G1) -> bool
where
    G0: NodeCompactIndexable + EdgeCount + GetAdjacencyMatrix + GraphProp + IntoNeighborsDirected,
    G1: NodeCompactIndexable
        + EdgeCount
        + GetAdjacencyMatrix
        + GraphProp<EdgeType = G0::EdgeType>
        + IntoNeighborsDirected,
{
    if g0.node_count() != g1.node_count() || g0.edge_count() != g1.edge_count() {
        return false;
    }
    let mut st = (Vf2State::new(&g0), Vf2State::new(&g1));
    self::matching::try_match(&mut st, &mut NoSemanticMatch, &mut NoSemanticMatch, false)
        .unwrap_or(false)
}
#[doc = " \\[Generic\\] Return `true` if the graphs `g0` and `g1` are isomorphic."]
#[doc = ""]
#[doc = " Using the VF2 algorithm, examining both syntactic and semantic"]
#[doc = " graph isomorphism (graph structure and matching node and edge weights)."]
#[doc = ""]
#[doc = " The graphs should not be multigraphs."]
pub fn is_isomorphic_matching<G0, G1, NM, EM>(
    g0: G0,
    g1: G1,
    mut node_match: NM,
    mut edge_match: EM,
) -> bool
where
    G0: NodeCompactIndexable
        + EdgeCount
        + DataMap
        + GetAdjacencyMatrix
        + GraphProp
        + IntoEdgesDirected,
    G1: NodeCompactIndexable
        + EdgeCount
        + DataMap
        + GetAdjacencyMatrix
        + GraphProp<EdgeType = G0::EdgeType>
        + IntoEdgesDirected,
    NM: FnMut(&G0::NodeWeight, &G1::NodeWeight) -> bool,
    EM: FnMut(&G0::EdgeWeight, &G1::EdgeWeight) -> bool,
{
    if g0.node_count() != g1.node_count() || g0.edge_count() != g1.edge_count() {
        return false;
    }
    let mut st = (Vf2State::new(&g0), Vf2State::new(&g1));
    self::matching::try_match(&mut st, &mut node_match, &mut edge_match, false).unwrap_or(false)
}
#[doc = " \\[Generic\\] Return `true` if `g0` is isomorphic to a subgraph of `g1`."]
#[doc = ""]
#[doc = " Using the VF2 algorithm, only matching graph syntactically (graph"]
#[doc = " structure)."]
#[doc = ""]
#[doc = " The graphs should not be multigraphs."]
#[doc = ""]
#[doc = " # Subgraph isomorphism"]
#[doc = ""]
#[doc = " (adapted from [`networkx` documentation](https://networkx.github.io/documentation/stable/reference/algorithms/isomorphism.vf2.html))"]
#[doc = ""]
#[doc = " Graph theory literature can be ambiguous about the meaning of the above statement,"]
#[doc = " and we seek to clarify it now."]
#[doc = ""]
#[doc = " In the VF2 literature, a mapping **M** is said to be a *graph-subgraph isomorphism*"]
#[doc = " iff **M** is an isomorphism between **G2** and a subgraph of **G1**. Thus, to say"]
#[doc = " that **G1** and **G2** are graph-subgraph isomorphic is to say that a subgraph of"]
#[doc = " **G1** is isomorphic to **G2**."]
#[doc = ""]
#[doc = " Other literature uses the phrase ‘subgraph isomorphic’ as in"]
#[doc = " ‘**G1** does not have a subgraph isomorphic to **G2**’. Another use is as an in adverb"]
#[doc = " for isomorphic. Thus, to say that **G1** and **G2** are subgraph isomorphic is to say"]
#[doc = " that a subgraph of **G1** is isomorphic to **G2**."]
#[doc = ""]
#[doc = " Finally, the term ‘subgraph’ can have multiple meanings. In this context,"]
#[doc = " ‘subgraph’ always means a ‘node-induced subgraph’. Edge-induced subgraph"]
#[doc = " isomorphisms are not directly supported. For subgraphs which are not"]
#[doc = " induced, the term ‘monomorphism’ is preferred over ‘isomorphism’."]
#[doc = ""]
#[doc = " **Reference**"]
#[doc = ""]
#[doc = " * Luigi P. Cordella, Pasquale Foggia, Carlo Sansone, Mario Vento;"]
#[doc = "   *A (Sub)Graph Isomorphism Algorithm for Matching Large Graphs*"]
pub fn is_isomorphic_subgraph<G0, G1>(g0: G0, g1: G1) -> bool
where
    G0: NodeCompactIndexable + EdgeCount + GetAdjacencyMatrix + GraphProp + IntoNeighborsDirected,
    G1: NodeCompactIndexable
        + EdgeCount
        + GetAdjacencyMatrix
        + GraphProp<EdgeType = G0::EdgeType>
        + IntoNeighborsDirected,
{
    if g0.node_count() > g1.node_count() || g0.edge_count() > g1.edge_count() {
        return false;
    }
    let mut st = (Vf2State::new(&g0), Vf2State::new(&g1));
    self::matching::try_match(&mut st, &mut NoSemanticMatch, &mut NoSemanticMatch, true)
        .unwrap_or(false)
}
#[doc = " \\[Generic\\] Return `true` if `g0` is isomorphic to a subgraph of `g1`."]
#[doc = ""]
#[doc = " Using the VF2 algorithm, examining both syntactic and semantic"]
#[doc = " graph isomorphism (graph structure and matching node and edge weights)."]
#[doc = ""]
#[doc = " The graphs should not be multigraphs."]
pub fn is_isomorphic_subgraph_matching<G0, G1, NM, EM>(
    g0: G0,
    g1: G1,
    mut node_match: NM,
    mut edge_match: EM,
) -> bool
where
    G0: NodeCompactIndexable
        + EdgeCount
        + DataMap
        + GetAdjacencyMatrix
        + GraphProp
        + IntoEdgesDirected,
    G1: NodeCompactIndexable
        + EdgeCount
        + DataMap
        + GetAdjacencyMatrix
        + GraphProp<EdgeType = G0::EdgeType>
        + IntoEdgesDirected,
    NM: FnMut(&G0::NodeWeight, &G1::NodeWeight) -> bool,
    EM: FnMut(&G0::EdgeWeight, &G1::EdgeWeight) -> bool,
{
    if g0.node_count() > g1.node_count() || g0.edge_count() > g1.edge_count() {
        return false;
    }
    let mut st = (Vf2State::new(&g0), Vf2State::new(&g1));
    self::matching::try_match(&mut st, &mut node_match, &mut edge_match, true).unwrap_or(false)
}
#[doc = " Using the VF2 algorithm, examine both syntactic and semantic graph"]
#[doc = " isomorphism (graph structure and matching node and edge weights) and,"]
#[doc = " if `g0` is isomorphic to a subgraph of `g1`, return the mappings between"]
#[doc = " them."]
#[doc = ""]
#[doc = " The graphs should not be multigraphs."]
pub fn subgraph_isomorphisms_iter<'a, G0, G1, NM, EM>(
    g0: &'a G0,
    g1: &'a G1,
    node_match: &'a mut NM,
    edge_match: &'a mut EM,
) -> Option<impl Iterator<Item = Vec<usize>> + 'a>
where
    G0: 'a
        + NodeCompactIndexable
        + EdgeCount
        + DataMap
        + GetAdjacencyMatrix
        + GraphProp
        + IntoEdgesDirected,
    G1: 'a
        + NodeCompactIndexable
        + EdgeCount
        + DataMap
        + GetAdjacencyMatrix
        + GraphProp<EdgeType = G0::EdgeType>
        + IntoEdgesDirected,
    NM: 'a + FnMut(&G0::NodeWeight, &G1::NodeWeight) -> bool,
    EM: 'a + FnMut(&G0::EdgeWeight, &G1::EdgeWeight) -> bool,
{
    if g0.node_count() > g1.node_count() || g0.edge_count() > g1.edge_count() {
        return None;
    }
    Some(self::matching::GraphMatcher::new(
        g0, g1, node_match, edge_match, true,
    ))
}
