// Iterator for node.rs module. Mainly allow various way to visit the graph/node
use crate::jit::node::*;
use bitvec::prelude::*;

// Visit each node once in the graph. Good for printing information and other
// usage. The order is mostly reserved as a DFS order
pub struct NodeOnceIter {
    visited: BitVec,
    wqueue: Nqueue,
}

impl NodeOnceIter {
    pub fn new(c: &Nref, max: u32) -> NodeOnceIter {
        let mut wq = Nqueue::new();
        let mut visited = BitVec::new();
        visited.resize(max as usize, false);

        let start = Nref::clone(c);
        *visited.get_mut(start.borrow().id as usize).unwrap() = true;
        wq.push_back(start);

        NodeOnceIter {
            visited: visited,
            wqueue: wq,
        }
    }
}

impl Iterator for NodeOnceIter {
    type Item = Nref;
    fn next(&mut self) -> Option<Self::Item> {
        if self.wqueue.len() == 0 {
            return Option::None;
        }
        let top = self.wqueue.pop_back().unwrap();
        let tid = top.borrow().id;
        debug_assert!(self.visited[tid as usize]);

        for xx in top.borrow().value.iter() {
            let id = xx.borrow().id as usize;
            if !self.visited[id] {
                *self.visited.get_mut(id).unwrap() = true;
                self.wqueue.push_back(Nref::clone(&xx));
            }
        }
        for xx in top.borrow().cfg.iter() {
            let id = xx.borrow().id as usize;
            if !self.visited[id] {
                *self.visited.get_mut(id).unwrap() = true;
                self.wqueue.push_back(Nref::clone(&xx));
            }
        }
        for xx in top.borrow().effect.iter() {
            let id = xx.borrow().id as usize;
            if !self.visited[id] {
                *self.visited.get_mut(id).unwrap() = true;
                self.wqueue.push_back(Nref::clone(&xx));
            }
        }

        return Option::Some(top);
    }
}
