// Iterator for node.rs module. Mainly allow various way to visit the graph/node
use crate::jit::node::*;
use bitvec::prelude::*;

// -----------------------------------------------------------------------------
// Visit each node once in the graph. Good for printing information and other
// usage. The order is mostly reserved as a PO order
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

// -----------------------------------------------------------------------------
// post order, ie visiting node as post order. This is mostly used in certain
// pass for expression simplification, ie folding, GVN, etc ... For CFG, the
// effective order will just be RPO which is essentially like reverse of post
// order
pub struct NodePOIter {
    order: Nqueue,
}

impl NodePOIter {
    pub fn new(c: &Nref, max: u32) -> NodePOIter {
        let mut visited = BitVec::new();
        let mut order = Nqueue::new();

        visited.resize(max as usize, false);
        NodePOIter::sort_po(c, &mut visited, &mut order);

        NodePOIter { order: order }
    }

    // Sorting out the order queue, used for later |next| function to work
    fn sort_po(n: &Nref, v: &mut BitVec, o: &mut Nqueue) {
        if v[n.borrow().id as usize] {
            return;
        }

        *v.get_mut(n.borrow().id as usize).unwrap() = true;
        for succ in n.borrow().cfg.iter() {
            NodePOIter::sort_po(succ, v, o);
        }
        o.push_back(Nref::clone(n));
    }
}

impl Iterator for NodePOIter {
    type Item = Nref;

    fn next(&mut self) -> Option<Self::Item> {
        if self.order.len() == 0 {
            return Option::None;
        }
        return Option::Some(self.order.pop_front().unwrap());
    }
}
