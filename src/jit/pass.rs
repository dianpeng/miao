use crate::jit::iter::*;
use crate::jit::j::*;
use crate::jit::node::*;

pub enum PassResult {
    OK,
    Bailout(JitBailout), // should not JIT
}

pub trait FGraphPass {
    fn run(&mut self) -> PassResult;
}

// Run callback on each nodes in PO iterator. If node is dead or visited multiple
// times, then it will be ignored
pub trait NodePass {
    fn run_node(&mut self, node: Nref) -> PassResult;
}

struct NodePOPass {
    j: Jitptr,
    f: FGraphptr,
    pass_list: Vec<Box<dyn NodePass>>,
}

impl NodePOPass {
    fn do_run_node(&mut self, c: Nref) -> PassResult {
        for p in self.pass_list.iter_mut() {
            match p.run_node(Nref::clone(&c)) {
                PassResult::Bailout(v) => {
                    return PassResult::Bailout(v);
                }
                _ => (),
            };

            if c.borrow().is_dead() {
                return PassResult::OK;
            }
        }

        return PassResult::OK;
    }
}

impl FGraphPass for NodePOPass {
    fn run(&mut self) -> PassResult {
        let bb = self.f.borrow().cfg_start.clone();
        let mid = self.j.borrow().max_node_id();
        for cur in NodePOIter::new(&bb, mid) {
            if cur.borrow().is_dead() {
                continue;
            }
            match self.do_run_node(cur) {
                PassResult::OK => (),
                PassResult::Bailout(v) => {
                    return PassResult::Bailout(v);
                }
            };
        }

        return PassResult::OK;
    }
}
