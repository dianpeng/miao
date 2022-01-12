use std::cell::RefCell;
use std::rc::Rc;

use crate::jit::node::*;

pub type Jitptr = Rc<RefCell<Jit>>;

pub struct Jit {
    pub mpool: Mpptr,

    // deoptimization table, notes during the IR building, we materialize each
    // deoptimization based on bytecode id, but do not link them into the graph
    // until we hit phase add deoptimization. The reason is better do VN on the
    // guard generated
    pub deopt_table: Vec<Option<Nref>>,

    // CFG related ------------------------------------------------------------
    pub cfg_start: Nref,
    pub cfg_end: Nref,
}

impl Jit {
    pub fn new() -> Jitptr {
        let m = Mpool::new();
        let start = m.borrow_mut().new_cfg_start();
        let end = m.borrow_mut().new_cfg_end();
        Jitptr::new(RefCell::new(Jit {
            mpool: m,
            deopt_table: Vec::new(),
            cfg_start: start,
            cfg_end: end,
        }))
    }
}
