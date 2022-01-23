use std::cell::RefCell;
use std::rc::Rc;

use crate::jit::node::*;
use crate::object::object::*;

pub type Jitptr = Rc<RefCell<Jit>>;
pub type FGraphptr = Rc<RefCell<FGraph>>;
pub type GraphList = Vec<FGraphptr>;

#[derive(Clone)]
pub struct FGraph {
    pub func: FuncRef,
    pub arg_count: u32,

    // deoptimization table, notes during the IR building, we materialize each
    // deoptimization based on bytecode id, but do not link them into the graph
    // until we hit phase add deoptimization. The reason is better do VN on the
    // guard generated
    pub deopt_table: Vec<Option<Nref>>,

    // CFG related ------------------------------------------------------------
    pub cfg_start: Nref,
    pub cfg_end: Nref,
}

pub struct Jit {
    pub mpool: Mpptr,
    pub graph_list: GraphList,
}

impl FGraph {
    pub fn new(
        m: &mut Mpptr,
        fref: FuncRef,
        arg_count: u32,
        frame: u32,
    ) -> FGraphptr {
        let start = m.borrow_mut().new_cfg_start(BcCtx::new(0, frame));
        let end = m.borrow_mut().new_cfg_end(BcCtx::new(0, frame));
        let code_size = fref.borrow().proto.code.array.len();
        let deopt_table = vec![Option::None; code_size];

        FGraphptr::new(RefCell::new(FGraph {
            func: fref,
            arg_count: arg_count,
            deopt_table: deopt_table,
            cfg_start: start,
            cfg_end: end,
        }))
    }
}

impl Jit {
    pub fn new() -> Jitptr {
        let m = Mpool::new();
        Jitptr::new(RefCell::new(Jit {
            mpool: m,
            graph_list: Vec::new(),
        }))
    }

    pub fn max_node_id(&self) -> Nid {
        return self.mpool.borrow().max_node_id();
    }
}

// JIT bailout reason, ie why we don't want to jit anymore
//
// 1. Type information is inconsistent, ie BUGs in our codes.
// 2. Function are too long and too complicated
pub enum JitBailout {
    TypeInconsitent,
    FunctionTooLong,
    NoReason,
}
