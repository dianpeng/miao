// Memory Optimization
// This pass will mainly handle the memory related operation.
//
//   1. During the IR construction, the memory related operation is passively
//      strict ordered and put into its corresponding BB, and also the use of
//      memory object, ie via RvListCreate and RvObjectCreate opcode, is been
//      ordered via RvEffectAfter node with the current side effect nodes.
//
//   2. The graph is verbose and also overly strict since the IR construction
//      does not perform alias analysis. We don't know the memory alias info.
//
//   This pass will mainly reconstruct much more loose order for the following
//   memory access :
//
//     1. RvMemIndexLoad
//     2. RvMemIndexStore
//     3. RvMemObjectLoad
//     4. RvMemObjectStore
//     5. RvEffectAfter
//     6. RvEffectStart
//
//   The above 5 instructions are been reordered, ie the first 4 instructions
//   are been lifted from the CFG's effect lists but been ordered by each
//   instruction's effect node; the RvEffectAfter will be rewritten in the newly
//   constructed effect order; and the RvEffectStart will be eliminated with
//   either EffectPhi or direct effect nodes
//
//   ---------------------------------------------------------------------------
//
//   The algorithm for us is following few passes
//
//   1. The first passes will rescan each BB and perform AA for each memory
//      location. At the same time, it will try to perform memory optimization
//
//      1.1 load sinking
//
//          RvMemIndexLoad/RvMemObjectLoad strictly follows a memory operation
//          that is known to us will be sinked directly with the specific value,
//          otherwise the load operation will be emitted with side effect points
//          to the current RvMem write effect node
//
//      1.2 escape analysis
//
//          Each RvMemXXX will be escapped when it is been used as following :
//
//          1. Phi
//
//             A phi node causes another alias for that memory location,
//             currently we don't track all the alias for each known memory
//             location for simplicity
//
//          2. Call/SetGlobal/SetUpvalue
//
//             This means the location can be aliased in anyway, so it kills of
//             the memory location
//
//     Notes, at the same time the memory operation node that originally been
//     pinned(stored) inside of the BB's effect lists will be hoisted out, and
//     be free again. Please be aware others are remained, for example Call nodes
//     global nodes are not touched.
//
//   2. The second pass will try to fix up various things, it will try to rewrite
//      the RvEffectAfter with newly constructed information. Additionally, it
//      will remove RvEffectStart accordingly with all its use.
//
//  Notes during IR construction, the builder also emit extra information for
//  the graph, ie it generates all the RvMemory node's list, it generates memory
//  access information in BB that resides inside of loop and also it generates
//  a list of global variable name that is been used.

use std::cell::RefCell;
use std::rc::Rc;

use crate::jit::node::*;
use bitvec::prelude::*;

// Memory effect
struct EffMem {
    mem: Oref, // memory node

    // last write_effect node happened, can be phi if killed, then just None,
    // which means user should treat it as a kill, ie total order
    write_effect: Option<Nref>,
}

// Effect environment, one per BB, and been kept alive until the analysis done
struct EnvMem {
    // a lists of known exclusive(none alias) memory position, if not shows up
    // then the address been assumed as aliased with each other, ie should be
    // maintained the total global order
    //
    // Initially every mem node is not aliased with each other since they are
    // been created inside of the function, until they are escapped
    mem_list: Vec<EffMem>,

    // current cfg
    cfg: Nref,
}

type Envptr = Rc<RefCell<EnvMem>>;

struct MemOpt {
    j: Jitptr,
    env_list: Vec<Envptr>,
    cur_env: Envptr,
    visited: BitVec,
    total_mem: u32,
}

impl MemOpt {
    // enter into the specified CFG, which will create a EnvMem object accordingly
    fn enter_env(&mut self, cfg: Nref) {
        debug_assert!(cfg.borrow().is_cfg());

        // new environment's memory tracking list
        let mut mem_list: Vec<EffMem> = Vec::new();

        let pred_list = cfg.borrow().pred_cfg();

        if pred_list.len() == 1 {
            mem_list = self.env_at(&pre).mem_list.clone();
        } else {
            debug_assert!(pred_list.len() > 1);
            let tt = self.total_mem;

            for i in 0..tt {
            }
        }

        // Speculatively create effect phi for joining more branches modification
        // to the exact same memory location
        let mut effect_phi = self
            .mptr()
            .borrow_mut()
            .new_rv_effect_phi(cfg.borrow().bc.clone());

        for pre in cfg.borrow().pred_cfg().iter() {
            // if the pred is been visited, we should just merge it as phi,
            // otherwise not.
            if self.visited_cfg(&pre) {
                let prev_env = self.env_at(&pre);
            } else {
            }
        }
    }

    // Scan one environment
    fn scan_cur_env(&mut self) {
        let cfg = self.cur_cfg();

        for x in EffectPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            match x.borrow().op.op {
                Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                    self.scan_mem_op_load(x.clone());
                }

                Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                    self.scan_mem_op_store(x.clone());
                }

                // Phis
                Opcode::RvPhi => {
                    self.scan_alias_phi(x.clone());
                }

                Opcode::RvSetGlobal => {
                    self.scan_alias_set_global(x.clone());
                }

                Opcode::RvSetUpvalue => {
                    self.scan_alias_set_upvalue(x.clone());
                }

                Opcode::RvIteratorNew => {
                    self.scan_alias_iterator_new(x.clone());
                }

                Opcode::RvHalt => {
                    self.scan_alias_halt(x.clone());
                }
                _ => (),
            };
        }
    }

    // get the memory address of specified node's
    fn mem_addr(&self, n: &Nref) -> Option<Nref> {
        let inner_node = match n.borrow().op.op {
            Opcode::RvEffectAfter => n.borrow().value[0].clone(),
            _ => n.clone(),
        };

        // notes phi should be ignored
        if inner_node.borrow().is_phi() {
            return Option::None;
        }

        return match inner_node.borrow().op.op {
            Opcode::RvObjectCreate | Opcode::RvListCreate => inner_node,
            _ => Option::None,
        };
    }

    fn write_effect_at_mem(&self, n: &Nref) -> Option<Nref> {
        debug_assert!(n.borrow().is_rv_memory());
        let pos = self
            .cur_env()
            .borrow()
            .env_list
            .iter()
            .position(|x| Nref::ptr_eq(&x.mem, n))?;

        return Option::Some(
            self.cur_env().borrow().env_list[pos]
                .write_effect
                .as_ref()?
                .unwrap()
                .clone(),
        );
    }

    fn remove_from_cur_effect_list(&self, n: &Nref) {
        Node::remove_effect_node(&mut self.cur_cfg(), n);
    }

    fn scan_mem_op_store(&mut self, n: Nref) -> Option<()> {
        let mem_node = match &n.borrow().op.op {
            Opcode::RvMemIndexLoad => n.borrow().value[0].clone(),
            Opcode::RvMemDotLoad => n.borrow().value[0].clone(),
            _ => unreachable!(),
        };

        let addr_node = self.mem_addr(&mem_addr)?;
        let write_effect = self.write_effect_at_mem(&addr_node)?;

        // remove it from the current effect list to let it float
        self.remove_from_cur_effect_list(&n);

        // add side effect chain
        Node::add_effect(&mut n, write_effect);

        // update the current write effect
        self.update_cur_effect(n);

        return Option::Some(());
    }

    fn scan_mem_op_load(&mut self, n: Nref) -> Option<()> {
        let mem_node = match &n.borrow().op.op {
            Opcode::RvMemIndexLoad => n.borrow().value[0].clone(),
            Opcode::RvMemDotLoad => n.borrow().value[0].clone(),
            _ => unreachable!(),
        };

        let addr_node = self.mem_addr(&n)?;
        let write_effect = self.write_effect_at_mem(&addr_node)?;

        // try to optimize with load sink
        if self.load_sink(n, write_effect).is_some() {
            return Option::Some(());
        }

        // remove it from the current effect list to let it float
        self.remove_from_cur_effect_list(&n);

        // add side effect chain
        Node::add_effect(&mut n, write_effect);

        return Option::Some(());
    }

    // finding the memory node specified and mark it as unknown for future use
    fn unknown_mem(&mut self, n: &Nref) {
        for x in self.cur_env().borrow_mut().mem_list.iter_mut() {
            if Nref::ptr_eq(&x.mem, n) {
                x.write_effect = Option::None;
                return;
            }
        }
    }

    fn scan_alias_phi(&mut self, n: Nref) -> Option<()> {
        for xx in n.borrow().value.iter() {
            let maybe_mem = self.mem_addr(&xx);
            match maybe_mem {
                Option::Some(m) => {
                    self.unknown_mem(&m);
                }
                _ => (),
            };
        }
        return Option::Some(());
    }

    fn scan_alias_set_global(&mut self, n: Nref) -> Option<()> {
        let v = n.borrow().value[1].clone();
        let mem = self.mem_addr(&xx)?;
        self.unknown_mem(&mem);
        return Option::Some(());
    }

    fn scan_alias_set_upvalue(&mut self, n: Nref) -> Option<()> {
        let v = n.borrow().value[1].clone();
        let mem = self.mem_addr(&xx)?;
        self.unknown_mem(&mem);
        return Option::Some(());
    }

    fn scan_alias_call(&mut self, n: Nref) -> Option<()> {
        for xx in n.borrow().value.iter() {
            let maybe_mem = self.mem_addr(&xx);
            match maybe_mem {
                Option::Some(m) => {
                    self.unknown_mem(&m);
                }
                _ => (),
            };
        }
        return Option::Some(());
    }

    fn scan_iterator_new(&mut self, n: Nref) -> Option<()> {
        let v = n.borrow().value[0].clone();
        let mem = self.mem_addr(&xx)?;
        self.unknown_mem(&mem);
        return Option::Some(());
    }
}
