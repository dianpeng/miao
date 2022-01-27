use std::cell::RefCell;
use std::rc::Rc;

use crate::jit::node::*;
use bitvec::prelude::*;

pub type LoopEffectPhiList = Vec<(u32, Nref)>;

// Memory effect
pub struct Memory {
    pub mem: Oref, // memory node

    // last write_effect node happened, can be phi if killed, then just None,
    // which means user should treat it as a kill, ie total order
    pub write_effect: Option<Nref>,
}

// Memory operation
pub enum MemOp {
    // indicate that the specified Nref node been aliased, which means the mem
    // location should be unknown to us in terms of dependency. The alias is
    // vector due to the fact a call instruction can alias multiple memory
    // location at the same time
    Alias(Vec<Nref>),

    // generate a write effect, ie should update the current effect
    Write(Nref),

    // No memory side effect has been generated, just let it go
    Nothing,
}

pub type MemOpList = Vec<MemOp>;

fn mem_addr(n: &Nref) -> Option<Nref> {
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

// Effect environment, one per BB, and been kept alive until the analysis done
pub struct BlkAA {
    // Current control flow graph
    pub cfg: Nref,

    // A lists of known exclusive(none alias) memory position, if not shows up
    // then the address been assumed as aliased with each other, ie should be
    // maintained the total global order
    //
    // Initially every mem node is not aliased with each other since they are
    // been created inside of the function, until they are escapped.
    //
    // Notes, after the first pass dataflow, this mem_list reflect the memory
    // status |AFTER| this BB been evaluated.
    pub mem_list: Vec<Memory>,

    // Initialized mem_list, used by later on phase, one can directly clone
    // the array. This array correctly setup the loop effect phi node.
    //
    // The mem_list indicates the status of BB after evaluation/meet operator
    // the init_mem_list indicates the status of BB before evaluation/mee operator
    pub init_mem_list: Vec<Memory>,

    // A list of status after each side effect node been executed, the element
    // of list indicates the memory modification generated after each side effect
    // node been evaluated
    pub after_effect_list: MemOpList,

    // lists of effect phi that requires patching due to the loopback edge
    pub loop_effect_phi_list: LoopEffectPhiList,

    // index of current scanning effect node's index in current cfg's effect
    // list. Used to generate after_effect_list's element etc ..
    effect_index: usize,
}

pub type Aaptr = Rc<RefCell<BlkAA>>;

impl BlkAA {
    fn is_unknown(&self) -> bool {
        return self.write_effect.is_none();
    }
    fn mark_unknown(&mut self) {
        self.write_effect = Option::None;
    }
}

// Alias Analysis
//
//  T :=
//
//    known region:
//      all known memory location is not aliased, ie RvObjectCreate/RvListCreate
//
//    unknown region:
//      and rest of memory access are all aliased with each other
//
//  /\ :=
//
//    RvSetGlobal(known_memory, ...)
//    RvSetUpvalue(known_memory, ...)
//    RvCall(known_memory, ....)
//    RvPhi(known_memory, ...)
//
//      => known_memory become unknown region
//
//
//    CFG (before execution)
//
//      => if a memory location from one of CFG(pred) is unknown, then the memory
//         location is unknown for this CFG
//
// Each effect node will optionally generate a modification towards our memory
// status, and afterwards the modification will be recorded. Each BB will have
// status before and after.

pub struct AA {
    j: Jitptr,
    graph: FGraphptr,

    env_map: Vec<u32>,
    env_list: Vec<Aaptr>,

    cur_env: Aaptr,
    visited: BitVec,
    total_mem: u32,
}

impl AA {
    // -------------------------------------------------------------------------
    // Enter into the specified CFG, which will create a BlkAA object
    fn enter_env(&mut self, cfg: Nref) {
        debug_assert!(cfg.borrow().is_cfg());

        // new environment's memory tracking list
        let mut mem_list;

        // predecessor block lists, ie other CFG node jumps into the current CFG
        let pred_list = cfg.borrow().pred_control();

        let loop_effect_phi_list = LoopEffectPhiList::new();

        if pred_list.len() == 1 {
            mem_list = self.env_at(&pre).mem_list.clone();
        } else {
            debug_assert!(pred_list.len() > 1);
            let tt = self.total_mem;
            let bc = cfg.borrow().bc.clone();

            // initialize memory list, notes the initialization of memory list
            // will just leave everything to be unknown to us, ie write_effect
            // to be Option::None
            mem_list = self.init_mem_list();

            // try to merge all the effect node from its predecessor. One should
            // be aware that if the predecessor is not visited yet, it is a loop
            // back edge so we just put a placeholder at that places for future
            // patch once the loop back node is been visited already.
            for i in 0..tt {
                let mut effect_phi =
                    self.mptr().borrow_mut().new_rv_effect_phi(bc.clone());

                let mut unknown = false;

                for pre in pred_list.iter() {
                    debug_assert!(pre.borrow().mem_list.len() == tt as usize);

                    if self.visited_cfg(&pre) {
                        // normal predecessor

                        // for predecessor, if the predecessor shows an unknown
                        // variable, then we just mark ours as unknown since the
                        // predecessor can reach current BB which effectively
                        // makes the memory location's alias status unknown

                        let alias_mem = pre.borrow().mem_list[i].is_unknown();

                        if !alias_mem {
                            Node::add_phi_value(
                                &mut effect_phi,
                                pre.borrow().mem_list[i].clone(),
                                pre.clone(),
                            );
                        } else {
                            unknown = true;
                            break;
                        }
                    } else {
                        let placeholder = self
                            .mptr()
                            .borrow_mut()
                            .new_loop_effect_placeholder(bc.clone());

                        Node::add_phi_value(
                            &mut effect_phi,
                            placeholder,
                            pre.clone(),
                        );
                    }
                }

                // if the node is unknown, then current BB will just leave it
                // as unknown status to us
                if !unknown {
                    mem_list[i].write_effect = Option::Some(effect_phi);
                }
            }

            // Record those effect node that contains placeholder and also been
            // modified inside of the loop, it require us to patch those node
            // after we finish scanning the whole BB.
            {
                let tt = self.total_mem;

                for i in 0..tt {
                    if let Option::Some(v) = mem_list[i].write_effect.clone() {
                        loop_effect_phi_list.push((i, v.clone()));
                    }
                }
            }
        }

        // Finish up the creation of BlkAA and set it as current env
        let envptr = BlkAA::new_ptr(mem_list, cfg, loop_effect_phi_list);

        self.env_list.push(Aaptr.clone());
        self.cur_env = envptr;
    }

    // Leave the current environment
    fn leave_env(&mut self) {
        let env = self.cur_env();
        let unknown_list = Vec::<u32>::new();

        for x in env.borrow().loop_effect_phi_list.iter_mut() {
            let is_unknown = env.borrow().mem_list[x.0 as usize].is_unknown();

            if is_unknown {
                unknown_list.push(x as usize);
            } else {
                // replace the write effect node with current write effect node
                let cur_write = env.borrow().mem_list[x.0 as usize]
                    .write_effect
                    .as_ref()
                    .unwrap()
                    .clone();

                let mut phi = x.1.clone();

                if !Nref::ptr_eq(&cur_write, &phi) {
                    // replace the phi with new effect phi nodes
                    assert!(Node::replace_phi_placeholder_value(
                        &mut phi,
                        cur_write,
                        env.borrow().cfg.clone()
                    ));
                }
            }
        }

        for x in unknown_list.iter() {
            env.borrow_mut().mem_list[x as usize].mark_unknown();
            env.borrow_mut().init_mem_list[x as usize].mark_unknown();
        }
    }

    fn cur_env(&self) -> Aaptr {
        return self.cur_env.clone();
    }

    fn cur_cfg(&self) -> Nref {
        return self.cur_env().borrow().cfg.clone();
    }

    fn has_write_effect(&self, n: &Nref) -> bool {
        debug_assert!(n.borrow().is_rv_memory());
        let pos = self
            .cur_env()
            .borrow()
            .env_list
            .iter()
            .position(|x| Nref::ptr_eq(&x.mem, n))?;

        return self.cur_env().borrow().mem_list[pos].is_unknown();
    }

    fn update_write_effect(&self, n: Nref) {
        let env = self.cur_env();
        let cfg = env.borrow().cfg.clone();
        let idx = env.borrow().effect_index;

        debug_assert!(idx <= cfg.borrow().effect.len());

        env.borrow_mut().after_effect_list[idx] = MemOp::Write(n);
    }

    // udpate alias information
    fn alias_mem(&mut self, n: &Nref) {
        let env = self.cur_env();
        let cfg = env.borrow().cfg.clone();
        let idx = env.borrow().effect_index;
        debug_assert!(idx <= cfg.borrow().effect.len());

        // insert an alias at current places
        if let MemOp::Alias(nlist) = &env.borrow_mut().after_effect_list[idx] {
            nlist.push(n);
        } else {
            let nlist = Vec::<Nref>::new();
            nlist.push(n);
            env.borrow_mut().after_effect_list[idx] = MemOp::Alias(nlist);
        }

        // update the write_effect in current env's mem_list
        for x in env.borrow_mut().mem_list.iter_mut() {
            if Nref::ptr_eq(&x.mem, n) {
                x.write_effect = Option::None;
                break;
            }
        }
    }

    fn scan_mem_op_store(&mut self, n: Nref) -> Option<()> {
        let mem_node = match &n.borrow().op.op {
            Opcode::RvMemIndexLoad => n.borrow().value[0].clone(),
            Opcode::RvMemDotLoad => n.borrow().value[0].clone(),
            _ => unreachable!(),
        };

        let addr_node = mem_addr(&mem_addr)?;
        if !self.has_write_effect(&addr_node) {
            return;
        }

        // update the current write effect
        self.update_write_effect(n);

        return Option::Some(());
    }

    fn scan_mem_op_load(&mut self, n: Nref) -> Option<()> {
        let mem_node = match &n.borrow().op.op {
            Opcode::RvMemIndexLoad => n.borrow().value[0].clone(),
            Opcode::RvMemDotLoad => n.borrow().value[0].clone(),
            _ => unreachable!(),
        };

        let addr_node = mem_addr(&n)?;
        if !self.has_write_effect(&addr_node) {
            return;
        }

        return Option::Some(());
    }

    fn scan_alias_phi(&mut self, n: Nref) -> Option<()> {
        for xx in n.borrow().value.iter() {
            let maybe_mem = mem_addr(&xx);
            match maybe_mem {
                Option::Some(m) => {
                    self.alias_mem(&m);
                }
                _ => (),
            };
        }
        return Option::Some(());
    }

    fn scan_alias_set_global(&mut self, n: Nref) -> Option<()> {
        let v = n.borrow().value[1].clone();
        let mem = mem_addr(&xx)?;
        self.alias_mem(&mem);
        return Option::Some(());
    }

    fn scan_alias_set_upvalue(&mut self, n: Nref) -> Option<()> {
        let v = n.borrow().value[1].clone();
        let mem = mem_addr(&xx)?;
        self.alias_mem(&mem);
        return Option::Some(());
    }

    fn scan_alias_call(&mut self, n: Nref) -> Option<()> {
        for xx in n.borrow().value.iter() {
            let maybe_mem = mem_addr(&xx);
            match maybe_mem {
                Option::Some(m) => {
                    self.alias_mem(&m);
                }
                _ => (),
            };
        }
        return Option::Some(());
    }

    pub fn run(&mut self) {
        let cfg = self.graph.borrow().cfg_start.clone();

        for c in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            let tt_side_effect = c.borrow().effect.len();

            self.enter_env(c.clone());

            for i in 0..tt_side_effect {
                self.update_env_effect_index(i);

                for effect_node in EffectPOIter::new(&i, &c.borrow().effect[i]) {
                    match effect_node.borrow().op.op {
                        Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                            self.scan_mem_op_load(x.clone());
                        }

                        Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                            self.scan_mem_op_store(x.clone());
                        }

                        Opcode::RvSetGlobal => {
                            self.scan_alias_set_global(x.clone());
                        }

                        Opcode::RvSetUpvalue => {
                            self.scan_alias_set_upvalue(x.clone());
                        }

                        Opcode::RvHalt => {
                            self.scan_alias_halt(x.clone());
                        }

                        // Phi is not pinned into the cfg's effect list, but if
                        // a phi is somehow reachable from usage of memory node,
                        // then we can proactively alias the memory as long as
                        // one of the phi argument is some memory location. The
                        // phi effectively creats an alias of an alias of access
                        // the memory other than the original memory node itself
                        //
                        // Notes, sometimes a phi is been created inside of the
                        // graph but it is not alive afterwards, or its memory
                        // component is not been accessed afterwards but just
                        // the phi as whole, if that's the case it is fine at
                        // all since its memory status is not relevent and we
                        // only care about alias situation when we access its
                        // component. The memory location loading order is
                        // maintained by sparse RvEffectAfter node
                        Opcode::RvPhi => {
                            self.scan_alias_phi(x.clone());
                        }

                        _ => (),
                    };
                }
            }

            self.leave_env();
        }
    }

    // -------------------------------------------------------------------------
    // Public interface exported from AA, used by phase2 Env
    pub fn env_aa_at(&self, cfg_id: Nid) -> Aaptr {
        let id = self.env_map[Nid];
        return self.env_list[id].clone();
    }
}
