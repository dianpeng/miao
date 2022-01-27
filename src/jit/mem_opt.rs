// Memory Optimization
//
//   ---------------------------------------------------------------------------
//
//   This pass will mainly handle the memory related operation.
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
//   The above 6 instructions are been hoisted, ie the first 4 instructions
//   are been lifted from the CFG's effect lists but been ordered by each
//   instruction's effect node; the RvEffectAfter will be rewritten in the newly
//   constructed effect order; and the RvEffectStart will be eliminated when the
//   memory location of order node is known to us
//
//   ---------------------------------------------------------------------------
//
//   The algorithm has 2 major passes:
//
//   Pass 1, alias anlysis of memory location
//
//     This pass will just use a simple data flow algorithm to do alias analysis
//     and it mainly outputs per basic block information.
//
//       1) It generates a memory side effect in terms of memory alias for each
//          side effect nodes located in each BB
//
//       2) It generates a memory snapshot compressed for each BB after the BB
//          been executed.
//
//
//     This pass just generates external information and does not break the CFG
//     the node in CFG is still the same.
//
//   Pass 2, does hoist of each node located inside of the BB's effect lits
//
//     Based on the previous AA, we can learn that each effect statement's
//     memory impact, by that we can know how to relink the dependency chain
//     of each effect node which manipulates memory location known to us, for
//     operation that involve memory that is not known to us, we keep them
//     still inside of the CFG's effect list.
//
//
//     Additionally, due to the Pass 1, the loopback edge generated effect is
//     also known to us, so the effect phi can be correctly inserted if needed,
//     since we know all BB's predecessor's memory location status before merging
//     them.
//
//
//     Additionally, some optimization will be performed on the fly
//
//       1) load sink will be performed in this phase since the alias information
//          is clear and we can just do load sink based on the dependency chain
//          directly.
//
//       2) RvEffectAfter/RvEffectStart can be rewritten correctly. We just need
//          to modify their dependency based on the lastest AA's result.
//
//     The second pass's secanning of node is sparse, since we only care about
//     the node right inside of the effect lists. For RvEffectXXX, we use extra
//     information list to scan.

use std::cell::RefCell;
use std::rc::Rc;

use crate::jit::node::*;
use bitvec::prelude::*;

type LoopEffectPhiList = Vec<(u32, Nref)>;

// Memory effect
struct Memory {
    mem: Oref, // memory node

    // last write_effect node happened, can be phi if killed, then just None,
    // which means user should treat it as a kill, ie total order
    write_effect: Option<Nref>,
}

enum MemOp {
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

type MemOpList = Vec<MemOp>;

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
struct EnvMemAA {
    // Current control flow graph
    cfg: Nref,

    // A lists of known exclusive(none alias) memory position, if not shows up
    // then the address been assumed as aliased with each other, ie should be
    // maintained the total global order
    //
    // Initially every mem node is not aliased with each other since they are
    // been created inside of the function, until they are escapped.
    //
    // Notes, after the first pass dataflow, this mem_list reflect the memory
    // status |AFTER| this BB been evaluated.
    mem_list: Vec<Memory>,

    // Initialized mem_list, used by later on phase, one can directly clone
    // the array. This array correctly setup the loop effect phi node.
    //
    // The mem_list indicates the status of BB after evaluation/meet operator
    // the init_mem_list indicates the status of BB before evaluation/mee operator
    init_mem_list: Vec<Memory>,

    // A list of status after each side effect node been executed, the element
    // of list indicates the memory modification generated after each side effect
    // node been evaluated
    after_effect_list: MemOpList,

    // lists of effect phi that requires patching due to the loopback edge
    loop_effect_phi_list: LoopEffectPhiList,

    // index of current scanning effect node's index in current cfg's effect
    // list. Used to generate after_effect_list's element etc ..
    effect_index: usize,
}

type Eaaptr = Rc<RefCell<EnvMemAA>>;

impl EnvMemAA {
    fn is_unknown(&self) -> bool {
        return self.write_effect.is_none();
    }
    fn mark_unknown(&mut self) {
        self.write_effect = Option::None;
    }
}

// Phase 1, Alias Analysis
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

struct AA {
    j: Jitptr,
    graph: FGraphptr,

    env_map: Vec<u32>,
    env_list: Vec<Eaaptr>,

    cur_env: Eaaptr,
    visited: BitVec,
    total_mem: u32,
}

impl AA {
    // -------------------------------------------------------------------------
    // Enter into the specified CFG, which will create a EnvMemAA object
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

        // Finish up the creation of EnvMemAA and set it as current env
        let envptr = EnvMemAA::new_ptr(mem_list, cfg, loop_effect_phi_list);

        self.env_list.push(Eaaptr.clone());
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

    fn cur_env(&self) -> Eaaptr {
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

    fn run(&mut self) {
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
    fn env_aa_at(&self, cfg_id: Nid) -> Eaaptr {
        let id = self.env_map[Nid];
        return self.env_list[id].clone();
    }
}

// -----------------------------------------------------------------------------
// Phase 2, Optimization

// BB external information during the memory optimization passes
struct EnvMem {
    cfg: Nref,
    aa: Eaaptr,
    mem_list: Vec<Memory>,
}

type Ememptr = Rc<RefCell<EnvMem>>;

struct MemOpt {
    j: Jitptr,
    graph: FGraphptr,
    aa: AA,

    env_list: Vec<Ememptr>,
    cur_env: Ememptr,
    visited: BitVec,
    total_mem: u32,
}

impl MemOpt {
    fn write_effect_at(&self, mem_node: &Nref) -> Option<Nref> {
        debug_assert!(n.borrow().is_rv_memory());

        let pos = self
            .cur_env()
            .borrow()
            .env_list
            .iter()
            .position(|x| Nref::ptr_eq(&x.mem, n))
            .unwrap();

        return self.cur_env().borrow().mem_list[pos].write_effect.clone();
    }

    fn update_write_effect(&self, mem_node: &Nref, write_effect: Nref) {
        let pos = self
            .cur_env()
            .borrow()
            .env_list
            .iter()
            .position(|x| Nref::ptr_eq(&x.mem, n))
            .unwrap();

        self.cur_env().borrow().mem_list[pos].write_effect =
            Option::Some(write_effect);
    }

    fn hoist_node_read(
        &mut self,
        idx: u32,
        mut effect_node: Nref,
    ) -> Option<()> {
        let mem_node = mem_addr(&effect_node)?;
        let effect = self.write_effect_at(&mem_node)?;

        // try perform load sink optimization
        if self
            .try_load_sink(&mem_node, &effect, effect_node.clone())
            .is_some()
        {
            return;
        }

        self.remove_effect_node_from_effect_list(idx);
        Node::add_effect(&mut effect_node, effect);
    }

    fn hoist_node_write(
        &mut self,
        idx: u32,
        mut effect_node: Nref,
    ) -> Option<()> {
        let mem_node = mem_addr(&effect_node)?;
        let effect = self.write_effect_at(&mem_node)?;

        self.remove_effect_node_from_effect_list(idx);
        Node::add_effect(&mut effect_node, effect);

        // update the memory effect node at that position
        self.update_write_effect(&mem_node, effect_node);
    }

    // Load Sinking optimization
    //
    //   try to directly forward the load member field known to us from its
    //   current position to where this instruction issued.

    fn try_load_sink(
        &self,
        mem_node: &Nref,
        write_effect: &Nref,
        n: Nref,
    ) -> Option<()> {
        // check whether the load node is a constant node, ie if it is a index
        // load then the index must be immediate, or a dot load, we will just
        // convert the load field into Imm object
        let field: Imm = {
            let op = n.borrow().op.op.clone();
            match op {
                Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                    let idx = n.borrow().value[1].clone();
                    if !idx.borrow().is_imm() {
                        return Option::None;
                    }
                    idx.imm
                }
                _ => unreachable!(),
            }
        };

        let mut cur_write_effect = write_effect;

        loop {
            // check whether the current write effect can satisfy the load
            match cur_write_effect.borrow().op.op {
                Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                    let idx = n.borrow().value[1].clone();
                    if idx.borrow().is_imm() && idx.borrow().imm == field {
                        let value = n.borrow().value[1].clone();
                        Node::replace_and_dispose(&mut n, value);
                        return Option::Some(());
                    }
                }
                _ => break, // break the loop, the write_effect node is unknown
            };

            // must have just one single effect predecessor
            if cur_write_effect.borrow().effect.len() != 1 {
                break;
            }

            cur_write_effect = cur_write_effect.borrow().effect[0].clone();
        }

        return Option::None;
    }

    fn patch_on_effect_after(&self, mut order: Nref) -> Option<()> {
        debug_assert!(order.borrow().op.op == Opcode::RvEffectAfter);

        let addr = mem_addr(&order.borrow().value[0].clone())?;
        let effect = self.write_effect_at(&addr)?;
        if !Nref::ptr_eq(&dep, &effect) {
            // Rewrite the effect node from value input to its effect node to
            // indicate that it has been rewritten
            Node::remove_value(&mut order, 1);
            Node::add_effect(&mut order, effect);
        }
        return Option::Some(());
    }

    fn alias_mem(&mut self, n: &Nref) {
        let env = self.cur_env();

        // update the write_effect in current env's mem_list
        for x in env.borrow_mut().mem_list.iter_mut() {
            if Nref::ptr_eq(&x.mem, n) {
                x.write_effect = Option::None;
                break;
            }
        }
    }

    fn update_write_effect(&mut self, mem: &Nref, effect: Nref) {
        let env = self.cur_env();

        // update the write_effect in current env's mem_list
        for x in env.borrow_mut().mem_list.iter_mut() {
            if Nref::ptr_eq(&x.mem, mem) {
                x.write_effect = Option::Some(effect);
                return;
            }
        }

        unreachable!();
    }

    fn apply_memory_impact(&mut self, idx: u32) {
        let aa_ptr = self.cur_aa();

        match aa_ptr.borrow().after_effect_list[idx] {
            MemOp::Alias(alist) => {
                for n in alist.iter() {
                    self.alias_mem(n);
                }
            }

            MemOp::Write(eff) => {
                let mem_node = mem_addr(&eff).unwrap();
                self.update_write_effect(&mem_node, eff.clone());
            }

            _ => (),
        };
    }

    // Environment management, ie setup the mem_list
    fn enter_env(&mut self, cfg: &Nref) {
        let aa_ptr = self.aa.env_aa_at(cfg.borrow().id);
        let env_ptr = EnvMem::new_ptr(cfg.clone(), aa_ptr);
        self.env_list.push(env_ptr.clone());
        self.cur_env = env_ptr;
    }

    fn leave_env(&mut self) {}

    // Finding out a effect node's RvEffectAfter use
    fn get_effect_order_list(&self, enode: &Nref) -> Vec<Nref> {
        let o: Vec<Nref> = Vec::new();

        for def_use in enode.borrow().def_use.iter() {
            if let DefUse::Value(wptr) = def_use {
                if let maybe_effect = wptr.upgrade() {
                    if maybe_effect.borrow().op.op == Opcode::RvEffectAfter {
                        o.push(maybe_effect);
                    }
                }
            }
        }

        return o;
    }

    // Scanning phase of memory optimization. Since all the memory related ops
    // are located inside of each CFG's effect list before memory optimize pass,
    // we can just sparsely scan the instruction, additionally since we already
    // scan each node's operands, so no need to revisit them again. We just scan
    // the effect node itself and try to hoist them out by setting up each node's
    // effect dependency based on AA's results. Additionally, we will revisit
    // the RvEffectAfter node and relink its dependency node based on newly
    // constructed alias information.
    fn opt_mem(&mut self) {
        let start = self.graph.borrow().cfg_start.clone();
        for cfg in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            self.enter_env(&cfg);

            let mut idx = 0;
            for enode in cfg.borrow().effect.iter() {
                self.apply_memory_impact(idx);

                match enode.borrow().op.op {
                    Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                        self.hoist_node_read(idx, enode.clone());
                    }
                    Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                        self.hoist_node_write(idx, enode.clone());
                    }
                    _ => (),
                };

                // patching the effect order node
                {
                    let effect_order_list =
                        self.get_effect_order_list(&effect_node);

                    for order in effect_order_list.iter() {
                        self.patch_on_effect_after(order.clone());
                    }
                }

                idx += 1;
            }

            self.leave_env();
        }
    }

    fn run(&mut self) {
        // (0) perform alias analysis to get enough information for later
        //     optimization
        self.aa.run();

        // (1) run the memory optimization
        self.opt_mem();
    }
}
