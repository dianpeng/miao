// Effect Analysis
//
//   Analias Analysis
//     Here we perform a simple AA based on traditional Anders's AA.
//
//     The algorithm is 1) flow sensitive, 2) sub field insensitive
//
//     The precise version requires us to use CFLGraph to accomdate the field
//     sensitivity, which is too much efforts to track. Instead, we allow basic
//     assignment style alias, as with sub field, we will give up the track.
//
//   ---------------------------------------------------------------------------
//
// The algorigthm works as following. It starts to scan the instruction at each
// BB for its side effect nodes. With each side effect node, we summary alias
// as following,
//
//   1) for any memory creation node, we summarize it as a known abstract memory
//      location;
//
//   2) for each global/upvalue set/load node, if the memory node is involved,
//      then we add an alias to the known memory node;
//
//   3) for any memory access operation node, if it involes memory access, we
//      set the underlying memory location to be unknown, ie the memory location
//      been merged into global unknown memory location which serves as fallback
//
//      Strictly speaking, if a memory location read instruction involes a
//      memory location this memory location read will be ordered after the
//      known memory location's recent most write; if a memory location write
//      instruction involes a write that doesn't write a memory location into,
//      then the memory location write instruction will be used to update the
//      known memory location's recent most write; otherwise the memory location
//      that is been written into, ie the write value, will be marked as an
//      unknow memory location since we are not field sensitive.
//
//   4) for memory access node, it tries to loose its dependency into the
//      memory location's latest write, ie true dependency
//
//  For a BB, the join of its Pred(BB) will be an effect phi of all the known
//  memory location modification, the effect phi will be treated as a effect
//  merge from different BB's execution path and will be placed as a write
//  effect. Additionally, a PHI node that involes memory location will also
//  mark all the memory location to be unknown.

use std::cell::RefCell;
use std::rc::Rc;

use crate::jit::node::*;
use bitvec::prelude::*;

type LoopEffectJoinList = Vec<(u32, Nref)>;
type R = Option<()>;

// Memory, represent a abstract location inside of program's heap, will be
// pointed to by different node when aliased. If it is unknown, it means we will
// need to use the fallback memory order
#[derive(Clone)]
struct Memory {
    // initialization node of this memory location, the init node will always
    // be an alias until the location is unknown to us
    init: Nref,

    // last write_effect node happened, can be phi if killed, then just None,
    // which means user should treat it as a kill, ie total order
    write_effect: Nref,

    // all the read node that happens *after* the write_effect currently
    // recorded, if write_effect is been recorded as Option::None, it means
    // it is an unknown memory location and the read_effect node list should
    // be empty
    read_effect: Vec<Nref>,
}

type Memptr = Rc<RefCell<Memory>>;

// Represent an abstract heap location, this is different from Memory since
// they can be merged but memory cannot, since they are unaliased known memory
// position inside of the heap.
#[derive(Clone)]
enum HeapLocation {
    Mem(Memptr),
    Alias(Memptr),
}

type Hlocptr = usize;
type Heap = Vec<Memptr>;

// Memory alias, represent a memory node and its abstract memory location's
// relationship. Notes. due to the fact that we don't have sub field alias
// so we allow memory location alias based on following rules, if 2 memory
// locations are partially overlapped, then they are aliased fully. Ie any
// operation involes these 2 memory location operations will be ordered.
struct MemoryRef {
    mem: Nref,
    loc: Hlocptr,
}

// A value lattice used to represent the Global/Upvalue alias value. The lattice
// is as following :
// ============================================================================
//
//                         Nothing
//                            |
//           -----------------+-----------------
//           |          |     |     |          |
//        Imm(a0) ... Imm(an) |   Mem(a0)    Mem(an)
//           |          |     |     |          |
//           +----------+     |     +----------+
//                |           |           |
//                |           |           |
//             NotMemory<-----+-----------+
//                            |
//                            |
//                         Unknown
//
// ============================================================================
//
// lattice meet operation :
//
//   1) Nothing ^ X = X
//   2) Unknown ^ A = Unknown
//   3) Imm(a) ^ Imm(b) = NotMemory
//   4) NotMemory ^ A = NotMemory (if NotMemory < A)
//   5) Memory(a) ^ A = Unknown
//
// ============================================================================
#[derive(PartialEq, Eq, Clone)]
enum Val {
    // Top of lattice, means we know nothing of the Val
    Nothing,

    // Constant value, and we know its actual constant value, can help global
    // variable load elimination
    Const(Imm),

    // Memory reference, ie pointed to a list of memory locations. Multiple
    // memory location is caused due to CFG
    Memory(Vec<Hlocptr>),

    // indicate some unknown constant, and we know it is definitly not a memory
    // reference
    NotMemory,

    // Bottom of the lattice, ie this should alias with anything we don't know
    Unknown,
}

// Global variable alias
#[derive(PartialEq, Eq, Clone)]
struct Galias {
    g: String,
    v: Val,
}

// Upvalue alias
#[derive(PartialEq, Eq, Clone)]
struct Ualias {
    offset: u32,
    v: Val,
}

// Side effect can be observed by a function/closure
#[derive(Clone)]
struct EffectEnv {
    // all the known heap/memory location, abstracted, and exclude from the
    // global unknown memory location. Additionally, each shape/location in
    // the tracking list are not overlapped with others
    memory_location: Heap,

    // memory alias
    memory_alias: Vec<MemoryRef>,

    // global variable alias known to us. If a global variable is not in the
    // list then it is pointed to unknown position, otherwise it may point to
    // valid HeapLocation.
    global_alias: Vec<Galias>,

    // upvalue alias known to us, similar as global_alias
    upvalue_alias: Vec<Ualias>,
}

impl Val {
    fn new_nothing() -> Val {
        return Val::Nothing;
    }
    fn new_not_memory() -> Val {
        return Val::Var;
    }
    fn new_unknown() -> Val {
        return Val::Unknown;
    }
    fn new_const(imm: Imm) -> Val {
        return Val::Const(imm);
    }
    fn new_memory(loc: Hlocptr) -> Val {
        let mut o = Vec::<Hlocptr>::new();
        o.push(mem);
        return Val::Memory(o);
    }

    fn is_const(&self) -> bool {
        match &self {
            Val::Const(_) => return true,
            _ => return false,
        };
    }
    fn is_memory(&self) -> bool {
        match &self {
            Val::Memory(_) => return true,
            _ => return false,
        };
    }
    fn is_var(&self) -> bool {
        match &self {
            Val::Var => return true,
            _ => return false,
        };
    }
    fn is_nothing(&self) -> bool {
        match &self {
            Val::Nothing => return true,
            _ => return false,
        };
    }
    fn is_unknown(&self) -> bool {
        match &self {
            Val::Unknown => return true,
            _ => return false,
        };
    }

    fn is_same(&self, that: &Val) -> bool {
        match (&self, &that) {
            (Val::Const(a), Val::Const(b)) => {
                return *a == *b;
            }
            (Val::Memory(a), Val::Memory(b)) => {
                return Nref::ptr_eq(a, b);
            }
            (Val::Var, Val::Var)
            | (Val::Nothing, Val::Nothing)
            | (Val::Unknown, Val::Unknown) => {
                return true;
            }
            _ => return false,
        };
    }

    fn lattice_height(&self) -> u32 {
        return match &self {
            Val::Nothing => 0,
            Val::Const(_) => 1,
            Val::Memory(_) => 1,
            Val::Var => 2,
            Val::Unknown => 3,
        };
    }

    fn meet(&mut self, that: &Val) {
        let tmp = Val::do_meet(self, that);
        *self = tmp;
    }

    fn do_meet(lhs: &Val, rhs: &Val) -> Val {
        // 1) nothing ^ X = X
        if lhs.is_nothing() {
            return rhs.clone();
        }

        // 2) reflexive
        if lhs.is_same(rhs) {
            return lhs.clone();
        }

        // 3) imm ^ imm = var
        //    imm ^ mem = var
        //    mem ^ imm = var
        //    mem ^ mem = var
        //    var ^ X = var (unknown != X)
        //    X ^ var = var (unknown != X)
        //    unknown ^ X = unknown
        //    X ^ unknown = unknown
        let lhs_h = lhs.lattice_height();
        let rhs_h = rhs.lattice_height();
        if lhs_h != rhs_h {
            if lhs_h < rhs_h {
                return rhs.clone();
            } else {
                return lhs.clone();
            }
        } else {
            if lhs_h == 1 {
                return Val::new_not_memory();
            } else {
                return lhs.clone();
            }
        }
    }

    // join, ie merge a list of value into one single val
    fn join(all: &Vec<Val>) -> Val {
        if all.len() == 0 {
            return Val::new_nothing();
        }
        let first = all[0].clone();
        for x in all.iter().skip(1) {
            first.meet(&x);
        }
        return first;
    }
}

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

// Environment, one per BB, and been kept alive until the analysis done
struct Env {
    // Current control flow graph
    cfg: Nref,

    // Starting effect env when BB is been setup
    init_effect: EffectEnv,

    // Current effect environment, been evaluated after abstract interpretation.
    // Once a BB has been interpreted done, this stores the effect lists after
    // the BB is done
    effect: EffectEnv,

    // ------------------------------------------------------------------------
    // Private Fields
    // ------------------------------------------------------------------------
    // lists of effect phi that requires patching due to the loopback edge
    loop_effect_join_list: LoopEffectJoinList,

    // index of current scanning effect node's index in current cfg's effect
    // list. Used to generate after_effect_list's element etc ..
    effect_index: usize,
}

type Envptr = Rc<RefCell<Env>>;

impl Env {
    fn is_unknown(&self) -> bool {
        return self.write_effect.is_none();
    }
    fn mark_unknown(&mut self) {
        self.write_effect = Option::None;
    }
}

struct BBMutStateInfo {
    mem_mut: Vec<Nref>,
    glb_mut: Vec<String>,
    upv_mut: Vec<u32>,
}

// Phase 1, Effect Analysis,
//
//   it includes following analysis,
//
//     1) global, upvalue and memory location Andersen's alias analysis
//        flow sensitive, sub-field insensitive
//
//     2) precise effect gen/kill at each execution point
//
struct EffectAnalysis {
    j: Jitptr,
    graph: FGraphptr,

    env_map: Vec<u32>,
    env_list: Vec<Envptr>,

    cur_env: Envptr,
    visited: BitVec,
    total_mem: u32,

    // ------------------------------------------------------------------------
    // Private information, used during effect analysis
    //
    // filled in by the pass 1, ie contains the global state mutation information
    // for different BB.
    mut_info: Vec<BBMutStateInfo>,
}

impl BBMutStateInfo {
    fn new() -> BBMutStateInfo {
        return BBMutStateInfo {
            mem_mut: Vec::new(),
            glb_mut: Vec::new(),
            upv_mut: Vec::new(),
        };
    }
}

impl EffectAnalysis {
    // * ----------------------------------------------------------------------
    // *  Pass 1
    // *
    // *  gather information of global/upvalue/memory access(write) information
    // *  in each basic block
    // * ----------------------------------------------------------------------
    fn pass_gather_information(&mut self) {
        let cfg = self.graph.borrow().cfg_start.clone();

        for c in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            let tt_side_effect = c.borrow().effect.len();
            let mut state = &mut self.mut_info[c.borrow().id];
            let func = self.grap.borrow().func.clone();

            for i in 0..tt_side_effect {
                for enode in EffectPOIter::new(&i, &c.borrow().effect[i]) {
                    match enode.borrow().op.op {
                        Opcode::RvSetGlobal => {
                            state.glb_mut.push(Node::global_name(&enode, func));
                        }

                        Opcode::RvSetUpval => {
                            state
                                .upv_mut
                                .push(Node::upvalue_index(&enode, func));
                        }

                        // Sub-field access which involes memory mutation
                        Opcode::RvMemIndexStore(x)
                        | Opcode::RvMemDotStore(x) => {
                            match Node::sub_field_memory_node(&enode) {
                                Option::Some(n) => {
                                    state.mem_mut(n.clone());
                                }
                                _ => (),
                            };
                        }
                        _ => (),
                    };
                }
            }
        }
    }

    fn is_upvalue_mut(&self, cfg: &Nref, idx: u32) -> bool {
        match self.mut_info[cfg.borrow().id as usize]
            .upv_mut
            .iter()
            .position(|x| *x == idx)
        {
            Option::Some(_) => {
                return true;
            }
            _ => return false,
        };
    }

    fn is_global_mut(&self, cfg: &Nref, name: &str) -> bool {
        match self.mut_info[cfg.borrow().id as usize]
            .glb_mut
            .iter()
            .position(|x| x == name)
        {
            Option::Some(_) => {
                return true;
            }
            _ => return false,
        };
    }

    fn is_memory_mut(&self, cfg: &Nref, mem: &Nref) -> bool {
        match self.mut_info[cfg.borrow().id as usize]
            .mem_mut
            .iter()
            .position(|x| Nref::ptr_eq(x, mem))
        {
            Option::Some(_) => {
                return true;
            }
            _ => return false,
        };
    }

    // * ----------------------------------------------------------------------
    // *  Pass 2
    // *    effect analysis, this pass will break all the side effect chain
    // *    based on alias analysis results, ie if multiple nodes access
    // *    the same piece of memory, based on analysis, they will be effect
    // *    depend on each other.
    // * ----------------------------------------------------------------------

    fn merge_env_memory(
        &self,
        phi_list: &mut LoopEffectJoinList,
        pred_list: &Vec<Nref>,
        tt: u32,
    ) -> Vec<Hlocptr> {
        let mut mem_list = new_unknown_mem_list(tt);

        // try to merge all the effect node from its predecessor. One should
        // be aware that if the predecessor is not visited yet, it is a loop
        // back edge so we just put a placeholder at that places for future
        // patch once the loop back node is been visited already.
        for i in 0..tt {
            let mut effect_join =
                self.mptr().borrow_mut().new_rv_effect_join(bc.clone());

            let mut unknown = false;

            for pre in pred_list.iter() {
                debug_assert!(pre.borrow().effect.len() == tt as usize);

                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();

                    // normal predecessor
                    // for predecessor, if the predecessor shows an unknown
                    // variable, then we just mark ours as unknown since the
                    // predecessor can reach current BB which effectively
                    // makes the memory location's alias status unknown
                    let alias_mem = pre_env
                        .borrow()
                        .effect
                        .memory_location
                        .borrow()
                        .is_unknown();

                    if !alias_mem {
                        let pre_memory = pre_env
                            .borrow()
                            .effect
                            .memory_location
                            .borrow()
                            .mem
                            .clone();

                        Node::add_phi_value(
                            &mut effect_join,
                            pre_memory,
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
                        &mut effect_join,
                        placeholder,
                        pre.clone(),
                    );
                }
            }

            // if the node is unknown, then current BB will just leave it
            // as unknown status to us
            if !unknown {
                mem_list[i].write_effect = Option::Some(effect_join);
            }
        }

        // Record those effect node that contains placeholder and also been
        // modified inside of the loop, it require us to patch those node
        // after we finish scanning the whole BB.
        for i in 0..tt {
            if let Option::Some(v) = mem_list[i].write_effect.clone() {
                phi_list.push((i, v.clone()));
            }
        }

        return mem_list;
    }

    fn merge_env_global(&mut self, pred_list: &Vec<Nref>) -> Vec<Galias> {
        let mut glist = Vec::<Galias>::new();
        let mut tt = self.total_global;

        for i in 0..tt {
            let mut l: Vec<Val>::new();
            let global_name = self.global_name(i);

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();
                    let pre_glb_val =
                        pre_env.borrow().effect.global_alias[i].v.clone();

                    l.push(pre_glb_val);
                } else {
                    if self.is_global_mut(&global_name, &pre) {
                        l.push(Val::new_not_memory());
                    } else {
                        l.push(Val::new_nothing());
                    }
                }
            }

            // perform the meet operation
            glist.push(Galias::new(global_name, Val::join(&l)));
        }

        return glist;
    }

    fn merge_env_upval(&mut self, pred_list: &Vec<Nref>) -> Vec<Ualias> {
        let mut glist = Vec::<Ualias>::new();
        let mut tt = self.total_upvalue;

        for i in 0..tt {
            let mut l: Vec<Val>::new();

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();
                    let pre_upval =
                        pre_env.borrow().effect.upvalue_alias[i].v.clone();

                    l.push(pre_upval);
                } else {
                    if self.is_upvalue_mut(i, &pre) {
                        l.push(Val::new_var());
                    } else {
                        l.push(Val::new_nothing());
                    }
                }
            }

            // perform the meet operation
            glist.push(Ualias::new(i, Val::join(&l)));
        }

        return glist;
    }

    // Enter into the specified CFG, which will create a Env object
    fn enter_env(&mut self, cfg: Nref) {
        debug_assert!(cfg.borrow().is_cfg());

        // new environment's memory tracking list
        let mut effect;

        // predecessor block lists, ie other CFG node jumps into the current CFG
        let pred_list = cfg.borrow().pred_control();

        let mut phi_list = LoopEffectJoinList::new();

        if pred_list.len() == 1 {
            effect = self.env_at(&pre).effect.clone();
        } else {
            debug_assert!(pred_list.len() > 1);

            let tt = self.total_mem;
            let bc = cfg.borrow().bc.clone();

            // merge the memory list
            let mem_list = self.merge_env_memory(&phi_list, &pred_list, tt);

            // merge the global alias list
            let global_list = self.merge_env_global(&pred_list);

            // merge the upvalue alias list
            let upval_list = self.merge_env_upval(&pred_list);

            effect = EffectEnv::new(mem_list, global_list, upval_list);
        }

        let eptr = Env::new_ptr(effect, cfg, phi_list);
        self.env_list.push(eptr.clone());
        self.cur_env = eptr;
    }

    // Leave the current environment
    fn leave_env(&mut self) {
        let env = self.cur_env();
        let unknown_list = Vec::<u32>::new();

        // patching the loop effect phi list
        for x in env.borrow().loop_effect_join_list.iter_mut() {
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

    fn cur_env(&self) -> Envptr {
        return self.cur_env.clone();
    }

    fn cur_cfg(&self) -> Nref {
        return self.cur_env().borrow().cfg.clone();
    }

    // Getting a node's memory if applicable. A node that can invole memory node
    // is those node that can result in, ie its value, a memory node. Notes,
    // most of the nodes will never result in a memory node, for example all the
    // numeric operations, comparison, unary etc ... will not.
    //
    // The node that could potentially result in a memory node, is as following
    //
    // 1) Global
    //    RvLoadGlobal
    //
    // 2) Upvalue
    //    RvLoadUpvalue
    //
    // 3) SubField
    //    RvMemIndexLoad
    //    RvMemDotLoad
    //
    // 4) Memory Access
    //    RvMemAccess
    //
    // 5) Other effect node
    //    Call
    //
    // 6) EffectJoin
    //    RvEffectJoin
    //
    // 7) Phi
    //
    // 8) Memory node itself
    //
    //    RvObjectCreate
    //    RvListCreate
    //
    // The phi node is not invole in our framework, when the analysis finds the
    // Phi, it will automatically mask all the Phi involved memory location to
    // be unknown. Other node will/may result in such things, and since we are
    // sensitive to subfield access, so the subfield memory loading node needs
    // to chase its use until we hit a none subfield node to learn whether it
    // is an alias or not.
    fn memory_of(&self, n: &Nref) -> Option<Hlocptr> {
        match &n.borrow().op.op {
            // (1) Global
            Opcode::RvLoadGlobal => {
                let global_name =
                    Node::global_name(n, &self.graph.borrow().func);
                return Option::Some(self.global_memory_of(global_name));
            }

            // (2) Upvalue
            Opcode::RvLoadUpvalue => {
                let upvalue_idx =
                    Node::upvalue_index(n, &self.graph.borrow().func);
                return Option::Some(self.upvalue_memory_of(upvalue_idx));
            }

            // (3) Subfield access
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                return self.sub_field_load_memory_of(n);
            }

            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                return self.sub_field_store_memory_of(n);
            }

            // (4) Memory access
            Opcode::RvMemAccess => {
                debug_assert!(n.borrow().has_value());

                // the first value input of node RvMemAccess will be a memory
                // node or PHI node.
                let mem = n.borrow().value[0].clone();

                if mem.borrow().is_rv_memory() {
                    return Option::Some(self.memory_alias_memory_of(mem));
                } else {
                    debug_assert!(mem.borrow().is_phi());

                    // it is a PHI, recursively call us
                    return self.memory_of(mem);
                }
            }

            // (5) Other effect node, which we cannot deal with though they are
            //     returning us the memory location's. These node needs to be
            //     handled properly
            Opcode::RvCall => {
                return Option::Some(self.unknown_memory.clone());
            }

            // (6) Effect join
            //     effect join is created during AA, they are just a barrier
            //     for different branch's modification of same piece of memory
            //     essentially speaking, EffectJoin always points to the same
            //     piece of memory.
            Opcode::RvEffectJoin => {
                return Option::Some(Node::effect_join_memory(n));
            }

            // (7) Phi node
            //     Phi node's memory is essentially been killed during AA, but
            //     we need a way to get all its involved memory location for
            //     us
            Option::RvPhi => {
                return Option::Some(self.unknown_memory.clone());
            }

            // Rest is not involved, ie cannot generate a memory location
            _ => return Option::None,
        }
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

    // trying to update the memory mutation/write effect with corresponding
    // memory address node
    fn try_update_memory_write_effect(&self, mem: &Nref, write: Nref) -> bool {
        debug_assert!(n.borrow().is_rv_memory());

        match self.cur_env().borrow().effect.memory_location_at(mem) {
            Option::Some(v) => {
                if v.borrow().is_unknown() {
                    return false;
                } else {
                    v.borrow_mut().update_write_effect(write);
                    return true;
                }
            }
            _ => return false,
        };
    }

    // udpate alias information
    fn alias_mem(&mut self, n: &Nref) {
        let env = self.cur_env();
        let cfg = env.borrow().cfg.clone();
        let idx = env.borrow().effect_index;
        debug_assert!(idx <= cfg.borrow().effect.len());

        // insert an alias at current places
        if let Effect::Alias(nlist) = &env.borrow_mut().after_effect_list[idx] {
            nlist.push(n);
        } else {
            let nlist = Vec::<Nref>::new();
            nlist.push(n);
            env.borrow_mut().after_effect_list[idx] = Effect::Alias(nlist);
        }

        // update the write_effect in current env's mem_list
        for x in env.borrow_mut().mem_list.iter_mut() {
            if Nref::ptr_eq(&x.mem, n) {
                x.write_effect = Option::None;
                break;
            }
        }
    }

    // ------------------------------------------------------------------------
    // Subroutine used to deal with each interested IR code -------------------
    fn on_sub_field(&mut self, n: Nref) -> R {
        // (0) get subfield access's target memory location if applicable
        let mem = self.memory_of(&n)?;

        // (1) check whether need to handle alias or not. Notes the sub field
        //     store can potentially generate alias
        match &n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                self.try_sub_field_store_alias(&n, &mem);
            }
        };

        // (2) update memory effect
        self.update_memory_effect(&mem, &n);

        return Option::Some(());
    }

    fn on_phi(&mut self, n: Nref) -> Option<()> {
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

    fn pass_effect_analysis(&mut self) {
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

    fn run(&mut self) {
        // phase 1, gather information
        self.pass_gather_information();

        // phase 2, perform the effect analysis
        self.pass_effect_analysis();
    }

    // -------------------------------------------------------------------------
    // Public interface exported from AA, used by phase2 Env
    fn env_aa_at(&self, cfg_id: Nid) -> Envptr {
        let id = self.env_map[Nid];
        return self.env_list[id].clone();
    }
}

// -----------------------------------------------------------------------------
// Phase 2, Optimization

// BB external information during the memory optimization passes
struct EnvMem {
    cfg: Nref,
    aa: Envptr,
    mem_list: Vec<HeapLocation>,
}

type Ememptr = Rc<RefCell<EnvMem>>;

struct EffectOpt {
    j: Jitptr,
    graph: FGraphptr,
    aa: AA,

    env_list: Vec<Ememptr>,
    cur_env: Ememptr,
    visited: BitVec,
    total_mem: u32,
}

impl EffectOpt {
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
            Effect::Alias(alist) => {
                for n in alist.iter() {
                    self.alias_mem(n);
                }
            }

            Effect::Write(eff) => {
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
