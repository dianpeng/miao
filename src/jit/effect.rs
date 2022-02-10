// Effect Analysis
//
//   Alias Analysis
//
//     Here we perform a simple AA based on traditional Andersen's AA.
//
//     The algorithm is 1) flow sensitive, 2) sub field insensitive
//
//     The precise version requires us to use CFLGraph to accomdate the field
//     sensitivity, which is too much efforts to track. Instead, we allow basic
//     assignment style alias, as with sub field, we will give up the track.
//
//
//   Global/Upvalue Analysis
//
//     It uses a simple value lattice to abstract out at each program point, the
//     value of Global and Upvalue's current status. The value lattice guarantees
//     that MOP join of value is conservative to be correct.
//
//   Transformation
//
//     Due to the fact it is a analysis, but it still performs transformation
//     by hoisting those effect opcodes from its CFG effect list when applicable.
//     The hoisted expression will have its effect list to have correct
//     dependency.  This transformation will apply to expression based on the AA
//     result. The hoisted expression later on can participate in GCM, GVN and
//     also load elimination optimization
//
//   ---------------------------------------------------------------------------
//
// The algorithm works as following. It starts to scan the instruction at each
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

use crate::jit::j::*;
use crate::jit::node::*;
use bitvec::prelude::*;

type LoopEffectJoinList = Vec<(u32, Nref)>;
type R = Option<()>;

// Represent a simple dependency chain, ie following dependency will be observed
//
//   1) true dependency, ie read after write
//   2) anti dependency, ie write after read
//   3) output dependency, ie write after write
//
// The above 3 dependencies are been modeled with following structures.
//
//   Every update to the *write* will make the *write* node's effect list points
//   to all the *read* node just been accumulated along with previous *write*.
//
//   Every update to the *read* will make the *read* node's effect list points to
//   the current *write* node if applicable.
//
#[derive(Clone)]
struct DepChain {
    write: Option<Nref>,
    read: Vec<Nref>,
}

type Dchainptr = Rc<RefCell<DepChain>>;

// Memory, represent a abstract location inside of program's heap, will be
// pointed to by different node when aliased. If it is unknown, it means we will
// need to use the fallback memory order
#[derive(Clone)]
struct Memory {
    // Initialize of this memory abstract location. It can be a memory node,
    // which means the memory node represents an exact location or a node that
    // just create the memory merge, ie sub-field store or the joining of 2
    // CFG nodes, ie effect join node.
    init: Nref,

    // Similar as parent, but recording successor nodes
    children: Vec<Memptr>,

    // Dependency chain of the memory region. Notes the first pass will not setup
    // the dependency information but the second pass will.
    dep: DepChain,

    // Signature, which enable quick comparison of each memory for equality,
    // ie it is just a set of id of memory node that forms the Memory structure
    sig: String,
}

type Memptr = Rc<RefCell<Memory>>;

// Helper memory lattice to allow joining different predecessor's memory table
// into one. It is only used during building each BB's memory table.
//
// Lattice as following:
//
//      [Nothing]
//          |
//      [Memory]
//          |
//      [Unknown]
//
// Notes, the maybe unknown is been materialized as Nothing which is part of the
// lattice. The MaybeUnknown is just a placeholder to show that a block which is
// not been visited could have memory use.
//
// This lattice is only used during merge environment's memory location of pass2
//
// -----------------------------------------------------------------------------
//
// Notes, here we give a brief proof of why we can use Nothing/UnknownMemory in
// not visited block during pass 2.
// The graph can only generate such cases when a loopback(jumpback) edge happened
//
//         |
//         |
//     [ Block A ]<--+
//         |         |
//         |         |
//        ...        |
//         |         |
//         |         |
//     [ Block B ]---+
//
//  So when we visit block A, we haven't visit block B, due to the loopback edge,
//  shown above. Notes, we cannot use prescanning of B to decide whether a memory
//  location X is been mutated in block B due to the AA is unknown to us. So we
//  just assume nothing happen at block B, and do the normal AA pass of block A
//  and block B.
//
//    Let's assume memory location X is mutated during pass2 and pass3 in block
//    A.
//
//    Assume memory location X in pass2 of block A is lattice Q, since the join
//    will only go down the lattice graph and cannot go up, ie
//
//                          Join(x, y) = GLB(x, y),
//
//    and we know that X location actually mutated during pass3, so it must be
//    the path A ... B does mutate the memory location X's lattice to a value P
//    which makes Join(Q, P) = GLB(Q, P), and we know the GLB(Q, P) != Q since X
//    is mutated. This means the correct version of X in Block A when the second
//    pass encounter X should be GLB(Q, P), lets denote GLB(Q, P) as lattice Y.
//
//    Okay, let's assume we learn this fact and set the memory location X's input
//    lattice as Y, and still along the path A ... B, the mutation happen will
//    set the lattice of location X to be P, this setting will not change along
//    the input lattice due to the programming model, so the new join operation
//    will be Join(Y, P), as we know that
//
//         Join(Q, P) = Y,
//
//      so Join(Y, P) = Join(Join(Q, P), P)
//
//    and the Join(Join(Q, P), P) = (Q ^ P) ^ P = Y ^ P = Y
//
//    Hence, perfect guess any memory location X's starting status and running
//    one pass for path A...B is same as running 2 passes. And 2 passes scanning
//    is convergent due to the fact that our lattice graph only has height of 2
//    (nothing is never set before data flow analysis)
//
// For global/upvalue, we use a much more simpler approach, ie we just assign
// the bottom of lattice for each global/upvalue when we hit a branch that has
// mutation operations, this is not ideal otherwise we will have to run iterative
// algorithm until it convergent since value lattice's height is not 2. For a
// mutation in branch we just record following :
//
//   1) memory operation happened
//      RvMemDotLoad/RvMemIndexLoad
//
//   2) global/upvalue mutation
//
//   3) call instruction
//
// So for global/upvalue, if the unknown block has above 3 cases happened, then
// we set bottom of lattice to these values and they are forever convergent
// Notes, when AA on the block A, the block A will only see its predecessor as
// block B, but we should count on all the above 3 operations along the path
// from B to A backwards. Due to the graph we generate is always a reducible
// so we just perform a only the fly backwards traversal of the CFG staring from
// B until we hit A, all nodes between A and B must hit A due to the loop, ie A
// dominats all loop body blocks in the loop nest.
enum MemoryLattice {
    Nothing,

    // Memory
    Mem(Memptr),

    // Unknown
    Unknown,
}

#[derive(Clone)]
struct MemoryLatticeFrom {
    cfg: Nref,
    memory: MemoryLattice,
}

impl MemoryLatticeFrom {
    fn new(cfg: Nref, mem: MemoryLattice) -> MemoryLatticeFrom {
        return MemoryLatticeFrom {
            cfg: cfg,
            memory: mem,
        };
    }
}

// Represent an abstract heap location, this is different from Memory since
// they can be merged but memory cannot, since they are unaliased known memory
// position inside of the heap.
#[derive(Clone)]
enum HeapLocation {
    Mem(Memptr),
    Unknown,
}

type Hlocptr = usize;
type Heap = Vec<Memptr>;
type HeapSite = Vec<HeapLocation>;

// Memory alias, represent a memory node and its abstract memory location's
// relationship. Notes. due to the fact that we don't have sub field alias
// so we allow memory location alias based on following rules, if 2 memory
// locations are partially overlapped, then they are aliased fully. Ie any
// operation involes these 2 memory location operations will be ordered.
struct MemRef {
    mem: Nref,
    loc: Hlocptr,
}

// A value lattice used to represent the Global/Upvalue alias value. The lattice
// is as following :
// ============================================================================
//
//                          Nothing
//                            |
//           -----------------+-----------------
//           |          |     |     |          |
//        Imm(a0) ... Imm(an) |   Mem(a0)    Mem(an)
//           |          |     |     |          |
//           +----------+     |     +----------+
//                |           |           |
//                |           |           |
//             NotMem         |           |
//                |           |           |
//                |           |           |
//                +------->Unknown<-------+
//
// ============================================================================
#[derive(PartialEq, Eq, Clone)]
enum ValueLattice {
    // Top of lattice, means we know nothing of the Val
    Nothing,

    // Constant value, and we know its actual constant value, can help global
    // variable load elimination
    Const(Imm),

    // Memory reference, ie pointed to a list of memory locations. Notes, we do
    // not support multiple memory location, ie set operation on memory location
    // for simplicity and complexity bound. If a join met 2 different memory
    // location then their GLB(x, y) is Unknown shown above in the graph
    Memory(Hlocptr),

    // indicate some unknown constant, and we know it is definitly not a memory
    // reference
    NotMem(Option<Nref>),

    // Bottom of the lattice, ie this should alias with anything we don't know
    Unknown,
}

// Used during join/meet operation
#[derive(Clone)]
struct ValueLatticeFrom {
    cfg: Nref,
    value: ValueLattice,
}

impl ValueLatticeFrom {
    fn new(cfg: Nref, value: ValueLattice) -> ValueLatticeFrom {
        return ValueLatticeFrom {
            cfg: cfg,
            value: value,
        };
    }
}

type ValueLatticeFromList = Vec<ValueLatticeFrom>;

// Global variable alias
#[derive(PartialEq, Eq, Clone)]
struct Galias {
    name: String,
    v: ValueLattice,
}

// Upvalue alias
#[derive(PartialEq, Eq, Clone)]
struct Ualias {
    offset: u32,
    v: ValueLattice,
}

// Side effect can be observed by a function/closure
#[derive(Clone)]
struct EffectEnv {
    // all observable site of the memory location, ie storing the HeapLocation
    // object. Where the Hlocptr points to
    heap_site: HeapSite,

    // memory alias
    memory_alias: Vec<MemRef>,

    // global variable alias known to us. If a global variable is not in the
    // list then it is pointed to unknown position, otherwise it may point to
    // valid HeapLocation.
    global_alias: Vec<Galias>,

    // upvalue alias known to us, similar as global_alias
    upvalue_alias: Vec<Ualias>,
}

// Environment, one per BB, and been kept alive until the analysis done
struct Env {
    // Current control flow graph
    cfg: Nref,

    // Current effect environment, been evaluated after abstract interpretation.
    // Once a BB has been interpreted done, this stores the effect lists after
    // the BB is done
    effect: EffectEnv,
}

type Envptr = Rc<RefCell<Env>>;

struct BBMutStateInfo {
    glb_mut: Vec<String>,
    upv_mut: Vec<u32>,

    // flag to indicate whether the subfield instructions are involved. This is
    // used to do consvertive AA of global/upvalue
    subfield_read: bool,
    subfield_write: bool,

    // flag to indicate whether we have call instructions
    call: bool,
}

// -----------------------------------------------------------------------------
// Effect Analysis,
//
//   Notes, the effect analysis is a static analyzer of the graph and it does
//   not mutate any node or states of the graph. All the information required
//   is been generated externally and recorded separately.
//
//   it includes following passes for analysis
//
//     1) Pass 1
//
//        Information gathering pass, mainly to allow global/upvalue to
//        conservatively guess its status when a block X's predecessor block Y
//        is not been visited, due to loopback edge. See comments above the
//        MemoryLattice struct.
//
//     2) Pass 2
//
//        this pass correctly set up each read/write/global/upvalue's effect
//        chain based on the on the fly alias analysis. After this pass, the
//        global order will be broken into different memory region related
//        effect dependency chain. However, this pass does not hoist the op
//        node from its CFG's effect list.
//
//     3) Pass 3
//
//        Revisit each CFG node again, and perform the join operation for each
//        exposed effect elements. Due to the pass 2, now all the predecessor
//        node must already been scanned and in well order, so the last pass
//        will generate final correct side effect set up for each BB. This pass
//        is mainly for compensating the cyclical path result of memory to be
//        unknown in the loop body but cannot be discovered without finishing
//        scanning the loop body.
// -----------------------------------------------------------------------------
struct EffectAnalysis {
    j: Jitptr,
    graph: FGraphptr,

    // For each memory node, ie RvListCreate/RvObjectCreate, we assign a number
    // to each one and put them into the list. Each BB's env node's
    // memory_location will make each memory_location the same order as in
    // the mem_num.
    mem_num: Vec<Nref>,

    // Mapping from CFG's node id to its corresponding Envptr, the result is a
    // u32(index) offset into env_list field
    env_map: Vec<u32>,

    // The actual list storing the Envptr
    env_list: Vec<Envptr>,

    // Used by pass2/3
    cur_env: Envptr,
    visited: BitVec,

    // Some bookkeeping data of function
    total_mem: u32,
    total_global: u32,
    total_upvalue: u32,

    // ------------------------------------------------------------------------
    // Private information, used during effect analysis
    //
    // filled in by the pass 1, ie contains the global state mutation information
    // for different BB.
    mut_info: Vec<BBMutStateInfo>,

    // Lists of memory that contain MaybeUnkonwn [element], we need to repatch
    // those memory pointer after all block been visited
    maybe_unknown_memory: Vec<Memptr>,
}

impl BBMutStateInfo {
    fn new() -> BBMutStateInfo {
        return BBMutStateInfo {
            glb_mut: Vec::new(),
            upv_mut: Vec::new(),
            subfield_read: false,
            subfield_write: false,
            call: false,
        };
    }
}

impl EffectAnalysis {
    fn mptr(&self) -> Mpptr {
        return self.j.borrow().mpool.clone();
    }

    // -------------------------------------------------------------------------
    // hlocptr related functions
    //
    //   The following functions maintain the value lattice correctly, user
    //   should use following function to manipulate hlocptr
    // -------------------------------------------------------------------------
    fn hlocptr_is_unknown(&self, hloc: &Hlocptr) -> bool {
        let loc = self.cur_env().borrow().effect.heap_site[*hloc].clone();
        return self.hloc_is_unknown(&loc);
    }

    fn hloc_is_unknown(&self, hloc: &HeapLocation) -> bool {
        match hloc {
            HeapLocation::Unknown => return true,
            HeapLocation::Mem(m) => {
                return m.borrow().is_unknown();
            }
        };
    }

    fn hloc_is_memory(&self, hloc: &HeapLocation) -> bool {
        return !self.hloc_is_unknown(hloc);
    }

    fn hlocptr_set_unknown(
        &mut self,
        hloc: &Hlocptr,
        barrier: Nref,
        update_effect: bool,
    ) {
        if self.hloc_is_unknown(&hloc) {
            return;
        }

        let loc = self.cur_env().borrow().heap_site[*hloc].clone();
        match loc {
            HeapLocation::Mem(m) => {
                m.borrow().set_unknown(barrier, update_effect);
            }
        };
        self.cur_env().borrow_mut().heap_site[*hloc].set_unknown();
    }

    fn hlocptr_different_memory_location(
        &self,
        lhs: &Hlocptr,
        rhs: &Hlocptr,
    ) -> bool {
        let l = self.cur_env().borrow().heap_site[*lhs].clone();
        let r = self.cur_env().borrow().heap_site[*rhs].clone();

        debug_assert!(!self.hloc_is_unknown(&l) && !self.hloc_is_unknown(&r));

        match (&l, &r) {
            (HeapLocation::Mem(a), HeapLocation::Mem(b)) => {
                return !Memptr::ptr_eq(a, b);
            }
            _ => {
                return false;
            }
        };
    }

    // Merge 2 piece of memory into one. This merging will just create a new
    // memory node that has merged memory chunk as its children. Note, we don't
    // flatten the memory region
    //  Notes, the |node| that triggers this effect chain flush, we just attach
    //  the current 2 (lhs, rhs)'s read operation as effect dependency for node
    //  |node| and mark each already existed branch to be empty.
    fn memory_merge(
        &self,
        node: &mut Nref,
        lhs_mem: &Memptr,
        rhs_mem: &Memptr,
        update_effect: bool,
    ) -> Memptr {
        // It is possible that the merged memory points to 2 same memory region
        // ie self assignment, for example like a.c = a; If so just do nothing
        if Memory::ptr_deep_eq(lhs_mem, rhs_mem) {
            return;
        }

        let new_mem = Memory::new_memory_join_ptr(lhs_mem, rhs_mem);

        fn merge_read_effect(n: &mut Nref, mem: &Memptr) {
            for x in mem.borrow().dep.read.iter() {
                Node::add_effect(n, x.clone());
            }
        }

        if update_effect {
            merge_read_effect(node, lhs_mem);
            merge_read_effect(node, rhs_mem);
        }

        new_mem.borrow_mut().dep.write = Option::Some(node.clone());

        return new_mem;
    }

    fn hlocptr_merge_memory_location(
        &self,
        n: &Nref,
        lhs: &Hlocptr,
        rhs: &Hlocptr,
        update_effect: bool,
    ) {
        let l = self.cur_env().borrow().heap_site[*lhs].clone();
        let r = self.cur_env().borrow().heap_site[*rhs].clone();

        debug_assert!(self.hloc_is_memory(&l) && self.hloc_is_memory(&r));

        // merge l and r into a single memory location
        let new_memory_site = match (&l, &r) {
            (HeapLocation::Mem(a), HeapLocation::Mem(b)) => {
                self.memory_merge(&n, &a, &b, update_effect)
            }
            _ => unreachable!(),
        };

        self.cur_env().borrow_mut().heap_site[*lhs] =
            HeapLocation::new_alias(new_memory_site.clone());

        self.cur_env().borrow_mut().heap_site[*rhs] =
            HeapLocation::new_alias(new_memory_site.clone());
    }

    fn hlocptr_maybe_memory(&self, loc: &Hlocptr) -> Option<Memptr> {
        if self.hlocptr_is_unknown(loc) {
            return Option::None;
        }
        let r = self.cur_env().borrow().heap_site[*loc].clone();
        match r {
            HeapLocation::Mem(x) => return x,
            _ => return Option::None,
        };
    }

    fn hlocptr_memory(&self, loc: &Hlocptr) -> Memptr {
        return self.hlocptr_maybe_memory(loc).unwrap();
    }

    fn hlocptr_update_read(&self, loc: &Hlocptr, n: &Nref, update_effect: bool) {
        let l = self.cur_env().borrow().heap_site[*loc].clone();
        let maybe_mem = self.hlocptr_maybe_memory(loc);
        match maybe_mem {
            Option::Some(mptr) => {
                mptr.borrow_mut().dep.update_read(n.clone(), update_effect);
            }
            _ => (),
        };
    }

    fn hlocptr_update_write(
        &self,
        loc: &Hlocptr,
        n: &Nref,
        update_effect: bool,
    ) {
        let l = self.cur_env().borrow().heap_site[*loc].clone();
        let maybe_mem = self.hlocptr_maybe_memory(loc);
        match maybe_mem {
            Option::Some(mptr) => {
                mptr.borrow_mut().dep.update_write(n.clone(), update_effect);
            }
            _ => (),
        };
    }

    // -------------------------------------------------------------------------
    // ValueLattice helper function
    //
    //    Same as hlocptr function, ValueLattice related function helps to
    //    enforce the lattice semantic for ValueLattice object. Notes, the
    //    ValueLattice object itself do have some feature to maintain but
    //    it doesn't have entire context
    // -------------------------------------------------------------------------
    fn vl_set_value_maybe_barrier(
        &self,
        dst: &mut ValueLattice,
        src: &ValueLattice,
        barrier: Nref,
        update_effect: bool,
    ) {
        if dst.is_unknown() {
            return;
        }

        match &dst {
            ValueLattice::Memory(memptr) => {
                self.hlocptr_set_unknown(&memptr, barrier, update_effect);
            }
            _ => (),
        };

        *dst = src;
    }

    fn set_global(&mut self, name: String, v: ValueLattice, b: Nref) {
        let env = self.cur_env();
        let pos = env.find_global_ref(name).unwrap();

        self.vl_set_value_maybe_barrier(pos, v, b, false);
    }

    fn set_global_and_order(&mut self, name: String, v: ValueLattice, b: Nref) {
        let env = self.cur_env();
        let pos = env.find_global_ref(name).unwrap();

        self.vl_set_value_maybe_barrier(pos, v, b, true);
    }

    fn set_upvalue(&mut self, idx: u32, v: ValueLattice, b: Nref) {
        let env = self.cur_env();
        let pos = env.find_upvalue_ref(idx).unwrap();

        self.vl_set_value_maybe_barrier(pos, v, b, false);
    }

    fn set_upvalue_and_order(&mut self, idx: u32, v: ValueLattice, b: Nref) {
        let env = self.cur_env();
        let pos = env.find_upvalue_ref(idx).unwrap();

        self.vl_set_value_maybe_barrier(pos, v, b, true);
    }

    // -------------------------------------------------------------------------
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
                let mem = n.borrow().v0();

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
                return Option::Some(self.unknown_hloc.clone());
            }

            // (6) Effect join
            //     effect join is created during AA, they are just a barrier
            //     for different branch's modification of same piece of memory
            //     essentially speaking, EffectJoin always points to the same
            //     piece of memory.
            Opcode::RvEffectJoin => {
                return Option::Some(
                    self.memory_alias_memory_of(Node::effect_join_memory(n)),
                );
            }

            // (7) Phi node
            //     Phi node's memory is essentially been killed during AA, but
            //     we need a way to get all its involved memory location for
            //     us
            Opcode::RvPhi => {
                return Option::Some(self.unknown_hloc.clone());
            }

            // (8) Memory node itself
            Opcode::RvObjectCreate | Opcode::RvListCreate => {
                return Option::Some(self.memory_alias_memory_of(n.clone()));
            }

            // Rest is not involved, ie cannot generate a memory location
            _ => return Option::None,
        }
    }

    fn global_memory_of(&self, name: String) -> Option<Hlocptr> {
        let v = self.cur_env().borrow().find_global(&name)?;

        if let ValueLattice::Memory(l) = v {
            return Option::Some(l);
        } else {
            return Option::None;
        }
    }

    fn upvalue_memory_of(&self, idx: u32) -> Option<Hlocptr> {
        let v = self.cur_env().borrow().find_upvalue(idx)?;

        if let ValueLattice::Memory(l) = v {
            return Option::Some(l);
        } else {
            return Option::None;
        }
    }

    fn sub_field_memory_of(&self, target: Nref) -> Option<Hlocptr> {
        return self.memory_of(target);
    }

    fn sub_field_load_memory_of(&self, n: Nref) -> Option<Hlocptr> {
        match n.borrow().op.op {
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                return self.sub_field_memory_of(n.borrow().una());
            }
            _ => unreachable!(),
        };
    }

    fn sub_field_store_memory_of(&self, n: Nref) -> Option<Hlocptr> {
        match n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                return self.sub_field_memory_of(n.borrow().v0());
            }
            _ => unreachable!(),
        };
    }

    // a memory node's aliased memory. Notes, in current set up, a memory node
    // directly alias at least its initial memory position, until it is been
    // sub field assigned with other memory, then the memory starts to alias
    // with each other.
    fn memory_alias_memory_of(&self, mem: Nref) -> Option<Hlocptr> {
        return self.cur_env().borrow().find_memory(&mem)?;
    }

    // Used by alias operation when we encounter a PHI node. This function will
    // recursively searching all the node(nested nodes) in PHI to find out a list
    // of invovled memory address.
    fn phi_memory_of(&self, phi: &Nref) -> Vec<Hlocptr> {
        let mut o = Vec::<Hlocptr>::new();
        for x in phi.borrow().value.iter() {
            match x.borrow().op.op {
                Opcode::RvPhi => {
                    let mut r = self.phi_memory_of(&x);
                    o.append(&mut r);
                }
                _ => {
                    let mem = self.memory_of(&x);
                    o.push(mem);
                }
            };
        }
    }

    // Value of, similar as memory_of but it tries to expand the value loading
    // into larger lattice scope.
    fn global_value_of(&self, name: String) -> ValueLattice {
        match self.cur_env().borrow().find_global(&name) {
            Option::Some(vv) => return vv,
            _ => unreachable!(),
        };
    }

    fn upvalue_value_of(&self, idx: u32) -> ValueLattice {
        match self.cur_env().borrow().find_upvalue(idx) {
            Option::Some(vv) => return vv,
            _ => unreachable!(),
        };
    }

    fn value_of(&self, n: &Nref) -> ValueLattice {
        match &n.borrow().op.op {
            // (1) Global
            Opcode::RvLoadGlobal => {
                let global_name =
                    Node::global_name(n, &self.graph.borrow().func);
                return self.global_value_of(global_name);
            }

            // (2) Upvalue
            Opcode::RvLoadUpvalue => {
                let upvalue_idx =
                    Node::upvalue_index(n, &self.graph.borrow().func);
                return self.upvalue_value_of(upvalue_idx);
            }

            // (3) Subfield access
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                match self.sub_field_load_memory_of(n) {
                    Option::Some(vv) => {
                        return ValueLattice::new_memory(vv);
                    }
                    _ => {
                        return ValueLattice::new_unknown();
                    }
                };
            }

            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                match self.sub_field_store_memory_of(n) {
                    Option::Some(vv) => {
                        return ValueLattice::new_memory(vv);
                    }
                    _ => {
                        return ValueLattice::new_unknown();
                    }
                };
            }

            // (4) Memory access
            Opcode::RvMemAccess => {
                debug_assert!(n.borrow().has_value());

                // the first value input of node RvMemAccess will be a memory
                // node or PHI node.
                let mem = n.borrow().v0();

                if mem.borrow().is_rv_memory() {
                    match self.memory_alias_memory_of(mem) {
                        Option::Some(vv) => {
                            return ValueLattice::new_memory(vv);
                        }
                        _ => {
                            return ValueLattice::new_unknown();
                        }
                    };
                } else {
                    debug_assert!(mem.borrow().is_phi());
                    return ValueLattice::new_unknown();
                }
            }

            // (5) Other effect node, which we cannot deal with though they are
            //     returning us the memory location's. These node needs to be
            //     handled properly
            Opcode::RvCall => {
                return ValueLattice::new_unknown();
            }

            // (6) Effect join
            //     effect join is created during AA, they are just a barrier
            //     for different branch's modification of same piece of memory
            //     essentially speaking, EffectJoin always points to the same
            //     piece of memory.
            Opcode::RvEffectJoin => {
                return ValueLattice::new_memory(
                    self.memory_alias_memory_of(Node::effect_join_memory(n)),
                );
            }

            // (7) Phi node
            //     Phi node's memory is essentially been killed during AA, but
            //     we need a way to get all its involved memory location for
            //     us
            Opcode::RvPhi => {
                return self.phi_value_of(n);
            }

            // (8) Memory node itself
            Opcode::RvObjectCreate | Opcode::RvListCreate => {
                match self.memory_alias_memory_of(n.clone()) {
                    Option::Some(vv) => {
                        return ValueLattice::new_memory(vv);
                    }
                    _ => {
                        return ValueLattice::new_unknown();
                    }
                };
            }

            // (9) Other nodes that we can recognize in our lattice system
            _ => {
                // 1) Node generate immediate numbers
                //
                // 2) Node that cannot generate a memory output
                //
                //    2.1) Arithmetic operations
                //    2.2) Comparison operations
                //    2.3) Unary operations

                if n.borrow().is_imm() {
                    let imm = n.borrow().imm.clone();
                    debug_assert!(imm.is_imm());
                    return ValueLattice::new_imm(imm);
                }

                // All other nodes must not be a memory node, otherwise we should
                // have already found it and we just mark all the other as
                // lattice not memory
                debug_assert!(n.borrow().must_not_memory());
                return ValueLattice::new_not_mem(n);
            }
        }

        return ValueLattice::new_unknown();
    }

    // -------------------------------------------------------------------------
    // Misc analysis helper functions
    // trying to update the memory mutation/write effect with corresponding
    // memory address node
    fn update_memory_effect(
        &self,
        loc: &Hlocptr,
        effect: &Nref,
        update_effect: bool,
    ) -> bool {
        if self.hlocptr_is_unknown(&loc) {
            return false;
        }

        match effect.borrow().op.op {
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                self.hlocptr_update_read(loc, effect, update_effect);
            }
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                self.hlocptr_update_write(loc, effect, update_effect);
            }
        };
        return true;
    }

    // mark the corresponding memory location to be unknown. whoever points to
    // that piece of memory location will learn that the memory is a unknown
    // region which is in the bottom of our value lattice, and all the RW for
    // it will become total order.
    fn alias_mem(&mut self, loc: &Hlocptr) {
        self.hlocptr_set_unknown(loc);
    }

    fn alias_mem_and_order(&mut self, loc: &Hlocptr, barrier: &Nref) {
        self.hlocptr_set_unknown_and_order(loc, barrier);
    }

    // ------------------------------------------------------------------------
    // This function tries to detect the alias information with the memory store
    // operation. memory_store(target, value), if target, value node points to
    // 2 different memory location, then we create a merge block from these 2
    // memory location and make them alias with each other
    fn try_sub_field_store_alias(
        &mut self,
        node: &Nref,
        target: &Hlocptr,
        update_effect: bool,
    ) {
        let val = node.borrow().value[2].clone();
        let value = self.memory_of(&val)?;

        // if value and target are different piece of memory location, including
        // unknown, notes unknown can be referred to anything
        if self.hlocptr_is_unknown(&value) || target.hlocptr_is_unknown(&target)
        {
            if update_effect {
                self.hlocptr_set_unknown_and_order(&value, node);
                self.hlocptr_set_unknown_and_order(&target, node);
            } else {
                self.hlocptr_set_unknown(&value);
                self.hlocptr_set_unknown(&target);
            }
            return;
        }

        // If they are pointed into different memory locations
        let diff = self.hlocptr_different_memory_location(&value, &target);
        if diff {
            self.hlocptr_merge_memory_location(
                node,
                &value,
                &target,
                update_effect,
            );
        }
    }

    // Environment accessor
    fn cur_env(&self) -> Envptr {
        return self.cur_env.clone();
    }

    fn cur_cfg(&self) -> Nref {
        return self.cur_env().borrow().cfg.clone();
    }

    fn env_at(&self, cfg: &Nref) -> Option<Envptr> {
        debug_assert!(cfg.borrow().is_cfg());

        let id = cfg.borrow().id;
        let index = self.env_map[id];
        if index == std::u32::MAX {
            return Option::None;
        } else {
            debug_assert!((index as usize) < self.env_list.len());
            return Option::Some(self.env_list[index as usize].clone());
        }
    }

    fn effect_barrier(&self, cfg: &Nref) {
        if cfg.borrow().effect.len() != 0 {
            return cfg.borrow().effect[0].clone();
        } else {
            let effect_start = self
                .mptr()
                .borrow_mut()
                .new_rv_effect_start(cfg.borrow().bc.clone());

            let mut mut_cfg = cfg.clone();
            Node::add_effect(&mut mut_cfg, effect_start.clone());
            return effect_start;
        }
    }

    // * ----------------------------------------------------------------------
    // *  Pass 1
    // *
    // *  gather information of global/upvalue/memory access(write) information
    // *  in each basic block
    // * ----------------------------------------------------------------------
    fn pass1_gather_information(&mut self) {
        let cfg = self.graph.borrow().cfg_start.clone();

        for c in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            let tt_side_effect = c.borrow().effect.len();
            let mut state = &mut self.mut_info[c.borrow().id];
            let func = self.graph.borrow().func.clone();

            for i in 0..tt_side_effect {
                for enode in EffectPOIter::new(&i, &c.borrow().effect[i]) {
                    match enode.borrow().op.op {
                        Opcode::RvSetGlobal => {
                            state.glb_mut.push(Node::global_name(&enode, &func));
                        }

                        Opcode::RvSetUpval => {
                            state
                                .upv_mut
                                .push(Node::upvalue_index(&enode, func));
                        }

                        Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                            state.subfield_read = true;
                        }

                        Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                            state.subfield_write = true;
                        }

                        Opcode::RvCall => {
                            state.call = true;
                        }

                        _ => (),
                    };
                }
            }
        }
    }

    fn backwards_test<C: FnMut(&Nref) -> bool>(
        looptail: &Nref,
        loophead: &Nref,
        max: u32,
        c: C,
    ) -> bool {
        let mut wq = Nqueue::new();
        let mut visited = BitVec::New();
        visited.resize(max as usize, false);

        wq.push(looptail.clone());
        while wq.len() != 0 {
            let top = wq.pop().unwrap();
            let id = top.borrow().id as usize;
            if !visited[id] {
                if c(&top) {
                    return true;
                }
                *visited.get_mut(id).unwrap() = true;
            }

            let plist = top.borrow().pred_control();
            for x in plist.iter_mut() {
                if !visited[x.borrow().id as usize]
                    && !Nref::ptr_eq(&x, loophead)
                {
                    wq.push(x);
                }
            }
        }

        return false;
    }

    fn is_upvalue_mut(
        &self,
        looptail: &Nref,
        loophead: &Nref,
        idx: u32,
    ) -> bool {
        let tester = |n| -> bool {
            let id = n.borrow().id as usize;
            if self.mut_info[id].subfield_write || self.mut_info[id].call {
                return true;
            }
            return self.mut_info[id]
                .upv_mut
                .iter()
                .position(|x| *x == idx)
                .is_some();
        };
        return EffectAnalysis::backwards_test(
            looptail,
            loophead,
            self.j.borrow().max_node_id(),
            tester,
        );
    }

    fn is_global_mut(
        &self,
        looptail: &Nref,
        loophead: &Nref,
        name: &str,
    ) -> bool {
        let tester = |n| -> bool {
            let id = n.borrow().id as usize;
            if self.mut_info[id].subfield_write || self.mut_info[id].call {
                return true;
            }
            return self.glb_mut[id]
                .upv_mut
                .iter()
                .position(|x| *x == idx)
                .is_some();
        };

        return EffectAnalysis::backwards_test(
            looptail,
            loophead,
            self.j.borrow().max_node_id(),
            tester,
        );
    }

    // * ----------------------------------------------------------------------
    // *  Pass 2
    // *    effect analysis, this pass will break all the side effect chain
    // *    based on alias analysis results, ie if multiple nodes access
    // *    the same piece of memory, based on analysis, they will be effect
    // *    depend on each other.
    // * ----------------------------------------------------------------------

    // try to merge all the known memory location of a BB's predecessor nodes
    fn pass2_merge_env_memory(
        &self,
        pred_list: &Vec<Nref>,
        barrier: Nref,
    ) -> Vec<HeapLocation> {
        let mut heap_list = Vec::<HeapLocation>::new();

        let tt = self.total_mem;

        for i in 0..tt {
            let mut mem_lattice_list = Vec::<MemoryLattice>::new();
            let mut mem_list = Heap::new();

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let prev_env = self.env_at(&pre).unwrap();

                    let prev_hlocptr =
                        prev_env.borrow().effect.memory_alias[i].loc;

                    let prev_hloc = prev_env.borrow().heap_site
                        [prev_hlocptr as usize]
                        .clone();

                    match prev_hloc {
                        HeapLocation::Unknown => {
                            mem_lattice_list.push(MemoryLattice::new_unknown());
                        }
                        HeapLocation::Mem(m) => {
                            mem_lattice_list.push(MemoryLattice::new_mem(m));
                        }
                    };
                } else {
                    mem_lattice_list.push(MemoryLattice::new_nothing());
                }
            }

            // Perform the join/meet operation for the memory lattice
            let mem = MemoryLattice::join(&mem_lattice_list, barrier, false);
            heap_list.push(mem);
        }

        return heap_list;
    }

    fn pass2_merge_env_global(
        &mut self,
        pred_list: &Vec<Nref>,
        barrier: Nref,
    ) -> Vec<Galias> {
        let mut glist = Vec::<Galias>::new();
        let mut tt = self.total_global;

        for i in 0..tt {
            let mut l = ValueLatticeFromList::new();
            let global_name = self.global_name(i);

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();
                    let pre_glb_val =
                        pre_env.borrow().effect.global_alias[i].v.clone();

                    l.push(ValueLatticeFrom::new(pre.clone(), pre_glb_val));
                } else {
                    let vlattice = if self.is_global_mut(&global_name, &pre) {
                        ValueLattice::new_unknown()
                    } else {
                        ValueLattice::new_nothing()
                    };

                    l.push(ValueLatticeFrom::new(pre.clone(), vlattice));
                }
            }

            // perform the meet operation
            glist.push(Galias::new(
                global_name,
                ValueLattice::join(&l, barrier, false),
            ));
        }

        return glist;
    }

    fn pass2_merge_env_upval(
        &mut self,
        pred_list: &Vec<Nref>,
        barrier: Nref,
    ) -> Vec<Ualias> {
        let mut ulist = Vec::<Ualias>::new();
        let mut tt = self.total_upvalue;

        for i in 0..tt {
            let mut l = ValueLatticeFromList::new();

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();
                    let pre_upval =
                        pre_env.borrow().effect.upvalue_alias[i].v.clone();

                    l.push(ValueLatticeFrom::new(pre.clone(), pre_upval));
                } else {
                    let vlattice = if self.is_upvalue_mut(i, &pre) {
                        ValueLattice::new_unknown()
                    } else {
                        ValueLattice::new_nothing()
                    };

                    l.push(ValueLatticeFrom::new(pre.clone(), vlattice));
                }
            }

            // perform the meet operation
            ulist.push(Ualias::new(i, ValueLattice::join(&l, barrier, false)));
        }

        return ulist;
    }

    // Enter into the specified CFG, which will create a Env object
    fn enter_env(&mut self, cfg: Nref) {
        debug_assert!(cfg.borrow().is_cfg());

        // new environment's memory tracking list
        let mut effect;

        // predecessor block lists, ie other CFG node jumps into the current CFG
        let pred_list = cfg.borrow().pred_control();

        let barrier = self.effect_barrier(&cfg);

        // merge the memory list
        let mem_list = self.pass2_merge_env_memory(&pred_list, barrier.clone());

        // merge the global alias list
        let global_list =
            self.pass2_merge_env_global(&pred_list, barrier.clone());

        // merge the upvalue alias list
        let upval_list = self.pass2_merge_env_upval(&pred_list, barrier.clone());

        effect = EffectEnv::new(mem_list, global_list, upval_list);

        let cfg_id = cfg.borrow().id;
        let eptr = Env::new_ptr(effect, cfg);
        self.env_list.push(eptr.clone());
        self.cur_env = eptr;
        self.env_map[cfg_id] = (self.env_list.len() - 1) as u32;
    }

    // Leave the current environment
    fn leave_env(&mut self) {}

    fn pass2_on_sub_field(&mut self, n: Nref) -> R {
        // (0) get subfield access's target memory location if applicable
        let mem = self.memory_of(&n)?;

        // (1) check whether need to handle alias or not. Notes the sub field
        //     store can potentially generate alias
        match &n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                self.try_sub_field_store_alias(&n, &mem, false);
            }
        };

        // (2) update memory effect
        self.update_memory_effect(&mem, &n, false);

        return Option::Some(());
    }

    fn pass2_on_phi(&mut self, n: Nref) -> R {
        let mem = self.phi_memory_of(&n);
        for xx in mem.iter() {
            self.alias_mem(&xx, &n);
        }
        return Option::Some(());
    }

    // Global operations, this pass can optimize away global load operation if
    // the global value is known to us
    fn pass2_on_set_global(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();
        let global_name = Node::global_name(&n, func);

        self.set_global(global_name, v, n);

        return Option::Some(());
    }

    fn pass2_on_set_upvalue(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();

        let upvalue_index = Node::upvalue_index(&n, func);

        self.set_upvalue(upvalue_index, v, n);

        return Option::Some(());
    }

    // Call instruction. For unknown call, the call's input is as following :
    //
    //   (Arg#1, ..., Arg#N; Global#1, ..., Global#N)
    //
    // After the call the Arg1 ... ArgN and Global1 ... GlobalN will have to be
    // assumed as in unknown region. Notes, we need to take care of each args,
    // since if args doesn't render a known memory location, then no need to
    // change it anymore
    fn pass2_on_call(&mut self, n: Nref) -> R {
        for xx in n.borrow().value.iter() {
            let maybe_mem = self.memory_of(&xx);

            match maybe_mem {
                Option::Some(m) => {
                    self.alias_mem(&m);
                }
                _ => (),
            };
        }

        // mark all the global in current CFG to be unknown, notes upvalue is
        // not needed since they are private to the current closure and cannot
        // be leaked outside unless from memory location which is been taken
        // care of already.
        self.cur_env().borrow_mut().unknownify_global();

        return Option::Some(());
    }

    fn pass2_effect_analysis(&mut self) {
        let cfg = self.graph.borrow().cfg_start.clone();

        for c in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            self.enter_env(c.clone());

            for enode in c.borrow().effect.iter() {
                for x in EffectPOIter::new(&i, &enode) {
                    match x.borrow().op.op {
                        Opcode::RvMemIndexLoad
                        | Opcode::RvMemDotLoad
                        | Opcode::RvMemIndexStore
                        | Opcode::RvMemDotStore => {
                            self.pass2_on_sub_field(x.clone());
                        }

                        Opcode::RvSetGlobal => {
                            self.pass2_on_set_global(x.clone());
                        }

                        Opcode::RvSetUpvalue => {
                            self.pass2_on_set_upvalue(x.clone());
                        }

                        Opcode::RvCall => {
                            self.pass2_on_call(x.clone());
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
                            self.pass2_on_phi(x.clone());
                        }

                        _ => (),
                    };
                }
            }

            self.leave_env();
        }
    }

    // * ----------------------------------------------------------------------
    // *  Pass 3
    // *    Use final information to generate node hoisting. This pass will
    // *    mutate the graph and generate entirely correct dependency chain
    // * ----------------------------------------------------------------------

    fn pass3_merge_env_memory(
        &self,
        pred_list: &Vec<Nref>,
        barrier: &Nref,
    ) -> Vec<HeapLocation> {
        let mut heap_list = Vec::<HeapLocation>::new();

        let tt = self.total_mem;

        for i in 0..tt {
            let mut mem_lattice_list = Vec::<MemoryLattice>::new();
            let mut mem_list = Heap::new();

            for pre in pred_list.iter() {
                let prev_env = self.env_at(&pre).unwrap();

                let prev_hlocptr = prev_env.borrow().effect.memory_alias[i].loc;

                let prev_hloc =
                    prev_env.borrow().heap_site[prev_hlocptr as usize].clone();

                match prev_hloc {
                    HeapLocation::Unknown => {
                        mem_lattice_list.push(MemoryLattice::new_unknown());
                    }
                    HeapLocation::Mem(m) => {
                        mem_lattice_list.push(MemoryLattice::new_mem(m));
                    }
                };
            }

            // perform the join/meet operation for the memory lattice
            let mem = MemoryLattice::join(mem_lattice_list, barrier, true);
            heap_list.push(mem);
        }

        return heap_list;
    }

    fn pass3_merge_env_global(
        &mut self,
        pred_list: &Vec<Nref>,
        barrier: &Nref,
    ) -> Vec<Galias> {
        let mut glist = Vec::<Galias>::new();
        let mut tt = self.total_global;

        for i in 0..tt {
            let mut l = ValueLatticeFromList::new();
            let global_name = self.global_name(i);

            for pre in pred_list.iter() {
                let pre_env = self.env_at(&pre).unwrap();
                let pre_glb_val =
                    pre_env.borrow().effect.global_alias[i].v.clone();

                assert!(pre_glb_val.maybe_not_mem_valid());

                l.push(ValueLatticeFrom::new(pre.clone(), pre_glb_val));
            }

            // perform the meet operation
            glist.push(Galias::new(
                global_name,
                ValueLattice::join(&l, barrier, true),
            ));
        }

        return glist;
    }

    fn pass3_merge_env_upval(
        &mut self,
        pred_list: &Vec<Nref>,
        barrier: &Nref,
    ) -> Vec<Ualias> {
        let mut ulist = Vec::<Ualias>::new();
        let mut tt = self.total_upvalue;

        for i in 0..tt {
            let mut l = ValueLatticeFromList::new();

            for pre in pred_list.iter() {
                let pre_env = self.env_at(&pre).unwrap();
                let pre_upval =
                    pre_env.borrow().effect.upvalue_alias[i].v.clone();

                assert!(pre_glb_val.maybe_not_mem_valid());

                l.push(ValueLatticeFrom::new(pre.clone(), pre_upval));
            }

            // perform the meet operation
            ulist.push(Ualias::new(i, ValueLattice::join(&l, barrier, true)));
        }

        return ulist;
    }

    fn revisit_env_start(&self, cfg: Nref) {
        debug_assert!(cfg.borrow().is_cfg());

        let envptr = self.env_at(&cfg);

        // redo joining operations
        let pred_list = cfg.borrow().pred_control();
        let barrier = self.effect_barrier(&cfg);

        let heap_list = self.pass3_merge_env_memory(&pred_list, &barrier);
        let global_list = self.pass3_merge_env_global(&pred_list, &barrier);
        let upval_list = self.pass3_merge_env_upval(&pred_list, &barrier);

        envptr
            .borrow()
            .effect
            .renter(heap_list, global_list, upval_list);

        self.cur_env = envptr;
    }

    fn revisit_env_end(&self) {}

    fn hoist_from_cfg_effect_list(&self, n: &Nref) {
        let mut cur_cfg = self.cur_cfg();
        assert!(Node::remove_effect_node(&mut cur_cfg, n));
    }

    fn update_memory_effect_and_try_hoist_from_cfg(
        &self,
        mem: &Hlocptr,
        n: &Nref,
    ) {
        if !self.update_memory_effect(mem, n, true) {
            return;
        }

        self.hoist_from_cfg_effect_list(n);
    }

    // For pass3, we do generate the side effect dependency for all the sub field
    // as long as the memory and also tries to hoist it out of the CFG's effect
    // list if applicable
    fn pass3_on_sub_field(&mut self, n: Nref) -> R {
        let mem = self.memory_of(&n)?;

        match &n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                self.try_sub_field_store_alias(&n, &mem, true);
            }
        };

        self.update_memory_effect_and_try_hoist_from_cfg(&mem, &n);
        return Option::Some(());
    }

    fn pass3_on_set_global(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();
        let global_name = Node::global_name(&n, &func);
        self.set_global_and_order(global_name, v, n);

        return Option::Some(());
    }

    // Optimize the load global instructions.
    //
    //   1) Try to eliminate the load global instructions if applicable
    //
    //      when global alias slot contains an immediate number or a constant
    //      number, the load global instruction will be eliminated entirely
    //
    //   2) Mutate the effect chain
    //
    //      By default, the load global instructions are all ordered in CFG's
    //      effect list which will not join GCM, so not able to be hoisted out
    //      of the loop even it is a invariant. For load global as long as we
    //      know the value is a not memory node, then we can simply hoist the
    //      load global out of the CFG effect list and chain it with the recent
    //      most global set node.
    //
    fn pass3_upvalue_or_global_load_optimize(
        &mut self,
        mut n: Nref,
        v: ValueLattice,
    ) {
        // The store-load elimination for global/upvalue. As long as we find out
        // that the current storage points-to value that is immutable, we can
        // eliminate the load of upvalue and global variables with the value
        // current pointed-to
        match v {
            ValueLattice::Const(imm) => {
                // the loading becomes a directly load a imm value
                let imm_node = self
                    .mptr()
                    .borrow_mut()
                    .new_imm_node(imm, n.borrow().bc.clone());

                // just use the immediate node to replace the global/upvalue
                // load operation
                Node::replace_and_dispose(&mut n, imm_node);
            }

            ValueLattice::NotMem(d) => {
                debug_assert!(d.is_some());

                // this node is the node that generates the not memory value.
                // do a replace of the load node n
                Node::replace_and_dispose(&mut n, d.as_ref().unwrap().clone());
            }

            ValueLattice::Memory(memloc) => {
                // Memory node has extra milellage to cover. If memory
                // location is not unknown, load the memory location with
                // RvMemAccess and chain it to the dependency chain currently
                // owned by the memory
                let is_unknown = self.hlocptr_is_unknown(&memloc);

                if !is_unknown {
                    let memptr = self.hlocptr_memory(&memloc);

                    // Check wether the pointed memory node is a singleton
                    // memory node, ie not merged because of sub-field operation.
                    // If so directly load the memory otherwise chain the current
                    // node in the read dependency and hoist it out of the
                    // current CFG
                    let is_exact = memptr.borrow().is_exact();

                    if is_exact {
                        let init = memptr.borrow().memory_node();

                        let access = self
                            .mptr()
                            .borrow_mut()
                            .new_rv_mem_access_no_effect(init);

                        memptr.borrow_mut().update_read(access.clone(), true);

                        Node::replace_and_dispose(&mut n, access);
                    } else {
                        memptr.borrow_mut().update_read(n.clone(), true);

                        self.hoist_from_cfg_effect_list(&n);
                    }
                }
            }

            _ => (),
        };
    }

    fn pass3_on_load_global(&mut self, mut n: Nref) -> R {
        let func = self.graph.borrow().func.clone();
        let global_name = Node::global_name(&n, func);
        let v = self.cur_env().borrow().find_global(global_name)?;
        self.pass3_upvalue_or_global_load_optimize(n, v);
        return Option::Some(());
    }

    fn pass3_on_set_upvalue(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();
        let upvalue_index = Node::upvalue_index(&n, &func);

        self.set_upvalue_and_order(upvalue_index, v, n);

        return Option::Some(());
    }

    fn pass3_on_load_upvalue(&mut self, n: Nref) -> R {
        let func = self.graph.borrow().func.clone();
        let func = self.graph.borrow().func.clone();
        let upvalue_index = Node::upvalue_index(&n, &func);
        let v = self.cur_env().borrow().find_upvalue(idx)?;

        self.pass3_upvalue_or_global_load_optimize(n, v);
        return Option::Some(());
    }

    fn pass3_on_phi(&mut self, n: Nref) -> R {
        let mem = self.phi_memory_of(&n);
        for xx in mem.iter() {
            self.alias_mem_and_order(&xx, &n);
        }
        return Option::Some(());
    }

    fn pass3_on_call(&mut self, n: Nref) -> R {
        for xx in n.borrow().value.iter() {
            let maybe_mem = self.memory_of(&xx);
            match maybe_mem {
                Option::Some(m) => {
                    self.alias_mem_and_order(&m, &n);
                }
                _ => (),
            };
        }

        // mark all the global in current CFG to be unknown, notes upvalue is
        // not needed since they are private to the current closure and cannot
        // be leaked outside unless from memory location which is been taken
        // care of already.
        self.cur_env().borrow_mut().unknownify_global_and_order(&n);

        return Option::Some(());
    }

    fn pass3_on_mem_access(&mut self, n: Nref) -> R {
        let mloc = self.memory_of(&n)?;
        let maybe_mem = self.hlocptr_maybe_memory(&mloc);
        match maybe_mem {
            Option::Some(mem) => {
                // this RvMemAccess node already have a dependency which is too
                // strong. We should remove it and the call update_read will
                // update its dependency chain
                debug_assert!(n.borrow().effect.len() >= 1);
                debug_assert!(n.borrow().effect[0].borrow().op.side_effect);

                n.borrow_mut().remove_effect(1);

                // the memory points to an exact memory position along with its
                // dependency chain.
                mem.borrow_mut().update_read(&n, true);
                self.hoist_from_cfg_effect_list(&n);
            }
            _ => (),
        };
    }

    fn pass3_effect_optimization(&mut self) {
        let cfg = self.graph.borrow().cfg_start.clone();

        for c in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            self.revisit_env_start(c.clone());

            for enode in c.borrow().effect.iter() {
                for x in EffectPOIter::new(&i, &enode) {
                    match x.borrow().op.op {
                        Opcode::RvMemIndexLoad
                        | Opcode::RvMemDotLoad
                        | Opcode::RvMemIndexStore
                        | Opcode::RvMemDotStore => {
                            self.pass3_on_sub_field(x.clone());
                        }

                        Opcode::RvSetGlobal => {
                            self.pass3_on_set_global(x.clone());
                        }
                        Opcode::RvLoadGlobal => {
                            self.pass3_on_load_global(x.clone());
                        }

                        Opcode::RvSetUpvalue => {
                            self.pass3_on_set_upvalue(x.clone());
                        }
                        Opcode::RvLoadUpvalue => {
                            self.pass3_on_load_upvalue(x.clone());
                        }

                        Opcode::RvCall => {
                            self.pass3_on_call(x.clone());
                        }
                        Opcode::RvPhi => {
                            self.pass3_on_phi(x.clone());
                        }
                        Opcode::RvMemAccess => {
                            self.pass3_on_mem_access(x.clone());
                        }

                        _ => (),
                    };
                }
            }

            self.revisit_env_end();
        }
    }

    fn run(&mut self) {
        // phase 1, gather information
        self.pass1_gather_information();

        // phase 2, perform the effect analysis
        self.pass2_effect_analysis();

        // phase 3, perform effect optimization
        self.pass3_effect_optimization();
    }

    // -------------------------------------------------------------------------
    // Public interface exported from AA, used by phase2 Env
    fn env_aa_at(&self, cfg_id: Nid) -> Envptr {
        let id = self.env_map[cfg_id];
        return self.env_list[id].clone();
    }
}

impl DepChain {
    fn update_write(&mut self, mut the_write: Nref, update_effect: bool) {
        // the write should be ordered after all the write and read
        match &self.write {
            Option::Some(w) => {
                if Nref::ptr_eq(&the_write, &w) {
                    return;
                }
                if update_effect {
                    Node::add_effect(&mut the_write, w.clone());
                }
            }
            _ => (),
        };

        for r in self.read.iter() {
            Node::add_effect(&mut the_write, r.clone());
        }

        // flush all the read and write
        self.write = Option::Some(the_write);
        self.read.clear();
    }

    fn update_read(&mut self, mut the_read: Nref, update_effect: bool) {
        // the read should happen right after the write
        match &self.write {
            Option::Some(w) => {
                if update_effect {
                    Node::add_effect(&mut the_read, w.clone());
                }
            }
            _ => (),
        };

        self.read.push(the_read);
    }

    fn flush(&mut self, mut the_barrier: Nref, update_effect: bool) {
        self.update_write(the_barrier, update_effect);
    }

    fn current_write(&self) -> Option<Nref> {
        return self.write.clone();
    }

    fn has_write(&self) -> bool {
        return self.write.is_some();
    }

    fn has_read(&self) -> bool {
        return self.read.len() != 0;
    }

    // -------------------------------------------------------------------------
    // Static methods
    fn new_default() -> DepChain {
        return DepChain {
            write: Option::Some,
            read: Vec::new(),
        };
    }

    fn new_default_ptr() -> Dchainptr {
        return Rc::new(RefCell::new(DepChain::new_default()));
    }

    fn new_write(w: Nref) -> DepChain {
        return DepChain {
            write: Option::Some(w),
            read: Vec::new(),
        };
    }

    fn new_write_ptr(w: Nref) -> Dchainptr {
        return Rc::new(RefCell::new(DepChain::new_write(w)));
    }

    fn eq(lhs: &DepChain, rhs: &DepChain) -> bool {
        match (lhs.write, rhs.write) {
            (Option::Some(wl), Option::Some(wr)) => {
                if !Nref::ptr_eq(wl, wr) {
                    return false;
                }
            }
            (Option::None, Option::None) => (),
            _ => return false,
        };

        if lhs.read.len() != rhs.read.len() {
            return false;
        }

        let l = lhs.read.len();
        for i in 0..l {
            if !Node::ptr_eq(&lhs.read[i], &rhs.read[i]) {
                return false;
            }
        }
        return true;
    }

    // Equality of 2 pointers
    fn ptr_deep_eq(lhs: &Dchainptr, rhs: &Dchainptr) -> bool {
        if Dchainptr::eq(&lhs, &rhs) {
            return true;
        }

        // compare the value of 2 value are same or not
        return DepChain::eq(&*lhs.borrow(), &*rhs.borrow());
    }
}

impl Memory {
    fn eq(lhs: &Memory, rhs: &Memory) -> bool {
        return lhs.sig.eq(&rhs.sig);
    }

    fn ptr_deep_eq(lhs: &Memptr, rhs: &Memptr) -> bool {
        return Memptr::ptr_eq(lhs, rhs)
            || Memory::eq(&*lhs.borrow(), &*rhs.borrow());
    }

    // Create a memory node based on its init memory block
    fn new_memory(init: Nref) -> Memory {
        let mut s = String::new();
        s.push_str(&init.borrow().id.to_string());

        return Memory {
            init: Option::Some(init),
            children: Vec::new(),
            dep: DepChain::new_default(),
            sig: s,
        };
    }

    fn new_memory_ptr(init: Nref) -> Memptr {
        return Rc::new(RefCell::new(Memory::new_memory(init)));
    }

    // Create a new chunk that is joined from 2 known position and this
    // will create valid signature for identity check. Notes this does not
    // set up valid DepChain, user needs to finish its dep chain settings
    fn new_memory_join(lhs: &Memptr, rhs: &Memptr) -> Memory {
        let mut s = String::new();
        s.push_str(&lhs.borrow().sig);
        s.push_str(&rhs.borrow().sig);

        let mut c = Vec::<Memptr>::new();
        c.push(lhs.clone());
        c.push(rhs.clone());

        return Memory {
            init: Option::None,
            children: c,
            dep: DepChain::new_default(),
            sig: s,
        };
    }

    fn new_memory_join_ptr(lhs: &Memptr, rhs: &Memptr) -> Memptr {
        return Rc::new(RefCell::new(Memory::new_memory_join(lhs, rhs)));
    }
}

impl ValueLattice {
    fn new_nothing() -> ValueLattice {
        return ValueLattice::Nothing;
    }
    fn new_not_mem_empty() -> ValueLattice {
        return ValueLattice::NotMem(Option::None);
    }
    fn new_not_mem(n: Nref) -> ValueLattice {
        return ValueLattice::NotMem(Option::Some(n));
    }
    fn new_unknown() -> ValueLattice {
        return ValueLattice::Unknown;
    }
    fn new_const(imm: Imm) -> ValueLattice {
        return ValueLattice::Const(imm);
    }
    fn new_memory(loc: Hlocptr) -> ValueLattice {
        return ValueLattice::Memory(loc);
    }

    fn is_not_mem_empty(&self) -> bool {
        if let ValueLattice::NotMem(x) = &self {
            return x.is_none();
        } else {
            return false;
        }
    }

    fn is_not_mem_valid(&self) -> bool {
        if let ValueLattice::NotMem(x) = &self {
            return x.is_some();
        } else {
            return false;
        }
    }

    fn maybe_not_mem_valid(&self) -> bool {
        if let ValueLattice::NotMem(x) = &self {
            return x.is_some();
        } else {
            return true;
        }
    }

    fn not_mem_node(&self) -> Option<Nref> {
        if let ValueLattice::NotMem(x) = &self {
            return x.clone();
        } else {
            return Option::None;
        }
    }

    fn is_const(&self) -> bool {
        match &self {
            ValueLattice::Const(_) => return true,
            _ => return false,
        };
    }
    fn is_memory(&self) -> bool {
        match &self {
            ValueLattice::Memory(_) => return true,
            _ => return false,
        };
    }
    fn is_not_memory(&self) -> bool {
        match &self {
            ValueLattice::NotMem(_) => return true,
            _ => return false,
        };
    }
    fn is_nothing(&self) -> bool {
        match &self {
            ValueLattice::Nothing => return true,
            _ => return false,
        };
    }
    fn is_unknown(&self) -> bool {
        match &self {
            ValueLattice::Unknown => return true,
            _ => return false,
        };
    }

    fn is_same(&self, that: &ValueLattice) -> bool {
        match (&self, &that) {
            (ValueLattice::Const(a), ValueLattice::Const(b)) => {
                return *a == *b;
            }
            (ValueLattice::Memory(a), ValueLattice::Memory(b)) => {
                return Memory::ptr_deep_eq(a, b);
            }
            (ValueLattice::NotMem(a), ValueLattice::NotMem(b)) => {
                if a.is_some() && b.is_some() {
                    return DepChain::ptr_deep_eq(
                        a.as_ref().unwrap(),
                        b.as_ref().unwrap(),
                    );
                } else {
                    // If both nodes are None, it is not the same the reason
                    // is because we don't know the actual value inside of the
                    // lattice
                    return false;
                }
            }

            (ValueLattice::Nothing, ValueLattice::Nothing)
            | (ValueLattice::Unknown, ValueLattice::Unknown) => {
                return true;
            }
            _ => return false,
        };
    }

    fn dup(x: &ValueLattice) -> ValueLattice {
        return match x {
            ValueLattice::NotMem(dptr) => {
                return ValueLattice::NotMem(dptr.borrow().dup_ptr());
            }
            _ => x.clone(),
        };
    }

    fn dup_me(&self) -> ValueLattice {
        return ValueLattice::dup(self);
    }

    // -------------------------------------------------------------------------
    // Lattice meet operator implementaion -------------------------------------
    //
    //   The join operation will only happen when each BB starts to join
    // -------------------------------------------------------------------------

    fn meet(
        &mut self,
        cfg: &Nref,
        that: &ValueLatticeFrom,
        order: Nref,
        update_effect: bool,
    ) {
        let tmp = ValueLattice::do_meet(self, that, order, update_effect);
        *self = tmp;
    }

    fn phi_from_const(
        &mut self,
        lhs: Imm,
        lhs_cfg: Nref,
        rhs: Nref,
        rhs_cfg: Imm,
        bc: BcCtx,
    ) -> Nref {
        let lhs_imm = self.mptr().borrow_mut().new_imm_node(lhs, bc.clone());
        let rhs_imm = self.mptr().borrow_mut().new_imm_node(rhs, bc.clone());
        let phi = self.mptr().borrow_mut().new_rv_phi(bc);
        Node::add_phi_value(&mut phi, lhs_imm, lhs_cfg);
        Node::add_phi_value(&mut phi, rhs_imm, rhs_cfg);
        return phi;
    }

    fn phi_from_imm_node(
        &mut self,
        lhs: Imm,
        lhs_cfg: Nref,
        rhs: Option<Nref>,
        rhs_cfg: Nref,
        bc: BcCtx,
    ) -> Nref {
        let lhs_imm = self.mptr().borrow_mut().new_imm_node(lhs, bc.clone());

        let phi = self.mptr().borrow_mut().new_rv_phi(bc);
        Node::add_phi_value(&mut phi, lhs_imm, lhs_cfg);
        Node::add_phi_value(&mut phi, rhs.unwrap(), rhs_cfg);
        return phi;
    }

    fn phi_from_node_imm(
        &mut self,
        lhs: Option<Nref>,
        lhs_cfg: Nref,
        rhs: Imm,
        rhs_cfg: Nref,
        bc: BcCtx,
    ) -> Nref {
        let rhs_imm = self.mptr().borrow_mut().new_imm_node(rhs, bc.clone());

        let phi = self.mptr().borrow_mut().new_rv_phi(bc);
        Node::add_phi_value(&mut phi, lhs.unwrap(), lhs_cfg);
        Node::add_phi_value(&mut phi, rhs_imm, rhs_cfg);
        return phi;
    }

    fn phi_from_node(
        &mut self,
        lhs: Option<Nref>,
        lhs_cfg: Nref,
        rhs: Option<Nref>,
        rhs_cfg: Nref,
        bc: BcCtx,
    ) -> Nref {
        let phi = self.mptr().borrow_mut().new_rv_phi(bc);
        Node::add_phi_value(&mut phi, lhs.unwrap(), lhs_cfg);
        Node::add_phi_value(&mut phi, rhs.unwrap(), rhs_cfg);
        return phi;
    }

    fn do_meet(
        lhs: &ValueLattice,
        lhs_cfg: &Nref,

        rhs: &ValueLattice,
        rhs_cfg: &Nref,

        barrier: Nref,
        update_effect: bool,
    ) -> ValueLattice {
        // 1) nothing ^ X = X
        //    X ^ nothing = X
        if lhs.is_nothing() {
            return rhs.value.dup_me();
        } else if rhs.is_nothing() {
            return lhs.value.dup_me();
        }

        // 2) reflexive
        if lhs.is_same(rhs) {
            return lhs.value.dup_me();
        }

        // 3) GLB operation
        //
        //    Notes, all the operation here is hard coded rules instead of
        //    having a general GLB resolver which requires fancy graph operations
        //
        //    Similarity has been testified already

        match (lhs, rhs) {
            (ValueLattice::Imm(l), ValueLattice::Imm(r)) => {
                if update_effect {
                    return ValueLattice::new_not_mem(self.phi_from_const(
                        l.clone(),
                        lhs_cfg.clone(),
                        r.clone(),
                        rhs_cfg.clone(),
                        barrier.borrow().bc.clone(),
                    ));
                } else {
                    return ValueLattice::new_not_mem_empty();
                }
            }

            // NotMem ^ X
            (ValueLattice::NotMem(x), ValueLattice::NotMem(y)) => {
                if udpate_effect {
                    return ValueLattice::new_not_mem(self.phi_from_node(
                        x.clone(),
                        lhs_cfg.clone(),
                        y.clone(),
                        rhs_cfg.clone(),
                        barrier.borrow().bc.clone(),
                    ));
                } else {
                    return ValueLattice::new_not_mem_empty();
                }
            }

            // NotMem ^ Imm
            (ValueLattice::Imm(l), ValueLattice::NotMem(r)) => {
                if udpate_effect {
                    return ValueLattice::phi_from_const_node(
                        l.clone(),
                        lhs_cfg.clone(),
                        r.clone(),
                        rhs_cfg.clone(),
                        barrier.borrow().bc.clone(),
                    );
                } else {
                    return ValueLattice::new_not_mem_empty();
                }
            }

            (ValueLattice::NotMem(l), ValueLattice::Imm(r)) => {
                if update_effect {
                    return ValueLattice::phi_from_node_const(
                        l.clone(),
                        lhs_cfg.clone(),
                        r.clone(),
                        rhs_cfg.clone(),
                        barrier.borrow().bc.clone(),
                    );
                } else {
                    return ValueLattice::new_not_mem_empty();
                }
            }

            _ => (),
        };

        return ValueLattice::new_unknown();
    }

    // join, ie merge a list of value into one single val
    fn join(
        all: &ValueLatticeFromList,
        barrier: Nref,
        update_effect: bool,
    ) -> ValueLattice {
        if all.len() == 0 {
            return ValueLattice::new_nothing();
        }

        let f = all[0].clone();

        let mut first = f.value;
        let first_cfg = f.cfg;

        for x in all.iter().skip(1) {
            first = first.meet(&first_cfg, &x, barrier.clone(), update_effect);
        }
        return first;
    }
}

// Memory Lattice, essentially this is the sub-graph of the original value
// lattice. We only use it during pass2's environmental memory merge. Due to the
// fact the graph is in SSA format, so the memory node's reference becomes
// direct use of the memory node itself. Essentially, the memory node's point-to
// is also part of the value lattice, but it will never reference Imm(x) and
// NotMem(y). Instead of using the value lattice directly, we use the memory
// lattice to represent it.
impl MemoryLattice {
    fn new_mem(m: Memptr) -> MemoryLattice {
        if m.borrow().is_unknown() {
            return MemoryLattice::new_unknown();
        }
        return MemoryLattice::Mem(m);
    }

    fn new_nothing() -> MemoryLattice {
        return MemoryLattice::Nothing;
    }

    // whether they are the same, used for join's reflexive testify
    fn is_same(&self, rhs: &MemoryLattice) -> bool {
        match (self, rhs) {
            (MemoryLattice::Nothing, MemoryLattice::Nothing)
            | (MemoryLattice::Unknown, MemoryLattice::Unknown) => {
                return true;
            }
            (MemoryLattice::Mem(l), MemoryLattice::Mem(r)) => {
                return Memory::ptr_deep_eq(l, r);
            }
            _ => return false,
        };
    }

    fn mem(&self) -> Option<Memptr> {
        match self {
            MemoryLattice::Mem(x) => Option::Some(x.clone()),
            _ => return Option::None,
        };
    }

    // -------------------------------------------------------------------------
    // Lattice meet operator implementaion -------------------------------------
    // -------------------------------------------------------------------------

    fn do_meet(
        l: &MemoryLattice,
        r: &MemoryLattice,
        // contain all the memory if the current lhs and rhs MemoryLattice are
        // both Memory lattice. Notes the l and r are pointed to the same heap
        // location.
        eq_set: &mut Vec<Memptr>,
        barrier: Nref,
        update_effect: bool,
    ) -> Option<MemoryLattice> {
        // nothing ^ X = X or X ^ nothing = X
        if l.is_nothing() {
            return Option::Some(r.dup_me());
        } else if r.is_nothing() {
            return Option::Some(r.dup_me());
        }

        // reflexive
        if l.is_same(r) {
            return Option::Some(l.dup_me());
        }

        // perform operations
        match (&l, &r) {
            // (0) Memory X Memory = Memory
            (MemoryLattice::Mem(a), MemoryLattice::Mem(b)) => {
                eq_set.push(a.clone());
                eq_set.push(b.clone());
                return Option::None;
            }
            // (1) Unknown X anything = Unknown
            (MemoryLattice::Unknown, MemoryLattice::Nothing)
            | (MemoryLattice::Nothing, MemoryLattice::Unknown) => {
                return Option::Some(MemoryLattice::new_unknown());
            }

            (MemoryLattice::Unknown, MemoryLattice::Mem(x))
            | (MemoryLattice::Mem(x), MemoryLattice::Unknown) => {
                x.borrow_mut().flush(barrier, update_effect);
                return Option::Some(MemoryLattice::new_unknown());
            }

            // (2) Nothing ^ Memory
            (MemoryLattice::Mem(x), _) | (_, MemoryLattice::Mem(x)) => {
                x.borrow_mut().flush(barrier, update_effect);
                return Option::Some(MemoryLattice::new_mem(
                    Memory::new_memory_ptr(barrier),
                ));
            }
        };
    }

    fn join(
        all: &Vec<MemoryLatticeFrom>,
        barrier: Nref,
        update_effect: bool,
        mpool: Mpptr,
    ) -> MemoryLattice {
        if all.len() == 0 {
            return MemoryLattice::new_nothing();
        }

        // (0) this pass tries to do a specialization when all the node in the
        //     lists are all lattice memory. if so we just do a effect join
        //     node to chain everyone, otherwise we do not do anything
        let all_mem = {
            let mut f = false;
            for x in all.iter() {
                match x {
                    MemoryLattice::Mem(_) => continue;
                    MemoryLattice::Nothing => continue;
                    _ => {
                        f = false;
                        break;
                    }
                }
            }

            f
        };

        if all_mem {
            let effect_join = {
                if all.len() == 1 {
                    all[0].mem().unwrap()
                } else {
                    let join = mpool.borrow_mut().new_rv_effect_join(
                        barrier.borrow().bc.clone()
                    );
                }
            };
        }

        let mut first = all[0].clone();

        let mut is_mem = match &first {
            MemoryLattice::Mem(_) => true,
            _ => false,
        };

        for x in all.iter().skip(1) {
            let v = MemoryLattice::do_meet(
                &first,
                &x,
                &mut eq_set,
                barrier.clone(),
                update_effect);

            if v.
        }
        return first;
    }
}

// Env helpers
impl EffectEnv {}
