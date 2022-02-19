// -----------------------------------------------------------------------------
// This passes works inside of the BB and only generate the BB final information
// summary without relinking each node's side effect. It does an analysis but
// not mutate the graph. After the Block level analysis, only the block's IN
// status is valid, its out status is not have to be valid in case the loop
// body mutate the node's status.

use crate::jit::j::*;
use crate::jit::node::*;
use bitvec::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

type Dcptr = Rc<RefCell<Dchain>>;
type Memptr = Rc<RefCell<Memory>>;
type Blkptr = Rc<RefCell<Blk>>;
type ValueLatticeList = Vec<ValueLattice>;
type R = Option<()>;

// Represent a single known memory's dependency order. Ie the following order
// is been maintained:
//
//   1) True Order
//   2) Anti Order
//   3) Output Order
//
// Notes the order join between each block is essentially been fully maintained,
// ie we record predecessor's Dchain in its sucessor's dchain object. The final
// materialization of this dependency is been merged back into IR which is done
// in the effect analysis
#[derive(Clone)]
struct Dchain {
    join: Vec<Dcptr>,
    write: Option<Nref>,
    read: Vec<Nref>,
}

// Memory region discovery, since subfield assignment/mutation does not
// effectively change the existed known shape of heap, so the discovery of memory
// region can be safely done by looking at each memory alias node's point-to
// information.  We always maintain this information, and they are sort of our
// memory location information source

type Mlocptr = Rc<RefCell<MLoc>>;
type Memwptr = Weak<RefCell<Memory>>;
type MLocUseList = Vec<Memwptr>;

#[derive(Clone)]
struct MLoc {
    children: Vec<Mlocptr>,
    dep: Dcptr,
    unknown: bool,

    // track who is using us, used when we merge location node
    use_list: MLocUseList,
}

struct Memory {
    mloc: Mlocptr,
}

#[derive(Clone)]
enum MemoryLattice {
    Nothing,
    Mem(Memptr),
    Unknown,
}

#[derive(Clone)]
struct MRef {
    mem: Nref,
    loc: Memptr,
}

// Since we cannot mutate the IR graph, we need to generate phi node when we
// meet 2 none memory node to yield lattice of NMem type. To record this we
// just use a special operator to indicate so.

#[derive(Clone)]
enum NMemVal {
    Node(Nref),

    // we don't need to record the join of nmem, since the effect analysis will
    // walk the instructions again to rebuild the phi
    Join,
}

#[derive(Clone)]
enum ValueLattice {
    // top of the lattice, ie nothing is known to us, start lattice for all the
    // exposed program states
    Nothing,

    // constant, use this when we know absolutely that what exact immediate value
    // been stored in the value, it is used by later phase to perform upvalue
    // and global load elimination when the dependency chain allows to do so
    Const(Imm),

    // this node will always points to a node that is immutable, so no need to
    // maintain dependency chain
    NMem(NMemVal),

    // memory node, ie pointing to a piece of memory region
    Mem(Memptr),

    // unknown lattice
    Unknown,
}

#[derive(Clone)]
struct Galias {
    name: String,
    v: ValueLattice,
}

#[derive(Clone)]
struct Ualias {
    idx: u32,
    v: ValueLattice,
}

struct EffectEnv {
    mem_alias: Vec<MRef>,
    glb_alias: Vec<Galias>,
    upv_alias: Vec<Ualias>,
}

struct Blk {
    cfg: Nref,
    effect: EffectEnv,
}

struct BlkAA {
    j: Jitptr,
    graph: FGraphptr,

    blk_map_from_id: Vec<u32>,
    blk_list: Vec<Blkptr>,
    cur_blk: Blkptr,

    visited: BitVec,

    tt_mem: u32,
    tt_glb: u32,
    tt_upv: u32,

    // global variable name mapping(numbering)
    glb_name: Vec<String>,

    unknown_mem: Memptr,
}

impl Ualias {
    fn new(i: u32, v: ValueLattice) -> Ualias {
        return Ualias { idx: i, v: v };
    }
}

impl Galias {
    fn new(n: String, v: ValueLattice) -> Galias {
        return Galias { name: n, v: v };
    }
}

impl NMemVal {
    fn dup(&self) -> NMemVal {
        match self {
            NMemVal::Node(x) => return NMemVal::Node(x.clone()),
            NMemVal::Join => return NMemVal::Join,
        };
    }

    fn eq(&self, that: &NMemVal) -> bool {
        match (self, that) {
            (NMemVal::Node(a), NMemVal::Node(b)) => {
                return Nref::ptr_eq(a, b);
            }
            (NMemVal::Join, NMemVal::Join) => {
                return true;
            }
            _ => return false,
        };
    }
}

impl Blk {
    fn new_ptr(cfg: Nref, eff: EffectEnv) -> Blkptr {
        return Rc::new(RefCell::new(Blk {
            cfg: cfg,
            effect: eff,
        }));
    }
}

impl EffectEnv {
    fn new(m: Vec<MRef>, g: Vec<Galias>, u: Vec<Ualias>) -> EffectEnv {
        return EffectEnv {
            mem_alias: m,
            glb_alias: g,
            upv_alias: u,
        };
    }
    fn find_global(&self, n: &str) -> Option<ValueLattice> {
        match self.glb_alias.iter().position(|x| &x.name == n) {
            Option::Some(x) => {
                return Option::Some(self.glb_alias[x].v.clone());
            }
            _ => return Option::None,
        };
    }

    fn find_upvalue(&self, n: u32) -> Option<ValueLattice> {
        match self.upv_alias.iter().position(|x| x.idx == n) {
            Option::Some(x) => {
                return Option::Some(self.upv_alias[x].v.clone());
            }
            _ => return Option::None,
        };
    }

    fn find_memory(&self, m: &Nref) -> Option<Memptr> {
        match self.mem_alias.iter().position(|x| Nref::ptr_eq(m, &x.mem)) {
            Option::Some(idx) => {
                return Option::Some(self.mem_alias[idx].loc.clone());
            }
            _ => return Option::None,
        };
    }

    fn unknownify_global(&mut self) {
        for x in self.glb_alias.iter_mut() {
            x.v.set_unknown();
        }
    }

    fn set_upvalue(&mut self, idx: u32, v: ValueLattice) {
        self.upv_alias[idx as usize].v = v;
    }

    fn set_global(&mut self, n: &str, v: ValueLattice) -> bool {
        match self.glb_alias.iter().position(|x| &x.name == n) {
            Option::Some(idx) => {
                self.glb_alias[idx].v = v;
                return true;
            }
            _ => {
                return false;
            }
        };
    }
}

impl Dchain {
    fn new_join(j: Vec<Dcptr>) -> Dchain {
        return Dchain {
            join: j,
            write: Option::None,
            read: Vec::new(),
        };
    }

    fn new() -> Dchain {
        return Dchain {
            join: Vec::new(),
            write: Option::None,
            read: Vec::new(),
        };
    }

    fn new_ptr() -> Dcptr {
        return Rc::new(RefCell::new(Dchain::new()));
    }

    fn new_join_ptr(lhs: &Dcptr, rhs: &Dcptr) -> Dcptr {
        // notes,
        //   the join operation is associative, ie the following 2 types of tree
        //   are equal relationship
        //
        //
        //    [p0, p1]
        //       |
        //    [p2, p3]   [p4, p5]
        //       |            |
        //       +-----[p]----+
        //
        //  is the same as
        //
        //     [p0, p1, p2, p3, p4, p5] ---> [p]
        //
        //  Notes, the meet operator in compiler is also commuative which means
        //  the order here is totally irrlevent, sigh ...
        //
        //  Notes, p2's predecessor information is been maintained, we only focus
        //  on p here.

        let mut v = vec![lhs.clone(), rhs.clone()];
        return Rc::new(RefCell::new(Dchain::new_join(v)));
    }

    fn update_write(&mut self, w: Nref) {
        self.write = Option::Some(w);
        self.read.clear();
    }

    fn update_read(&mut self, r: Nref) {
        self.read.push(r);
    }

    fn update_flush(&mut self) {
        self.write = Option::None;
        self.read.clear();
    }

    fn dup(&self) -> Dchain {
        // notes the dup just duplicate the node itself instead of duplicate
        // its structure, ie join doesn't do recrusive dup
        return Dchain {
            join: self.join.clone(),
            write: self.write.clone(),
            read: self.read.clone(),
        };
    }

    fn dup_ptr(&self) -> Dcptr {
        return Rc::new(RefCell::new(self.dup()));
    }
}

impl MLoc {
    fn new_unknown() -> MLoc {
        return MLoc {
            children: Vec::new(),
            dep: Dchain::new_ptr(),
            unknown: true,
            use_list: MLocUseList::new(),
        };
    }

    fn new_unknown_ptr() -> Mlocptr {
        return Rc::new(RefCell::new(MLoc::new_unknown()));
    }

    fn new() -> MLoc {
        return MLoc {
            children: Vec::new(),
            dep: Dchain::new_ptr(),
            unknown: false,
            use_list: MLocUseList::new(),
        };
    }

    fn new_ptr() -> Mlocptr {
        return Rc::new(RefCell::new(MLoc::new()));
    }

    fn new_ptr_from_dchain(d: Dcptr) -> Mlocptr {
        return Rc::new(RefCell::new(MLoc {
            children: Vec::new(),
            dep: d,
            unknown: false,
            use_list: MLocUseList::new(),
        }));
    }

    fn use_by(&mut self, x: Memwptr) {
        self.use_list.push(x);
    }

    fn new_join_ptr(l: Mlocptr, r: Mlocptr) -> Mlocptr {
        l.borrow_mut().use_list.clear();
        r.borrow_mut().use_list.clear();

        return Rc::new(RefCell::new(MLoc {
            children: vec![l, r],
            dep: Dchain::new_ptr(),
            unknown: false,
            use_list: MLocUseList::new(),
        }));
    }

    fn dup(&self) -> MLoc {
        return MLoc {
            children: self.children.clone(),
            dep: self.dep.borrow().dup_ptr(),
            unknown: self.unknown,
            use_list: self.use_list.clone(),
        };
    }

    fn dup_ptr(&self) -> Mlocptr {
        return Rc::new(RefCell::new(self.dup()));
    }
}

impl Memory {
    fn new_memory_ptr() -> Memptr {
        let m = Memory {
            mloc: MLoc::new_ptr(),
        };

        let mptr = Rc::new(RefCell::new(m));
        let wk = Rc::downgrade(&mptr);
        mptr.borrow_mut().mloc.borrow_mut().use_by(wk);
        return mptr;
    }

    fn new_memory_unknown_ptr() -> Memptr {
        let m = Memory {
            mloc: MLoc::new_unknown_ptr(),
        };

        let mptr = Rc::new(RefCell::new(m));
        let wk = Rc::downgrade(&mptr);
        mptr.borrow_mut().mloc.borrow_mut().use_by(wk);
        return mptr;
    }

    fn new_memory_ptr_from_dchain(d: Dcptr) -> Memptr {
        let m = Memory {
            mloc: MLoc::new_ptr_from_dchain(d),
        };

        let mptr = Rc::new(RefCell::new(m));
        let wk = Rc::downgrade(&mptr);
        mptr.borrow_mut().mloc.borrow_mut().use_by(wk);
        return mptr;
    }

    fn is_unknown(&self) -> bool {
        return self.mloc.borrow().unknown;
    }
    fn set_unknown(&mut self) {
        self.mloc.borrow_mut().unknown = true;
    }

    fn update_read(&mut self, n: Nref) {
        self.mloc.borrow_mut().dep.borrow_mut().update_read(n);
    }
    fn update_write(&mut self, n: Nref) {
        self.mloc.borrow_mut().dep.borrow_mut().update_write(n);
    }

    // get all the mloc that is aliased from this memory node
    fn enumerate_mloc(&self) -> Vec<Mlocptr> {
        let mut o = Vec::<Mlocptr>::new();
        let mut wlist = Vec::<Mlocptr>::new();

        wlist.push(self.mloc.clone());

        while wlist.len() != 0 {
            let top = wlist.pop().unwrap();
            o.push(top);
            for x in top.borrow().children {
                wlist.push(x.clone());
            }
        }
        return o;
    }

    // merging 2 piece of memory, after the merge operation, the original 2
    // memory location will learn that the memory has been merged from the
    // original memory site.
    fn merge_memory(lhs: &Memptr, rhs: &Memptr) {
        let lhs_mloc = lhs.borrow().mloc.clone();
        let rhs_mloc = rhs.borrow().mloc.clone();

        let lhs_use = lhs_mloc.borrow_mut().use_list;
        let rhs_use = rhs_mloc.borrow_mut().use_list;

        let join_loc = MLoc::new_join_ptr(lhs_mloc, rhs_mloc);

        for x in lhs_use {
            if let Option::Some(u) = x.upgrade() {
                u.borrow_mut().mloc = join_loc.clone();
            }
        }
        for x in rhs_use {
            if let Option::Some(u) = x.upgrade() {
                u.borrow_mut().mloc = join_loc.clone();
            }
        }
    }

    fn dup(&self) -> Memory {
        return Memory {
            mloc: self.mloc.borrow().dup_ptr(),
        };
    }

    fn dup_ptr(&self) -> Memptr {
        return Rc::new(RefCell::new(self.dup()));
    }
}

impl MemoryLattice {
    fn new_nothing() -> MemoryLattice {
        return MemoryLattice::Nothing;
    }
    fn new_mem(m: Memptr) -> MemoryLattice {
        return MemoryLattice::Mem(m);
    }
    fn new_unknown() -> MemoryLattice {
        return MemoryLattice::Unknown;
    }
    fn dup(&self) -> MemoryLattice {
        match &self {
            MemoryLattice::Nothing => return MemoryLattice::new_nothing(),
            MemoryLattice::Unknown => return MemoryLattice::new_unknown(),
            MemoryLattice::Mem(x) => {
                return MemoryLattice::new_mem(x.borrow().dup_ptr());
            }
        };
    }

    fn is_nothing(&self) -> bool {
        match self {
            MemoryLattice::Nothing => return true,
            _ => return false,
        };
    }

    fn is_mem(&self) -> bool {
        match self {
            MemoryLattice::Mem(_) => return true,
            _ => return false,
        };
    }

    fn is_unknown(&self) -> bool {
        match self {
            MemoryLattice::Unknown => return true,
            _ => return false,
        };
    }

    // lattice meet/join operation ---------------------------------------------
    //
    // Lattice graph is simple
    //
    //       [Nothing]
    //           |
    //         [Mem]
    //           |
    //       [Unknown]
    //
    fn do_meet(lhs: &MemoryLattice, rhs: &MemoryLattice) -> MemoryLattice {
        // Identity check, ie nothing ^ any = any (nothing is identity of lattice)
        if lhs.is_nothing() {
            return rhs.dup();
        } else if rhs.is_nothing() {
            return lhs.dup();
        }

        // Unknown check, ie unknown ^ any = unknown
        if lhs.is_unknown() | rhs.is_unknown() {
            return MemoryLattice::new_unknown();
        }

        debug_assert!(lhs.is_mem());
        debug_assert!(rhs.is_mem());

        match (lhs, rhs) {
            (MemoryLattice::Mem(a), MemoryLattice::Mem(b)) => {
                let d = Dchain::new_join_ptr(
                    &a.borrow().mloc.borrow().dep,
                    &b.borrow().mloc.borrow().dep,
                );
                let mem = Memory::new_memory_ptr_from_dchain(d);
                return MemoryLattice::new_mem(mem);
            }
            _ => unreachable!(),
        };
    }

    fn join(l: &Vec<MemoryLattice>) -> MemoryLattice {
        if l.len() == 0 {
            return MemoryLattice::new_nothing();
        }

        let mut first = l[0].dup();
        for x in l.iter().skip(1) {
            let tmp = MemoryLattice::do_meet(&first, &x);
            first = tmp;
        }

        return first;
    }

    fn join_ptr(l: &Vec<MemoryLattice>) -> Memptr {
        let lattice = MemoryLattice::join(l);

        match lattice {
            MemoryLattice::Nothing => unreachable!(),
            MemoryLattice::Mem(x) => return x,
            MemoryLattice::Unknown => return Memory::new_memory_unknown_ptr(),
        };
    }
}

impl ValueLattice {
    fn new_nothing() -> ValueLattice {
        return ValueLattice::Nothing;
    }
    fn new_const(imm: Imm) -> ValueLattice {
        return ValueLattice::Const(imm);
    }
    fn new_nmem_join() -> ValueLattice {
        return ValueLattice::NMem(NMemVal::Join);
    }
    fn new_nmem_node(n: Nref) -> ValueLattice {
        return ValueLattice::NMem(NMemVal::Node(n));
    }

    fn new_mem(m: Memptr) -> ValueLattice {
        return ValueLattice::Mem(m);
    }
    fn new_unknown() -> ValueLattice {
        return ValueLattice::Unknown;
    }

    fn is_nothing(&self) -> bool {
        return match self {
            ValueLattice::Nothing => true,
            _ => false,
        };
    }
    fn is_const(&self) -> bool {
        return match self {
            ValueLattice::Const(_) => true,
            _ => false,
        };
    }
    fn is_nmem(&self) -> bool {
        return match self {
            ValueLattice::NMem(_) => true,
            _ => false,
        };
    }
    fn is_mem(&self) -> bool {
        return match self {
            ValueLattice::Mem(_) => true,
            _ => false,
        };
    }
    fn is_unknown(&self) -> bool {
        return match self {
            ValueLattice::Unknown => true,
            ValueLattice::Mem(x) => x.borrow().is_unknown(),
            _ => false,
        };
    }

    fn as_nmem(&self) -> Option<NMemVal> {
        return match self {
            ValueLattice::NMem(x) => Option::Some(x.clone()),
            _ => Option::None,
        };
    }
    fn as_mem(&self) -> Option<Memptr> {
        return match self {
            ValueLattice::Mem(x) => {
                if x.borrow().is_unknown() {
                    Option::None
                } else {
                    Option::Some(x.clone())
                }
            }
            _ => Option::None,
        };
    }

    fn set_unknown(&mut self) {
        if let ValueLattice::Mem(m) = &self {
            m.clone().borrow_mut().set_unknown();
        }
        *self = ValueLattice::Unknown;
    }

    fn dup(&self) -> ValueLattice {
        return match self {
            ValueLattice::Nothing => ValueLattice::Nothing,
            ValueLattice::Const(imm) => ValueLattice::Const(imm.clone()),
            ValueLattice::NMem(x) => ValueLattice::NMem(x.clone()),
            ValueLattice::Mem(x) => ValueLattice::Mem(x.borrow().dup_ptr()),
            ValueLattice::Unknown => ValueLattice::Unknown,
        };
    }

    // testify element's identity to allow reflexive operation easier to maintain
    //
    //   Per anti-sysmetric rules,
    //
    //     a ^ b = b ^ a <=> a = b (= is equality relationship)
    //
    //  notes, to really testify the identical of each instruction are really
    //  hard and hard to express in IR way. In the math, the element in the
    //  set is mostly been considered as deduped and unique, but in our IR graph
    //  its not the case. If we testify exact identical of nodes by recursively
    //  compare all its component's identical then it will not work since the
    //  bytecode index is not the same, if we omit it then the semantic is not
    //  the same. For simplicity, we just compare 2 nodes ptr for eqaulity
    //  relationship which is not ideal since this leaves out many optimization
    //  situations. We will figure out the identical feature of 2 instructions
    //  in a much more strict mathmatical way later on.
    fn is_same(&self, that: &ValueLattice) -> bool {
        match (self, that) {
            (ValueLattice::Nothing, ValueLattice::Nothing)
            | (ValueLattice::Unknown, ValueLattice::Unknown) => {
                return true;
            }

            (ValueLattice::Const(a), ValueLattice::Const(b)) => {
                return a.eq(b);
            }

            (ValueLattice::NMem(x), ValueLattice::NMem(y)) => {
                return x.eq(&y);
            }

            (ValueLattice::Mem(x), ValueLattice::Mem(y)) => {
                return Memptr::ptr_eq(&x, &y);
            }

            _ => return false,
        };
    }

    // Value lattice graph. do_meet is essentially a hardcode GLB finder.
    //
    //                  [Nothing]
    //                      |
    //   +--------------------------------------+
    //   |          |             |             |
    // Imm(0) ... Imm(n)        Mem(a0)       Mem(an)
    //   |          |             |             |
    //   +----------+             +-------------+
    //        |                          |
    //        |                          |
    //       NMem                        |
    //        |                          |
    //        |                          |
    //        +----------Unknown---------+

    fn do_meet(lhs: &ValueLattice, rhs: &ValueLattice) -> ValueLattice {
        if lhs.is_nothing() {
            return rhs.dup();
        } else if rhs.is_nothing() {
            return lhs.dup();
        }

        if lhs.is_unknown() || rhs.is_unknown() {
            return ValueLattice::new_unknown();
        }

        // reflexive
        if lhs.is_same(rhs) {
            return lhs.clone();
        }

        // backwards oritend, there're 2 types result can be generated
        // 1) NMem, can only be generated by
        //   1.1) Imm ^ Imm
        //   1.2) NMem ^ Imm/Imm ^ NMem
        //   1.3) NMem ^ NMem
        //
        // 2) Unkonwn

        match (lhs, rhs) {
            (ValueLattice::Const(_), ValueLattice::Const(_))
            | (ValueLattice::Const(_), ValueLattice::NMem(_))
            | (ValueLattice::NMem(_), ValueLattice::Const(_))
            | (ValueLattice::NMem(_), ValueLattice::NMem(_)) => {
                return ValueLattice::new_nmem_join();
            }
            _ => return ValueLattice::Unknown,
        };
    }

    fn join(l: &Vec<ValueLattice>) -> ValueLattice {
        if l.len() == 0 {
            return ValueLattice::new_nothing();
        }

        let mut first = l[0].dup();
        for x in l.iter().skip(1) {
            let tmp = ValueLattice::do_meet(&first, &x);
            first = tmp;
        }

        return first;
    }
}

impl BlkAA {
    fn global_name(&self, i: u32) -> String {
        return self.glb_name[i as usize].clone();
    }

    fn init_global_list(&self) -> Vec<Galias> {
        return (1..self.tt_upv)
            .map(|x| Galias {
                name: x.to_string(),
                v: ValueLattice::new_unknown(),
            })
            .collect();
    }

    fn init_upval_list(&self) -> Vec<Ualias> {
        return self
            .glb_name
            .iter()
            .enumerate()
            .map(|(i, &x)| Ualias {
                idx: i as u32,
                v: ValueLattice::new_unknown(),
            })
            .collect();
    }

    fn init_mem_list(&self) -> Vec<MRef> {
        return self
            .graph
            .borrow()
            .extra_info
            .mem_node
            .iter()
            .map(|x| MRef {
                mem: x.clone(),
                loc: Memory::new_memory_ptr(),
            })
            .collect();
    }

    fn wrap_mref_list(&self, v: Vec<Memptr>) -> Vec<MRef> {
        debug_assert!(v.len() == self.graph.borrow().extra_info.mem_node.len());

        let l = v.len();
        return (0..l)
            .map(|idx| MRef {
                mem: self.graph.borrow().extra_info.mem_node[idx].clone(),
                loc: v[idx],
            })
            .collect();
    }

    fn global_memory_of(&self, name: String) -> Option<Memptr> {
        let v = self.cur_env().borrow().effect.find_global(&name)?;

        if let ValueLattice::Mem(l) = v {
            return Option::Some(l);
        } else {
            return Option::None;
        }
    }

    fn upvalue_memory_of(&self, idx: u32) -> Option<Memptr> {
        let v = self.cur_env().borrow().effect.find_upvalue(idx)?;

        if let ValueLattice::Mem(l) = v {
            return Option::Some(l);
        } else {
            return Option::None;
        }
    }

    fn sub_field_memory_of(&self, target: Nref) -> Option<Memptr> {
        return self.memory_of(&target);
    }

    fn sub_field_load_memory_of(&self, n: &Nref) -> Option<Memptr> {
        match n.borrow().op.op {
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                return self.sub_field_memory_of(n.borrow().una());
            }
            _ => unreachable!(),
        };
    }

    fn sub_field_store_memory_of(&self, n: &Nref) -> Option<Memptr> {
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
    fn memory_alias_memory_of(&self, mem: &Nref) -> Option<Memptr> {
        return self.cur_env().borrow().effect.find_memory(mem);
    }

    // Used by alias operation when we encounter a PHI node. This function will
    // recursively searching all the node(nested nodes) in PHI to find out a list
    // of invovled memory address.
    fn phi_memory_of(&self, phi: &Nref) -> Vec<Memptr> {
        let mut o = Vec::<Memptr>::new();
        for x in phi.borrow().value.iter() {
            match x.borrow().op.op {
                Opcode::RvPhi => {
                    let mut r = self.phi_memory_of(&x);
                    o.append(&mut r);
                }
                _ => {
                    match self.memory_of(&x) {
                        Option::Some(m) => o.push(m),
                        _ => (),
                    };
                }
            };
        }
        return o;
    }

    // Notes, the result of the memory_of is been modeled as following :
    //
    //   1) A None represent the memory_of decide that the node does not yield a
    //      memory node. Notes this function is a conservatively guess of the
    //      node that will generate memory. Ie the memory_of returns means the
    //      node |CAN| generate memory, but if a node never generate memory, then
    //      memory_of will never return a not-none value.
    //
    //   2) Otherwise it must be a lattice we know for memory
    fn memory_of(&self, n: &Nref) -> Option<Memptr> {
        match &n.borrow().op.op {
            // (1) Global
            Opcode::RvLoadGlobal => {
                let global_name =
                    Node::global_name(n, &self.graph.borrow().func);
                return self.global_memory_of(global_name);
            }

            // (2) Upvalue
            Opcode::RvLoadUpvalue => {
                let upvalue_idx =
                    Node::upvalue_index(n, &self.graph.borrow().func);
                return self.upvalue_memory_of(upvalue_idx);
            }

            // (3) Subfield access
            //     Essentially speaking, subfield opcode does not generate
            //     memory but manipluate memory node. We need to take care of
            //     subfield since we are subfield insensitive, ie a sub region
            //     mutation of memory shape is equal to the full memory mutation
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                return self.sub_field_load_memory_of(&n);
            }

            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                return self.sub_field_store_memory_of(&n);
            }

            // (4) Memory access
            Opcode::RvMemAccess => {
                debug_assert!(n.borrow().has_value());
                let mem = n.borrow().v0();
                return self.memory_of(&mem);
            }

            // (5) Other effect node, which we cannot deal with though they are
            //     returning us the memory location's. These node needs to be
            //     handled properly
            Opcode::RvCall => {
                return Option::Some(self.unknown_mem.clone());
            }
            // (5.1) The iterator value returned is from user's code, so should
            //       be treated as unknown memory
            Opcode::RvIteratorValue => {
                return Option::Some(self.unknown_mem.clone());
            }

            // (6) Effect join
            //     effect join is created during AA, they are just a barrier
            //     for different branch's modification of same piece of memory
            //     essentially speaking, EffectJoin always points to the same
            //     piece of memory.
            Opcode::EffectJoin => {
                unreachable!();
            }

            // (7) Phi node
            //     Phi node's memory is essentially been killed during AA, but
            //     we need a way to get all its involved memory location for
            //     us
            Opcode::RvPhi => {
                return Option::Some(self.unknown_mem.clone());
            }

            // (8) Memory node itself
            Opcode::RvObjectCreate | Opcode::RvListCreate => {
                return self.memory_alias_memory_of(&n);
            }

            // Ie this node does not (never) generate any types of memory
            _ => {
                // Ie this node does not (never) generate any types of memory
                debug_assert!(n.borrow().must_not_generate_memory());
                return Option::None;
            }
        }
    }

    // Value of, similar as memory_of but it tries to expand the value loading
    // into larger lattice scope.
    fn global_value_of(&self, name: String) -> ValueLattice {
        match self.cur_env().borrow().effect.find_global(&name) {
            Option::Some(vv) => return vv,
            _ => unreachable!(),
        };
    }

    fn upvalue_value_of(&self, idx: u32) -> ValueLattice {
        match self.cur_env().borrow().effect.find_upvalue(idx) {
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
                        return ValueLattice::new_mem(vv);
                    }
                    _ => {
                        return ValueLattice::new_unknown();
                    }
                };
            }

            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                match self.sub_field_store_memory_of(n) {
                    Option::Some(vv) => {
                        return ValueLattice::new_mem(vv);
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
                let x = n.borrow().v0();
                return self.value_of(&x);
            }

            // (5) Other effect node, which we cannot deal with though they are
            //     returning us the memory location's. These node needs to be
            //     handled properly
            Opcode::RvCall | Opcode::RvIteratorValue => {
                return ValueLattice::new_unknown();
            }

            // (6) Effect join
            //     effect join is created during AA, they are just a barrier
            //     for different branch's modification of same piece of memory
            //     essentially speaking, EffectJoin always points to the same
            //     piece of memory.
            Opcode::EffectJoin => {
                unreachable!();
            }

            // (7) Phi node
            //     Phi node's memory is essentially been killed during AA, but
            //     we need a way to get all its involved memory location for
            //     us
            Opcode::RvPhi => {
                return ValueLattice::new_unknown();
            }

            // (8) Memory node itself
            Opcode::RvObjectCreate | Opcode::RvListCreate => {
                match self.memory_alias_memory_of(&n) {
                    Option::Some(vv) => {
                        return ValueLattice::new_mem(vv);
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
                    return ValueLattice::new_const(imm);
                }

                // All other nodes must not be a memory node, otherwise we should
                // have already found it and we just mark all the other as
                // lattice not memory
                debug_assert!(n.borrow().must_not_generate_memory());
                return ValueLattice::new_nmem_node(n.clone());
            }
        }
    }

    fn alias_mem(&mut self, m: &Memptr) {
        m.clone().borrow_mut().set_unknown();
    }

    // * ----------------------------------------------------------------------
    // *  Pass
    // *    effect analysis, this pass will break all the side effect chain
    // *    based on alias analysis results, ie if multiple nodes access
    // *    the same piece of memory, based on analysis, they will be effect
    // *    depend on each other.
    // * ----------------------------------------------------------------------

    // try to merge all the known memory location of a BB's predecessor nodes
    fn merge_memory(&self, pred_list: &Vec<Nref>) -> Vec<MRef> {
        let mut heap_list = Vec::<Memptr>::new();

        let tt = self.tt_mem;

        for i in 0..tt {
            let mut mem_lattice_list = Vec::<MemoryLattice>::new();
            let mut heap_list = Vec::<Memptr>::new();

            for pre in pred_list.iter() {
                if self.visited_at(&pre) {
                    let prev_env = self.env_at(&pre);

                    let prev_mem = prev_env.borrow().effect.mem_alias
                        [i as usize]
                        .loc
                        .clone();

                    mem_lattice_list.push(if prev_mem.borrow().is_unknown() {
                        MemoryLattice::new_unknown()
                    } else {
                        MemoryLattice::new_mem(prev_mem)
                    });
                } else {
                    mem_lattice_list.push(MemoryLattice::new_nothing());
                }
            }

            // Perform the join/meet operation for the memory lattice
            let mem = MemoryLattice::join_ptr(&mem_lattice_list);
            heap_list.push(mem);
        }

        return self.wrap_mref_list(heap_list);
    }

    fn merge_global(&mut self, pred_list: &Vec<Nref>) -> Vec<Galias> {
        let mut glist = Vec::<Galias>::new();
        let mut tt = self.tt_glb;

        for i in 0..tt {
            let mut l = ValueLatticeList::new();
            let global_name = self.global_name(i);

            for pre in pred_list.iter() {
                if self.visited_at(&pre) {
                    let pre_env = self.env_at(&pre);
                    let pre_glb_val =
                        pre_env.borrow().effect.glb_alias[i as usize].v.clone();

                    l.push(pre_glb_val);
                } else {
                    l.push(ValueLattice::new_nothing());
                }
            }

            // perform the meet operation
            glist.push(Galias::new(global_name, ValueLattice::join(&l)));
        }

        return glist;
    }

    fn merge_upval(&mut self, pred_list: &Vec<Nref>) -> Vec<Ualias> {
        let mut ulist = Vec::<Ualias>::new();
        let mut tt = self.tt_upv;

        for i in 0..tt {
            let mut l = ValueLatticeList::new();

            for pre in pred_list.iter() {
                if self.visited_at(&pre) {
                    let pre_env = self.env_at(&pre);
                    let pre_upval =
                        pre_env.borrow().effect.upv_alias[i as usize].v.clone();

                    l.push(pre_upval);
                } else {
                    l.push(ValueLattice::new_nothing());
                }
            }

            // perform the meet operation
            ulist.push(Ualias::new(i, ValueLattice::join(&l)));
        }

        return ulist;
    }

    fn cur_env(&self) -> Blkptr {
        return self.cur_blk.clone();
    }

    fn env_at(&self, n: &Nref) -> Blkptr {
        let idx = self.blk_map_from_id[n.borrow().id as usize];
        return self.blk_list[idx as usize].clone();
    }

    // Enter into the specified CFG, which will create a Env object
    fn enter_env(&mut self, cfg: Nref) {
        debug_assert!(cfg.borrow().is_cfg());

        // new environment's memory tracking list
        let mut effect;
        let mem_list;
        let global_list;
        let upval_list;

        // predecessor block lists, ie other CFG node jumps into the current CFG
        let pred_list = Node::pred_control(&cfg);

        if pred_list.len() == 0 {
            // the first block, ie start block, then just make all the xxx_list
            // as default.
            mem_list = self.init_mem_list();
            global_list = self.init_global_list();
            upval_list = self.init_upval_list();
        } else {
            // merge the memory list
            mem_list = self.merge_memory(&pred_list);

            // merge the global alias list
            global_list = self.merge_global(&pred_list);

            // merge the upvalue alias list
            upval_list = self.merge_upval(&pred_list);

            effect = EffectEnv::new(mem_list, global_list, upval_list);
        }

        let cfg_id = cfg.borrow().id;

        let bptr = Blk::new_ptr(cfg, effect);

        self.blk_list.push(bptr.clone());
        self.cur_blk = bptr;
        self.blk_map_from_id[cfg_id as usize] = (self.blk_list.len() - 1) as u32;
    }

    // Leave the current environment
    fn leave_env(&mut self) {}

    // -------------------------------------------------------------------------
    // Sub field access/mutation
    fn try_sub_field_store_alias(&mut self, node: &Nref, target: &Memptr) {
        let val = node.borrow().v2();
        let val_memory = match self.memory_of(&val) {
            Option::Some(v) => v,
            _ => return,
        };

        // If the memory of the target node already is an unknow, we just need
        // to set the aliased part to be unknown
        let should_be_unknown =
            val_memory.borrow().is_unknown() || target.borrow().is_unknown();

        if should_be_unknown {
            val_memory.borrow_mut().set_unknown();
            target.borrow_mut().set_unknown();
            return;
        }

        // If they are in different memory location, then just merge them
        // into one and also needs to mutate the memory object that both node
        // points to learn this facts
        Memory::merge_memory(&val_memory, target);
    }

    fn on_sub_field(&mut self, n: Nref) -> R {
        // (0) get subfield access's target memory location if applicable
        let mem = self.memory_of(&n)?;

        // (1) check whether need to handle alias or not. Notes the sub field
        //     store can potentially generate alias
        match &n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                self.try_sub_field_store_alias(&n, &mem);
            }
            _ => (),
        };

        // (2) update memory effect
        match &n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                mem.borrow_mut().update_write(n);
            }
            Opcode::RvMemIndexLoad | Opcode::RvMemDotLoad => {
                mem.borrow_mut().update_read(n);
            }
            _ => (),
        };

        return Option::Some(());
    }

    fn on_phi(&mut self, n: Nref) -> R {
        let mem = self.phi_memory_of(&n);
        for xx in mem.iter() {
            self.alias_mem(&xx);
        }
        return Option::Some(());
    }

    // -------------------------------------------------------------------------
    // Global value and upvalue
    fn on_set_global(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();

        let global_name = Node::global_name(&n, &func);

        self.cur_env()
            .borrow_mut()
            .effect
            .set_global(&global_name, v);

        return Option::Some(());
    }

    fn on_set_upvalue(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();

        let upvalue_index = Node::upvalue_index(&n, &func);

        self.cur_env()
            .borrow_mut()
            .effect
            .set_upvalue(upvalue_index, v);

        return Option::Some(());
    }

    // Call instruction. For unknown call, the call's input is as following ----
    //
    //   (Arg#1, ..., Arg#N; Global#1, ..., Global#N)
    //
    // After the call the Arg1 ... ArgN and Global1 ... GlobalN will have to be
    // assumed as in unknown region. Notes, we need to take care of each args,
    // since if args doesn't render a known memory location, then no need to
    // change it anymore
    fn on_call(&mut self, n: Nref) -> R {
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
        self.cur_env().borrow_mut().effect.unknownify_global();

        return Option::Some(());
    }

    fn on_iterator_new(&mut self, n: Nref) -> R {
        // the iterator takes an input(mostly a memory node) to generate a iter
        // that node should also been marked as unknown if we can recognize it
        // as an memory. Sometimes, it is not a memory or we never will be able
        // to mark it as unknown.
        {
            let maybe_mem = self.memory_of(&n.borrow().v0());
            match maybe_mem {
                Option::Some(m) => {
                    m.borrow_mut().set_unknown();
                }
                _ => (),
            };
        }

        self.cur_env().borrow_mut().effect.unknownify_global();
        return Option::Some(());
    }

    fn on_iterator_next(&mut self, _: Nref) -> R {
        self.cur_env().borrow_mut().effect.unknownify_global();
        return Option::Some(());
    }

    fn on_iterator_has(&mut self, _: Nref) -> R {
        self.cur_env().borrow_mut().effect.unknownify_global();
        return Option::Some(());
    }

    fn on_iterator_value(&mut self, _: Nref) -> R {
        self.cur_env().borrow_mut().effect.unknownify_global();
        return Option::Some(());
    }

    fn visited_at(&self, cfg: &Nref) -> bool {
        return self.visited[cfg.borrow().id as usize];
    }

    fn blk_analysis(&mut self) {
        self.visited
            .resize(self.j.borrow().max_node_id() as usize, false);

        let cfg = self.graph.borrow().cfg_start.clone();
        let mid = self.j.borrow().max_node_id();

        for c in CfgPOIter::new(&cfg, mid) {
            self.enter_env(c.clone());
            *self.visited.get_mut(c.borrow().id as usize).unwrap() = true;

            for enode in c.borrow().effect.iter() {
                for x in EffectPOIter::new(&enode, mid) {
                    match x.borrow().op.op {
                        Opcode::RvMemIndexLoad
                        | Opcode::RvMemDotLoad
                        | Opcode::RvMemIndexStore
                        | Opcode::RvMemDotStore => {
                            self.on_sub_field(x.clone());
                        }
                        Opcode::RvSetGlobal => {
                            self.on_set_global(x.clone());
                        }
                        Opcode::RvSetUpvalue => {
                            self.on_set_upvalue(x.clone());
                        }
                        Opcode::RvPhi => {
                            self.on_phi(x.clone());
                        }

                        // bytecode that forms a barrier to the global variables
                        Opcode::RvCall => {
                            self.on_call(x.clone());
                        }

                        Opcode::RvIteratorNew => {
                            self.on_iterator_new(x.clone());
                        }
                        Opcode::RvIteratorHas => {
                            self.on_iterator_has(x.clone());
                        }
                        Opcode::RvIteratorNext => {
                            self.on_iterator_next(x.clone());
                        }
                        Opcode::RvIteratorValue => {
                            self.on_iterator_value(x.clone());
                        }

                        // essentially the builtin assert halt will also generate
                        // a barrier to the global variable, but they are just
                        // mostly readonly, we ignore them here.
                        _ => (),
                    };
                }
            }

            self.leave_env();
        }
    }
}
