// -----------------------------------------------------------------------------
// This passes works inside of the BB and only generate the BB final information
// summary without relinking each node's side effect. It does an analysis but
// not mutate the graph. After the Block level analysis, only the block's IN
// status is valid, its out status is not have to be valid in case the loop
// body mutate the node's status.

type Dcptr = Rc<RefCell<DChain>>;
type Memptr = Rc<RefCell<Memory>>;
type Blkptr = Rc<RefCell<Blk>>;
type ValueLatticeList = Vec<ValueLattice>;

#[derive(Clone)]
struct Dchain {
    write: Nref,
    read: Vec<Nref>,
}

// Memory region discovery, since subfield assignment/mutation does not effectively
// change the existed known shape of heap, so the discovery of memory region can
// be safely done by looking at each memory alias node's point-to information.
// We always maintain this information, and they are sort of our memory location
// information source
#[derive(Clone)]
struct Memory {
    init: Nref,
    children: Vec<Memptr>,
    dep: Option<Dcptr>,
    sig: String,
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

#[derive(Clone)]
enum ValueLattice {
    Nothing,
    Const(Imm),
    // this node will always points to a node that is immutable, so no need to
    // maintain dependency chain
    NMem(Option<Nref>),
    Mem(Option<Memptr>),
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

// Public structure used for query
pub struct Dep {
    // represent the union of write effect from different branches, this can
    // be easily mapped to a EffectJoin IR node
    write: Nref,
    read: Vec<Nref>,
}

pub struct MemNode {
    init: Nref,
    children: Vec<
}

pub struct MemoryLoc {
    effect_join: Vec<LastEffect>,
}

pub enum Val {
    Const(Imm),
    NotMemory(Nref),
    Memory(
}

struct EffectEnv {
    mem_alias: Vec<MRef>,
    glb_alias: Vec<Galias>,
    upv_alias: Vec<Ualias>,
}

struct Blk {
    cfg: Nref,
    effect: EffEnv,
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
}

impl BlkAA {
    fn memory_of(&self, n: &Nref) -> Option<Memptr> {
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
                let mem = n.borrow().v0();

                if mem.borrow().is_rv_memory() {
                    match self.memory_alias_memory_of(mem) {
                        Option::Some(vv) => {
                            return ValueLattice::new_mem(vv);
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
                return ValueLattice::new_mem(
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
                debug_assert!(n.borrow().must_not_memory());
                return ValueLattice::new_nmem(n);
            }
        }

        return ValueLattice::new_unknown();
    }

    // * ----------------------------------------------------------------------
    // *  Pass
    // *    effect analysis, this pass will break all the side effect chain
    // *    based on alias analysis results, ie if multiple nodes access
    // *    the same piece of memory, based on analysis, they will be effect
    // *    depend on each other.
    // * ----------------------------------------------------------------------

    // try to merge all the known memory location of a BB's predecessor nodes
    fn merge_memory(&self, pred_list: &Vec<Nref>) -> Vec<Memptr> {
        let mut heap_list = Vec::<Memptr>::new();

        let tt = self.total_mem;

        for i in 0..tt {
            let mut mem_lattice_list = Vec::<MemoryLattice>::new();
            let mut mem_list = Heap::new();

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let prev_env = self.env_at(&pre).unwrap();

                    let prev_mem = prev_env.borrow().effect.mem_alias[i].clone();

                    mem_lattice_list.push(if prev_mem.borrow().is_unknow() {
                        MemoryLattice::new_unknown()
                    } else {
                        MemoryLattice::new_mem_init()
                    });
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

    fn merge_global(&mut self, pred_list: &Vec<Nref>) -> Vec<Galias> {
        let mut glist = Vec::<Galias>::new();
        let mut tt = self.total_global;

        for i in 0..tt {
            let mut l = ValueLatticeList::new();
            let global_name = self.global_name(i);

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();
                    let pre_glb_val =
                        pre_env.borrow().effect.glb_alias[i].v.clone();

                    l.push(pre_glb_val);
                } else {
                    l.push(ValueLattice::new_nothing());
                }
            }

            // perform the meet operation
            glist.push(Galias::new(global_name, ValueLattice::join(&l, false)));
        }

        return glist;
    }

    fn merge_upval(&mut self, pred_list: &Vec<Nref>) -> Vec<Ualias> {
        let mut ulist = Vec::<Ualias>::new();
        let mut tt = self.total_upvalue;

        for i in 0..tt {
            let mut l = ValueLatticeList::new();

            for pre in pred_list.iter() {
                if self.visited_cfg(&pre) {
                    let pre_env = self.env_at(&pre).unwrap();
                    let pre_upval = pre_env.borrow().effect.upv_alias[i].clone();

                    l.push(pre_upval);
                } else {
                    l.push(ValueLattice::new_nothing());
                }
            }

            // perform the meet operation
            ulist.push(Ualias::new(i, ValueLattice::join(&l, false)));
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

        // merge the memory list
        let mem_list = self.merge_memory(&pred_list);

        // merge the global alias list
        let global_list = self.merge_global(&pred_list);

        // merge the upvalue alias list
        let upval_list = self.merge_upval(&pred_list);

        effect = EffectEnv::new(mem_list, global_list, upval_list);

        let cfg_id = cfg.borrow().id;
        let eptr = Env::new_ptr(effect, cfg);

        self.env_list.push(eptr.clone());
        self.cur_env = eptr;
        self.env_map[cfg_id] = (self.env_list.len() - 1) as u32;
    }

    // Leave the current environment
    fn leave_env(&mut self) {}

    fn on_sub_field(&mut self, n: Nref) -> R {
        // (0) get subfield access's target memory location if applicable
        let mem = self.memory_of(&n)?;

        // (1) check whether need to handle alias or not. Notes the sub field
        //     store can potentially generate alias
        match &n.borrow().op.op {
            Opcode::RvMemIndexStore | Opcode::RvMemDotStore => {
                self.try_sub_field_store_alias(&n, &mem, false);
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
            self.alias_mem(&xx, &n);
        }
        return Option::Some(());
    }

    // Global operations, this pass can optimize away global load operation if
    // the global value is known to us
    fn on_set_global(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();
        let global_name = Node::global_name(&n, func);

        self.cur_env().borrow_mut().set_global(global_name, v);

        return Option::Some(());
    }

    fn on_set_upvalue(&mut self, n: Nref) -> R {
        let val = n.borrow().value[1].clone();
        let v = self.value_of(&val);
        let func = self.graph.borrow().func.clone();

        let upvalue_index = Node::upvalue_index(&n, func);

        self.cur_env().borrow_mut().set_upvalue(upvalue_index, v);

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
        self.cur_env().borrow_mut().unknownify_global();

        return Option::Some(());
    }

    fn blk_ana(&mut self) {
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
                            self.on_sub_field(x.clone());
                        }
                        Opcode::RvSetGlobal => {
                            self.on_set_global(x.clone());
                        }
                        Opcode::RvSetUpvalue => {
                            self.on_set_upvalue(x.clone());
                        }
                        Opcode::RvCall => {
                            self.on_call(x.clone());
                        }
                        Opcode::RvPhi => {
                            self.on_phi(x.clone());
                        }

                        _ => (),
                    };
                }
            }

            self.leave_env();
        }
    }

    // Forward pass, propagate data flow final result/equation to each block.
    // This pass just needs a simple cfg's PO order (ignore the backedge). This
    // pass will make our value lattice convergent based on the fact the
    // height of lattice.

    fn join_info(&mut self, env: Blkptr) {
    }

    fn blk_join(&mut self) {
        let cfg = self.graph.borrow().cfg_start.clone();

        for c in CfgPOIter::new(&cfg, self.j.borrow().max_node_id()) {
            let env = self.env_at(&c);
            self.join_info(env);
        }
    }
}
