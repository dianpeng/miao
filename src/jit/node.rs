use crate::jit::j::*;
use crate::object::object::*;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::iter;
use std::rc::Rc;
use std::rc::Weak;

pub type JFref = Rc<RefCell<JFunc>>;
pub type Oref = Rc<Op>;
pub type Nref = Rc<RefCell<Node>>;
pub type WkNref = Weak<RefCell<Node>>;
pub type Aref = Rc<RefCell<Alias>>;
pub type BBref = Rc<RefCell<BBlk>>;
pub type Nid = u32;
pub type BBList = Vec<BBref>;
pub type Mpptr = Rc<RefCell<Mpool>>;

pub type Nqueue = VecDeque<Nref>;
pub type Nidqueue = VecDeque<Nid>;

pub trait Reclaim {
    // User implementation of is_dead, since we need to somehow to break the
    // cyclical reference conuting. The only way to do so is let each part to
    // decide whether it is dead or not.
    fn is_dead(&self) -> bool;

    // Invoked by Mpool when the corresponding node is been marked as dead
    fn on_reclaim(&mut self);
}

pub type RecRef = Weak<RefCell<dyn Reclaim>>;
pub type NodeGCList = Vec<RecRef>;

pub enum AliasType {
    Not,
    May,
    Must,
}

pub enum AComp {
    Index(Nref),
    Dot(Nref),
}

pub struct Alias {
    slot: Aref,
    base: Nref,
    comp: AComp,
}

// Denote as the input list, ie representing the value dependency
pub type ValueList = Vec<Nref>;

pub enum DefUse {
    Value(WkNref),
    Control(WkNref),
    Effect(WkNref),
    Invalid,
}

pub type DefUseList = Vec<DefUse>;

#[derive(Debug)]
pub enum Imm {
    Index(u32),
    ImmU32(u32),
    ImmU16(u16),
    ImmU8(u8),
    ImmI64(i64),
    ImmI32(i32),
    ImmI16(i16),
    ImmI8(i8),
    ImmF64(f64),
    Invalid,
}

pub struct JType;

// The graph is orgnized as following, we use a hybrid method of Sea of nodes and
// classical basic block. For each instruction, if it has a side effect, then
// it is been placed into the node's effect list, which is only been used by
// cfg node, otherwise, the effect list will be empty; for any none side effect
// node, then it will be like sea of nodes, ie floating inside of the graph and
// been only utilized at certain control flow node, later on we can perform GVN
// on the floating node for dedup purpose.

pub struct Node {
    // -----------------------------------------------------------------------
    // Node operator, ie used for showing the opcode of the node
    pub op: Oref,

    // -----------------------------------------------------------------------
    // Value dependency, any-ary
    pub value: ValueList,

    // -----------------------------------------------------------------------
    // CFG dependency, any-ary, used in situation like normal CFG or Phi nodes
    pub cfg: ValueList,

    // -----------------------------------------------------------------------
    // Effect dependency, singleton
    pub effect: ValueList,

    // -----------------------------------------------------------------------
    // DefUse list, ie representing who are using me as whatever input
    pub def_use: DefUseList,

    // -----------------------------------------------------------------------
    // For special usage, ie encoding immediate number when Op is Imm tiered
    pub imm: Imm,

    // -----------------------------------------------------------------------
    // Some nodes needs to take some special arguments, like Immediate number
    // will needs to take the value alongside with itself. In order to make
    // the life simpler, we just place the immediate number directly at the
    // node here. The immediate does not account for input arguments etc ...
    pub id: Nid,
    pub bcid: u32,

    // -----------------------------------------------------------------------
    // Other information
    pub alias: Option<Aref>,
    pub jtype: Option<JType>,

    // Whether the node is been treated as dead or not
    pub dead: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpTier {
    Imm,      // immediate numbers, ie constant
    Cfg,      // control flow node
    Snapshot, // Checkpoint
    Pseudo,   // man made node, ie DeoptEntry etc ..., they will be materialized
    // back to lower structure or been removed entirely
    Phi,
    Bval, // box/unbox operations, we will have special phase to lower them
    Rval, // high level operations, ie Rval(standsfor Rust Value, boxed)
    Mid,  // middle tier IR, standsfor none-architecture specific instructions
    Arch, // arch tier IR, closely related to the target assembly language
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Opcode {
    // Pseudo nodes

    // placeholder node, used to mark that the value is not materialized yet
    Placeholder,

    // Snapshot usage
    Snapshot,
    RestoreCell,

    // Immediate numbers
    ImmIndex,
    ImmU32,
    ImmU16,
    ImmU8,
    ImmI64,
    ImmI32,
    ImmI16,
    ImmI8,
    ImmF64,

    // Boxing and unboxing operation

    // BoxXXX operation, take a machine representation value and box it into a
    // Rval, ie the boxed version.
    BoxI64,
    BoxI32,
    BoxI16,
    BoxI8,
    BoxU32,
    BoxU16,
    BoxU8,
    BoxF64,
    BoxTrue,
    BoxFalse,
    BoxNull,

    // UnboxXXX operation, take a box value and unbox value back to the machine
    // type represented by the instruction itself. Notes, if the boxed value has
    // a different type, then the behavior is undefined, these operation doesn't
    // do check. If check is needed, perform with CheckUnboxXXX instruction is
    // preferred, which will check the type of the value and deoptimize if the
    // instruction failed
    UnboxI64,
    UnboxI32,
    UnboxI16,
    UnboxI8,
    UnboxU32,
    UnboxU16,
    UnboxU8,
    UnboxF64,
    UnboxTrue,
    UnboxFalse,
    UnboxNull,

    // Type guards, these operations will check whether the boxed value has type
    // otherwise it will just deoptimize, ie jumping to the deoptimize branch

    // high level, dealing with boxing value and all the script semantic, all
    // the instruction prefixed with Rv, stands for Rust value, which means the
    // boxed value.

    // Rv phis , high level PHIS, ie marshal 2 boxed value,
    RvPhi,

    // Rust value arithmetic value, both sides must be a boxed value,
    RvAdd,
    RvSub,
    RvMul,
    RvDiv,
    RvMod,
    RvPow,

    // Rust value comparison, both side must be boxed value and it generates a
    // boxed boolean value.
    RvEq,
    RvNe,
    RvGt,
    RvGe,
    RvLt,
    RvLe,

    // Rust value unary, generates a boxed value.
    RvNot,
    RvNeg,

    // Logic helper, the original And/Or/Ternary does too much things, we will
    // lower them into an expression/artihmetic operator + control flow opeartor
    RvAnd,
    RvOr,

    // Literal constant loading, notes, the value been loaded is a boxed value
    RvLoadInt,
    RvLoadReal,
    RvLoadString,
    RvLoadFunction,
    RvLoadNull,
    RvLoadTrue,
    RvLoadFalse,

    // Concatenate strings
    RvConStr,

    // List creation operations, this operation creates a new list in boxed
    // version
    RvListCreate,
    RvListAdd,

    RvObjectCreate,
    RvObjectAdd,

    RvIteratorNew,
    RvIteratorHas,
    RvIteratorNext,
    RvIteratorValue,

    // Global
    RvLoadGlobal,
    RvSetGlobal,

    // Upvalue
    RvLoadUpvalue,
    RvSetUpvalue,

    // Indexing/Dot, memory operation, generate side effect and should be used
    // with alias analysis
    RvMemIndexLoad,
    RvMemIndexStore,
    RvMemDotLoad,
    RvMemDotStore,

    // Builtins
    RvAssert1,
    RvAssert2,
    RvTrace,
    RvTypeof,
    RvSizeof,
    RvHalt,

    // Conversion, hidden operation hide inside of the interpreter(semantic)
    RvToString,
    RvToBoolean,

    // none arch, after aggressive type inference or speculative instruction
    // mostly bound to a fixed width machine word operation. All the instruction
    // prefixed with Fw, which stands for fixed width.

    // low level, ie nearly 1to2 assembly translation. All the instruction prefix
    // with M, which stands for machine.

    // Comparison related, ie forming the basic block as following :
    #[rustfmt::skip]
		/*

							+-------+
							| IfCmp |
						+-+-------+-+
						|           |
						|           |
				+---v--+    +---v---+
				| Blk  |    |  Blk  |
				+------+    +-------+
							 |    |
							 |    |
							+v----v-+
							|  Blk  |
							+-------+
		*/
    CfgStart,
    CfgEnd,

    CfgMergeReturn,
    CfgMergeHalt,

    /*
    CfgMergeAssert,
    CfgMergeDeopt,
    CfgMergeBug,
    */
    // generall block
    CfgHalt,
    CfgReturn,
    CfgIfCmp,
    CfgJump,
    CfgLoopBack,
}

pub enum ParamLimits {
    Limit(u8),
    Any,
}

pub struct MachineFlag;

pub struct Op {
    pub name: String,
    pub tier: OpTier,
    pub op: Opcode,
    pub side_effect: bool,
    pub deoptimize: bool,
    pub in_size: ParamLimits,
    pub flag_size: MachineFlag,
}

// Control-flow block, used when we start to do scheduling of instructions.
pub struct BBlk {
    // list of isntruction been scheduled into the basic block, notes the inst
    // here has the partial order already
    ins_list: Vec<Nref>,

    // tree
    pred: BBList,
    lhs: BBref,
    rhs: BBref,

    // dominator
    // immediate dominator inside of the dominator trees that dominates this
    // block
    idom: BBref,

    // dominator set, ie all the block been dominated by this block
    domset: BBList,
}

// All the allocation will be done inside of the Mpool which keeps a strong
// ref to each node and also call reclaim if needed, this makes us away from
// leaking memory internally due to cyclical reference counting.
pub struct Mpool {
    gc_list: NodeGCList,
    node_id: Nid,

    // =======================================================================
    // Opcode static parts, each Op is singleton through out the code generation
    // since itself is readonly.
    op_imm_index: Oref,
    op_imm_u32: Oref,
    op_imm_u16: Oref,
    op_imm_u8: Oref,
    op_imm_i64: Oref,
    op_imm_i32: Oref,
    op_imm_i16: Oref,
    op_imm_i8: Oref,
    op_imm_f64: Oref,

    op_box_i64: Oref,
    op_box_i32: Oref,
    op_box_i16: Oref,
    op_box_i8: Oref,
    op_box_u32: Oref,
    op_box_u16: Oref,
    op_box_u8: Oref,
    op_box_f64: Oref,
    op_box_true: Oref,
    op_box_false: Oref,
    op_box_null: Oref,

    op_unbox_i64: Oref,
    op_unbox_i32: Oref,
    op_unbox_i16: Oref,
    op_unbox_i8: Oref,
    op_unbox_u32: Oref,
    op_unbox_u16: Oref,
    op_unbox_u8: Oref,
    op_unbox_f64: Oref,
    op_unbox_true: Oref,
    op_unbox_false: Oref,
    op_unbox_null: Oref,

    // (((((((((((((((((((((( PESUDO ))))))))))))))))))))))
    op_placeholder: Oref,

    // (((((((((((((((((((((( SNAPSHOT ))))))))))))))))))))))
    op_snapshot: Oref,
    op_restore_cell: Oref,

    // (((((((((((((((((((((( RvIR ie HIR ))))))))))))))))))))))
    op_rv_add: Oref,
    op_rv_sub: Oref,
    op_rv_mul: Oref,
    op_rv_div: Oref,
    op_rv_mod: Oref,
    op_rv_pow: Oref,

    op_rv_con_str: Oref,

    op_rv_eq: Oref,
    op_rv_ne: Oref,
    op_rv_gt: Oref,
    op_rv_ge: Oref,
    op_rv_lt: Oref,
    op_rv_le: Oref,

    op_rv_not: Oref,
    op_rv_neg: Oref,

    op_rv_and: Oref,
    op_rv_or: Oref,

    op_rv_load_int: Oref,
    op_rv_load_real: Oref,
    op_rv_load_string: Oref,
    op_rv_load_null: Oref,
    op_rv_load_true: Oref,
    op_rv_load_false: Oref,
    op_rv_load_function: Oref,

    op_rv_list_create: Oref,
    op_rv_list_add: Oref,

    op_rv_object_create: Oref,
    op_rv_object_add: Oref,

    op_rv_iterator_new: Oref,
    op_rv_iterator_has: Oref,
    op_rv_iterator_next: Oref,
    op_rv_iterator_value: Oref,

    op_rv_load_global: Oref,
    op_rv_set_global: Oref,

    op_rv_load_upvalue: Oref,
    op_rv_set_upvalue: Oref,

    op_rv_mem_index_load: Oref,
    op_rv_mem_index_store: Oref,
    op_rv_mem_dot_load: Oref,
    op_rv_mem_dot_store: Oref,

    op_rv_assert1: Oref,
    op_rv_assert2: Oref,
    op_rv_trace: Oref,
    op_rv_typeof: Oref,
    op_rv_sizeof: Oref,
    op_rv_halt: Oref,

    op_rv_to_string: Oref,
    op_rv_to_boolean: Oref,

    // Phi
    op_rv_phi: Oref,

    // -------------------------------------------------------------------------
    // (((((((((((((((((((((( CFG node ))))))))))))))))))))))
    op_cfg_start: Oref,
    op_cfg_end: Oref,
    op_cfg_merge_return: Oref,
    op_cfg_merge_halt: Oref,

    op_cfg_halt: Oref,
    op_cfg_return: Oref,
    op_cfg_if_cmp: Oref,
    op_cfg_jump: Oref,
    op_cfg_loop_back: Oref,
}

// represent the function that been parsed
pub struct JFunc {
    jit: Jitptr,

    // (start, end) block
    start: BBref,

    // The graph is linked reversly, so we should always try to start the visiting
    // of CFG from end block
    end: BBref,

    // deoptimization end block
    deopt: BBref,

    // return end block
    ret: BBref,

    // halt end block
    halt: BBref,

    // error end block, ie generated by builtin assert1/2 etc ...
    error: BBref,

    // bug end block, ie generated by compiler to allow future debugging
    bug: BBref,

    // rpo order
    rpo: BBList,
}

impl Reclaim for Node {
    fn is_dead(&self) -> bool {
        return self.dead;
    }

    fn on_reclaim(&mut self) {
        self.value.clear();
        self.cfg.clear();
        self.effect.clear();
        self.def_use.clear();
    }
}

impl Node {
    // -----------------------------------------------------------------------
    // Node operator. User needs to use the following APIs to mutate the node
    // otherwise the node may be in inconsistent state, since we have extra
    // metadata record for each node's def/use situations.
    pub fn add_value(node: &mut Nref, v: Nref) {
        // (0) adding the def-use into the v
        {
            let weak_ref = Rc::downgrade(node);
            v.borrow_mut().def_use.push(DefUse::Value(weak_ref));
        }

        // (1) update the value lists
        node.borrow_mut().value.push(v);

        // sanity check against the number of operands added into the nodes
        if let ParamLimits::Limit(num) = &node.borrow().op.in_size {
            debug_assert!((*num as usize) >= node.borrow().value.len());
        }
    }

    pub fn add_control(node: &mut Nref, v: Nref) {
        // (0) adding the def-use into the v
        {
            let weak_ref = Rc::downgrade(node);
            v.borrow_mut().def_use.push(DefUse::Control(weak_ref));
        }
        node.borrow_mut().cfg.push(v);
    }

    pub fn add_effect(node: &mut Nref, v: Nref) {
        assert!(node.borrow().is_cfg());

        // (0) adding the def-use into the v
        {
            let weak_ref = Rc::downgrade(node);
            v.borrow_mut().def_use.push(DefUse::Effect(weak_ref));
        }
        node.borrow_mut().effect.push(v);
    }

    pub fn add_phi_value(phi: &mut Nref, value: Nref, cfg: Nref) {
        debug_assert!(phi.borrow().is_phi());
        debug_assert!(cfg.borrow().is_cfg());
        debug_assert!(value.borrow().is_value());
        Node::add_value(phi, value);
        Node::add_control(phi, cfg);
    }

    // -----------------------------------------------------------------------
    // Factory Method
    // -----------------------------------------------------------------------
    fn new(op: Oref, id: Nid, bcid: u32) -> Nref {
        return Nref::new(RefCell::new(Node {
            op: op,
            value: ValueList::new(),
            cfg: ValueList::new(),
            effect: ValueList::new(),
            def_use: DefUseList::new(),
            imm: Imm::Invalid,
            id: id,
            bcid: bcid,
            alias: Option::None,
            jtype: Option::None,
            dead: false,
        }));
    }

    fn new_imm(op: Oref, id: Nid, bcid: u32, imm: Imm) -> Nref {
        return Nref::new(RefCell::new(Node {
            op: op,
            value: ValueList::new(),
            cfg: ValueList::new(),
            effect: ValueList::new(),
            def_use: DefUseList::new(),
            imm: imm,
            id: id,
            bcid: bcid,
            alias: Option::None,
            jtype: Option::None,
            dead: false,
        }));
    }

    fn new_unary(op: Oref, id: Nid, bcid: u32, v0: Nref) -> Nref {
        let mut n = Node::new(op, id, bcid);
        Node::add_value(&mut n, v0);
        return n;
    }

    fn new_binary(op: Oref, id: Nid, bcid: u32, lhs: Nref, rhs: Nref) -> Nref {
        let mut n = Node::new(op, id, bcid);
        Node::add_value(&mut n, lhs);
        Node::add_value(&mut n, rhs);
        return n;
    }

    fn new_ternary(
        op: Oref,
        id: Nid,
        bcid: u32,
        v0: Nref,
        v1: Nref,
        v2: Nref,
    ) -> Nref {
        let mut n = Node::new(op, id, bcid);
        Node::add_value(&mut n, v0);
        Node::add_value(&mut n, v1);
        Node::add_value(&mut n, v2);
        return n;
    }

    // ------------------------------------------------------------------------
    // Convinient methods
    pub fn is_cfg(&self) -> bool {
        return Op::is_cfg(&self.op);
    }
    pub fn is_phi(&self) -> bool {
        return Op::is_phi(&self.op);
    }
    pub fn is_value(&self) -> bool {
        return Op::is_value(&self.op);
    }
    pub fn mark_dead(&mut self) {
        self.dead = true;
    }
    pub fn has_side_effect(&self) -> bool {
        return self.op.side_effect;
    }
    pub fn is_rv_list_create(&self) -> bool {
        return self.op.op == Opcode::RvListCreate;
    }
    pub fn is_rv_object_create(&self) -> bool {
        return self.op.op == Opcode::RvObjectCreate;
    }
    pub fn is_cfg_start(&self) -> bool {
        return self.op.op == Opcode::CfgStart;
    }
    pub fn is_cfg_end(&self) -> bool {
        return self.op.op == Opcode::CfgEnd;
    }
}

impl Op {
    // testify categories -----------------------------------------------------
    fn is_cfg(x: &Oref) -> bool {
        return x.tier == OpTier::Cfg;
    }

    fn is_phi(x: &Oref) -> bool {
        return x.tier == OpTier::Phi;
    }

    fn is_value(x: &Oref) -> bool {
        return x.tier == OpTier::Bval
            || x.tier == OpTier::Phi
            || x.tier == OpTier::Rval
            || x.tier == OpTier::Mid
            || x.tier == OpTier::Arch
            || x.tier == OpTier::Imm;
    }

    // factory method categories ----------------------------------------------
    fn new(
        n: &str,
        tier: OpTier,
        op: Opcode,
        side: bool,
        deopt: bool,
        isz: ParamLimits,
        fsz: MachineFlag,
    ) -> Op {
        Op {
            name: n.to_string(),
            tier: tier,
            op: op,
            side_effect: side,
            deoptimize: deopt,
            in_size: isz,
            flag_size: fsz,
        }
    }

    fn new_binary(
        n: &str,
        tier: OpTier,
        opcode: Opcode,
        side: bool,
        deopt: bool,
        fsz: MachineFlag,
    ) -> Op {
        return Op::new(
            n,
            tier,
            opcode,
            side,
            deopt,
            ParamLimits::Limit(2),
            fsz,
        );
    }

    fn new_rv_binary(n: &str, opcode: Opcode) -> Op {
        return Op::new_binary(
            n,
            OpTier::Rval,
            opcode,
            false, // does not have side effect
            true,  // but generate deoptimize information
            MachineFlag {},
        );
    }

    fn new_unary(
        n: &str,
        tier: OpTier,
        opcode: Opcode,
        side: bool,
        deopt: bool,
        fsz: MachineFlag,
    ) -> Op {
        return Op::new(
            n,
            tier,
            opcode,
            side,
            deopt,
            ParamLimits::Limit(1),
            fsz,
        );
    }

    fn new_rv_unary(n: &str, opcode: Opcode) -> Op {
        return Op::new_unary(
            n,
            OpTier::Rval,
            opcode,
            false,
            true,
            MachineFlag {},
        );
    }

    fn new_rv_load(n: &str, opcode: Opcode) -> Op {
        return Op::new(
            n,
            OpTier::Rval,
            opcode,
            false,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        );
    }

    fn new_imm(n: &str, opcode: Opcode) -> Op {
        return Op::new(
            n,
            OpTier::Imm,
            opcode,
            false,
            false,
            ParamLimits::Limit(0),
            MachineFlag {},
        );
    }

    // static composer of op structure, used in Mpool to initialize its ref
    // notes, these functions are never supposed to be used anywhere else except
    // the Mpool, the creation of Op needs to take place in Mpool and Op is most
    // just immutable placeholder
    fn imm_index() -> Oref {
        Oref::new(Op::new_imm("imm_index", Opcode::ImmIndex))
    }
    fn imm_u32() -> Oref {
        Oref::new(Op::new_imm("imm_u32", Opcode::ImmU32))
    }
    fn imm_u16() -> Oref {
        Oref::new(Op::new_imm("imm_u16", Opcode::ImmU16))
    }
    fn imm_u8() -> Oref {
        Oref::new(Op::new_imm("imm_u8", Opcode::ImmU8))
    }

    fn imm_i64() -> Oref {
        Oref::new(Op::new_imm("imm_i64", Opcode::ImmI64))
    }
    fn imm_i32() -> Oref {
        Oref::new(Op::new_imm("imm_i32", Opcode::ImmI32))
    }
    fn imm_i16() -> Oref {
        Oref::new(Op::new_imm("imm_i16", Opcode::ImmI16))
    }
    fn imm_i8() -> Oref {
        Oref::new(Op::new_imm("imm_i8", Opcode::ImmI8))
    }
    fn imm_f64() -> Oref {
        Oref::new(Op::new_imm("imm_f64", Opcode::ImmF64))
    }

    fn box_i64() -> Oref {
        Oref::new(Op::new_unary(
            "box_i64",
            OpTier::Bval,
            Opcode::BoxI64,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_i32() -> Oref {
        Oref::new(Op::new_unary(
            "box_i32",
            OpTier::Bval,
            Opcode::BoxI32,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_i16() -> Oref {
        Oref::new(Op::new_unary(
            "box_i16",
            OpTier::Bval,
            Opcode::BoxI16,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_i8() -> Oref {
        Oref::new(Op::new_unary(
            "box_i8",
            OpTier::Bval,
            Opcode::BoxI8,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_u32() -> Oref {
        Oref::new(Op::new_unary(
            "box_u32",
            OpTier::Bval,
            Opcode::BoxU32,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn box_u16() -> Oref {
        Oref::new(Op::new_unary(
            "box_u16",
            OpTier::Bval,
            Opcode::BoxU16,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn box_u8() -> Oref {
        Oref::new(Op::new_unary(
            "box_u8",
            OpTier::Bval,
            Opcode::BoxU8,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn box_f64() -> Oref {
        Oref::new(Op::new_unary(
            "box_f64",
            OpTier::Bval,
            Opcode::BoxF64,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn box_true() -> Oref {
        Oref::new(Op::new_unary(
            "box_true",
            OpTier::Bval,
            Opcode::BoxTrue,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn box_false() -> Oref {
        Oref::new(Op::new_unary(
            "box_false",
            OpTier::Bval,
            Opcode::BoxFalse,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn box_null() -> Oref {
        Oref::new(Op::new_unary(
            "box_null",
            OpTier::Bval,
            Opcode::BoxNull,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_i64() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_i64",
            OpTier::Bval,
            Opcode::UnboxI64,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_i32() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_i32",
            OpTier::Bval,
            Opcode::UnboxI32,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_i16() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_i16",
            OpTier::Bval,
            Opcode::UnboxI16,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_i8() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_i8",
            OpTier::Bval,
            Opcode::UnboxI8,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_u32() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_u32",
            OpTier::Bval,
            Opcode::UnboxU32,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_u16() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_u16",
            OpTier::Bval,
            Opcode::UnboxU16,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn unbox_u8() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_u8",
            OpTier::Bval,
            Opcode::UnboxU8,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn unbox_f64() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_f64",
            OpTier::Bval,
            Opcode::UnboxF64,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn unbox_true() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_true",
            OpTier::Bval,
            Opcode::UnboxTrue,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn unbox_false() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_false",
            OpTier::Bval,
            Opcode::UnboxFalse,
            false,
            false,
            MachineFlag {},
        ))
    }
    fn unbox_null() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_null",
            OpTier::Bval,
            Opcode::UnboxNull,
            false,
            false,
            MachineFlag {},
        ))
    }
    // =============================================================
    // (((Pseudo)))
    // =============================================================
    fn placeholder() -> Oref {
        Oref::new(Op::new(
            "placeholder",
            OpTier::Pseudo,
            Opcode::Placeholder,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    // =============================================================
    // (((Pseudo)))
    // =============================================================
    fn snapshot() -> Oref {
        Oref::new(Op::new(
            "snapshot",
            OpTier::Snapshot,
            Opcode::Snapshot,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }
    fn restore_cell() -> Oref {
        Oref::new(Op::new(
            "restore_cell",
            OpTier::Snapshot,
            Opcode::RestoreCell,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    // =============================================================
    // (((Rval)))
    // =============================================================
    fn rv_add() -> Oref {
        Oref::new(Op::new_rv_binary("rv_add", Opcode::RvAdd))
    }
    fn rv_sub() -> Oref {
        Oref::new(Op::new_rv_binary("rv_sub", Opcode::RvSub))
    }
    fn rv_mul() -> Oref {
        Oref::new(Op::new_rv_binary("rv_mul", Opcode::RvMul))
    }
    fn rv_div() -> Oref {
        Oref::new(Op::new_rv_binary("rv_div", Opcode::RvDiv))
    }
    fn rv_mod() -> Oref {
        Oref::new(Op::new_rv_binary("rv_mod", Opcode::RvMod))
    }
    fn rv_pow() -> Oref {
        Oref::new(Op::new_rv_binary("rv_pow", Opcode::RvPow))
    }
    fn rv_con_str() -> Oref {
        Oref::new(Op::new_rv_binary("rv_con_str", Opcode::RvConStr))
    }

    fn rv_le() -> Oref {
        Oref::new(Op::new_rv_binary("rv_le", Opcode::RvLe))
    }
    fn rv_lt() -> Oref {
        Oref::new(Op::new_rv_binary("rv_lt", Opcode::RvLt))
    }
    fn rv_gt() -> Oref {
        Oref::new(Op::new_rv_binary("rv_gt", Opcode::RvGt))
    }
    fn rv_ge() -> Oref {
        Oref::new(Op::new_rv_binary("rv_ge", Opcode::RvGe))
    }
    fn rv_eq() -> Oref {
        Oref::new(Op::new_rv_binary("rv_eq", Opcode::RvEq))
    }
    fn rv_ne() -> Oref {
        Oref::new(Op::new_rv_binary("rv_ne", Opcode::RvNe))
    }

    fn rv_and() -> Oref {
        Oref::new(Op::new_rv_binary("rv_and", Opcode::RvAnd))
    }
    fn rv_or() -> Oref {
        Oref::new(Op::new_rv_binary("rv_or", Opcode::RvOr))
    }

    fn rv_not() -> Oref {
        Oref::new(Op::new_rv_unary("rv_not", Opcode::RvNot))
    }
    fn rv_neg() -> Oref {
        Oref::new(Op::new_rv_unary("rv_neg", Opcode::RvNeg))
    }

    fn rv_load_int() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_int", Opcode::RvLoadInt))
    }
    fn rv_load_real() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_real", Opcode::RvLoadReal))
    }
    fn rv_load_string() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_string", Opcode::RvLoadString))
    }
    fn rv_load_function() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_function", Opcode::RvLoadFunction))
    }
    fn rv_load_null() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_null", Opcode::RvLoadNull))
    }
    fn rv_load_true() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_true", Opcode::RvLoadTrue))
    }
    fn rv_load_false() -> Oref {
        Oref::new(Op::new_rv_load("rv_load_false", Opcode::RvLoadFalse))
    }

    fn rv_list_create() -> Oref {
        Oref::new(Op::new(
            "rv_list_create",
            OpTier::Rval,
            Opcode::RvListCreate,
            false,
            false,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }

    fn rv_list_add() -> Oref {
        Oref::new(Op::new(
            "rv_list_add",
            OpTier::Rval,
            Opcode::RvListAdd,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    fn rv_object_create() -> Oref {
        Oref::new(Op::new(
            "rv_object_create",
            OpTier::Rval,
            Opcode::RvObjectCreate,
            false,
            false,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }
    fn rv_object_add() -> Oref {
        Oref::new(Op::new(
            "rv_object_add",
            OpTier::Rval,
            Opcode::RvObjectAdd,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    // iterator
    fn rv_iterator_new() -> Oref {
        Oref::new(Op::new(
            "rv_iterator_new",
            OpTier::Rval,
            Opcode::RvIteratorNew,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_iterator_has() -> Oref {
        Oref::new(Op::new(
            "rv_iterator_has",
            OpTier::Rval,
            Opcode::RvIteratorHas,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_iterator_next() -> Oref {
        Oref::new(Op::new(
            "rv_iterator_next",
            OpTier::Rval,
            Opcode::RvIteratorNext,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_iterator_value() -> Oref {
        Oref::new(Op::new(
            "rv_iterator_value",
            OpTier::Rval,
            Opcode::RvIteratorValue,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_load_global() -> Oref {
        Oref::new(Op::new(
            "rv_load_global",
            OpTier::Rval,
            Opcode::RvLoadGlobal,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }
    fn rv_set_global() -> Oref {
        Oref::new(Op::new(
            "rv_set_global",
            OpTier::Rval,
            Opcode::RvSetGlobal,
            true,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn rv_load_upvalue() -> Oref {
        Oref::new(Op::new(
            "rv_load_upvalue",
            OpTier::Rval,
            Opcode::RvLoadUpvalue,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }
    fn rv_set_upvalue() -> Oref {
        Oref::new(Op::new(
            "rv_set_global",
            OpTier::Rval,
            Opcode::RvSetUpvalue,
            true,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn rv_mem_index_load() -> Oref {
        Oref::new(Op::new(
            "rv_mem_index_load",
            OpTier::Rval,
            Opcode::RvMemIndexLoad,
            true,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }
    fn rv_mem_index_store() -> Oref {
        Oref::new(Op::new(
            "rv_mem_index_store",
            OpTier::Rval,
            Opcode::RvMemIndexStore,
            true,
            false,
            ParamLimits::Limit(3),
            MachineFlag {},
        ))
    }

    fn rv_mem_dot_load() -> Oref {
        Oref::new(Op::new(
            "rv_mem_dot_load",
            OpTier::Rval,
            Opcode::RvMemDotLoad,
            true,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }
    fn rv_mem_dot_store() -> Oref {
        Oref::new(Op::new(
            "rv_mem_dot_store",
            OpTier::Rval,
            Opcode::RvMemDotStore,
            true,
            false,
            ParamLimits::Limit(3),
            MachineFlag {},
        ))
    }

    // conversion
    fn rv_to_string() -> Oref {
        Oref::new(Op::new(
            "rv_to_string",
            OpTier::Rval,
            Opcode::RvToString,
            true,
            true,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_to_boolean() -> Oref {
        // To boolean operation will never fail, so we treat it as none side
        // effect, to_string operation may fail which requires checkpoint, so
        // it has side effect
        Oref::new(Op::new(
            "rv_to_boolean",
            OpTier::Rval,
            Opcode::RvToBoolean,
            false,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_phi() -> Oref {
        Oref::new(Op::new(
            "rv_phi",
            OpTier::Phi,
            Opcode::RvPhi,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    // Builtins, the builtin will be lowered into the corresponding intrinsic
    // during builtin lower phase.
    fn rv_assert1() -> Oref {
        Oref::new(Op::new(
            "rv_assert1",
            OpTier::Rval,
            Opcode::RvAssert1,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_assert2() -> Oref {
        Oref::new(Op::new(
            "rv_assert1",
            OpTier::Rval,
            Opcode::RvAssert2,
            true,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn rv_trace() -> Oref {
        Oref::new(Op::new(
            "rv_trace",
            OpTier::Rval,
            Opcode::RvTrace,
            true,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    fn rv_typeof() -> Oref {
        Oref::new(Op::new(
            "rv_typeof",
            OpTier::Rval,
            Opcode::RvTypeof,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_sizeof() -> Oref {
        Oref::new(Op::new(
            "rv_sizeof",
            OpTier::Rval,
            Opcode::RvSizeof,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn rv_halt() -> Oref {
        Oref::new(Op::new(
            "rv_halt",
            OpTier::Rval,
            Opcode::RvHalt,
            true,
            false,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //                          Control Flow Graph
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    fn cfg_start() -> Oref {
        Oref::new(Op::new(
            "cfg_start",
            OpTier::Cfg,
            Opcode::CfgStart,
            true,
            true,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }

    fn cfg_end() -> Oref {
        Oref::new(Op::new(
            "cfg_end",
            OpTier::Cfg,
            Opcode::CfgEnd,
            true,
            true,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }

    fn cfg_merge_return() -> Oref {
        Oref::new(Op::new(
            "cfg_merge_return",
            OpTier::Cfg,
            Opcode::CfgMergeReturn,
            true,
            true,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }

    fn cfg_merge_halt() -> Oref {
        Oref::new(Op::new(
            "cfg_merge_halt",
            OpTier::Cfg,
            Opcode::CfgMergeHalt,
            true,
            true,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }

    fn cfg_halt() -> Oref {
        Oref::new(Op::new(
            "cfg_halt",
            OpTier::Cfg,
            Opcode::CfgHalt,
            true,
            true,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn cfg_return() -> Oref {
        Oref::new(Op::new(
            "cfg_return",
            OpTier::Cfg,
            Opcode::CfgReturn,
            true,
            true,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn cfg_if_cmp() -> Oref {
        Oref::new(Op::new(
            "cfg_if_cmp",
            OpTier::Cfg,
            Opcode::CfgIfCmp,
            true,
            true,
            ParamLimits::Limit(1),
            MachineFlag {},
        ))
    }

    fn cfg_jump() -> Oref {
        Oref::new(Op::new(
            "cfg_jump",
            OpTier::Cfg,
            Opcode::CfgJump,
            true,
            true,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }

    fn cfg_loop_back() -> Oref {
        Oref::new(Op::new(
            "cfg_loop_back",
            OpTier::Cfg,
            Opcode::CfgLoopBack,
            true,
            true,
            ParamLimits::Limit(0),
            MachineFlag {},
        ))
    }
}

impl Mpool {
    pub fn new() -> Mpptr {
        return Mpptr::new(RefCell::new(Mpool {
            gc_list: NodeGCList::new(),
            node_id: 0,

            // ----------------------------------------------------------------
            // initialization of node opr -------------------------------------
            op_imm_index: Op::imm_index(),
            op_imm_u32: Op::imm_u32(),
            op_imm_u16: Op::imm_u16(),
            op_imm_u8: Op::imm_u8(),
            op_imm_i64: Op::imm_i64(),
            op_imm_i32: Op::imm_i32(),
            op_imm_i16: Op::imm_i16(),
            op_imm_i8: Op::imm_i8(),
            op_imm_f64: Op::imm_f64(),

            op_box_i64: Op::box_i64(),
            op_box_i32: Op::box_i32(),
            op_box_i16: Op::box_i16(),
            op_box_i8: Op::box_i8(),
            op_box_u32: Op::box_u32(),
            op_box_u16: Op::box_u16(),
            op_box_u8: Op::box_u8(),
            op_box_f64: Op::box_f64(),
            op_box_true: Op::box_true(),
            op_box_false: Op::box_false(),
            op_box_null: Op::box_null(),

            op_unbox_i64: Op::unbox_i64(),
            op_unbox_i32: Op::unbox_i32(),
            op_unbox_i16: Op::unbox_i16(),
            op_unbox_i8: Op::unbox_i8(),
            op_unbox_u32: Op::unbox_u32(),
            op_unbox_u16: Op::unbox_u16(),
            op_unbox_u8: Op::unbox_u8(),
            op_unbox_f64: Op::unbox_f64(),
            op_unbox_true: Op::unbox_true(),
            op_unbox_false: Op::unbox_false(),
            op_unbox_null: Op::unbox_null(),

            // Pesudo nodes
            op_placeholder: Op::placeholder(),

            // Snapshot
            op_snapshot: Op::snapshot(),
            op_restore_cell: Op::restore_cell(),

            // Rval nodes
            op_rv_add: Op::rv_add(),
            op_rv_sub: Op::rv_sub(),
            op_rv_mul: Op::rv_mul(),
            op_rv_div: Op::rv_div(),
            op_rv_mod: Op::rv_mod(),
            op_rv_pow: Op::rv_pow(),

            op_rv_con_str: Op::rv_con_str(),

            op_rv_eq: Op::rv_eq(),
            op_rv_ne: Op::rv_ne(),
            op_rv_gt: Op::rv_gt(),
            op_rv_ge: Op::rv_ge(),
            op_rv_lt: Op::rv_lt(),
            op_rv_le: Op::rv_le(),

            op_rv_not: Op::rv_not(),
            op_rv_neg: Op::rv_neg(),

            op_rv_and: Op::rv_and(),
            op_rv_or: Op::rv_or(),

            op_rv_load_int: Op::rv_load_int(),
            op_rv_load_real: Op::rv_load_real(),
            op_rv_load_string: Op::rv_load_string(),
            op_rv_load_null: Op::rv_load_null(),
            op_rv_load_true: Op::rv_load_true(),
            op_rv_load_false: Op::rv_load_false(),
            op_rv_load_function: Op::rv_load_function(),

            op_rv_list_create: Op::rv_list_create(),
            op_rv_list_add: Op::rv_list_add(),
            op_rv_object_create: Op::rv_object_create(),
            op_rv_object_add: Op::rv_object_add(),

            op_rv_iterator_new: Op::rv_iterator_new(),
            op_rv_iterator_has: Op::rv_iterator_has(),
            op_rv_iterator_next: Op::rv_iterator_next(),
            op_rv_iterator_value: Op::rv_iterator_value(),

            op_rv_load_global: Op::rv_load_global(),
            op_rv_set_global: Op::rv_set_global(),

            op_rv_load_upvalue: Op::rv_load_upvalue(),
            op_rv_set_upvalue: Op::rv_set_upvalue(),

            op_rv_mem_index_load: Op::rv_mem_index_load(),
            op_rv_mem_index_store: Op::rv_mem_index_store(),
            op_rv_mem_dot_load: Op::rv_mem_dot_load(),
            op_rv_mem_dot_store: Op::rv_mem_dot_store(),

            op_rv_assert1: Op::rv_assert1(),
            op_rv_assert2: Op::rv_assert2(),
            op_rv_trace: Op::rv_trace(),
            op_rv_typeof: Op::rv_typeof(),
            op_rv_sizeof: Op::rv_sizeof(),
            op_rv_halt: Op::rv_halt(),

            op_rv_to_string: Op::rv_to_string(),
            op_rv_to_boolean: Op::rv_to_boolean(),
            op_rv_phi: Op::rv_phi(),

            // CFG
            op_cfg_start: Op::cfg_start(),
            op_cfg_end: Op::cfg_end(),
            op_cfg_merge_return: Op::cfg_merge_return(),
            op_cfg_merge_halt: Op::cfg_merge_halt(),
            op_cfg_halt: Op::cfg_halt(),
            op_cfg_return: Op::cfg_return(),
            op_cfg_if_cmp: Op::cfg_if_cmp(),
            op_cfg_jump: Op::cfg_jump(),
            op_cfg_loop_back: Op::cfg_loop_back(),
        }));
    }

    ///// -------------------------------------------------------------------
    ///// Private methods for internal usage
    ///// -------------------------------------------------------------------
    fn node_next_id(&mut self) -> Nid {
        let nid = self.node_id;
        self.node_id += 1;
        return nid;
    }

    fn watch_node(&mut self, x: &Nref) {
        let xx = Rc::downgrade(x);
        self.gc_list.push(xx);
    }

    // maximum mpool node size currently have
    pub fn max_node_id(&self) -> Nid {
        return self.node_id;
    }

    ///// -------------------------------------------------------------------
    ///// Factory method for Nref(node)
    ///// -------------------------------------------------------------------
    pub fn new_imm_index(&mut self, index: u32, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_index),
            self.node_next_id(),
            bcpos,
            Imm::Index(index),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_u32(&mut self, v: u32, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_u32),
            self.node_next_id(),
            bcpos,
            Imm::ImmU32(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_u16(&mut self, v: u16, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_u16),
            self.node_next_id(),
            bcpos,
            Imm::ImmU16(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_u8(&mut self, v: u8, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_u8),
            self.node_next_id(),
            bcpos,
            Imm::ImmU8(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i64(&mut self, v: i64, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i64),
            self.node_next_id(),
            bcpos,
            Imm::ImmI64(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i32(&mut self, v: i32, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i32),
            self.node_next_id(),
            bcpos,
            Imm::ImmI32(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i16(&mut self, v: i16, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i16),
            self.node_next_id(),
            bcpos,
            Imm::ImmI16(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i8(&mut self, v: i8, bcpos: u32) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i8),
            self.node_next_id(),
            bcpos,
            Imm::ImmI8(v),
        );
        self.watch_node(&x);
        return x;
    }

    pub fn new_rv_node_unary(&mut self, op: Oref, v: Nref, bcpos: u32) -> Nref {
        let x = Node::new_unary(op, self.node_next_id(), bcpos, v);
        self.watch_node(&x);
        return x;
    }

    pub fn new_rv_node_binary(
        &mut self,
        op: Oref,
        lhs: Nref,
        rhs: Nref,
        bcpos: u32,
    ) -> Nref {
        let x = Node::new_binary(op, self.node_next_id(), bcpos, lhs, rhs);
        self.watch_node(&x);
        return x;
    }

    pub fn new_rv_add(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_add),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_sub(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_sub),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_mul(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_mul),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_div(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_div),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_mod(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_mod),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_pow(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_pow),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_eq(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_eq),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_ne(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_ne),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_le(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_le),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_lt(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_lt),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_ge(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_ge),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_gt(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_gt),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_and(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_and),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_or(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_or),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_con_str(&mut self, lhs: Nref, rhs: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_con_str),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_not(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_unary(Oref::clone(&self.op_rv_not), v, bcpos);
    }
    pub fn new_rv_neg(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_unary(Oref::clone(&self.op_rv_neg), v, bcpos);
    }
    pub fn new_rv_to_string(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_unary(
            Oref::clone(&self.op_rv_to_string),
            v,
            bcpos,
        );
    }
    pub fn new_rv_to_boolean(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_node_unary(
            Oref::clone(&self.op_rv_to_boolean),
            v,
            bcpos,
        );
    }
    fn new_rv_load(&mut self, op: Oref, idx: Nref, bcid: u32) -> Nref {
        let mut node = Node::new(op, self.node_next_id(), bcid);
        Node::add_value(&mut node, idx);
        self.watch_node(&node);
        return node;
    }
    fn new_rv_load_none(&mut self, op: Oref, bcid: u32) -> Nref {
        let node = Node::new(op, self.node_next_id(), bcid);
        self.watch_node(&node);
        return node;
    }

    pub fn new_rv_load_int(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_int), v, bcpos);
    }

    pub fn new_rv_load_real(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_real), v, bcpos);
    }

    pub fn new_rv_load_string(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_string), v, bcpos);
    }

    pub fn new_rv_load_function(&mut self, v: Nref, bcpos: u32) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_string), v, bcpos);
    }

    pub fn new_rv_load_null(&mut self, bcpos: u32) -> Nref {
        return self.new_rv_load_none(Oref::clone(&self.op_rv_load_null), bcpos);
    }

    pub fn new_rv_load_true(&mut self, bcpos: u32) -> Nref {
        return self.new_rv_load_none(Oref::clone(&self.op_rv_load_true), bcpos);
    }

    pub fn new_rv_load_false(&mut self, bcpos: u32) -> Nref {
        return self
            .new_rv_load_none(Oref::clone(&self.op_rv_load_false), bcpos);
    }

    // -------------------------------------------------------------------------
    // list creation
    pub fn new_rv_list_create(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_list_create),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_list_add(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_list_add),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // object creation
    pub fn new_rv_object_create(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_object_create),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_object_add(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_object_add),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // iterators
    pub fn new_rv_iterator_new(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_new),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_iterator_has(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_has),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_iterator_next(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_next),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_iterator_value(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_value),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Globals
    pub fn new_rv_load_global(&mut self, idx: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_load_global),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, idx);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_set_global(
        &mut self,
        idx: Nref,
        value: Nref,
        bcpos: u32,
    ) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_set_global),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, idx);
        Node::add_value(&mut n, value);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Memory
    pub fn new_dot_access(&mut self, recv: Nref, idx: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_mem_dot_load),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, recv);
        Node::add_value(&mut n, idx);
        self.watch_node(&n);
        return n;
    }
    pub fn new_dot_store(
        &mut self,
        recv: Nref,
        idx: Nref,
        val: Nref,
        bcpos: u32,
    ) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_mem_dot_store),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, recv);
        Node::add_value(&mut n, idx);
        Node::add_value(&mut n, val);
        self.watch_node(&n);
        return n;
    }

    pub fn new_index_load(&mut self, recv: Nref, idx: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_mem_index_load),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, recv);
        Node::add_value(&mut n, idx);
        self.watch_node(&n);
        return n;
    }
    pub fn new_index_store(
        &mut self,
        recv: Nref,
        idx: Nref,
        val: Nref,
        bcpos: u32,
    ) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_mem_index_store),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, recv);
        Node::add_value(&mut n, idx);
        Node::add_value(&mut n, val);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Upvalue
    pub fn new_rv_load_upvalue(&mut self, index: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_load_upvalue),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, index);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_set_upvalue(
        &mut self,
        index: Nref,
        value: Nref,
        bcpos: u32,
    ) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_set_upvalue),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, index);
        Node::add_value(&mut n, value);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Builtin
    pub fn new_rv_assert1(&mut self, cond: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_assert1),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, cond);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_assert2(&mut self, cond: Nref, msg: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_assert2),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, cond);
        Node::add_value(&mut n, msg);
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_typeof(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_typeof),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_sizeof(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_sizeof),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_trace(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_trace),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_halt(&mut self, v: Nref, bcpos: u32) -> Nref {
        let mut n =
            Node::new(Oref::clone(&self.op_rv_halt), self.node_next_id(), bcpos);
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Phi
    pub fn new_rv_phi(&mut self, bcpos: u32) -> Nref {
        let n =
            Node::new(Oref::clone(&self.op_rv_phi), self.node_next_id(), bcpos);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Pseudo
    pub fn new_placeholder(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_placeholder),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Pseudo
    pub fn new_restore_cell(
        &mut self,
        idx: Nref,
        val: Nref,
        bcpos: u32,
    ) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_restore_cell),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, idx);
        Node::add_value(&mut n, val);
        self.watch_node(&n);
        return n;
    }
    pub fn new_snapshot(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_snapshot),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //                          Control Flow Graph
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    pub fn new_cfg_start(&mut self) -> Nref {
        let n =
            Node::new(Oref::clone(&self.op_cfg_start), self.node_next_id(), 0);
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_end(&mut self) -> Nref {
        let n = Node::new(Oref::clone(&self.op_cfg_end), self.node_next_id(), 0);
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_merge_return(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_merge_return),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_merge_halt(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_merge_halt),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_halt(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_halt),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_return(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_return),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_if_cmp(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_if_cmp),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_jump(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_jump),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_loop_back(&mut self, bcpos: u32) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_loop_back),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }
}
