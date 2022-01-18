use crate::ic::ftype::*;
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

fn find_val(x: &ValueList, y: &Nref) -> Option<usize> {
    x.iter().position(|x| Nref::ptr_eq(x, y))
}

#[derive(Clone)]
pub enum DefUse {
    Value(WkNref),
    Control(WkNref),
    Effect(WkNref),
}

pub type DefUseList = Vec<DefUse>;

#[derive(Clone, Debug)]
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
    ImmBool(bool),
    ImmNull,

    // duplicate the string from the string table
    ImmString(String),

    Invalid,
}

// Type information for the node. Each node should have a type information, and
// the type information will be used later on during the optimization pass to
// enable certain optimization

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum MainType {
    // High level types
    Int,
    Real,
    Boolean,
    Null,
    Str,
    List,
    Object,
    Function,
    NFunction,
    Iter,

    Unknown,
}

pub fn ftype_map_main_type(x: &FType) -> MainType {
    return match x {
        FType::Int => MainType::Int,
        FType::Real => MainType::Real,
        FType::Boolean => MainType::Boolean,
        FType::Null => MainType::Null,
        FType::Str => MainType::Str,
        FType::List => MainType::List,
        FType::Object => MainType::Object,
        FType::Function => MainType::Function,
        FType::NFunction => MainType::NFunction,
        FType::Iter => MainType::Iter,
        FType::Unknown => MainType::Unknown,
    };
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum SubType {
    // Integer sub type
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,

    // Float sub type
    F64,

    // Not available
    Invalid,
}

#[derive(Clone, Debug)]
pub struct JType {
    pub main: MainType,
    pub sub: SubType,
}

// Bytecode context, used to record the bytecode related information, ie for
// generating the deoptimize stub etc ...
#[derive(Clone, Debug)]
pub struct BcCtx {
    pub bc: u32,
    pub frame: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuiltinCall {
    // arithmetic
    ArithAdd,
    ArithSub,
    ArithMul,
    ArithDiv,
    ArithMod,
    ArithPow,

    // comparison
    ArithEq,
    ArithNe,
    ArithLt,
    ArithLe,
    ArithGt,
    ArithGe,

    // unary
    UnaToString,
    UnaToBoolean,
    UnaNot,
    UnaNeg,
    // string cons
}

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
    pub bc: BcCtx,

    // -----------------------------------------------------------------------
    // Other information
    pub alias: Option<Aref>,

    // -----------------------------------------------------------------------
    // Type information
    //
    // the the_type indicates the original type been emitted by the instruction
    // since some instruction is already typped. type_hint is the type that is
    // been inferred from the typper, they are not stable nor for sure. It can
    // be used as a way to allow speculative type inference.
    pub the_type: JType,
    pub type_hint: MainType,

    // Whether the node is been treated as dead or not
    pub dead: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpTier {
    Imm,         // immediate numbers, ie constant
    Placeholder, // dummy entry
    Cfg,         // control flow node
    Snapshot,    // Checkpoint
    Pseudo, // man made node, ie DeoptEntry etc ..., they will be materialized
    // back to lower structure or been removed entirely
    Phi,
    Bval,  // box/unbox operations, we will have special phase to lower them
    Guard, // guard operations
    Rval,  // high level operations, ie Rval(standsfor Rust Value, boxed)
    Mid,   // middle tier IR, standsfor none-architecture specific instructions
    Arch,  // arch tier IR, closely related to the target assembly language
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Opcode {
    // Pseudo nodes

    // placeholder node, used to mark that the value is not materialized yet
    Placeholder,

    // used as a placeholder for building loop IV
    LoopIVPlaceholder,

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
    ImmBool,
    ImmNull,
    ImmString,

    // -------------------------------------------------------------------------
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

    BoxBoolean,
    BoxTrue,
    BoxFalse,

    BoxNull,

    BoxPtr,
    BoxStr,
    BoxList,
    BoxObject,
    BoxIter,
    BoxFunction,
    BoxNFunction,

    // UnboxXXX operation, take a box value and unbox value back to the machine
    // type represented by the instruction itself. Notes, if the boxed value has
    // a different type, then the behavior is undefined, these operation doesn't
    // do check. If check is needed, perform with CheckUnboxXXX instruction is
    // preferred, which will check the type of the value and deoptimize if the
    // instruction failed
    UnboxI64,
    UnboxF64,
    UnboxBoolean,
    UnboxNull,

    // Heap
    UnboxPtr,
    UnboxStr,
    UnboxList,
    UnboxObject,
    UnboxIter,
    UnboxFunction,
    UnboxNFunction,

    // box operation, used for box lowering
    BoxLoadTypeI64,

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //                             Guard
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //
    // Guard, used to generate assertion during the runtime. Most guard are just
    // type guard, which assert a value's type and bailout from optimization if
    // guard assertion failed. Apart from the current type guard, we can
    // optionally have other guard special guard for certain usage, like:
    //
    //    guard_not_zero
    //    guard_false
    //    guard_true
    //
    //  These value guards are used to emit certain usage during lowering. A
    //  guard is essentially a If tests + a trap node. The trap node will be
    //  just jump to the related deoptimization point for bailing out from
    //  JIT frame back to interp frame
    //
    // 1. Type Guards
    GuardInt, // notes we can only guard a main type, sub type is unknown since
    // boxing value doesn't generate detail sub type for now
    GuardReal,
    GuardBoolean,
    GuardNull,

    // Heap object
    GuardHeap,

    // All heap allocated object
    GuardStr,
    GuardList,
    GuardObject,
    GuardFunction,
    GuardNFunction,
    GuardIter,

    // 2. Value guard
    GuardTrue,
    GuardFalse,
    GuardI64NotZero,

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //                               Phis
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //
    // high level, dealing with boxing value and all the script semantic, all
    // the instruction prefixed with Rv, stands for Rust value, which means the
    // boxed value.

    // Rv phis , high level PHIS, ie marshal 2 boxed value,
    RvPhi,

    // Represent the function input arguments,
    RvParam,

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //                       Rv instruction, ie HIR
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    //
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

    RvToString,
    RvToBoolean,

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

    // Indexing/Dot, memory operation, generates side effect and should be used
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

    // Invoke
    RvCall,

    // =========================================================================
    // Mid tier instructions
    //
    //   which is static typed and also directly handle unboxed value. Any
    //   instruction requires the operand to be explicit unboxed via Unbox value
    //   and mostly must be guarded with type guards
    //
    //
    // For simplicity, these instructions are not prefixed with any naming tag
    //
    // =========================================================================

    // Typed arithmetic
    I64Add,
    F64Add,

    I64Sub,
    F64Sub,

    I64Mul,
    F64Mul,

    I64Div,
    F64Div,

    I64Mod,
    F64Mod,

    I64Pow,
    F64Pow,

    // Typed comparison
    I64Eq,
    I64Ne,
    I64Lt,
    I64Le,
    I64Gt,
    I64Ge,

    F64Eq,
    F64Ne,
    F64Lt,
    F64Le,
    F64Gt,
    F64Ge,

    // Pointer comparison, required the previous unbox result in pointer types
    //
    //   UnboxStr
    //   UnboxList
    //   UnboxObject
    //   UnboxFunction
    //   UnboxNFunction
    //   UnboxIter
    //
    PtrEq,
    PtrNe,

    // Boolean type, only worked here for boolean types, other types are not
    // allowed here.
    BooleanEq,
    BooleanNe,

    // Just testify whether the lhs/rhs are both null or not
    NullEq,
    NullNe,

    // String comparison types, specifically designed for string's
    //  They can provide extra lower hint when compiler find it is capable o
    //  performing fancy optimization internally
    StrEq,
    StrNe,
    StrLt,
    StrLe,
    StrGt,
    StrGe,

    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    // Unary helpers
    // -------------------------------------------------------------------------
    // -------------------------------------------------------------------------
    // ToBoolean
    I64ToBoolean,
    F64ToBoolean,
    StrToBoolean,
    ListToBoolean,
    ObjectToBoolean,
    IterToBoolean,

    // Function/NFunction all goes to false, Null flase, boolean is the value
    // itself.
    // Not operator
    I64Negate,
    F64Negate,

    FlipBoolean,

    // Builtin function calls, accepts a builtin call numbers and then use a addr
    // book to perform the call. The builtin call are just normal calls
    CallBuiltin0,
    CallBuiltin1,
    CallBuiltin2,
    CallBuiltin3,
    CallBuiltin4,

    // -------------------------------------------------------------------------
    // Control Flow Graph Node
    // -------------------------------------------------------------------------
    CfgStart,
    CfgEnd,

    CfgMergeReturn,
    CfgMergeHalt,

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
    op_imm_bool: Oref,
    op_imm_null: Oref,
    op_imm_string: Oref,

    op_box_i64: Oref,
    op_box_i32: Oref,
    op_box_i16: Oref,
    op_box_i8: Oref,
    op_box_u32: Oref,
    op_box_u16: Oref,
    op_box_u8: Oref,
    op_box_f64: Oref,

    op_box_boolean: Oref,
    op_box_true: Oref,
    op_box_false: Oref,

    op_box_null: Oref,
    op_box_str: Oref,
    op_box_list: Oref,
    op_box_object: Oref,
    op_box_iter: Oref,
    op_box_function: Oref,
    op_box_nfunction: Oref,

    op_unbox_i64: Oref,
    op_unbox_f64: Oref,
    op_unbox_boolean: Oref,
    op_unbox_null: Oref,

    op_unbox_str: Oref,
    op_unbox_list: Oref,
    op_unbox_object: Oref,
    op_unbox_iter: Oref,
    op_unbox_function: Oref,
    op_unbox_nfunction: Oref,

    // boxing operation
    op_box_load_type_i64: Oref, // loading the type flag into i64 value

    // (((((((((((((((((((((( GUARD ))))))))))))))))))))))
    op_guard_int: Oref,
    op_guard_real: Oref,
    op_guard_boolean: Oref,
    op_guard_null: Oref,

    // heap object
    op_guard_heap: Oref,
    op_guard_str: Oref,
    op_guard_list: Oref,
    op_guard_object: Oref,
    op_guard_function: Oref,
    op_guard_nfunction: Oref,
    op_guard_iter: Oref,

    // value guards
    op_guard_true: Oref,
    op_guard_false: Oref,
    op_guard_i64_not_zero: Oref,

    // (((((((((((((((((((((( PLACEHOLDER ))))))))))))))))))))))
    op_placeholder: Oref,
    op_loop_iv_placeholder: Oref,

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
    op_rv_param: Oref,

    // -------------------------------------------------------------------------
    // (((((((((((((((((((((( Mid Tier ))))))))))))))))))))))
    op_i64_add: Oref,
    op_i64_sub: Oref,
    op_i64_mul: Oref,
    op_i64_div: Oref,
    op_i64_mod: Oref,
    op_i64_pow: Oref,

    op_f64_add: Oref,
    op_f64_sub: Oref,
    op_f64_mul: Oref,
    op_f64_div: Oref,
    op_f64_mod: Oref,
    op_f64_pow: Oref,

    op_i64_eq: Oref,
    op_i64_ne: Oref,
    op_i64_lt: Oref,
    op_i64_le: Oref,
    op_i64_gt: Oref,
    op_i64_ge: Oref,

    op_f64_eq: Oref,
    op_f64_ne: Oref,
    op_f64_lt: Oref,
    op_f64_le: Oref,
    op_f64_gt: Oref,
    op_f64_ge: Oref,

    op_ptr_eq: Oref,
    op_ptr_ne: Oref,

    op_boolean_eq: Oref,
    op_boolean_ne: Oref,

    op_str_eq: Oref,
    op_str_ne: Oref,
    op_str_le: Oref,
    op_str_lt: Oref,
    op_str_ge: Oref,
    op_str_gt: Oref,

    op_i64_to_boolean: Oref,
    op_f64_to_boolean: Oref,
    op_str_to_boolean: Oref,
    op_list_to_boolean: Oref,
    op_object_to_boolean: Oref,
    op_iter_to_boolean: Oref,

    op_f64_negate: Oref,
    op_i64_negate: Oref,
    op_flip_boolean: Oref,

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

impl JType {
    pub fn new_unknown() -> JType {
        JType {
            main: MainType::Unknown,
            sub: SubType::Invalid,
        }
    }

    pub fn map_ftype(f: &FType) -> JType {
        return match f {
            FType::Int => JType {
                main: MainType::Int,
                sub: SubType::I64,
            },
            FType::Real => JType {
                main: MainType::Real,
                sub: SubType::F64,
            },
            FType::Boolean => JType {
                main: MainType::Boolean,
                sub: SubType::Invalid,
            },
            FType::Null => JType {
                main: MainType::Null,
                sub: SubType::Invalid,
            },
            FType::Str => JType {
                main: MainType::Str,
                sub: SubType::Invalid,
            },
            FType::List => JType {
                main: MainType::List,
                sub: SubType::Invalid,
            },
            FType::Object => JType {
                main: MainType::Object,
                sub: SubType::Invalid,
            },
            FType::Function => JType {
                main: MainType::Function,
                sub: SubType::Invalid,
            },
            FType::NFunction => JType {
                main: MainType::NFunction,
                sub: SubType::Invalid,
            },
            FType::Iter => JType {
                main: MainType::Iter,
                sub: SubType::Invalid,
            },
            FType::Unknown => JType {
                main: MainType::Unknown,
                sub: SubType::Invalid,
            },
        };
    }

    pub fn is_unknown(&self) -> bool {
        return self.main == MainType::Unknown;
    }
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

impl BcCtx {
    pub fn new(bc: u32, fr: u32) -> BcCtx {
        BcCtx { bc: bc, frame: fr }
    }

    pub fn new_main(bc: u32) -> BcCtx {
        BcCtx::new(bc, 0)
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
        debug_assert!(value.borrow().is_value());
        debug_assert!(cfg.borrow().is_cfg());

        Node::add_value(phi, value);
        Node::add_control(phi, cfg);
    }

    fn remove_def_use(x: &mut Nref, what: DefUse) -> bool {
        let wk = match what {
            DefUse::Effect(a) | DefUse::Control(a) | DefUse::Value(a) => a,
        };
        let index = match x.borrow_mut().def_use.iter().position(|x| {
            match x {
                DefUse::Effect(a) | DefUse::Control(a) | DefUse::Value(a) => {
                    return Weak::ptr_eq(a, &wk);
                }
            };
        }) {
            Option::Some(index) => index,
            _ => return false,
        };
        x.borrow_mut().def_use.remove(index);
        return true;
    }

    pub fn find_use_idx(x: &Nref, y: &Nref) -> Option<usize> {
        let wy = Rc::downgrade(y);
        let mut idx = 0;
        for u in x.borrow().def_use.iter() {
            match u {
                DefUse::Effect(a) | DefUse::Control(a) | DefUse::Value(a) => {
                    if Weak::ptr_eq(a, &wy) {
                        return Option::Some(idx);
                    }
                }
            };
            idx += 1;
        }
        return Option::None;
    }

    pub fn find_use(x: &Nref, y: &Nref) -> Option<DefUse> {
        return Option::Some(
            x.borrow().def_use[Node::find_use_idx(x, y)?].clone(),
        );
    }

    pub fn has_use(x: &Nref, y: &Nref) -> bool {
        return Node::find_use(x, y).is_some();
    }

    pub fn remove_value(x: &mut Nref, i: usize) {
        let vv = DefUse::Value(Nref::downgrade(&x.borrow().value[i]));
        Node::remove_def_use(x, vv);

        x.borrow_mut().value.remove(i);
    }

    pub fn remove_control(x: &mut Nref, i: usize) {
        let vv = DefUse::Control(Nref::downgrade(&x.borrow().cfg[i]));
        Node::remove_def_use(x, vv);

        x.borrow_mut().cfg.remove(i);
    }

    pub fn remove_effect(x: &mut Nref, i: usize) {
        let vv = DefUse::Control(Nref::downgrade(&x.borrow().effect[i]));
        Node::remove_def_use(x, vv);

        x.borrow_mut().effect.remove(i);
    }

    pub fn remove_value_node(x: &mut Nref, v: &Nref) {
        let idx = match x.borrow().value.iter().position(|l| Nref::ptr_eq(l, v))
        {
            Option::Some(idx) => idx,
            _ => return,
        };
        Node::remove_value(x, idx);
    }

    pub fn remove_control_node(x: &mut Nref, v: &Nref) {
        let idx = match x.borrow().cfg.iter().position(|l| Nref::ptr_eq(l, v)) {
            Option::Some(idx) => idx,
            _ => return,
        };
        Node::remove_control(x, idx);
    }

    pub fn remove_effect_node(x: &mut Nref, v: &Nref) {
        let idx = match x.borrow().effect.iter().position(|l| Nref::ptr_eq(l, v))
        {
            Option::Some(idx) => idx,
            _ => return,
        };
        Node::remove_effect(x, idx);
    }

    // If Y is been used by X, then we remove Y from X's usage list.
    pub fn remove_use(x: &mut Nref, y: &Nref) -> bool {
        let r = Node::find_use(y, x);
        if r.is_none() {
            return false;
        }
        match r.unwrap() {
            DefUse::Effect(_) => {
                Node::remove_effect_node(x, y);
                return true;
            }
            DefUse::Value(_) => {
                Node::remove_value_node(x, y);
                return true;
            }
            DefUse::Control(_) => {
                Node::remove_control_node(x, y);
                return true;
            }
        };
    }

    // remove all the use of node x. Notes, this doesn't impact x's own value
    pub fn remove_all_use(x: &mut Nref) {
        for u in x.borrow().def_use.iter() {
            match u {
                DefUse::Effect(ptr) => {
                    if let Option::Some(mut tar) = ptr.upgrade() {
                        Node::remove_effect_node(&mut tar, x);
                    }
                }
                DefUse::Value(ptr) => {
                    if let Option::Some(mut tar) = ptr.upgrade() {
                        Node::remove_value_node(&mut tar, x);
                    }
                }
                DefUse::Control(ptr) => {
                    if let Option::Some(mut tar) = ptr.upgrade() {
                        Node::remove_control_node(&mut tar, x);
                    }
                }
            };
        }

        x.borrow_mut().def_use.clear();
    }

    // Replace cur's all usage into tar
    pub fn replace(cur: &mut Nref, tar: Nref) {
        let dup_def_use = cur.borrow().def_use.clone();

        // remove all use that uses cur
        Node::remove_all_use(cur);

        // replay all the cur's use with tar
        for u in dup_def_use.iter() {
            match u {
                DefUse::Effect(ptr) => {
                    if let Option::Some(mut recv) = ptr.upgrade() {
                        Node::add_effect(&mut recv, Nref::clone(&tar));
                    }
                }

                DefUse::Value(ptr) => {
                    if let Option::Some(mut recv) = ptr.upgrade() {
                        Node::add_value(&mut recv, Nref::clone(&tar));
                    }
                }

                DefUse::Control(ptr) => {
                    if let Option::Some(mut recv) = ptr.upgrade() {
                        Node::add_control(&mut recv, Nref::clone(&tar));
                    }
                }
            };
        }
    }

    pub fn replace_and_dispose(x: &mut Nref, y: Nref) {
        Node::replace(x, y);
        x.borrow_mut().mark_dead();
    }

    pub fn remove_phi_value(x: &mut Nref, i: usize) {
        debug_assert!(x.borrow().is_phi());
        Node::remove_value(x, i);
        Node::remove_control(x, i);
    }

    // Replace placeholder node in the value dependency list
    pub fn replace_placeholder_value(x: &mut Nref, value: Nref) -> usize {
        let mut i = 0;
        for mut v in x.borrow_mut().value.iter_mut() {
            if v.borrow().is_placeholder() {
                assert!(Node::remove_def_use(
                    &mut v,
                    DefUse::Value(Nref::downgrade(&Nref::clone(x)))
                ));
                *v = Nref::clone(&value);

                i += 1;
            }
        }
        return i;
    }

    pub fn simplify_phi(x: &mut Nref) -> bool {
        debug_assert!(x.borrow().is_phi());

        // (0) check wether the phi has placeholder, if so just remove the
        //     placeholder
        {
            let len = x.borrow().value.len();
            let mut i = 0;
            let mut death_list = Vec::<usize>::new();

            while i < len {
                if x.borrow_mut().value[i].borrow().is_placeholder() {
                    death_list.push(i);
                }
                i += 1;
            }

            // notes, every removal of value will shrink the position of each
            // value component
            let mut offset = 0;
            for v in death_list.iter() {
                Node::remove_phi_value(x, *v - offset);
                offset += 1;
            }
        }

        // (1) If the current node has become singleton, then just make the
        //     phi to be replaced by its value
        if x.borrow().value.len() == 1 {
            debug_assert!(x.borrow().cfg.len() == 1);
            let v = x.borrow().value[0].clone();
            Node::replace(x, v);
            return true;
        }
        return false;
    }

    // -----------------------------------------------------------------------
    // Factory Method
    // -----------------------------------------------------------------------
    fn new(op: Oref, id: Nid, bcid: BcCtx) -> Nref {
        return Nref::new(RefCell::new(Node {
            op: op,
            value: ValueList::new(),
            cfg: ValueList::new(),
            effect: ValueList::new(),
            def_use: DefUseList::new(),
            imm: Imm::Invalid,
            id: id,
            bc: bcid,
            alias: Option::None,
            the_type: JType::new_unknown(),
            type_hint: MainType::Unknown,
            dead: false,
        }));
    }

    fn new_imm(op: Oref, id: Nid, bcid: BcCtx, imm: Imm) -> Nref {
        return Nref::new(RefCell::new(Node {
            op: op,
            value: ValueList::new(),
            cfg: ValueList::new(),
            effect: ValueList::new(),
            def_use: DefUseList::new(),
            imm: imm,
            id: id,
            bc: bcid,
            alias: Option::None,
            the_type: JType::new_unknown(),
            type_hint: MainType::Unknown,
            dead: false,
        }));
    }

    fn new_unary(op: Oref, id: Nid, bcid: BcCtx, v0: Nref) -> Nref {
        let mut n = Node::new(op, id, bcid);
        Node::add_value(&mut n, v0);
        return n;
    }

    fn new_binary(op: Oref, id: Nid, bcid: BcCtx, lhs: Nref, rhs: Nref) -> Nref {
        let mut n = Node::new(op, id, bcid);
        Node::add_value(&mut n, lhs);
        Node::add_value(&mut n, rhs);
        return n;
    }

    fn new_ternary(
        op: Oref,
        id: Nid,
        bcid: BcCtx,
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

    pub fn lhs(&self) -> Nref {
        debug_assert!(self.is_value_binary());
        return self.value[0].clone();
    }

    pub fn rhs(&self) -> Nref {
        debug_assert!(self.is_value_binary());
        return self.value[1].clone();
    }

    pub fn una(&self) -> Nref {
        debug_assert!(self.is_value_unary());
        return self.value[0].clone();
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

    pub fn is_value_binary(&self) -> bool {
        return self.is_value() && self.value.len() == 2;
    }
    pub fn is_value_unary(&self) -> bool {
        return self.is_value() && self.value.len() == 1;
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
    pub fn is_placeholder(&self) -> bool {
        return self.op.tier == OpTier::Placeholder;
    }

    // loop iv placeholder can only be inserted into the value slot
    pub fn has_loop_iv_placeholder(&self) -> bool {
        for x in self.value.iter() {
            if x.borrow().op.op == Opcode::LoopIVPlaceholder {
                return true;
            }
        }
        return false;
    }

    pub fn print(&self) -> String {
        return format!(
            "{:?}[{}][{}.{}]({:?}) => {}:{}:{}",
            self.op.op,
            self.id,
            self.bc.frame,
            self.bc.bc,
            self.imm,
            self.value.len(),
            self.cfg.len(),
            self.effect.len()
        );
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
            || x.tier == OpTier::Guard
            || x.tier == OpTier::Phi
            || x.tier == OpTier::Rval
            || x.tier == OpTier::Mid
            || x.tier == OpTier::Arch
            || x.tier == OpTier::Imm
            || x.tier == OpTier::Placeholder;
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
    fn imm_bool() -> Oref {
        Oref::new(Op::new_imm("imm_bool", Opcode::ImmBool))
    }
    fn imm_null() -> Oref {
        Oref::new(Op::new_imm("imm_null", Opcode::ImmNull))
    }
    fn imm_string() -> Oref {
        Oref::new(Op::new_imm("imm_string", Opcode::ImmString))
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

    fn box_boolean() -> Oref {
        Oref::new(Op::new_unary(
            "box_boolean",
            OpTier::Bval,
            Opcode::BoxBoolean,
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

    fn box_str() -> Oref {
        Oref::new(Op::new_unary(
            "box_str",
            OpTier::Bval,
            Opcode::BoxStr,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_list() -> Oref {
        Oref::new(Op::new_unary(
            "box_list",
            OpTier::Bval,
            Opcode::BoxList,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_object() -> Oref {
        Oref::new(Op::new_unary(
            "box_object",
            OpTier::Bval,
            Opcode::BoxObject,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_iter() -> Oref {
        Oref::new(Op::new_unary(
            "box_iter",
            OpTier::Bval,
            Opcode::BoxIter,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_function() -> Oref {
        Oref::new(Op::new_unary(
            "box_function",
            OpTier::Bval,
            Opcode::BoxFunction,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_nfunction() -> Oref {
        Oref::new(Op::new_unary(
            "box_nfunction",
            OpTier::Bval,
            Opcode::BoxNFunction,
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

    fn unbox_boolean() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_boolean",
            OpTier::Bval,
            Opcode::UnboxBoolean,
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

    fn unbox_str() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_str",
            OpTier::Bval,
            Opcode::UnboxStr,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_list() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_list",
            OpTier::Bval,
            Opcode::UnboxList,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_object() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_object",
            OpTier::Bval,
            Opcode::UnboxObject,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_iter() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_iter",
            OpTier::Bval,
            Opcode::UnboxIter,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_function() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_function",
            OpTier::Bval,
            Opcode::UnboxFunction,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn unbox_nfunction() -> Oref {
        Oref::new(Op::new_unary(
            "unbox_nfunction",
            OpTier::Bval,
            Opcode::UnboxNFunction,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn box_load_type_i64() -> Oref {
        Oref::new(Op::new_unary(
            "box_load_type_i64",
            OpTier::Bval,
            Opcode::BoxLoadTypeI64,
            false,
            false,
            MachineFlag {},
        ))
    }

    // =============================================================
    // (((Guard)))
    // =============================================================
    fn guard_int() -> Oref {
        Oref::new(Op::new_unary(
            "guard_int",
            OpTier::Guard,
            Opcode::GuardInt,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_real() -> Oref {
        Oref::new(Op::new_unary(
            "guard_real",
            OpTier::Guard,
            Opcode::GuardReal,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_boolean() -> Oref {
        Oref::new(Op::new_unary(
            "guard_boolean",
            OpTier::Guard,
            Opcode::GuardBoolean,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_null() -> Oref {
        Oref::new(Op::new_unary(
            "guard_null",
            OpTier::Guard,
            Opcode::GuardNull,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_heap() -> Oref {
        Oref::new(Op::new_unary(
            "guard_heap",
            OpTier::Guard,
            Opcode::GuardHeap,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_str() -> Oref {
        Oref::new(Op::new_unary(
            "guard_str",
            OpTier::Guard,
            Opcode::GuardStr,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_list() -> Oref {
        Oref::new(Op::new_unary(
            "guard_list",
            OpTier::Guard,
            Opcode::GuardList,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_object() -> Oref {
        Oref::new(Op::new_unary(
            "guard_object",
            OpTier::Guard,
            Opcode::GuardObject,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_function() -> Oref {
        Oref::new(Op::new_unary(
            "guard_function",
            OpTier::Guard,
            Opcode::GuardFunction,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_nfunction() -> Oref {
        Oref::new(Op::new_unary(
            "guard_nfunction",
            OpTier::Guard,
            Opcode::GuardNFunction,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_iter() -> Oref {
        Oref::new(Op::new_unary(
            "guard_iter",
            OpTier::Guard,
            Opcode::GuardIter,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_true() -> Oref {
        Oref::new(Op::new_unary(
            "guard_true",
            OpTier::Guard,
            Opcode::GuardTrue,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_false() -> Oref {
        Oref::new(Op::new_unary(
            "guard_false",
            OpTier::Guard,
            Opcode::GuardFalse,
            false,
            false,
            MachineFlag {},
        ))
    }

    fn guard_i64_not_zero() -> Oref {
        Oref::new(Op::new_unary(
            "guard_i64_not_zero",
            OpTier::Guard,
            Opcode::GuardI64NotZero,
            false,
            false,
            MachineFlag {},
        ))
    }

    // =============================================================
    // (((Placeholder)))
    // =============================================================
    fn placeholder() -> Oref {
        Oref::new(Op::new(
            "placeholder",
            OpTier::Placeholder,
            Opcode::Placeholder,
            false,
            false,
            ParamLimits::Any,
            MachineFlag {},
        ))
    }

    fn loop_iv_placeholder() -> Oref {
        Oref::new(Op::new(
            "loop_iv_placeholder",
            OpTier::Placeholder,
            Opcode::LoopIVPlaceholder,
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

    fn rv_param() -> Oref {
        Oref::new(Op::new(
            "rv_param",
            OpTier::Rval,
            Opcode::RvParam,
            false,
            false,
            ParamLimits::Limit(1),
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
    fn i64_add() -> Oref {
        Oref::new(Op::new(
            "i64_add",
            OpTier::Mid,
            Opcode::I64Add,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_sub() -> Oref {
        Oref::new(Op::new(
            "i64_sub",
            OpTier::Mid,
            Opcode::I64Sub,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_mul() -> Oref {
        Oref::new(Op::new(
            "i64_sub",
            OpTier::Mid,
            Opcode::I64Mul,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_div() -> Oref {
        Oref::new(Op::new(
            "i64_div",
            OpTier::Mid,
            Opcode::I64Div,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_pow() -> Oref {
        Oref::new(Op::new(
            "i64_pow",
            OpTier::Mid,
            Opcode::I64Pow,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_mod() -> Oref {
        Oref::new(Op::new(
            "i64_mod",
            OpTier::Mid,
            Opcode::I64Mod,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_add() -> Oref {
        Oref::new(Op::new(
            "f64_add",
            OpTier::Mid,
            Opcode::F64Add,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_sub() -> Oref {
        Oref::new(Op::new(
            "f64_sub",
            OpTier::Mid,
            Opcode::F64Sub,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_mul() -> Oref {
        Oref::new(Op::new(
            "f64_sub",
            OpTier::Mid,
            Opcode::F64Mul,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_div() -> Oref {
        Oref::new(Op::new(
            "f64_div",
            OpTier::Mid,
            Opcode::F64Div,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_pow() -> Oref {
        Oref::new(Op::new(
            "f64_pow",
            OpTier::Mid,
            Opcode::F64Pow,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_mod() -> Oref {
        Oref::new(Op::new(
            "f64_mod",
            OpTier::Mid,
            Opcode::F64Mod,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_eq() -> Oref {
        Oref::new(Op::new(
            "i64_eq",
            OpTier::Mid,
            Opcode::I64Eq,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_ne() -> Oref {
        Oref::new(Op::new(
            "i64_ne",
            OpTier::Mid,
            Opcode::I64Ne,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_lt() -> Oref {
        Oref::new(Op::new(
            "i64_lt",
            OpTier::Mid,
            Opcode::I64Lt,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_le() -> Oref {
        Oref::new(Op::new(
            "i64_le",
            OpTier::Mid,
            Opcode::I64Le,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_gt() -> Oref {
        Oref::new(Op::new(
            "i64_gt",
            OpTier::Mid,
            Opcode::I64Gt,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_ge() -> Oref {
        Oref::new(Op::new(
            "i64_ge",
            OpTier::Mid,
            Opcode::I64Ge,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_eq() -> Oref {
        Oref::new(Op::new(
            "f64_eq",
            OpTier::Mid,
            Opcode::F64Eq,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_ne() -> Oref {
        Oref::new(Op::new(
            "f64_ne",
            OpTier::Mid,
            Opcode::F64Ne,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_lt() -> Oref {
        Oref::new(Op::new(
            "f64_lt",
            OpTier::Mid,
            Opcode::F64Lt,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_le() -> Oref {
        Oref::new(Op::new(
            "f64_le",
            OpTier::Mid,
            Opcode::F64Le,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_gt() -> Oref {
        Oref::new(Op::new(
            "f64_gt",
            OpTier::Mid,
            Opcode::F64Gt,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_ge() -> Oref {
        Oref::new(Op::new(
            "f64_ge",
            OpTier::Mid,
            Opcode::F64Ge,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_eq() -> Oref {
        Oref::new(Op::new(
            "str_eq",
            OpTier::Mid,
            Opcode::StrEq,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_ne() -> Oref {
        Oref::new(Op::new(
            "str_ne",
            OpTier::Mid,
            Opcode::StrNe,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_lt() -> Oref {
        Oref::new(Op::new(
            "str_lt",
            OpTier::Mid,
            Opcode::StrLt,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_le() -> Oref {
        Oref::new(Op::new(
            "str_le",
            OpTier::Mid,
            Opcode::StrLe,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_gt() -> Oref {
        Oref::new(Op::new(
            "str_gt",
            OpTier::Mid,
            Opcode::StrGt,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_ge() -> Oref {
        Oref::new(Op::new(
            "str_ge",
            OpTier::Mid,
            Opcode::StrGe,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn ptr_eq() -> Oref {
        Oref::new(Op::new(
            "ptr_eq",
            OpTier::Mid,
            Opcode::PtrEq,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn ptr_ne() -> Oref {
        Oref::new(Op::new(
            "ptr_ne",
            OpTier::Mid,
            Opcode::PtrNe,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn boolean_eq() -> Oref {
        Oref::new(Op::new(
            "boolean_eq",
            OpTier::Mid,
            Opcode::BooleanEq,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn boolean_ne() -> Oref {
        Oref::new(Op::new(
            "boolean_ne",
            OpTier::Mid,
            Opcode::BooleanNe,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_to_boolean() -> Oref {
        Oref::new(Op::new(
            "i64_to_boolean",
            OpTier::Mid,
            Opcode::I64ToBoolean,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_to_boolean() -> Oref {
        Oref::new(Op::new(
            "f64_to_boolean",
            OpTier::Mid,
            Opcode::F64ToBoolean,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn str_to_boolean() -> Oref {
        Oref::new(Op::new(
            "str_to_boolean",
            OpTier::Mid,
            Opcode::StrToBoolean,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn list_to_boolean() -> Oref {
        Oref::new(Op::new(
            "list_to_boolean",
            OpTier::Mid,
            Opcode::ListToBoolean,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn object_to_boolean() -> Oref {
        Oref::new(Op::new(
            "object_to_boolean",
            OpTier::Mid,
            Opcode::ObjectToBoolean,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn iter_to_boolean() -> Oref {
        Oref::new(Op::new(
            "iter_to_boolean",
            OpTier::Mid,
            Opcode::IterToBoolean,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn f64_negate() -> Oref {
        Oref::new(Op::new(
            "f64_negate",
            OpTier::Mid,
            Opcode::F64Negate,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn i64_negate() -> Oref {
        Oref::new(Op::new(
            "i64_negate",
            OpTier::Mid,
            Opcode::I64Negate,
            false,
            false,
            ParamLimits::Limit(2),
            MachineFlag {},
        ))
    }

    fn flip_boolean() -> Oref {
        Oref::new(Op::new(
            "flip_boolean",
            OpTier::Mid,
            Opcode::FlipBoolean,
            false,
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
            op_imm_bool: Op::imm_bool(),
            op_imm_null: Op::imm_null(),
            op_imm_string: Op::imm_string(),

            op_box_i64: Op::box_i64(),
            op_box_i32: Op::box_i32(),
            op_box_i16: Op::box_i16(),
            op_box_i8: Op::box_i8(),
            op_box_u32: Op::box_u32(),
            op_box_u16: Op::box_u16(),
            op_box_u8: Op::box_u8(),
            op_box_f64: Op::box_f64(),

            op_box_boolean: Op::box_boolean(),
            op_box_true: Op::box_true(),
            op_box_false: Op::box_false(),

            op_box_null: Op::box_null(),

            op_box_str: Op::box_str(),
            op_box_list: Op::box_list(),
            op_box_object: Op::box_object(),
            op_box_iter: Op::box_iter(),
            op_box_function: Op::box_function(),
            op_box_nfunction: Op::box_nfunction(),

            op_unbox_i64: Op::unbox_i64(),
            op_unbox_f64: Op::unbox_f64(),
            op_unbox_boolean: Op::unbox_boolean(),
            op_unbox_null: Op::unbox_null(),

            op_unbox_str: Op::unbox_str(),
            op_unbox_list: Op::unbox_list(),
            op_unbox_object: Op::unbox_object(),
            op_unbox_iter: Op::unbox_iter(),
            op_unbox_function: Op::unbox_function(),
            op_unbox_nfunction: Op::unbox_nfunction(),

            op_box_load_type_i64: Op::box_load_type_i64(),

            // Guard
            op_guard_int: Op::guard_int(),
            op_guard_real: Op::guard_real(),
            op_guard_boolean: Op::guard_boolean(),
            op_guard_null: Op::guard_null(),

            op_guard_heap: Op::guard_heap(),
            op_guard_str: Op::guard_str(),
            op_guard_list: Op::guard_list(),
            op_guard_object: Op::guard_object(),
            op_guard_function: Op::guard_function(),
            op_guard_nfunction: Op::guard_nfunction(),
            op_guard_iter: Op::guard_iter(),

            op_guard_true: Op::guard_true(),
            op_guard_false: Op::guard_false(),
            op_guard_i64_not_zero: Op::guard_i64_not_zero(),

            // Pesudo nodes
            op_placeholder: Op::placeholder(),
            op_loop_iv_placeholder: Op::loop_iv_placeholder(),

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
            op_rv_param: Op::rv_param(),

            // Mid
            op_i64_add: Op::i64_add(),
            op_i64_sub: Op::i64_sub(),
            op_i64_mul: Op::i64_mul(),
            op_i64_div: Op::i64_div(),
            op_i64_mod: Op::i64_mod(),
            op_i64_pow: Op::i64_pow(),

            op_f64_add: Op::f64_add(),
            op_f64_sub: Op::f64_sub(),
            op_f64_mul: Op::f64_mul(),
            op_f64_div: Op::f64_div(),
            op_f64_mod: Op::f64_mod(),
            op_f64_pow: Op::f64_pow(),

            op_i64_eq: Op::i64_eq(),
            op_i64_ne: Op::i64_ne(),
            op_i64_lt: Op::i64_lt(),
            op_i64_le: Op::i64_le(),
            op_i64_gt: Op::i64_gt(),
            op_i64_ge: Op::i64_ge(),

            op_f64_eq: Op::f64_eq(),
            op_f64_ne: Op::f64_ne(),
            op_f64_lt: Op::f64_lt(),
            op_f64_le: Op::f64_le(),
            op_f64_gt: Op::f64_gt(),
            op_f64_ge: Op::f64_ge(),

            op_ptr_eq: Op::ptr_eq(),
            op_ptr_ne: Op::ptr_ne(),

            op_boolean_eq: Op::boolean_eq(),
            op_boolean_ne: Op::boolean_ne(),

            op_str_eq: Op::str_eq(),
            op_str_ne: Op::str_ne(),
            op_str_lt: Op::str_lt(),
            op_str_le: Op::str_le(),
            op_str_gt: Op::str_gt(),
            op_str_ge: Op::str_ge(),

            op_i64_to_boolean: Op::i64_to_boolean(),
            op_f64_to_boolean: Op::f64_to_boolean(),
            op_str_to_boolean: Op::str_to_boolean(),
            op_list_to_boolean: Op::list_to_boolean(),
            op_object_to_boolean: Op::object_to_boolean(),
            op_iter_to_boolean: Op::iter_to_boolean(),

            op_f64_negate: Op::f64_negate(),
            op_i64_negate: Op::i64_negate(),
            op_flip_boolean: Op::flip_boolean(),

            // Control flow graph
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
    pub fn new_imm_index(&mut self, index: u32, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_index),
            self.node_next_id(),
            bcpos,
            Imm::Index(index),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_u32(&mut self, v: u32, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_u32),
            self.node_next_id(),
            bcpos,
            Imm::ImmU32(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_u16(&mut self, v: u16, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_u16),
            self.node_next_id(),
            bcpos,
            Imm::ImmU16(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_u8(&mut self, v: u8, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_u8),
            self.node_next_id(),
            bcpos,
            Imm::ImmU8(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i64(&mut self, v: i64, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i64),
            self.node_next_id(),
            bcpos,
            Imm::ImmI64(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i32(&mut self, v: i32, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i32),
            self.node_next_id(),
            bcpos,
            Imm::ImmI32(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i16(&mut self, v: i16, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i16),
            self.node_next_id(),
            bcpos,
            Imm::ImmI16(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_i8(&mut self, v: i8, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_i8),
            self.node_next_id(),
            bcpos,
            Imm::ImmI8(v),
        );
        self.watch_node(&x);
        return x;
    }
    pub fn new_imm_f64(&mut self, v: f64, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_f64),
            self.node_next_id(),
            bcpos,
            Imm::ImmF64(v),
        );
        self.watch_node(&x);
        return x;
    }

    pub fn new_imm_true(&mut self, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_bool),
            self.node_next_id(),
            bcpos,
            Imm::ImmBool(true),
        );
        self.watch_node(&x);
        return x;
    }

    pub fn new_imm_false(&mut self, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_bool),
            self.node_next_id(),
            bcpos,
            Imm::ImmBool(false),
        );
        self.watch_node(&x);
        return x;
    }

    pub fn new_imm_null(&mut self, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_null),
            self.node_next_id(),
            bcpos,
            Imm::ImmNull,
        );
        self.watch_node(&x);
        return x;
    }

    pub fn new_imm_string(&mut self, s: String, bcpos: BcCtx) -> Nref {
        let x = Node::new_imm(
            Oref::clone(&self.op_imm_string),
            self.node_next_id(),
            bcpos,
            Imm::ImmString(s),
        );
        self.watch_node(&x);
        return x;
    }

    pub fn new_rv_node_unary(
        &mut self,
        op: Oref,
        v: Nref,
        bcpos: BcCtx,
    ) -> Nref {
        let x = Node::new_unary(op, self.node_next_id(), bcpos, v);
        self.watch_node(&x);
        return x;
    }

    pub fn new_rv_node_binary(
        &mut self,
        op: Oref,
        lhs: Nref,
        rhs: Nref,
        bcpos: BcCtx,
    ) -> Nref {
        let x = Node::new_binary(op, self.node_next_id(), bcpos, lhs, rhs);
        self.watch_node(&x);
        return x;
    }

    pub fn new_rv_add(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_add),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_sub(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_sub),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_mul(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_mul),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_div(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_div),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_mod(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_mod),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_pow(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_pow),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_eq(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_eq),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_ne(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_ne),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_le(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_le),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_lt(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_lt),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_ge(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_ge),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_gt(&mut self, lhs: Nref, rhs: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_gt),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_con_str(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bcpos: BcCtx,
    ) -> Nref {
        return self.new_rv_node_binary(
            Oref::clone(&self.op_rv_con_str),
            lhs,
            rhs,
            bcpos,
        );
    }

    pub fn new_rv_not(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_unary(Oref::clone(&self.op_rv_not), v, bcpos);
    }
    pub fn new_rv_neg(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_unary(Oref::clone(&self.op_rv_neg), v, bcpos);
    }
    pub fn new_rv_to_string(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_unary(
            Oref::clone(&self.op_rv_to_string),
            v,
            bcpos,
        );
    }
    pub fn new_rv_to_boolean(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_node_unary(
            Oref::clone(&self.op_rv_to_boolean),
            v,
            bcpos,
        );
    }
    fn new_rv_load(&mut self, op: Oref, idx: Nref, bcid: BcCtx) -> Nref {
        let mut node = Node::new(op, self.node_next_id(), bcid);
        Node::add_value(&mut node, idx);
        self.watch_node(&node);
        return node;
    }
    fn new_rv_load_none(&mut self, op: Oref, bcid: BcCtx) -> Nref {
        let node = Node::new(op, self.node_next_id(), bcid);
        self.watch_node(&node);
        return node;
    }

    pub fn new_rv_load_int(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_int), v, bcpos);
    }

    pub fn new_rv_load_real(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_real), v, bcpos);
    }

    pub fn new_rv_load_string(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_string), v, bcpos);
    }

    pub fn new_rv_load_function(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        return self.new_rv_load(Oref::clone(&self.op_rv_load_string), v, bcpos);
    }

    pub fn new_rv_load_null(&mut self, bcpos: BcCtx) -> Nref {
        return self.new_rv_load_none(Oref::clone(&self.op_rv_load_null), bcpos);
    }

    pub fn new_rv_load_true(&mut self, bcpos: BcCtx) -> Nref {
        return self.new_rv_load_none(Oref::clone(&self.op_rv_load_true), bcpos);
    }

    pub fn new_rv_load_false(&mut self, bcpos: BcCtx) -> Nref {
        return self
            .new_rv_load_none(Oref::clone(&self.op_rv_load_false), bcpos);
    }

    // -------------------------------------------------------------------------
    // list creation
    pub fn new_rv_list_create(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_list_create),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_list_add(&mut self, bcpos: BcCtx) -> Nref {
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
    pub fn new_rv_object_create(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_object_create),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_object_add(&mut self, bcpos: BcCtx) -> Nref {
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
    pub fn new_rv_iterator_new(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_new),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_iterator_has(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_has),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_iterator_next(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_iterator_next),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_iterator_value(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
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
    pub fn new_rv_load_global(&mut self, idx: Nref, bcpos: BcCtx) -> Nref {
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
        bcpos: BcCtx,
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
    pub fn new_dot_access(
        &mut self,
        recv: Nref,
        idx: Nref,
        bcpos: BcCtx,
    ) -> Nref {
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
        bcpos: BcCtx,
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

    pub fn new_index_load(
        &mut self,
        recv: Nref,
        idx: Nref,
        bcpos: BcCtx,
    ) -> Nref {
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
        bcpos: BcCtx,
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
    pub fn new_rv_load_upvalue(&mut self, index: Nref, bcpos: BcCtx) -> Nref {
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
        bcpos: BcCtx,
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
    pub fn new_rv_assert1(&mut self, cond: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_assert1),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, cond);
        self.watch_node(&n);
        return n;
    }
    pub fn new_rv_assert2(
        &mut self,
        cond: Nref,
        msg: Nref,
        bcpos: BcCtx,
    ) -> Nref {
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

    pub fn new_rv_typeof(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_typeof),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_sizeof(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_sizeof),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_trace(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_rv_trace),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_halt(&mut self, v: Nref, bcpos: BcCtx) -> Nref {
        let mut n =
            Node::new(Oref::clone(&self.op_rv_halt), self.node_next_id(), bcpos);
        Node::add_value(&mut n, v);
        self.watch_node(&n);
        return n;
    }

    pub fn new_rv_param(&mut self, idx: Nref, bcpos: BcCtx) -> Nref {
        let mut n = Node::new(
            Oref::clone(&self.op_rv_param),
            self.node_next_id(),
            bcpos,
        );
        Node::add_value(&mut n, idx);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Phi
    pub fn new_rv_phi(&mut self, bcpos: BcCtx) -> Nref {
        let n =
            Node::new(Oref::clone(&self.op_rv_phi), self.node_next_id(), bcpos);
        self.watch_node(&n);
        return n;
    }

    // -------------------------------------------------------------------------
    // Pseudo
    pub fn new_placeholder(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_placeholder),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_loop_iv_placeholder(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_loop_iv_placeholder),
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
        bcpos: BcCtx,
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
    pub fn new_snapshot(&mut self, bcpos: BcCtx) -> Nref {
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
    pub fn new_cfg_start(&mut self, bcid: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_start),
            self.node_next_id(),
            bcid,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_end(&mut self, bcid: BcCtx) -> Nref {
        let n =
            Node::new(Oref::clone(&self.op_cfg_end), self.node_next_id(), bcid);
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_merge_return(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_merge_return),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_merge_halt(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_merge_halt),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_halt(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_halt),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_return(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_return),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_if_cmp(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_if_cmp),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_jump(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_jump),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }

    pub fn new_cfg_loop_back(&mut self, bcpos: BcCtx) -> Nref {
        let n = Node::new(
            Oref::clone(&self.op_cfg_loop_back),
            self.node_next_id(),
            bcpos,
        );
        self.watch_node(&n);
        return n;
    }
}
