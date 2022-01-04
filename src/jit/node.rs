use std::cell::Cell;
use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

type Oref = Rc<RefCell<Op>>;
type Nref = Rc<RefCell<Node>>;
type WkNref = Weak<RefCell<Node>>;
type Aref = Rc<RefCell<Alias>>;
type Bref = Rc<RefCell<Block>>;
type Fref = Rc<RefCell<Func>>;

type Nid = u32;
type Bcid = u32; // bytecode id, used to reference back to the bytecode

type BlockList = Vec<Bref>;

struct DefUse {
    d: Nref,
    u: Nref,
}

type DefUseChain = Vec<DefUse>;
type DefUseTable = Vec<Box<DefUseChain>>;

enum AliasType {
    Not,
    May,
    Must,
}

enum AComp {
    Index(Nref),
    Dot(Nref),
}

struct Alias {
    slot: Aref,
    base: Nref,
    comp: Acomp,
}

struct Node {
    op: Oref,
    lhs: Nref,
    rhs: Nref,
    effect: Nref,

    id: Nid,
    bcid: Bcid,
    alias: Aref,
    tp: TypeHint,
}

enum OpType {
    Const,
    IntArith,
    FloatArith,
    IntComp,
    FloatComp,
    Jump,
    Global,
    Cfg,
    Misc,
}

enum OpTier {
    Rval,
    NArch,
    Arch,
}

enum OpVal {
    // misc of the OpVal
    // indicate a numeric index used for loading constantn
    ConstIndex,

    // indicate the size of the calling argument
    CallArgSize,

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

    // Checked version of unbox instruction, will automatically deoptimize if the
    // testify fails
    CheckUnboxI64,
    CheckUnboxI32,
    CheckUnboxI16,
    CheckUnboxI8,
    CheckUnboxU32,
    CheckUnboxU16,
    CheckUnboxU8,
    CheckUnboxF64,
    CheckUnboxTrue,
    CheckUnboxFalse,
    CheckUnboxNull,

    // Type guards, these operations will check whether the boxed value has type
    // otherwise it will just deoptimize, ie jumping to the deoptimize branch

    // high level, dealing with boxing value and all the script semantic, all
    // the instruction prefixed with Rv, stands for Rust value, which means the
    // boxed value.

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
    RvBoolean,

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

    // none arch, after aggressive type inference or speculative instruction
    // mostly bound to a fixed width machine word operation. All the instruction
    // prefixed with Fw, which stands for fixed width.

    // low level, ie nearly 1to2 assembly translation. All the instruction prefix
    // with M, which stands for machine.

    // Control Flow
    CfJjmp,
    CfJtrue,
    CfJfalse,
}

struct Op {
    name: String,
    tp: OpType,
    tier: OpTier,
    op: OpVal,
    side_effect: bool,

    // mostly used when the Op is lower instruction
    in_size: u8,
    out_size: u8,
    flag_size: u8,
}

// Control-flow block, used when we start to do scheduling of instructions.
struct Block {
    // list of isntruction been scheduled into the basic block, notes the inst
    // here has the partial order already
    ins_list: Vec<Nref>,

    // tree
    pred: BlockList,
    lhs: Bref,
    rhs: Bref,

    // dominator
    // immediate dominator inside of the dominator trees that dominates this
    // block
    idom: Bref,

    // dominator set, ie all the block been dominated by this block
    domset: BlockList,

    // we don't use dominator frontier to generate SSA, so it is not needed for
    // now.
    // other propertis
    jmp: BlockJmp,
    tp: BlockType,
}

trait Reclaim {
    // User implementation of is_dead, since we need to somehow to break the
    // cyclical reference conuting. The only way to do so is let each part to
    // decide whether it is dead or not.
    fn is_dead(&self) -> bool;

    // Invoked by Mpool when the corresponding node is been marked as dead
    fn on_reclaim(&mut self);
}

// all the allocation will be done inside of the Mpool which keeps a strong
// ref to each node and also call reclaim if needed, this makes us away from
// leaking memory internally due to cyclical reference counting.
struct Mpool {
}

// represent the function that been parsed
struct Func {
    g: Grptr,

    // (start, end) block
    start: Bref,
    end: Bref,

    // deoptimization end block
    deopt: Bref,

    // return end block
    ret: Bref,

    // halt end block
    halt: Bref,

    // error end block, ie generated by builtin assert1/2 etc ...
    error: Bref,

    // bug end block, ie generated by compiler to allow future debugging
    bug: Bref,

    // rpo order
    rpo: BlockList,
}

struct Graph {}
