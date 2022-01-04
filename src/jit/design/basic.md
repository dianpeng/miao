# IR graph

We use a sea of nodes style IR graph. In our graph, there's no edge except for
gather index information. All nodes in the graph will take some value, ie input
and then generate a single output, ie output. Corresponding to classical 3 addr
instruction this will be like :

  (
    instr ==> output
    instr.input[0] ==> input[0]
    instr.input[1] ==> input[1]
  ) 

We don't allow instruction to take more than 3 input in terms of data dependency.

Each ir node will also have a effect node which implicitly define its side effect
order. The side effect will be generated on demand when an instruction that has
side effect that is been inserted into the graph. The PHI node will also have its
variance, ie the effect phi. Then the control flow graph is implicitly been
indicated inside of the graph, ie the CFG node that generates side effect or rely
on value, ie If, Loop, Return etc, will have its corresponding part.

# Node

type Oref = Rc<RefCell<Op>>;
type Nref = Rc<RefCell<Node>>;
type WkNref = Weak<RefCell<Node>>;
type Aref = Rc<RefCell<Alias>>;

// indicate def use relationship, ie who uses the value been generated
// this information is kept as an external reference in the function scope
struct DefUse {
  def: Nref,
  use: Nref,
}

type DefUseChain = Vec<DefUse>;
type DefUseTbl = Vec<DefUseChain>;

struct Graph {
  def_use: DefUseTbl, // use node.id to access its own def use chain
}

enum OpType {
  Const,
  IntArith,
  FloatArith,
  Comparison,
  Jump,
  Global,
  Misc
}

enum AliasType {
  Not,
  May,
  Must,
}

// The compiler only generates memory as following form factor, ie structural
// object
enum AComp {
  Idx(Nref),
  Dot(Nref),
}

struct Alias {
  slot: Aref,
  base: Nref,
  comp: AComp, 
}

struct Node {
  op: Oref,
  lhs: Nref,
  rhs: Nref,
  id : Nid,
  alias: Aref,
}


