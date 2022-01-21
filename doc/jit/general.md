# Backends General Introduction

## Brief

The JIT backend features a optimized JIT compiler which does a bunch of pass
to optimize the code and try to generate competitive machine code. Notes, this
is not a simple full-code-generation style JIT compiler but does heavy weight
optimization. If not, why am I here ? :)


## IR

The IR graph of the language uses a hybrid Sea-of-nodes and traditional CFG style.
The statement will be translated into 2 categories:

 1. Effect statement, ie which generates side effect, and they should be ordered
    before alias anlysis. Each effect statement also will generate snapshot of
    the VM states, which is used for generating deoptimization trampoline.

 2. Trivial statement, ie they can be executed multiple times without having any
    side effect. These statements are floating around inside of the graph and
    they are not been ordered until they are been scheduled.

The IR graph consists of CFG nodes, which contains a list of effect instruction.
Based on the type of the CFG nodes, with its last jump instruction, each CFG node
can optionally have a CFG data dependency. For example a IfCmp CFG node can have
a expression to indicate the branch been taken by IfCmp operation. For any none
CFG node, it will be put into several category, but mostly they are node that
take values in and generates values out. The value list contains each node's
value that will be consumed and also the node itself, which contains a op field
to indicate what it is, will be the value that is generated. The reference of
a value is by taking its Rc pointer, since each node will be immutable after the
construction and also the use def is implicitly forwarded by having def's pointer,
so essentially it forms a basic SSA style graph.

If the graph been visualized, you could see that any effect bounded statement is
been kept inside of the CFG node's effect statement and been strictly ordered,
other nodes are "floating around" the graph. Later on, we can just let the
floating nodes to participate certain global optimization pass, like GVN or GCM.

Additionaly, each nodes will have control dependency as well. They are mostly
used by CFG node and PHI node. The CFG node uses dependency to link the CFG graph
forward and the PHI node will use control dependency to record its value input's
bounded CFG nodes.

Notes, the def-use/value dependency is been linked backwardly, ie the use takes
pointer points back to its def; but CFG are linked forwardly.

Lastly, each node will also have a reverse list which records which node are
using the current node, ie the def-use chain. The servese list have label to
tell the edge category. This is mostly for modifying the node in the graph, ie
like duplication of node, replace of node etc ...

## Optimization

Internally several pass of lowering will be performed but they are really not
mean to do optimization but just to enable future optimization and allow code
generation to happen. We just ignore the lower part for now.

### Simplification

This is really just fixup ceratin redundancy been emitted by the IR builder, and
enable future optimization afterwards

### Typer

This is a speculative type pass to mark node's type. Each node can have 2 types,
one is the |the_type| which contains exact type of the nodes. The other is the
|type_hint| which is used to enable speculative typping. User should always check
|the_type| for exact type, and |type_hint| when it generates guards. Typer is
just a enabler for more aggresive type guessing and enable future speculative
optimization.

### Fold

Folding pass, this gives way more than constant folding though. They are mostly
worked inside of the local expression, which may covers following optimization

  1) Constant Folding
  2) Algebra reassociation(commute ...)
  3) Identity testing (0+a, a+0, 1*a, ....)
  4) Strength reduction

### DCE (dead code elimination)

Mainly for eliminate dead block, due to previous constant folding etc ...

### Memory

Store forwarding and load sinking. After store forwarding and load sinking, the
unneeded memory allocation will become dead code. Avoiding using a expensive
escape analysis.

### GVN (global value numbering)

Deduplicate the global value. Notes the guard is floating, so it is important
for us to have GVN to de-dup the guard instruction

### GCM (global code motion)

Global code motion, enable schedule range of floating expression

### LICM (loop invariants code motion)

Loop peeling + GVN

### Scheduling

Schedule floating instruction into the IR graph and linking the deoptimization
point for each guard instruction. And remove unneeded deoptimization entry by
DCE.


### Intruction Selection

Selection of instruction for lower machine and prepare for the specific ARCH
and RA.


### RA (register allocation)

Liner scan RA + bin coallesc

### Code Generation

Generate arch specific machine code
