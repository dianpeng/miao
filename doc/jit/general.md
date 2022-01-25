# Backends General Introduction

## Brief

The JIT backend features a optimized JIT compiler which does a bunch of passes
to optimize the code and try to generate performant machine code.

## IR

### HIR (High level IR)
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

Notes, the def-use/value dependency is been linked backwards, ie the use takes
pointer points back to its def; but CFG are linked forward.

Lastly, each node will also have a reverse list which records which node are
using the current node, ie the def-use chain. The reverse list have label to
tell the edge category. This is mostly for modifying the node in the graph, ie
like duplication of node, replace of node etc ...

### LIR (Low IR)

The low IR will have a separate structure than the src/jit/node.rs, which is
just high and middle IR. The low IR is located inside of the backend folder
which features a minimal opcode traditionall CFG style graph. The instruction in
low IR is fixed inside of a certain places of CFG and also it uses its value
with speicalized SSATmp. The SSATmp indicates a local temporary which enables
future RA to kick in.

A traditional CFG will allow us to easily lower certain ir node in a much more
aggressive way, ie StrLt will become inline loops etc ...

## Compiler Pass (Analytics, Transform, Optimization)

### 1. Simplification

This is really just fixup ceratin redundancy been emitted by the IR builder, and
enable future optimization afterwards. 

It mainly does 

  1) trivial PHI nodes simplifcation and 
  2) propagation of immediate value to its load.

     ie LoadInt ---> [Imm(xxx)] replaced with [Imm(yyy)] from the prototype's
        constant table

### 2. Typer

This is a speculative type pass to mark node's type. Each node can have 2 types,
one is the |the_type| which contains exact type of the nodes. The other is the
|type_hint| which is used to enable speculative typping. User should always check
|the_type| for exact type, and |type_hint| when it generates guards. Typer is
just a enabler for more aggresive type guessing and enable future speculative
optimization.

### 3. Constant Folding

Simple constant folding

### 4. GVN(Global Value Numbering)

This pass tires to dedup those common sub experssion in global scope and it just
impacts none side effect nodes

### 5. Memory optimization (alias analysis)

Store forwarding and load sinking. After store forwarding and load sinking, the
unneeded memory allocation will become dead code. Avoiding using a expensive
escape analysis.

### 6. Rvalue Lower

This pass tries to lower the RvXXX instruction, ie the high level IR into mid
and/or lower IR. After this pass, all the guard/trap instruction is inserted and
all the actual polymorphic operation will be lowered with actual low level
operation with deoptimization point generated.

This pass mainly utilize feedback type and typer's type propagation.


### 7. Expression Optimization

This pass tries to aggressively optimize experssion, ie strength reduction,
sparse/conditional constant folding, algebra reassociation, normal constant
folding etc ...


### 8. DCE (dead code elimination)

Dead code elimination, this pass is basically used for deleting control flow
nodes that are not in used anymore

### 9. GCM (global code motion)

This pass basically does LICM optimization via global code motion, which also
fix the floating instruction into the BB, pretty much like scheduling.

### 10. Scheduling

This follows the GCM, which already fixes all the floating instruction into the
BB.

After this pass, all the nodes will not be sea-of-nodes but traditionally CFG.

Once the scheduling is done, a CFG lower will be performed which will rewrite
bunch of mid/low tier IR instruction or any intrinsic instruction into its
corresponding CFG data structure. Additionally, all the guards/trap instructions
will be lowered into if-else jumps. The CFG should only contain basic control
flow instruction, typed arithmetic operation, bitwise operation, memory move
operation, and call operation.

The call will have to fix its ABI things which is done in the low IR pass

Notes, each node will reference its value/use with a speical SSATmp object which
is used later on in Phi elimination and RA.

### 11. Intruction Selection

Selection of instruction for lower machine and prepare for the specific ARCH
and RA.

### 12. Phi Elimination

All the phi will be eliminated by having extra moves, the extra moves will be
optimized or fixed to register/stack slot by RA passes. After this all the PHI
should be gone.

### 13. RA (register allocation)

Liner scan RA + bin coallesc, move-move elimination will be performed

ABI fixing will be here either.

### 14. Code Generation

Generate specific machine code into the code buffer and resume the execution
of interp. When certain bytecode is hit, ie loop back or call, we will resume
the execution in jitted code. 

Finally, yeah! :)


## Future Workers (Why the important optimization X is missing?)

### Inline is missing purposely
inline will be added in the future

### Loop optimization other than LCIM is missing
vectorization, unrolling, unswitching, predicate optimization, explicit rotation
peeling, scalar evolution anlysis, where are they? 

The fancy loop optimization will be deferred until we have time to implement :)


### fancy scheduling
I really want to experiment hyperblock or superblock clonning during scheduling,
and I will try it when I have resource
