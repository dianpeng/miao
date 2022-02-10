# Effect Analysis

This pass implements a dependency(side effect) analaysis along with several
store-load elemination.

This algorithm is kind of messy since it tries to do alias analysis and global
upvalue dependency reassociation and load store elimination at the same time.

## Brief

To precisly capture the side effect dependency of each instructions, we implement
a simple 1) flow sensitive; 2) sub field insensitive AA, basic idea is almost
the same as Steensgaard's algorithm except we record flow information.

The effect pass does

1) points-to analysis; 2) global value analysis; 3) upvalue analysis;

The graph is been constructed as SSA, so all the local variable is captured and
immutable, except for global value, upvalue and also the memory mutation. The
core concept of our model is that we assigned each program status that is of
our interests with a value lattice.

## Formulation

The algorithm defines a normal data flow equation and solve it via a typical
data flow iterative algorithm.

### Program Status

The status we tries to observe/solve at each program point is as following :

PS = GS ^ US ^ MS

GS = Global variable's status set
US = Upvalue variable's status set
MS = Memory node's status set

For GS and US, it contains all the global variable and upvalue visiable to the
function F, we can know for sure how many globals and upvalues are visiable to
the function F during compilation.

#### Value Lattice

The core model is our value lattice, we have separate memory lattice which is
just a sub-graph of the value lattice hasse graph.

##### Lattice

For each observed status, we assign its value into a lattice. The lattice forms
the core abstraction of our effect analysis framework.

The lattice is as following (hasse graph)


```
                   [Nothing]
                       |
      +---------------------------------+
      |                                 |
 (all the immediate)         (all the known memory loc)
   [Imm(0)] .... [Imm(N)]      [Mem(0)] .... [Mem(n)]
      |             |             |            |
      +------+------+             +------+-----+
             |                           |
             |                           |
        [NotMemory]                      |
             |                           |
             +------->[Unknown]<---------+
```

Nothing will not be set indeed, this turns this lattice into following attributes

1) The height of the lattice is 3, which means at most 3 times can be convergent

2) Top = Nothing and Bot = Unknown. The unknown maintains a fully total order of
   the corresponding value, ie any unknown value means any operation that related
   to it will be total order.

3) Every lattice element is not gonna alias with each other interms of its value.
   The Imm and NotMemory is immutable and Memory is immutable so points-to info
   needs to be generated for each known memory position

4) Unknown is been treated as anything that is not known to us, and it has its
   own order. The unknown order is to maintain how the program is been written,
   ie the node that should observe effect and been treated in unknown effect will
   be just put inside of its initial CFG's effect list.

   Reading and writing to a unknown value lattice is been put into list hence
   having total order. (Notes, this hasn't to be like this since all the read
   just needs to observe the recent most mutation's effect)

##### Memory

To describe the abstract memory location/shape in the heap, we dedicate a special
struct Memory to describe a known memory location which is not aliased with other
lattice element. Unknown can be viewed as {Heap} - {Memory}

Since memory can be aliased with each other via sub-field assignment/load. This
problem is traditionally been solved by using CFLGraph which is sort of
complicated, instead, we just do a simple merge.

When a sub field assignment happened on memory location X from memory location Y,
we simply merge memory X and memory Y into a memory Z which forms the parental
of X and Y. Any reference of X and Y will be updated to pointed to Z. Basically
X and Y are merged into a new memory which has its own dependency order.

The merge of memory location X and Y will also flush the dependency chain of both
X and Y, ie the merge operation is happened via a triggered ir node, could be
sub-field store or branch join. Then that ir node serves as a barrier to flush
the dependency chain.

Now let's briefly prove the fact that merging memory region having the same
effect as CFLGraph except it is an approximation.

Let's call CFLGraph's relation (edge and edge with label/parenthesis) as set G;
and our goal is to prove that our strategy will produce set H which has G <= H.
Effectively we may have more relation than the G but that's fine, since these
addition just means we have more approximation of alias, ie maybe not alised
variable are been considered as alias, which doesn't change the meaning of the
program.

To prove it, we denot our approach as a graph X, when a subfield is been assigned
or loaded, denote left hand side memory node as A, right hand side memory node as
B, we draw a pair of edges between A and B and mutually connect A and B. Same for
newly generated node U. Thus the memory node after the merge becomes a perfect
graph. And we purposely not label the edge since all them are connectable. For
any relationship in CFLGraph with parenthesis matching edge, in our graph is
obviously reachable. Hence set G <= H.

##### Not Memory

For NotMemory lattice element, it means a node that produces a value X which is
not an memory. In our graph, node that produces value that is not a memory will
be immutable. Therefore, each mutation of the NotMemory element will not need
to follow the dependency chain. Instead we just record the node that produce the
value, notes this may create new node during the analysis, and then for all the
not memory lattice, the store load elimination will be applied automatically.

##### Dependency

For value in lattice that is guaranteed to be not aliased with each other, we try
to maintain its dependency separatedly with other lattice element.

1) For imm(X) the store-load elimination will kick in which just remove the load
   operation entirely, this works for all following instructions

   1.1 global load
   1.2 upvalue load
   1.3 memory sub-field load

   It also means that for imm(X) it does not need to maintain its order chain
   since it is immutable.

2) For Mem(X), which follows the old school Andersen's Alias Analysis. The
   Mem(X) represent a shape/location in the abstract heap, and it can be
   pointed-to by multiple sites, ie global, upvalue or directly access. The
   reference points-to the abstraction location is named a spearate type called
   HeapLocation to address some Rust language things.

   For each instructions that could potentially alias the memory location with
   certain program status, we map different transfer function for each
   instruction to perform abstract interpretation.

   Since multiple point-to relationships are maintained precesily, any mutation
   of memory will be observed. Regarding the specific memory, its partial order
   is maintained.

The order(partial order) we try to maintain for each lattice element that is not
Unknown is as following

1) True Dependency
   ie read after write dependency. A read happened at certain lattice element
   is guaranteed to observe its previous write or its initial value.

2) Anti Dependency
   ie write after read dependency, A write happened after a bunch of reads is
   gauranteed to happen after these reads, ie these reads will not observe
   write happened afterwards.

3) Output Dependency
   ie write after write dependency. Since the above dependency is materialized
   and dependency is chained in the single order list, based on transitive of
   partial order, the output dependency is gauranteed automatically

To solve the dependency problem in programming, we dedicate a special struct
called DepChain to do this for each lattic element that is required to observe
dependecy chain.

Every newly update/mutate will be recorded in the DepChain struct and the
previous mutate will be dropped. The dependency is ordered as IR node's effect
dependency.

##### Memory Lattice

During the BB merge operation, a simple memory lattice will be setup for each
memory location. The memory location is really simple since it is used just for
construction of BB's status IN.

The lattice is essentially just the ValueLattice strips out the Imm and NotMemory

##### Put it all together

A ValueLattice structure is defined as following (Sum type)

```

enum ValueLattice {
  Nothing,          // top of lattice, never used during solving the equation
  Const(Imm),       // immediate(0...n)
  NotMemory(Nref),  // the value node that is not a memory
  Mem(Memory),      // the memory location of memory lattice
  Unknown,          // bottom of lattice
}

```

So the program status can be written as following

1. struct Galias represents any global X's value at program point Y

Galias {
  value: ValueLattice
};

2. struct Ualias represents any upvalue X's value at program ponit Y

Ualias {
  value: ValueLattice
};

3. memory location is been represented by Memory itself, but due the SSA graph
   directly access memory via RvMemAccess/RvObjectCreate/RvListCreate is also
   possible, they are just holding a pointer points to the Memory structure.


So the program status can be formulated as programming structure.

### Meet operation

Due to the loop lies inside of the graph, (notes our graph is reducible for sure).

When visit X, when it has unvisited predecessor block, for different set of the
program status, we use different strategy

1) for global, we use a pre-pass to get some information about the potential
   operations along the path from Y to X (edge (Y, X) must be a loopback edge).
   Only when find that direct global load and sub-field load or function call
   happened in the loop, the global will eagerly be marked as unknown.

2) for upvalue, almost same as global, though we only care that there's a direct
   update to upvalue index in the loop.


3) for memory, we do a different way but ignore the unvisited predecessor block
   and just assume nothing happent there. Instead, we run the analysis 2 times
   to be convergent. The reason why we can do this is because memory lattice has
   height of 2. After 2 times, we gonna be convergent on memory's value lattice
   along with its dependency chain.


The classical meet operation in data flow framework is defined as following :

    meet(X, Y) = GLB(X, Y); // GLB stands for greatest lower bound in hasse

### Transfer function F

For each bytecode that we are interested in, we designate a transfer function f
to solve the flow equation.

Notes, 3 helper functions needs to defined, we will revisit them later on.

1) ValueOf, ie turns a ir node into a value lattice based on current program
   status

2) MemoryOf, ie turns a ir node into a memory lattice based on current program
   status

3) UpdateEffect, ie it tries to update effect on any dependency chain related
   information.

1. RvLoadGlobal/RvLoadUpvalue
   
   Based on its value node, find out the value lattice it points to, notes, all
   node can find its mapping inside of the ValueLattice.

2. RvSetGlobal/RvSetUpvalue
   
   Based on the value node, find out the value lattice it points to, and set the
   current global/upvalue's value lattice to this lattice. Additionally, perform
   UpdateEffect sub-routine.

3. RvMemIndexStore/RvMemDotStore

   Based on the value node, find the memory it tries to point to, and then tries
   to do memory merge if applicable. Additionally, find the operation's memory
   it tries to mutate and modify the memory dependency chain or modify the
   lattice to be unknown.

4. RvMemIndexLoad/RvMemDotLoad

   Find the corresponding operand memory node, perform UpdateEffect.

5. RvCall

   Since a call to external function means expose arguments and global variable
   to the called routine, once it returns, we should put all the global variables
   and also arguments into unknown lattice element.

   So after the call, we mutate all the global variable's value into unknown, 
   notes the if a value lattice points to memory, this memory should also be
   unknowified. And also enumerate all the arguments the function takes, and
   get its value lattice and then tries to mark them as unknown.

6. RvPhi

   For phi nodes, we uses a simple way to handle. Any involved memory inside of
   phi nodes will be marked as unknown. Phi node is been handled specially in
   helper routine ValueOf and MemoryOf to cooperate with RvPhi's handling way.

### Helper function 




