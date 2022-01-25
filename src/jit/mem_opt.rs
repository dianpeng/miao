// (1) Alias Analysis ===> IR Graph Reconstruction
//
// When we finish the graph building, we will have a graph that put all the side
// effect operation into the corresponding CFG's effect lists. This is the most
// pessimistic assumption, ie all the operations are side effect and must be
// ordered. This makes GCM(global code motion) impossible to work or we will have
// to use traditional LICM(loop invariant code motion) to hoist the variable in
// loop outside of it.
//
// To address this issue, the AA pass will generate AA information and tries to
// break the dependency chain into much more flexiable dependency.
//
// After the pass, the side effect dependent input will not be ordered inside of
// the CFG, but instead for any expression that can be floated around, it will
// be released from the effect list but have its dependency inside of its own
// effect list.
//
//
//  Example :
//                             |--0--> [Node1]
//    [CfgJump] --> Effect --> |--1--> [Node2]
//                             |--2--> [Node3]
//
//
//  Assume after the AA, we learn that node3 only alias with node1 instead of 2,
//
//  so the dependency chain breaks and result in following graph
//
//                             |--0--> [Node1] <--- Effect --- [Node3]
//    [CfgJump] --> Effect --> |--1--> [Node2]
//                             | ....
//
//
//  Because this chain breaking will cross boundary of CFG, so each alias chain
//  will have its very first root been fixed inside of corresponding CFG and
//  each other will be just linked inside of its effect's own dependency chain
//  which is floating inside of the graph, later on the GCM will just work.
//
//  Notes EffectPhi will be inserted when a node breaks from its pinned CFG.
//
// (2) Alias Anaylisis ===> Algorithm
//
//   We mainly use basic abstract interpretation of program to do alias analysis,
//   the local variable is been SSA, so it will not hide alias. The places that
//   can alias a certain memory region is following :
//
//     1) Memory operation, ie index or dot operation
//     2) Upvalue
//     3) Globals
//     4) Function call (ie not inlined)
//
//
//   We basically maintain a memory env for each BB. Each memory env records
//   following information:
//
//     1) All the globals and upvalue's current memory status,
//     2) All the known memory location, ie created via RvObjectCreate and
//        RvListCreate's current status
//
//   The memory status is been designed using set theory for now and enable
//   future extension
//
//     1) AllOther
//
//        Memory set that is not known to us, ie any RvObjectCreate and
//        RvListCreate seen in the program is not aliased with AllOther
//
//     2) All
//        
//        Everything, it alias everything, can be thought as bottom
//
//     3) Memory
//       
//        Specific memory, always points to a RvObjectCreate or RvListCreate,
//        indicates the memory that is currently used.
//
//   Initially the program's memory region is been treated as ALL, ie all the
//   upvalue, globals etc ... will be treated as a aliased with each other until
//   new information is found. When we see 
//
