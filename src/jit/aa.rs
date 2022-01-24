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
//  The alias analysis mainly try to fix up few things
//
//  1. Global variable dependency
//
//     The IR generation will put global variable into statement lists which is
//     ordered, but mostly they are not alias with any other node except for 
//     node that may modify global variable, ie user function invocation. This
//     is really important since we do not want the global variable
