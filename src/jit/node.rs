// Node, ie the basic IR(HIR, MIR, LIR) element. Essentially we need some way
// to orgnize the IR into a graph so we need node and edge. To simplify the
// problem we just use node to represent everything and edge is been part of the
// node as some external reference.
//
// In technique term, we gonna produce a sea of nodes style IR, featuring a SSA
// style graph.
//
// In order to work with the strigent rust type system, we gonna use some simple
// way to represent each relationship.
// We have following relationship :
//
//   1) use-def 
//      ie, the node may needs value to compute its output, ie like add node
//      needs 2 operand as input and generate one output, indicated by the node
//      itself.
//
//   2) def-use
//      In order to reversely look up the user of node, ie who uses this node as 
//      its value input, we also need a def-use. use def and def use are
//      essentially 2 counterpart of value use relationship.
//
//   3) control flow
//      Since we are using sea of nodes, there's no explicity CFG until we start
//      to do scheduling. But it will be nice for us to indicate the CFG node for
//      some of the value nodes. Additionally, since we are using a single graph
//      to represent everything, a node needs to hjave CFG relationship in later
//      phase, ie after scheduling and instruction selection.
//
//   4) effect
//      The effect is another dependency that is implicitly represented by the
//      code. The most obvious dependency are data dependency, we have 3 data
//      dependency based on compiler theory, 1) true dependency; 2) anti
//      dependency; and 3) output dependency. But sometimes some code needs to
//      have other implicit dependency, for example if we the compiler cannot
//      decide alias rules, 2 variables may alias so even if their symbol name
//      differs but the data dependency may implicitly held. And since the 2
//      expression/statement may not have any use-def/def-use relationship, we
//      will need to use effect to represent the dependency.
//
//
//  Apart from dependency, each node also needs to hold several other information
//
//  1) Op, ie what types of operation the node has. In our framework, op is a
//     single giant flat enum containing, both high, middle and low operations,
//     ranging from boxing style polymorphic operator to low operator close to
//     1 to 1 assembler operations, like add_int32 etc ..
//
//  2) Type, each node needs a type indication as well to represent the type
//     inferrence result. The type itself is also a simple lattice.
//
//  3) Id, used to allow user to hold external information
//
//  4) Misc information added on demand for other usage, for example rpo order,
//     dominator tree, RA status etc ... They are all placed into the node for
//     simplicity.
//
// To reference a node, user needs to use a node reference instead of using the
// node itself. This makes replacing node easier to perform and also not too
// much rust hustle. A node reference is essentially a number/index pointing
// into a vector of node.

