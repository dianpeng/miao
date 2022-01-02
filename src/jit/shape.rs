// shape of object, ie used by object.Obj to allow JIT code path to learn the
// object.Obj's hidden class. Its techinique name can be hidden class from the
// old self language implementation.
//
// The basic idea is as following, the whole engine maintains a type system
// shared by each VM, or G object. The type system is graph/tree structure where
// each nodes represent a type for object.
//
// In c++/rust, you can define a structure/class's scheme directly in the lang,
// but in miao, the type system is not clear. User just create an object,
// typically it is a hash/dictionary, and then later on user will change its
// shape by adding new member or directly adding member into it. Anyway, in most
// cases, after running some initial code for mutating the object's member, then
// this object stays the same for rest of its life time. If we can learn this
// scheme, then we can make the access of an object member not via hash, but
// leverage the most performant method, like index. As long as we can prove the
// object's shape is with our assumption.  Since most program will have just
// a enumrable scheme based object, so instead of maintaing the shape per object
// we can share the shape/scheme across the object. The shape/scheme basically
// just encapsulate the memory layout and access way of each object.
//
// The root of the shape is basically empty and all the internal shape will join
// in a single bottom shape, called hash_map, which effectively forms a simple
// lattice. All object after been created by bytecode ObjectStart will have shape
// empty. And if user start to mutate its shape, then its shape start to change.
// Giving following example :
//
// let a = {};  --> shape empty
// a.a = 10;    --> shape #1
// a.b = 20;    --> shape #2
//
// So our internal shape tree will have following looking :
//
//     [empty]
//       |
//       | adding 'a'
//       |
//      [#1]
//       |
//       | adding 'b'
//       |
//      [#2]
//
// If user run another piece of code as following :
//
// let b = {};     --> shape empty
// b.a = "string"; --> shape #1
// b.c = true;     --> shape #3
//
// Then the above shape tree will become following :
//
//     [empty]
//       |
//       | adding 'a'
//       |    adding 'c'
//      [#1] -------------> [#3]
//       |
//       | adding 'b'
//       |
//      [#2]
//
// In this tree, [empty], [#1], [#2], [#3] all represent a way to materialize
// the object's memory layout. ie each represent a class, suppose we are using
// c++, the class are as following :
//
// 1) empty represent struct {};
// 2) #1 represents struct { Handle a; };
// 3) #2 represents struct { Handle a; Handle b; };
// 4) #3 represents struct { Handle a; Handle c; };
//
// Some JIT engine will even try to guess the type in each fields, this brings
// near native performance boots, for this project, we just make simple hidden
// class to simplify our implementation :)
//
// In order to avoid too much hidden class, since some object are really been
// used as large hash map, we define a static number each object can use, ie
// each object can start to mutate its shape by 8 times, if we found the object
// mutate itself for more than 8 times, then we are gonna just bailout from the
// hidden class but mark object to be a HashMap shape, ie the slowest way.
//
// Why this is so important for us? Since the shape marker, #1, #2 ... gives us
// a cheap way to compare whether the object is still in that shape. During the
// JIT, the object access will be utilizing the shape of the object to generate
// the machine code and the machine code will just use the shape's internal
// access way to generate the code, ie via offset and direct memory addressing.
// But if the object's shape changed, then the machine code is incorrect to be
// executed and we need a cheap way to verify it. The shape # gives us a cheap
// way to check against the object. Each machine code generated object access
// call site will insert a guard instruction sequences, ie checking whether the
// object been addressed with is still having the assumed shape, if so we can
// use the generated code otherwise we will have to deoptimize.
