# A small scripting language implemented with pure SAFE Rust

# Introduction

- Langauage Feature
  - Curly bracket style language
  - First order function
  - JavaScript style object model but no prototype inheritance
  - Closure and upvalue capture regardless of lexical scope

- Implementation Feature
  - One pass bytecode generation
  - Bytecode style interpreter
  - Simple object model with STW GC
  - Implementation with only safe Rust

# How does it look like ?

````

/**
 * this is a multiple line comments
 */

// this is a line comment
# bash style comment are also supported

// basic primitive type
let a_integer = 1;
let a_real = 1.0;
let a_bool = true;
let a_null = null;

assert a_integer == 1;
assert a_real == 1.0;
assert a_bool;
assert a_null == null;

// an expression
let local_expr = 1+2*3;

// store a global variable
global_expr = 1+2*3;

// load a global variable
let dup_global_expr = global_expr;

// define a global function
func foo(a, b) {
  return a + b;
}

// or define a global variable with function closure
global_foo = func(a, b) {
  return a * b;
};

// or define a local variable with function closure
let bar = func(a, b, c) {
  return a + b + c;
};

// closure upvalue capture
let upvalue1 = 1;
let upvalue2 = [1, 2];

let a_closure = func() {
  // capture upvalue that is not belonged to this function
  return upvalue1 + upvalue2[1];
};

assert a_closure() == 3;

// a nested closure, allows user to capture value multiple level of the
// function definition
let a_closure2 = func() {
  let a = 1;
  return func() {
    let b = 2;
    return func() {
      let c = 3;
      return func() {
        let d = 4;
        return func() {
          return a + b + c + d;
        };
      };
    };
  };
};

assert a_closure2()()()()() == 1+2+3+4;

// object model
// define an object, backed by Rust std::collections::HasMap
let obj = {
  "miao" : "miao",
  "huhu" : "haha"
};

// add more entry into the map
obj.a = 10;

// or you can use index way
obj["b"] = 100;

// define a list, much like JSON
let list = [1, 2, 3, 4];

// extend the list, notes, the hole inside of the array will be feeded with
// null value.
list[10] = 200;

// modify existed slot
list[1] = 20;

// Mixing function closure with data member into object
let xx = {
  "foo" : func() {
    return 100;
  },
  "bar" : func() {
    return "valval";
  },
  "my_data" : 100
};

// Typical control flow is supported
let xxx = func(v)  {
  if (v >= 100) {
    return 1;
  } elif (v >= 1000) {
    return 2;
  } else {
    return 0;
  }
};

assert xxx(1) == 0;
assert xxx(101) == 1;

// For loop, we support 2 styles
//  1) forever loop
//       for { .... }
//  2) for in loop, with implicit iterator conversion
//
//       for x in obj { .... }
//
//     currently, we only support string, list, object for iterator

let count_iter_len = func(obj) {
  let sum = 0;
  for _ in obj {
    sum = sum + 1;
  }
  return sum;
};

assert count_iter_len([]) == 0;
assert count_iter_len([1, 2, {}]) == 3;
assert count_iter_len({"a": 1, "b": 2}) == 2;

let another_loop = func(start, step, limit) {
  let sum = 0;
  for {
    if start > limit break;
    start = start + 1;
    sum = step + step;
  }
  return sum;
};

trace "ANOTHER LOOP: ", another_loop(1, 2, 10);

assert another_loop(1, 2, 10);

````

# Notes

## License
The code is in public domain. Use it at your own risk!

## Caveats

This is a project that I write mainly for learning Rust in the last two weaks. The code is not 
pollished and BUGS are expected. Since I already add many tests into the project, it does have
some stability and usability. I will try to spend some spare cycle to pollish it and make it a
more mature project.
