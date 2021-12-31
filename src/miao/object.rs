use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;
use std::rc::Weak;

use crate::miao::bytecode::*;
use crate::miao::exec::Conversion;
use crate::miao::heap::*;

// -----------------------------------------------------------------------------
// Execution of the runtime, ie the VM.
#[derive(Clone)]
pub struct Verror {
    pub description: String,
}

#[derive(Clone, Eq, PartialEq)]
pub enum ResultType {
    Return,
    Halt,
}

#[derive(Clone)]
pub struct Vresult {
    pub value: Handle,
    pub result_type: ResultType,
}

impl Vresult {
    pub fn ret(v: Handle) -> Vresult {
        Vresult {
            value: v,
            result_type: ResultType::Return,
        }
    }

    pub fn halt(v: Handle) -> Vresult {
        Vresult {
            value: v,
            result_type: ResultType::Halt,
        }
    }
}

// Global status, representing the Virtual machine for execution purpos
pub struct G {
    pub heap: GHeap,
}

pub type Gptr = Rc<RefCell<G>>;

// Sink callback, user can use its own customized sinker for trace statement,
// returns None means continuing execution otherwise the VM will be halt with
// error provided
pub type TraceSinker = fn(r: &mut Runptr, arg_count: u32) -> Option<Verror>;

pub fn default_trace_sinker(
    run: &mut Runptr,
    arg_count: u32,
) -> Option<Verror> {
    let mut idx = arg_count;
    print!("{}", "[trace]: ");
    while idx > 0 {
        let tos = run.borrow().tos(idx - 1);
        let x = Conversion::debug_string(&tos, run);
        print!("{} ", x);
        idx -= 1;
    }
    print!("{}", "\n");
    return Option::None;
}

pub struct Run {
    pub g: Gptr,

    // following variables are considered as GC roots
    pub global: ObjRef,            // globals
    pub stack: HandleList,         // run's stack
    pub rcall: Option<FuncRef>,    // current invocation handler's handle
    pub trace_sinker: TraceSinker, // trace sinker
    gc: GC,                        // gc marker
}

// Used to save calling context when invoking another function defined in script
pub struct Rctx {
    pub rcall: FuncRef,
    pub pc: usize,
    pub offset: u32,
}

pub type Runptr = Rc<RefCell<Run>>;
pub type WkRunptr = Weak<RefCell<Run>>;

impl Verror {
    pub fn from_str(msg: String) -> Verror {
        Verror { description: msg }
    }
}

impl G {
    pub fn new(heap_config: GHeapConfig) -> Gptr {
        Gptr::new(RefCell::new(G {
            heap: GHeap::new_with_config(heap_config),
        }))
    }
    pub fn global_get(&self, key: &StrRef) -> Option<Handle> {
        self.heap.global_get(key)
    }
    pub fn global_add(&mut self, key: &StrRef, v: Handle) {
        self.heap.global_add(key, v)
    }
    pub fn global_del(&mut self, key: &StrRef) -> bool {
        self.heap.global_del(key)
    }
    pub fn global_has(&self, key: &StrRef) -> bool {
        self.heap.global_has(key)
    }
    pub fn global_clear(&self) {
        self.heap.global_clear()
    }
}

impl Run {
    pub fn new_test(mut heap: GHeap) -> Runptr {
        let g = heap.new_obj();
        let stk = HandleList::new();

        Runptr::new(RefCell::new(Run {
            g: Gptr::new(RefCell::new(G { heap: heap })),
            global: g,
            stack: stk,
            rcall: Option::None,
            trace_sinker: default_trace_sinker,
            gc: GC::nil(),
        }))
    }
    pub fn new(
        g: Gptr,
        glb: ObjRef,
        stk: HandleList,
        ts: TraceSinker,
    ) -> Runptr {
        Rc::new(RefCell::new(Run {
            g: g,
            global: glb,
            stack: stk,
            rcall: Option::None,
            trace_sinker: ts,
            gc: GC::nil(),
        }))
    }
    pub fn rcall_ref(&self) -> &FuncRef {
        self.rcall.as_ref().unwrap()
    }

    // global related manipulation
    pub fn global_get(&self, key: &StrRef) -> Option<Handle> {
        self.global.borrow().get(key)
    }
    pub fn global_add(&mut self, key: &StrRef, v: Handle) {
        self.global.borrow_mut().add(key, v)
    }
    pub fn global_del(&mut self, key: &StrRef) -> bool {
        self.global.borrow_mut().del(key)
    }
    pub fn global_has(&self, key: &StrRef) -> bool {
        self.global.borrow().has(key)
    }
    pub fn global_clear(&self) {
        self.global.borrow_mut().clear()
    }

    // stack manipulation, used to get function/callback argument passed in
    pub fn tos(&self, idx: u32) -> Handle {
        let idx = self.stack.len() - (idx + 1) as usize;
        return self.stack[idx].clone();
    }
    pub fn try_tos(&self, idx: u32) -> Option<Handle> {
        if (idx + 1) as usize >= self.stack.len() {
            return Option::None;
        }
        return Option::Some(self.tos(idx));
    }

    // used to dumpping the current status of the stack
    pub fn dump_stack(&self) -> String {
        let mut o = Vec::<String>::new();
        let mut idx = 0;
        for x in self.stack.iter().rev() {
            o.push(format!("{}. {:?}", idx, handle_type_name(x)));
            idx += 1;
        }
        return o.join("\n");
    }
}

impl HRef for Run {
    fn otype(&self) -> OType {
        return OType::Run;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        if !self.move_gc_mark_grey() {
            return;
        }

        // global
        self.global.borrow_mut().gc_mark();

        // stack
        for x in self.stack.iter_mut() {
            GHeap::gc_mark(x);
        }

        // current call frame
        match &mut self.rcall {
            Some(v) => v.borrow_mut().gc_mark(),
            _ => (),
        };

        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {
        self.stack.clear();
    }
    fn debug_info(&self) -> String {
        return "[run]".to_string();
    }
    fn sizeof(&self) -> usize {
        return 1;
    }
}

// A basic object model for the small scripting language we want to implement.
//
// Each object will be put into 2 categories
//
// 1 => primitive types :
//
// 1) int
// 2) real
// 3) true
// 4) false
//
// 2 => heap types:
//
// 1) string
// 2) list
// 3) object
// 4) function
// 5) iterator
//
// ----------------------------------------------------------------------------
//
// The reference object is called Handle, which is essentially just a enum, and
// for each primitive type, they are passed by value and for heap types, they
// are passing around by reference.
//
// Each heap types have a HRef trait, which contains function belong to how to
// do GC traversal and also how to indentify it as a GC based object.

#[derive(PartialEq, Eq)]
pub enum GCMark {
    White,
    Grey,
    Black,
}

#[derive(PartialEq, Eq)]
pub struct GC {
    mark: GCMark,
}

impl GC {
    pub fn nil() -> GC {
        GC {
            mark: GCMark::White,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum OType {
    // GC types
    Str,
    List,
    Object,
    Function,
    Iter,
    NFunction,

    // Internal structures
    Run,
}

pub type GCRef = Rc<RefCell<dyn HRef>>;
pub type WkGCRef = Weak<RefCell<dyn HRef>>;
pub type GCList = Vec<GCRef>;

// Currently, we cannot use rust to cast a trait back to its struct T, which makes
// our work have to go with a enum based value model. Instead of having a shared
// common base for each GCed object, we need to have each one seprately. But the
// memory is been held by the GCRef chain with GCRef, essentially just a Rc of
// the trait. And each reference towards the object itself is just a shared pointer
// of the actual value

pub type StrRef = Rc<RefCell<Str>>;
pub type WkStrRef = Weak<RefCell<Str>>;

pub type ListRef = Rc<RefCell<List>>;
pub type WkListRef = Weak<RefCell<List>>;

pub type ObjRef = Rc<RefCell<Obj>>;
pub type WkObjRef = Weak<RefCell<Obj>>;

pub type FuncRef = Rc<RefCell<Function>>;
pub type WkFuncRef = Weak<RefCell<Function>>;

pub type NFuncRef = Rc<RefCell<dyn NFunc>>;
pub type WkNFuncRef = Weak<RefCell<dyn NFunc>>;

pub type IterRef = Rc<RefCell<dyn Iter>>;
pub type WkIterRef = Weak<RefCell<dyn Iter>>;

#[derive(Clone)]
pub enum Handle {
    Int(i64),
    Real(f64),
    Boolean(bool),
    Null,

    Str(StrRef),
    List(ListRef),
    Object(ObjRef),
    Function(FuncRef),
    NFunction(NFuncRef),
    Iter(IterRef),
}

pub fn handle_is_type_same(l: &Handle, r: &Handle) -> bool {
    std::mem::discriminant(l) == std::mem::discriminant(r)
}

pub fn handle_type_name(v: &Handle) -> &str {
    return match v {
        Handle::Int(_) => "int",
        Handle::Real(_) => "real",
        Handle::Boolean(_) => "boolean",
        Handle::Null => "null",
        Handle::Str(_) => "string",
        Handle::List(_) => "list",
        Handle::Object(_) => "object",
        Handle::Function(_) => "function",
        Handle::NFunction(_) => "native_function",
        Handle::Iter(_) => "iterator",
    };
}

pub fn handle_sizeof(v: &Handle) -> usize {
    return match v {
        Handle::Int(_) => 1,
        Handle::Real(_) => 1,
        Handle::Boolean(_) => 1,
        Handle::Null => 1,
        Handle::Str(v) => v.borrow().sizeof(),
        Handle::List(v) => v.borrow().sizeof(),
        Handle::Object(v) => v.borrow().sizeof(),
        Handle::Function(v) => v.borrow().sizeof(),
        Handle::NFunction(v) => v.borrow().sizeof(),
        Handle::Iter(v) => v.borrow().sizeof(),
    };
}

// handler helper function
pub fn handle_is_int(x: &Handle) -> bool {
    match x {
        Handle::Int(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_real(x: &Handle) -> bool {
    match x {
        Handle::Real(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_number(x: &Handle) -> bool {
    return handle_is_int(x) || handle_is_real(x);
}

pub fn handle_is_boolean(x: &Handle) -> bool {
    match x {
        Handle::Boolean(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_true(x: &Handle) -> bool {
    match x {
        Handle::Boolean(v) => return *v,
        _ => return false,
    };
}

pub fn handle_is_false(x: &Handle) -> bool {
    match x {
        Handle::Boolean(v) => return !*v,
        _ => return false,
    };
}

pub fn handle_is_null(x: &Handle) -> bool {
    match x {
        Handle::Null => return true,
        _ => return false,
    };
}

pub fn handle_is_list(x: &Handle) -> bool {
    match x {
        Handle::List(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_object(x: &Handle) -> bool {
    match x {
        Handle::Object(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_str(x: &Handle) -> bool {
    match x {
        Handle::Str(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_function(x: &Handle) -> bool {
    match x {
        Handle::Function(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_native_function(x: &Handle) -> bool {
    match x {
        Handle::NFunction(_) => return true,
        _ => return false,
    };
}

pub fn handle_is_iter(x: &Handle) -> bool {
    match x {
        Handle::Iter(_) => return true,
        _ => return false,
    };
}

pub type HandleList = Vec<Handle>;

pub trait HRef {
    fn otype(&self) -> OType;

    fn gc_ref(&self) -> &GC;
    fn gc_ref_mut(&mut self) -> &mut GC;

    fn set_gc_mark(&mut self, m: GCMark) {
        self.gc_ref_mut().mark = m;
    }
    fn gcmark(&self) -> &GCMark {
        return &self.gc_ref().mark;
    }
    fn gcmark_mut(&mut self) -> &mut GCMark {
        return &mut self.gc_ref_mut().mark;
    }
    fn set_gc_mark_white(&mut self) {
        self.set_gc_mark(GCMark::White);
    }
    fn set_gc_mark_grey(&mut self) {
        self.set_gc_mark(GCMark::Grey);
    }
    fn set_gc_mark_black(&mut self) {
        self.set_gc_mark(GCMark::Black);
    }

    // following the GCMark lattice to move the state machine forward, this
    // method could fail.
    // precondition: mark == WHITE; aftercondition mark == GREY
    fn move_gc_mark_grey(&mut self) -> bool {
        if *self.gcmark() == GCMark::White {
            self.set_gc_mark_grey();
            return true;
        }
        return false;
    }
    fn move_gc_mark_black(&mut self) -> bool {
        if *self.gcmark() == GCMark::Grey {
            self.set_gc_mark_black();
            return true;
        }
        return false;
    }

    // Recursively marking the current HRef along with its internal fields GC
    // fields. Once it is done, it can returns. The invocation will follow the
    // GC root to starts. All the HRef will be a reference
    fn gc_mark(&mut self);

    // Finalize, this function is needed when we are about to remove the object
    // from our current list. Notes, it means the object itself should take this
    // time to reclaim its internal gut, ie breaking the reference counting cycle
    // since we are using the reference counting in rust to keep the object alive
    fn gc_finalize(&mut self);

    // property
    fn debug_info(&self) -> String;
    fn sizeof(&self) -> usize;

    // --------------------------------------------------------------------------
    // Convinient methods
    fn is_str(&self) -> bool {
        return self.otype() == OType::Str;
    }
    fn is_list(&self) -> bool {
        return self.otype() == OType::List;
    }
    fn is_object(&self) -> bool {
        return self.otype() == OType::Object;
    }
    fn is_function(&self) -> bool {
        return self.otype() == OType::Function;
    }
    fn is_native_function(&self) -> bool {
        return self.otype() == OType::NFunction;
    }
    fn is_iter(&self) -> bool {
        return self.otype() == OType::Iter;
    }
}

// User extension type, ie NFunction
pub trait NFunc: HRef {
    fn invoke(
        &mut self,
        argcount: u32,
        run: &mut Runptr,
    ) -> Result<Vresult, Verror>;
}

// The issue is in rust, storing the pair into the HashMap and maintain a easy
// to use iterator is nearly impossible. So we need to do some trick to combine
// the HashMap and vector together otherwise it will just never works out. The
// key is we store the value pair into another vector and use HashMap as simple
// index into the value pair.

pub struct Obj {
    index: HashMap<String, usize>,
    value: Vec<(StrRef, Handle)>,
    gc: GC,
}

impl HRef for Obj {
    fn otype(&self) -> OType {
        return OType::Object;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        if !self.move_gc_mark_grey() {
            return;
        }
        for (key, val) in self.value.iter_mut() {
            key.borrow_mut().gc_mark();
            GHeap::gc_mark(val);
        }
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {
        self.index.clear();
    }
    fn debug_info(&self) -> String {
        return "[object]".to_string();
    }
    fn sizeof(&self) -> usize {
        return self.index.len();
    }
}

// Specialized object methods, for CRUD
impl Obj {
    pub fn new() -> ObjRef {
        ObjRef::new(RefCell::new(Obj {
            index: HashMap::<String, usize>::new(),
            value: Vec::<(StrRef, Handle)>::new(),
            gc: GC::nil(),
        }))
    }
    pub fn add(&mut self, key: &StrRef, value: Handle) {
        {
            let hkey = &key.borrow().string;

            // (0) first check whether the index already have such value or not, if
            //     so then just do an update
            match self.index.get(hkey) {
                Some(v) => {
                    self.value[*v].1 = value;
                    return;
                }
                _ => (),
            };
        }

        // (1) okay, add a new entry
        {
            let idx = self.value.len();
            let kk = key.borrow().string.clone();
            self.value.push((StrRef::clone(key), value));
            self.index.insert(kk, idx);
        }
    }
    pub fn del(&mut self, key: &StrRef) -> bool {
        let hkey = &key.borrow().string;

        match self.index.get(hkey) {
            Some(v) => {
                self.value.remove(*v);
                self.index.remove(hkey);
                return true;
            }
            _ => return false,
        };
    }
    pub fn get(&self, key: &StrRef) -> Option<Handle> {
        let hkey = &key.borrow().string;
        match self.index.get(hkey) {
            Some(v) => {
                return Option::Some(self.value[*v].1.clone());
            }
            _ => return Option::None,
        };
    }
    pub fn has(&self, key: &StrRef) -> bool {
        let hkey = &key.borrow().string;
        return self.index.contains_key(hkey);
    }
    pub fn clear(&mut self) {
        self.index.clear();
        self.value.clear();
    }
    pub fn len(&self) -> usize {
        return self.index.len();
    }
}

pub struct List {
    value: Vec<Handle>,
    gc: GC,
}

impl HRef for List {
    fn otype(&self) -> OType {
        return OType::List;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        if !self.move_gc_mark_grey() {
            return;
        }
        for v in self.value.iter_mut() {
            GHeap::gc_mark(v);
        }
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {
        self.value.clear();
    }
    fn debug_info(&self) -> String {
        return "[list]".to_string();
    }
    fn sizeof(&self) -> usize {
        return self.value.len();
    }
}

impl List {
    pub fn new() -> ListRef {
        Rc::new(RefCell::new(List {
            value: Vec::<Handle>::new(),
            gc: GC::nil(),
        }))
    }
    pub fn add(&mut self, h: Handle) {
        self.value.push(h);
    }
    pub fn assign(&mut self, x: u32, h: Handle) {
        let expect_len = (x + 1) as usize;
        if expect_len > self.value.len() {
            self.value.resize((x + 1) as usize, Handle::Null);
        }
        self.value[x as usize] = h;
    }
    pub fn len(&self) -> usize {
        self.value.len()
    }
    pub fn index(&self, idx: usize) -> Handle {
        self.value[idx].clone()
    }
    pub fn clear(&mut self) {
        self.value.clear();
    }
}

#[derive(PartialEq, Eq)]
pub struct Str {
    pub string: String,   // the actual string value
    pub char_size: usize, // character size
    gc: GC,               // gc field
}

// It is just a string pool that keeps a reference into the GCList to dedup
// the String object. Notes, the StrPool never owns a string object, but owns
// its weak reference
pub struct StrPool {
    index: HashMap<String, WkStrRef>,
}

impl Str {
    pub fn new(s: String) -> StrRef {
        let cs = s.chars().count();
        Rc::new(RefCell::new(Str {
            string: s,
            char_size: cs,
            gc: GC::nil(),
        }))
    }
}

// this is just a fly-weight thin wrapper
impl StrPool {
    pub fn new() -> StrPool {
        StrPool {
            index: HashMap::<String, WkStrRef>::new(),
        }
    }

    // return (reclaimed size, where to start next time)
    pub fn run_gc(&mut self, from: u32, limit: u32) -> (u32, u32) {
        let sz = self.index.len() as u32;
        let runs = cmp::min(limit, sz);
        let mut idx = 0;
        let mut name_set = HashSet::<String>::new();
        let mut start = from;
        if start >= sz {
            start = 0;
        }

        for (key, value) in self.index.iter().skip(start as usize) {
            if value.strong_count() == 0 {
                name_set.insert(key.to_string());
            }
            idx += 1;
            if idx >= runs {
                break;
            }
        }

        // make sure we scan at least runs object.
        if idx < runs {
            for (key, value) in self.index.iter() {
                if value.strong_count() == 0 {
                    name_set.insert(key.to_string());
                }
                idx += 1;
                if idx >= runs {
                    break;
                }
            }
        }

        // remove index's entry based on the name_set recording
        for key in name_set.iter() {
            self.index.remove(key);
        }

        let mut next_start = start + idx;
        if next_start >= self.index.len() as u32 {
            next_start = 0;
        }

        return (name_set.len() as u32, next_start);
    }

    // this function tries to get a pooled string based on the input, if the
    // string is not in the pool then None is returned
    pub fn get(&mut self, n: &str) -> Option<StrRef> {
        self.index.get(n)?.upgrade()
    }
    pub fn set(&mut self, k: &str, r: WkStrRef) {
        self.index.insert(k.to_string(), r);
    }

    // dump all the string pool content out
    pub fn dump(&self) -> String {
        let mut o = Vec::<String>::new();
        for (k, v) in self.index.iter() {
            let val: String = if v.strong_count() == 0 {
                "[N/A]".to_string()
            } else {
                v.upgrade().unwrap().borrow().string.clone()
            };
            o.push(format!("{} => {}", k, val));
        }
        return o.join("\n");
    }

    pub fn len(&self) -> usize {
        self.index.len()
    }
}

impl HRef for Str {
    fn otype(&self) -> OType {
        return OType::Str;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {}
    fn debug_info(&self) -> String {
        return self.string.clone();
    }
    fn sizeof(&self) -> usize {
        return self.char_size;
    }
}

// function, ie the runtime materialization of prototype object. Notes, the
// function object will need to fillup the upvalue array based on prototype's
// upvalue indication.
pub struct Function {
    pub proto: ProtoRc,
    upvalue: HandleList,
    gc: GC,
}

impl Function {
    pub fn new(this_p: ProtoRc, uplist: HandleList) -> FuncRef {
        Rc::new(RefCell::new(Function {
            proto: this_p,
            upvalue: uplist,
            gc: GC::nil(),
        }))
    }
    pub fn get_upvalue(&self, idx: u32) -> Handle {
        return self.upvalue[idx as usize].clone();
    }
    pub fn set_upvalue(&mut self, idx: u32, h: Handle) {
        self.upvalue[idx as usize] = h;
    }
}

impl HRef for Function {
    fn otype(&self) -> OType {
        return OType::Function;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        if !self.move_gc_mark_grey() {
            return;
        }
        for x in self.upvalue.iter_mut() {
            GHeap::gc_mark(x);
        }
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {
        self.upvalue.clear();
    }
    // used to generate runtime diagnostic information here
    fn debug_info(&self) -> String {
        format!(
            "{}: [upvalue={}]",
            self.proto.debug_info(),
            self.upvalue.len()
        )
    }
    fn sizeof(&self) -> usize {
        return 1;
    }
}

// iterator. Internally used as part of the iterator protocol to allow iteration
// of compound element like List/Object. Currently, we just support 3 types of
// iterator for internal object, ie List/Object/String for now.
//
// the Iter trait is just a HRef extension, user needs to impelment both
pub trait Iter: HRef {
    fn has(&self, _: &mut Runptr) -> bool;
    fn next(&mut self, _: &mut Runptr) -> bool;
    fn value(&self, _: &mut Runptr) -> Result<Handle, Verror>;
}

// string's iterator
pub struct StrIter {
    // since rust's string is utf8 encoded, each iterator will have to decode
    // the string at once and make it as a char array and then performing the
    // iteration for now, for optimization, we can do incremental decoding of
    // the unicode code point but that will force us to copy the current string
    // either.
    codepoint: Vec<char>,
    cursor: usize,
    gc: GC,
}

pub struct ListIter {
    list: ListRef,
    cursor: usize,
    gc: GC,
}

pub struct ObjIter {
    object: ObjRef,
    cursor: usize,
    gc: GC,
}

impl Iter for StrIter {
    fn has(&self, _: &mut Runptr) -> bool {
        return self.cursor < self.codepoint.len();
    }

    fn next(&mut self, r: &mut Runptr) -> bool {
        self.cursor += 1;
        return self.has(r);
    }

    fn value(&self, r: &mut Runptr) -> Result<Handle, Verror> {
        assert!(self.has(r));
        let ch = self.codepoint[self.cursor];
        return Result::Ok(
            r.borrow()
                .g
                .borrow_mut()
                .heap
                .new_string_handle(ch.to_string()),
        );
    }
}

impl HRef for StrIter {
    fn otype(&self) -> OType {
        return OType::Object;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {}
    fn debug_info(&self) -> String {
        return "[string-iterator]".to_string();
    }
    fn sizeof(&self) -> usize {
        return 1;
    }
}

impl StrIter {
    pub fn new(s: StrRef) -> StrIter {
        StrIter {
            codepoint: s.borrow_mut().string.chars().collect(),
            cursor: 0,
            gc: GC::nil(),
        }
    }
}

impl Iter for ListIter {
    fn has(&self, _: &mut Runptr) -> bool {
        return self.cursor < self.list.borrow_mut().value.len();
    }
    fn next(&mut self, r: &mut Runptr) -> bool {
        self.cursor += 1;
        return self.has(r);
    }

    fn value(&self, r: &mut Runptr) -> Result<Handle, Verror> {
        assert!(self.has(r));
        return Result::Ok(self.list.borrow_mut().value[self.cursor].clone());
    }
}

impl HRef for ListIter {
    fn otype(&self) -> OType {
        return OType::Object;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        if !self.move_gc_mark_grey() {
            return;
        }
        self.list.borrow_mut().gc_mark();
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {}
    fn debug_info(&self) -> String {
        return "[list-iterator]".to_string();
    }
    fn sizeof(&self) -> usize {
        return 1;
    }
}

impl ListIter {
    pub fn new(s: ListRef) -> ListIter {
        ListIter {
            list: s,
            cursor: 0,
            gc: GC::nil(),
        }
    }
}

impl Iter for ObjIter {
    fn has(&self, _: &mut Runptr) -> bool {
        return self.cursor < self.object.borrow_mut().value.len();
    }
    fn next(&mut self, r: &mut Runptr) -> bool {
        self.cursor += 1;
        return self.has(r);
    }
    fn value(&self, r: &mut Runptr) -> Result<Handle, Verror> {
        assert!(self.has(r));
        let tuple = self.object.borrow_mut().value[self.cursor].clone();
        return Result::Ok(
            r.borrow()
                .g
                .borrow_mut()
                .heap
                .new_pair_handle(Handle::Str(tuple.0), tuple.1),
        );
    }
}

impl HRef for ObjIter {
    fn otype(&self) -> OType {
        return OType::Object;
    }
    fn gc_ref(&self) -> &GC {
        return &self.gc;
    }
    fn gc_ref_mut(&mut self) -> &mut GC {
        return &mut self.gc;
    }
    fn gc_mark(&mut self) {
        if !self.move_gc_mark_grey() {
            return;
        }
        self.object.borrow_mut().gc_mark();
        self.set_gc_mark_black();
    }
    fn gc_finalize(&mut self) {}
    fn debug_info(&self) -> String {
        return "[object-iterator]".to_string();
    }
    fn sizeof(&self) -> usize {
        return 1;
    }
}

impl ObjIter {
    pub fn new(s: ObjRef) -> ObjIter {
        ObjIter {
            object: s,
            cursor: 0,
            gc: GC::nil(),
        }
    }
}
