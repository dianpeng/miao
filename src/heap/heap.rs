use std::cell::RefCell;
use std::rc::Rc;

use crate::bc::bytecode::ProtoRc;
use crate::object::object::*;

// The design of the runtime GC is very simple, we just use a STW method to do
// the GC. the GHeap take care of all the object creation and register object's
// HRef into its global list, |current|. The scanning of root starts from the
// GHeap's internal global and all its run object, then recursively goes into
// run's own state, including stack, current frame, global object etc... After
// the scan, a wipe out operation will be performed.
//
// However, the issue is we use Rc in rust to manage the object, ie if the object
// gets removed from the current list, it is not guaranteed that the object is
// not alive additionally RC has referecence cycel issue, to address these issue,
// we will call a gc_reclaim function on each object been marked to be wiped out.
// And each object can use gc_reclaim to break the cycle, ie list can clear its
// internal list and object can clear the map etc ...
//
// Additionally, we have another problem, since the GC kicks in randomly, ie
// mostly in callsite of allocation, at that moment the GC root only consider
// the part of script, but not the native Rust, ie if we pops the object out
// of the root but are still using them on Rust's side, ie holding an Rc. The
// object is not actually removed. The typical solution for this is by using
// another wrapper to bridge the native root to the scanning root, ie V8 it
// uses XXXHandle method to register the object temporarily on native side into
// global root list. This doesn't work in Rust, since the Rust borrow rule makes
// this extreamly hard to work, the registeration requires mutation, and will
// have to be implemented as a RAII, which means the RAII object will need to
// have a mutate borrow of GHeap, while in that scope we are for sure gonna
// need a mutate borrow of GHeap to create object, :(. Anyway, currently we
// delay the actual GC operation until each instruction been interpreted done,
// since each instruction forms a solid atomic boundary, so after each bytecode
// been interpreted, the native side state should be convergented, however this
// may lead to extreaml rare memory consumption since user may allocate too many
// data temporarily but have to wait for safepoint to allow GC kicks in.

#[derive(Clone)]
pub struct GHeapConfig {
    pub gc_trigger_minimum_size: usize,
    pub gc_trigger_init_size: usize,
    pub gc_debug: Option<usize>,
    pub string_pool_gc_size: u32,
}

impl GHeapConfig {
    pub fn default() -> GHeapConfig {
        GHeapConfig {
            gc_trigger_minimum_size: 1000,
            gc_trigger_init_size: 5000,
            gc_debug: Option::None,
            string_pool_gc_size: 2000,
        }
    }
    pub fn dbg(s: usize, sp: u32) -> GHeapConfig {
        GHeapConfig {
            gc_trigger_minimum_size: 1000,
            gc_trigger_init_size: 5000,
            gc_debug: Option::Some(s),
            string_pool_gc_size: sp,
        }
    }
}

// GC trigger design.
//
// Each gc generation is triggered by heuristic information, we set an initial
// value and update them dyanmically based on each trigger's results. The main
// idea is as following :
//
// 1) when the memory growth out of a certain threshold,
//
//    we should try to do a GC, but the GC may not work, ie result in large chunk
//    of memory still been alive
//
// 2) Then we based on the result update the next trigger
//
// The gc just only have one triggers, ie the gc_threshold. The gc_threshold is
// been measured as object number for now, which is not ideal, but okay for us.
struct GCTrigger {
    // next gc threshold, when gc_current_size is larger than this we should do
    // gc operation, and it will be updated dynamically
    gc_threshold: usize,

    // when the size is lower than this number, gurantee to not trigger a gc
    gc_minimum_size: usize,

    // last gc's reclaim size
    gc_last_reclaim: usize,

    // debug usage
    gc_debug: Option<usize>,
}

pub struct GHeap {
    str_pool: StrPool,
    current: GCList,
    gc_trigger: GCTrigger,

    // global variables visiable for all running scripts,
    pub global: ObjRef,

    // list of Runptr's weak pointer
    run_list: Vec<WkRunptr>,

    // the GC will be delayed until the finish of each instruction, so we place
    // a flag here to indicate whether we should trigger gc when interpreter
    // feel fine to do so.
    gc_pending: bool,

    // string pool iteration
    string_pool_next: u32,
    string_pool_size: u32,
}

impl GHeap {
    pub fn new_with_config(c: GHeapConfig) -> GHeap {
        let mut g = GHeap {
            str_pool: StrPool::new(),
            current: GCList::new(),
            gc_trigger: GCTrigger {
                gc_threshold: c.gc_trigger_init_size,
                gc_minimum_size: c.gc_trigger_minimum_size,
                gc_last_reclaim: 0,
                gc_debug: c.gc_debug,
            },
            // this is just a workaround for Rc, we just need a place holder and
            // later on we will just replace this object
            global: Obj::new(),
            run_list: Vec::<WkRunptr>::new(),
            gc_pending: false,
            string_pool_next: 0,
            string_pool_size: c.string_pool_gc_size,
        };
        g.global = g.new_obj();
        g
    }

    pub fn new() -> GHeap {
        GHeap::new_with_config(GHeapConfig::default())
    }

    pub fn allocation_len(&self) -> usize {
        self.current.len()
    }

    // GC ======================================================================
    // We try to perform GC when the current gc list is too large. And nothing
    // special at all
    #[inline(always)]
    fn try_gc(&mut self) -> bool {
        if self.should_gc() {
            self.gc_pending = true;
            return true;
        }
        return false;
    }

    #[inline(always)]
    pub fn should_gc(&mut self) -> bool {
        match self.gc_trigger.gc_debug {
            Option::Some(v) => {
                return self.current.len() > v;
            }
            _ => return self.current.len() > self.gc_trigger.gc_threshold,
        };
    }

    // -------------------------------------------------------------------------
    // Scanning routine of the GC functions
    fn scan_root(&mut self) {
        // scanning G's global table
        self.global.borrow_mut().gc_mark();

        // scanning all Run's code
        let mut delete_list = Vec::<usize>::new();
        let mut idx = 0;
        for r in self.run_list.iter_mut() {
            match r.upgrade() {
                Option::Some(v) => {
                    // Notes if we cannot borrow it, it means some other one is
                    // borrowing it and it means it is been scanning for now, so
                    // we can just let it go
                    v.borrow_mut().gc_mark();
                }
                _ => {
                    delete_list.push(idx);
                }
            };
            idx += 1;
        }

        for i in delete_list.iter() {
            self.run_list.remove(*i);
        }
    }

    // invoked by interpreter to perform GC sparsely
    #[inline(always)]
    pub fn run_gc(&mut self) -> bool {
        if self.gc_pending {
            self.force_gc();
            self.gc_pending = false;
            return true;
        }
        return false;
    }

    pub fn force_gc(&mut self) -> u32 {
        let current_size = self.current.len();

        // (0) performing marking, ie from the root position
        self.scan_root();

        let mut death = 0;
        {
            let mut new_current = GCList::new();
            for v in self.current.iter_mut() {
                let should_finalize = *v.borrow().gcmark() == GCMark::White;
                if should_finalize {
                    death += 1;
                    v.borrow_mut().gc_finalize();
                    continue;
                } else {
                    v.borrow_mut().set_gc_mark_white();
                    new_current.push(Rc::clone(v));
                }
            }
            self.current = new_current;
        }

        // also flip all the run
        for r in self.run_list.iter_mut() {
            match r.upgrade() {
                Option::Some(v) => {
                    v.borrow_mut().set_gc_mark_white();
                }
                _ => (),
            };
        }

        // run string pool GC if needed
        {
            let (_, next) = self
                .str_pool
                .run_gc(self.string_pool_next, self.string_pool_size);

            self.string_pool_next = next;
        }

        // the reclaim size ratio, we treat that the collection cannot collect
        // at least 10% of garbage as none effective, ie it means we should
        // increase the threshold. We use a simple weighted formula as following
        //      dr         ratio
        // 1) [0  -10%]      -1
        // 2) [10%-20%]     -0.5
        // 3) [20%-40%]     0.5
        // 4) [40%-80%]     0.8
        // 5) [80%]         1.2
        //
        // the formula is as following :
        //   new_thershold =
        //      clamp(
        //          current_threshold * clamp(1-death_ratio*ratio),
        //          min_threshold
        //      );

        let death_ratio = death as f64 / current_size as f64;
        let ratio: f64;
        if death_ratio < 0.1 {
            ratio = -1.0;
        } else if death_ratio < 0.2 {
            ratio = -0.5;
        } else if death_ratio < 0.4 {
            ratio = 0.5;
        } else if death_ratio < 0.8 {
            ratio = 1.0;
        } else {
            ratio = 1.2;
        }

        let clamp = |v, min| {
            if v < min {
                min
            } else {
                v
            }
        };

        let current_threshold = self.gc_trigger.gc_threshold;

        let new_threshold = clamp(
            current_threshold as f64 * (1.0 - ratio * death_ratio),
            self.gc_trigger.gc_minimum_size as f64,
        ) as usize;

        self.gc_trigger.gc_last_reclaim = death;
        self.gc_trigger.gc_threshold = new_threshold;

        return death as u32;
    }

    // helper functions for gc marking
    pub fn gc_mark(h: &mut Handle) {
        match h {
            Handle::Str(x) => x.borrow_mut().gc_mark(),
            Handle::List(x) => x.borrow_mut().gc_mark(),
            Handle::Object(x) => x.borrow_mut().gc_mark(),
            Handle::Function(x) => x.borrow_mut().gc_mark(),
            Handle::NFunction(x) => x.borrow_mut().gc_mark(),
            Handle::Iter(x) => x.borrow_mut().gc_mark(),
            _ => (),
        };
    }

    // ------------------------------------------------------------------------
    // ------------------------------------------------------------------------
    // ------------------------------------------------------------------------
    // Factory for builtin types

    // ------------------------------------------------------------------------
    // string
    pub fn new_string(&mut self, s: String) -> StrRef {
        self.try_gc();

        match self.str_pool.get(&s) {
            Some(v) => {
                return v;
            }
            _ => {
                let str_value = Str::new(s);

                // update the index
                self.str_pool
                    .set(&str_value.borrow().string, Rc::downgrade(&str_value));

                // update the GC list
                let gcp = Rc::clone(&str_value);
                self.current.push(gcp);

                return str_value;
            }
        };
    }

    pub fn new_string_handle(&mut self, s: String) -> Handle {
        Handle::Str(self.new_string(s))
    }

    pub fn new_str(&mut self, s: &str) -> StrRef {
        return self.new_string(s.to_string());
    }

    pub fn new_str_handle(&mut self, s: &str) -> Handle {
        return self.new_string_handle(s.to_string());
    }

    // ------------------------------------------------------------------------
    // list
    pub fn new_list(&mut self) -> ListRef {
        self.try_gc();

        let list_value = List::new();

        let gcp = Rc::clone(&list_value);
        self.current.push(gcp);

        return list_value;
    }

    pub fn new_list_handle(&mut self) -> Handle {
        return Handle::List(self.new_list());
    }

    // ------------------------------------------------------------------------
    // pair
    // the pair is just specialized list with just 2 elements
    pub fn new_pair(&mut self, v0: Handle, v1: Handle) -> ListRef {
        let l = self.new_list();
        l.borrow_mut().add(v0);
        l.borrow_mut().add(v1);
        return l;
    }

    pub fn new_pair_handle(&mut self, v0: Handle, v1: Handle) -> Handle {
        return Handle::List(self.new_pair(v0, v1));
    }

    // ------------------------------------------------------------------------
    // object
    pub fn new_obj(&mut self) -> ObjRef {
        self.try_gc();

        let obj_value = Obj::new();

        let gcp = Rc::clone(&obj_value);
        self.current.push(gcp);
        return obj_value;
    }

    pub fn new_obj_handle(&mut self) -> Handle {
        return Handle::Object(self.new_obj());
    }

    // ------------------------------------------------------------------------
    // native function
    //
    // for native function, user could creat its concrete type and then register
    // into the GHeap for GC monitor purpose.
    pub fn watch_native_function(&mut self, nfunc: GCRef) {
        assert!(nfunc.borrow().otype() == OType::NFunction);
        self.current.push(nfunc);
    }

    // ------------------------------------------------------------------------
    // function
    pub fn new_function(
        &mut self,
        this_p: ProtoRc,
        _: &mut Runptr,
        uplist: HandleList,
    ) -> FuncRef {
        self.try_gc();

        let func_obj = Function::new(this_p, uplist);

        let gcp = Rc::clone(&func_obj);
        self.current.push(gcp);

        return func_obj;
    }

    pub fn new_main(&mut self, this_p: ProtoRc, run: &mut Runptr) -> FuncRef {
        return self.new_function(this_p, run, HandleList::new());
    }

    pub fn new_function_handle(
        &mut self,
        this_p: ProtoRc,
        run: &mut Runptr,
        uplist: HandleList,
    ) -> Handle {
        return Handle::Function(self.new_function(this_p, run, uplist));
    }

    pub fn new_main_handle(
        &mut self,
        this_p: ProtoRc,
        run: &mut Runptr,
    ) -> Handle {
        return Handle::Function(self.new_main(this_p, run));
    }

    // ------------------------------------------------------------------------
    // Run
    // notes the run will not been pushed into the current list but just keep
    // a weak pointer to it.
    pub fn new_run(&mut self, g: Gptr) -> Runptr {
        self.try_gc();

        let glb = self.new_obj();
        let stk = HandleList::new();

        let run_ptr = Run::new(g, glb, stk, default_trace_sinker);

        // add it to the list for future GC
        self.run_list.push(Rc::downgrade(&run_ptr));

        return run_ptr;
    }

    // ------------------------------------------------------------------------
    // StrIter
    pub fn new_str_iter(&mut self, s: StrRef) -> IterRef {
        self.try_gc();

        let itr_value = Rc::new(RefCell::new(StrIter::new(s)));

        let gcp = Rc::clone(&itr_value);
        self.current.push(gcp);
        return itr_value;
    }

    pub fn new_str_iter_handle(&mut self, s: StrRef) -> Handle {
        return Handle::Iter(self.new_str_iter(s));
    }

    // ------------------------------------------------------------------------
    // ListIter
    pub fn new_list_iter(&mut self, s: ListRef) -> IterRef {
        self.try_gc();

        let itr_value = Rc::new(RefCell::new(ListIter::new(s)));

        let gcp = Rc::clone(&itr_value);
        self.current.push(gcp);
        return itr_value;
    }

    pub fn new_list_iter_handle(&mut self, s: ListRef) -> Handle {
        return Handle::Iter(self.new_list_iter(s));
    }

    // ------------------------------------------------------------------------
    // ObjIter
    pub fn new_obj_iter(&mut self, s: ObjRef) -> IterRef {
        self.try_gc();

        let itr_value = Rc::new(RefCell::new(ObjIter::new(s)));

        let gcp = Rc::clone(&itr_value);
        self.current.push(gcp);
        return itr_value;
    }

    pub fn new_obj_iter_handle(&mut self, s: ObjRef) -> Handle {
        return Handle::Iter(self.new_obj_iter(s));
    }

    // ------------------------------------------------------------------------
    // global
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
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// TESTING
#[cfg(test)]
mod tests {
    use super::*;

    // testing of gc based object ----------------------------------------------G
    // string
    #[test]
    fn test_gheap_init() {
        let mut gheap = GHeap::new();
        assert_eq!(gheap.current.len(), 1);
        assert_eq!(gheap.global.borrow().len(), 0);
        assert!(!gheap.try_gc());
    }

    fn check_nil_gc(x: &dyn HRef) -> bool {
        *x.gcmark() == GCMark::White
    }

    fn check_nil_gc_iter(x: &dyn Iter) -> bool {
        *x.gcmark() == GCMark::White
    }

    fn eq_int(lhs: Handle, rhs: Handle) -> bool {
        match lhs {
            Handle::Int(lv) => match rhs {
                Handle::Int(rv) => return lv == rv,
                _ => (),
            },
            _ => (),
        };
        return false;
    }

    fn is_obj(v: &Handle) -> bool {
        match v {
            Handle::Object(_) => return true,
            _ => return false,
        };
    }

    fn get_str(v: Handle) -> String {
        match v {
            Handle::Str(v) => v.borrow().string.clone(),
            _ => {
                assert!(false);
                "".to_string()
            }
        }
    }

    fn get_int(v: Handle) -> i64 {
        match v {
            Handle::Int(v) => {
                return v;
            }
            _ => {
                assert!(false);
                0
            }
        }
    }

    fn is_list(v: &Handle) -> bool {
        match v {
            Handle::List(_) => return true,
            _ => return false,
        };
    }

    #[test]
    fn test_object_obj() {
        {
            let mut gheap = GHeap::new();
            let objref = gheap.new_obj();
            let keyref = gheap.new_str("Hello");
            assert_eq!(gheap.current.len(), 3);
            assert!(check_nil_gc(&*objref.borrow()));

            assert_eq!(0, objref.borrow().len());
            objref.borrow_mut().add(&keyref, Handle::Int(1));
            assert_eq!(1, objref.borrow().len());

            assert!(objref.borrow().has(&keyref));
            match objref.borrow().get(&keyref) {
                Some(v) => {
                    assert!(eq_int(v, Handle::Int(1)));
                }
                None => {
                    assert!(false);
                }
            };

            assert!(objref.borrow_mut().del(&keyref));
            assert_eq!(0, objref.borrow().len());

            match objref.borrow().get(&keyref) {
                Some(_) => assert!(false),
                None => assert!(true),
            };
        }
        {
            let mut gheap = GHeap::new();
            let objref = gheap.new_obj();
            let keyref = gheap.new_str("Hello");
            assert_eq!(gheap.current.len(), 3);

            assert_eq!(0, objref.borrow().len());
            objref.borrow_mut().add(&keyref, Handle::Int(1));
            assert_eq!(1, objref.borrow().len());
            objref.borrow_mut().clear();
            assert_eq!(0, objref.borrow().len());

            assert_eq!(gheap.current.len(), 3);
        }

        {
            let mut gheap = GHeap::new();
            let h = gheap.new_obj_handle();
            assert!(is_obj(&h));
        }
    }

    #[test]
    fn test_object_list() {
        {
            let mut gheap = GHeap::new();
            let list = gheap.new_list();
            assert_eq!(2, gheap.allocation_len());
            assert_eq!(0, list.borrow().len());
            list.borrow_mut().add(Handle::Int(100));
            assert_eq!(1, list.borrow().len());
            assert!(eq_int(list.borrow().index(0), Handle::Int(100)));
            list.borrow_mut().clear();
            assert_eq!(0, list.borrow().len());
            assert!(check_nil_gc(&*list.borrow()));
        }
        {
            let mut gheap = GHeap::new();
            let list = gheap.new_list_handle();
            assert!(is_list(&list));
        }
    }

    #[test]
    fn test_object_str() {
        {
            let mut gheap = GHeap::new();
            let list = gheap.new_str("Alloha");
            assert_eq!(2, gheap.allocation_len());
            assert!(check_nil_gc(&*list.borrow()));
            assert_eq!(list.borrow().string, "Alloha");
        }
        {
            let mut gheap = GHeap::new();
            let s = gheap.new_str_handle("Alloha");
            match s {
                Handle::Str(v) => {
                    assert_eq!(v.borrow().string, "Alloha");
                }
                _ => assert!(false),
            };
        }

        // string pool behavior, ie same string will always return the same
        // piece of allocation instead of using another one.
        {
            let mut gheap = GHeap::new();
            let s = gheap.new_str("a");
            let s2 = gheap.new_str("a");
            assert!(Rc::ptr_eq(&s, &s2));
            assert_eq!(s.borrow().string, "a");
            assert_eq!(1, gheap.str_pool.len());
        }
    }

    // a simple string factory method which bypass string pool. used to test
    // string pool implementation
    fn mk_str(s: &str) -> StrRef {
        return Str::new(s.to_string());
    }

    #[test]
    fn test_str_pool() {
        {
            let mut str_pool = StrPool::new();
            let str_ref = mk_str("Hello");
            {
                let weak_ref = Rc::downgrade(&str_ref);
                str_pool.set("Hello", weak_ref);
            }
            {
                let maybe_ref = str_pool.get("Hello");
                match maybe_ref {
                    Some(v) => {
                        assert_eq!(v.borrow().string, "Hello");
                        assert!(Rc::ptr_eq(&v, &str_ref));
                    }
                    _ => assert!(false),
                };
            }

            // duplicate sets
            {
                let _ = mk_str("Hello");
                let weak_ref = Rc::downgrade(&str_ref);
                str_pool.set("Hello", weak_ref);
            }

            {
                let maybe_ref = str_pool.get("Hello");
                match maybe_ref {
                    Some(v) => {
                        assert_eq!(v.borrow().string, "Hello");
                    }
                    _ => assert!(false),
                };
            }
        }

        // testing when corresponding strong_ref is gone, the weak_ref will be
        // dropped, ie cannot upgrade
        {
            let mut str_pool = StrPool::new();
            let str_ref = mk_str("Hello");
            {
                let weak_ref = Rc::downgrade(&str_ref);
                str_pool.set("Hello", weak_ref);
            }
            drop(str_ref);

            match str_pool.get("Hello") {
                None => (),
                _ => assert!(false),
            };
        }
    }

    #[test]
    fn test_object_str_iter() {
        {
            let mut gheap = GHeap::new();
            let string = gheap.new_str("Hello World");
            let itr = gheap.new_str_iter(string);
            assert!(check_nil_gc_iter(&*itr.borrow()));

            let mut run = Run::new_test(gheap);

            let mut idx = 0;
            let mut o = Vec::<String>::new();
            while itr.borrow_mut().has(&mut run) {
                idx += 1;
                match itr.borrow_mut().value(&mut run) {
                    Ok(v) => o.push(get_str(v)),
                    _ => assert!(false),
                };
                itr.borrow_mut().next(&mut run);
            }

            assert_eq!(idx, 11);
            assert_eq!(o.len(), 11);
            assert_eq!(
                run.borrow_mut().g.borrow_mut().heap.allocation_len(),
                12
            );
            assert_eq!("Hello World", o.join(""));
        }

        // empty string
        {
            let mut gheap = GHeap::new();
            let string = gheap.new_str("");
            let itr = gheap.new_str_iter(string);
            assert!(check_nil_gc_iter(&*itr.borrow()));

            let mut run = Run::new_test(gheap);

            let mut idx = 0;
            let mut o = Vec::<String>::new();
            while itr.borrow_mut().has(&mut run) {
                idx += 1;
                match itr.borrow_mut().value(&mut run) {
                    Ok(v) => o.push(get_str(v)),
                    _ => assert!(false),
                };
                itr.borrow_mut().next(&mut run);
            }

            assert_eq!(idx, 0);
            assert_eq!(o.len(), 0);
            assert_eq!("", o.join(""));
        }
    }

    #[test]
    fn test_object_list_iter() {
        {
            let mut gheap = GHeap::new();
            let list = gheap.new_list();
            list.borrow_mut().add(Handle::Int(1));
            list.borrow_mut().add(Handle::Int(2));
            list.borrow_mut().add(Handle::Int(3));

            let itr = gheap.new_list_iter(list);
            assert!(check_nil_gc_iter(&*itr.borrow()));

            let mut run = Run::new_test(gheap);

            let mut idx = 0;
            let mut o = Vec::<i64>::new();
            while itr.borrow_mut().has(&mut run) {
                idx += 1;
                match itr.borrow_mut().value(&mut run) {
                    Ok(v) => o.push(get_int(v)),
                    _ => assert!(false),
                };
                itr.borrow_mut().next(&mut run);
            }

            assert_eq!(idx, 3);
            assert_eq!(o.len(), 3);
            assert_eq!(o, vec![1, 2, 3]);
        }
        {
            let mut gheap = GHeap::new();
            let list = gheap.new_list();

            let itr = gheap.new_list_iter(list);
            assert!(check_nil_gc_iter(&*itr.borrow()));

            let mut run = Run::new_test(gheap);

            let mut idx = 0;
            let mut o = Vec::<i64>::new();
            while itr.borrow_mut().has(&mut run) {
                idx += 1;
                match itr.borrow_mut().value(&mut run) {
                    Ok(v) => o.push(get_int(v)),
                    _ => assert!(false),
                };
                itr.borrow_mut().next(&mut run);
            }

            assert_eq!(idx, 0);
            assert_eq!(o.len(), 0);
            assert_eq!(o, vec![]);
        }
    }

    #[test]
    fn test_object_object_iter() {
        {
            let mut gheap = GHeap::new();
            let obj = gheap.new_obj();
            obj.borrow_mut().add(&gheap.new_str("H1"), Handle::Int(0));
            obj.borrow_mut().add(&gheap.new_str("H2"), Handle::Int(1));

            let itr = gheap.new_obj_iter(obj);
            assert!(check_nil_gc_iter(&*itr.borrow()));

            let mut run = Run::new_test(gheap);

            let mut idx = 0;
            let mut o = Vec::<(String, i64)>::new();
            while itr.borrow_mut().has(&mut run) {
                idx += 1;
                match itr.borrow_mut().value(&mut run) {
                    Ok(v) => {
                        // the returned value is a pair, ie [Handle, Handle]
                        match v {
                            Handle::List(v) => {
                                assert_eq!(v.borrow().len(), 2);
                                o.push((
                                    get_str(v.borrow().index(0)),
                                    get_int(v.borrow().index(1)),
                                ));
                            }
                            _ => assert!(false),
                        };
                    }
                    _ => assert!(false),
                };
                itr.borrow_mut().next(&mut run);
            }

            assert_eq!(idx, 2);
            assert_eq!(o.len(), 2);
            assert_eq!(o, vec![("H1".to_string(), 0), ("H2".to_string(), 1)]);
        }

        {
            let mut gheap = GHeap::new();
            let obj = gheap.new_obj();
            let itr = gheap.new_obj_iter(obj);
            assert!(check_nil_gc_iter(&*itr.borrow()));

            let mut run = Run::new_test(gheap);

            let mut idx = 0;
            while itr.borrow_mut().has(&mut run) {
                idx += 1;
                match itr.borrow_mut().value(&mut run) {
                    Ok(v) => {
                        // the returned value is a pair, ie [Handle, Handle]
                        match v {
                            Handle::List(_) => assert!(false),
                            _ => assert!(false),
                        };
                    }
                    _ => assert!(false),
                };
                itr.borrow_mut().next(&mut run);
            }

            assert_eq!(idx, 0);
        }
    }

    // GC testing --------------------------------------------------------------
    // notes this GC testing is mainly for testing GC's own correctness. as with
    // integration test with interpreter is done externally

    // GC related testing
    #[test]
    fn test_gc_finalize() {
        // all the object is been allocated and put on the native stack, so no
        // run is alive, therefore the GC will/shoud collect everything
        let mut gheap = GHeap::new();
        let o1 = gheap.new_obj();
        let o2 = gheap.new_list();
        let o3 = gheap.new_str("Hello World");
        // later on we can check wether finalize is invoked or not
        o1.borrow_mut().add(&o3, Handle::Int(1));
        o2.borrow_mut().add(Handle::Int(100));

        assert_eq!(gheap.allocation_len(), 4);
        assert_eq!(gheap.force_gc(), 3);

        // finalize should be invoked
        assert_eq!(o2.borrow().len(), 0);
        assert_eq!(o1.borrow().len(), 0);
    }

    #[test]
    fn test_gc_cross_reference() {
        let mut gheap = GHeap::new();
        let o1 = gheap.new_obj();
        let o2 = gheap.new_list();

        // now let's do a cross reference, ie cyclical reference
        {
            let dup = ObjRef::clone(&o1);
            o2.borrow_mut().add(Handle::Object(dup));
        }
        {
            let dup = ListRef::clone(&o2);
            o1.borrow_mut().add(&gheap.new_str("X"), Handle::List(dup));
        }
        assert_eq!(gheap.force_gc(), 3);
        assert_eq!(o1.borrow().len(), 0);
        assert_eq!(o2.borrow().len(), 0);
    }

    #[test]
    fn test_gc_with_run() {
        let gptr = G::new(GHeapConfig::default());
        let run;

        // create a runptr
        {
            let dup = Gptr::clone(&gptr);
            run = gptr.borrow_mut().heap.new_run(dup);
        }

        // the following will be collected since they are not been referenced by
        // any run's internal gut
        // count == 3
        gptr.borrow_mut().heap.new_str("a");
        gptr.borrow_mut().heap.new_list();
        gptr.borrow_mut().heap.new_obj();

        // this single object will be referenced by the GHeap's global object
        // count == 1
        {
            let x = gptr.borrow_mut().heap.new_str("X");
            gptr.borrow_mut().global_add(&x, Handle::Int(1));
        }

        // this single object will be referenced by run's stack
        {
            let x = gptr.borrow_mut().heap.new_str("Y");
            run.borrow_mut().stack.push(Handle::Str(x));
        }

        // this single object will be referenced by run's global table
        {
            let x = gptr.borrow_mut().heap.new_str("Z");
            run.borrow_mut().global_add(&x, Handle::Int(2));
        }

        assert_eq!(gptr.borrow_mut().heap.force_gc(), 3);
    }
}
