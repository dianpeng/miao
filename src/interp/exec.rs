use std::rc::Rc;

use crate::bc::bytecode::*;
use crate::ic::ftype::*;
use crate::object::object::*;

pub struct Arithmetic;
pub struct Unary;
pub struct Comparison;
pub struct Conversion;
struct Util;

pub struct Exec {
    error_msg: Option<Verror>,
}

enum CallFunc {
    Script(FuncRef),
    Native(NFuncRef),
}

impl Exec {
    fn push_val(&mut self, run: &mut Runptr, v: Handle) {
        run.borrow_mut().stack.push(v);
    }
    fn top(&mut self, off: usize, run: &mut Runptr) -> Handle {
        let b = run.borrow();
        let stack = &b.stack;

        assert!((off + 1) <= stack.len());
        let idx = stack.len() - off - 1;
        return stack[idx].clone();
    }
    fn top0(&mut self, run: &mut Runptr) -> Handle {
        return self.top(0, run);
    }
    fn top1(&mut self, run: &mut Runptr) -> Handle {
        return self.top(1, run);
    }
    fn top2(&mut self, run: &mut Runptr) -> Handle {
        return self.top(2, run);
    }
    fn replace(&mut self, run: &mut Runptr, v: Handle) {
        self.pop(run);
        self.push_val(run, v);
    }

    // ========================================================================
    // interpreter handler :
    //   1) arithmetic bytecode handler
    fn bin_result(&mut self, run: &mut Runptr, v: Handle) -> bool {
        self.pop_n(2, run);
        self.push_val(run, v);
        true
    }
    fn una_result(&mut self, run: &mut Runptr, v: Handle) -> bool {
        self.pop(run);
        self.push_val(run, v);
        true
    }

    fn add(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Arithmetic::add(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn sub(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Arithmetic::sub(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn mul(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Arithmetic::mul(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn div(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Arithmetic::div(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }
    fn mod_(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Arithmetic::mod_(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn pow(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Arithmetic::pow(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    // Comparison
    fn eq(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Comparison::eq(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn ne(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Comparison::ne(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn gt(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Comparison::gt(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn ge(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Comparison::ge(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn lt(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Comparison::lt(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    fn le(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let lhs = self.top1(run);
        let rhs = self.top0(run);
        return match Comparison::le(lhs, rhs, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.bin_result(run, v),
        };
    }

    // unary
    fn not(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let v = self.top0(run);
        return match Unary::not(v, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.una_result(run, v),
        };
    }
    fn neg(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let v = self.top0(run);
        return match Unary::neg(v, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.una_result(run, v),
        };
    }
    fn typeof_(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let v = self.top0(run);
        return match Unary::typeof_(v, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.una_result(run, v),
        };
    }
    fn sizeof(&mut self, run: &mut Runptr, pc: usize) -> bool {
        let v = self.top0(run);
        return match Unary::sizeof(v, run, pc) {
            Result::Err(v) => self.interp_err(v),
            Result::Ok(v) => self.una_result(run, v),
        };
    }

    fn boolean(&mut self, run: &mut Runptr) -> bool {
        let v = self.top0(run);
        let value = Conversion::to_boolean(&v, run);
        return self.una_result(run, Handle::Boolean(value));
    }

    fn con_str(&mut self, num: u32, run: &mut Runptr) -> bool {
        let mut out = String::new();
        {
            let b = run.borrow();
            let stack = &b.stack;
            let len = stack.len() as u32;
            let mut start = len - num;

            while start < len {
                let v = stack[start as usize].clone();
                match Conversion::to_rust_string(&v) {
                    Option::Some(s) => {
                        out.push_str(&s);
                    }
                    Option::None => {
                        self.error(format!(
                            "cannot convert {} to string",
                            handle_type_name(&v),
                        ));
                        return false;
                    }
                };

                start += 1;
            }
        }

        self.pop_n(num as usize, run);
        {
            let str =
                run.borrow_mut().g.borrow_mut().heap.new_string_handle(out);
            self.push_val(run, str);
        }
        return true;
    }

    fn load_int(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let v = run.borrow().rcall_ref().borrow().proto.code.load_int(a0);
        self.push_val(run, Handle::Int(v));
        return true;
    }
    fn load_real(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let v = run.borrow().rcall_ref().borrow().proto.code.load_real(a0);
        self.push_val(run, Handle::Real(v));
        return true;
    }

    fn from_string_index(&mut self, a0: u32, run: &mut Runptr) -> StrRef {
        let v = run.borrow().rcall_ref().borrow().proto.code.load_str(a0);
        return run.borrow().g.borrow_mut().heap.new_string(v);
    }

    fn load_string(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let v = run.borrow().rcall_ref().borrow().proto.code.load_str(a0);
        let str_handle = run.borrow().g.borrow_mut().heap.new_string_handle(v);
        self.push_val(run, str_handle);
        return true;
    }
    fn load_function(&mut self, a0: u32, run: &mut Runptr, off: u32) -> bool {
        let v = run.borrow().rcall_ref().borrow().proto.code.load_proto(a0);
        let rcall = {
            let run_borrow = run.borrow();
            run_borrow.rcall.as_ref().unwrap().clone()
        };

        // notes we have to populate the upvalue here otherwise it will panic
        // because of the mutable borrow already been borrowed while during
        // initialization of the function
        let mut upvalue_list = HandleList::new();
        for x in v.upvalue.iter() {
            if x.is_stack {
                upvalue_list
                    .push(run.borrow().stack[(x.index + off) as usize].clone());
            } else {
                upvalue_list.push(rcall.borrow().get_upvalue(x.index));
            }
        }

        let func = {
            let run_borrow = run.borrow_mut();
            let mut g_borrow = run_borrow.g.borrow_mut();
            let x = g_borrow.heap.new_function_handle(v, upvalue_list);
            x
        };

        self.push_val(run, func);
        return true;
    }

    fn load_null(&mut self, run: &mut Runptr) -> bool {
        self.push_val(run, Handle::Null);
        return true;
    }
    fn load_true(&mut self, run: &mut Runptr) -> bool {
        self.push_val(run, Handle::Boolean(true));
        return true;
    }
    fn load_false(&mut self, run: &mut Runptr) -> bool {
        self.push_val(run, Handle::Boolean(false));
        return true;
    }
    fn load_upvalue(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let v = run.borrow().rcall_ref().borrow().get_upvalue(a0);
        self.push_val(run, v);
        return true;
    }
    fn set_upvalue(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let top = self.top0(run);
        run.borrow_mut()
            .rcall_ref()
            .borrow_mut()
            .set_upvalue(a0, top);
        self.pop(run);
        return true;
    }

    fn list_start(&mut self, run: &mut Runptr) -> bool {
        let v = run.borrow().g.borrow_mut().heap.new_list_handle();
        self.push_val(run, v);
        return true;
    }
    fn list_add(&mut self, a0: u32, run: &mut Runptr) -> bool {
        // counting up to a0 element from the top of the stack
        let list_handle = self.top(a0 as usize, run);
        match list_handle {
            Handle::List(v) => {
                {
                    let mut mut_ref = run.borrow_mut();
                    let stack = &mut mut_ref.stack;
                    let len = stack.len() as u32;
                    let mut idx = len - a0;
                    while idx < len {
                        v.borrow_mut().add(stack[idx as usize].clone());
                        idx += 1;
                    }
                    if a0 != 0 {
                        stack.truncate((len - a0) as usize);
                    }
                }
                debug_assert!(handle_is_list(&self.top0(run)));
            }
            _ => unreachable!(),
        };
        return true;
    }

    fn object_start(&mut self, run: &mut Runptr) -> bool {
        let v = run.borrow().g.borrow_mut().heap.new_obj_handle();
        self.push_val(run, v);
        return true;
    }
    fn object_add(&mut self, a0: u32, run: &mut Runptr) -> bool {
        // counting up to a0 element from the top of the stack
        let obj_handle = self.top((a0 * 2) as usize, run);
        match obj_handle {
            Handle::Object(v) => {
                {
                    let mut mut_ref = run.borrow_mut();
                    let gptr = Gptr::clone(&mut_ref.g);

                    let stack = &mut mut_ref.stack;
                    let len = stack.len() as u32;
                    let mut idx = len - a0 * 2;

                    while idx < len {
                        match Conversion::to_rust_string(&stack[idx as usize]) {
                            Option::Some(kk) => {
                                let key = gptr.borrow_mut().heap.new_string(kk);
                                v.borrow_mut().add(
                                    &key,
                                    stack[(idx + 1) as usize].clone(),
                                );
                            }
                            _ => {
                                self.err("key cannot be converetd to string");
                                return false;
                            }
                        };

                        idx += 2;
                    }
                    if a0 != 0 {
                        stack.truncate((len - a0 * 2) as usize);
                    }
                }
                debug_assert!(handle_is_object(&self.top0(run)));
            }
            _ => unreachable!(),
        };
        return true;
    }

    // Iterator ==============================================================
    fn iterator_new(&mut self, run: &mut Runptr) -> bool {
        let top = self.top0(run);
        let iter_handle = match top {
            Handle::Str(x) => {
                run.borrow().g.borrow_mut().heap.new_str_iter_handle(x)
            }
            Handle::Object(x) => {
                run.borrow().g.borrow_mut().heap.new_obj_iter_handle(x)
            }
            Handle::List(x) => {
                run.borrow().g.borrow_mut().heap.new_list_iter_handle(x)
            }
            _ => {
                return self.error(format!(
                    "type {} doesn't support iterator",
                    handle_type_name(&top)
                ));
            }
        };

        self.replace(run, iter_handle);
        return true;
    }

    fn iterator_start(&mut self, run: &mut Runptr) -> bool {
        return self.iterator_has(run);
    }

    fn iterator_has(&mut self, run: &mut Runptr) -> bool {
        let itr_handle = self.top0(run);
        match itr_handle {
            Handle::Iter(v) => {
                let value = Handle::Boolean(v.borrow().has(run));
                self.push_val(run, value);
            }
            _ => {
                println!(
                    "{} {}",
                    handle_type_name(&itr_handle),
                    run.borrow().dump_stack()
                );
                unreachable!();
            }
        };
        return true;
    }

    fn iterator_next(&mut self, run: &mut Runptr) -> bool {
        let itr_handle = self.top0(run);
        match itr_handle {
            Handle::Iter(v) => {
                let cond = v.borrow_mut().next(run);
                self.push_val(run, Handle::Boolean(cond));
            }
            _ => {
                unreachable!();
            }
        };
        return true;
    }

    fn iterator_value(&mut self, run: &mut Runptr) -> bool {
        let itr_handle = self.top0(run);
        match itr_handle {
            Handle::Iter(v) => {
                match v.borrow().value(run) {
                    Result::Ok(v) => {
                        self.push_val(run, v);
                        return true;
                    }
                    Result::Err(e) => {
                        self.iter_error(e);
                        return false;
                    }
                };
            }
            _ => {
                unreachable!();
            }
        };
    }

    // stack related
    fn pop(&mut self, run: &mut Runptr) -> bool {
        run.borrow_mut().stack.pop().unwrap();
        return true;
    }
    fn pop_n(&mut self, len: usize, run: &mut Runptr) -> bool {
        let mut b = run.borrow_mut();
        let stack = &mut b.stack;
        assert!(len <= stack.len());
        let left = stack.len() - len;
        stack.truncate(left);
        return true;
    }
    fn dup(&mut self, run: &mut Runptr) -> bool {
        let tos = self.top0(run);
        self.push_val(run, tos);
        return true;
    }
    fn dup2(&mut self, run: &mut Runptr) -> bool {
        let t0 = self.top0(run);
        let t1 = self.top1(run);

        // order matters
        self.push_val(run, t1);
        self.push_val(run, t0);
        return true;
    }
    fn push_n(&mut self, len: usize, run: &mut Runptr) -> bool {
        let mut b = run.borrow_mut();
        let stack = &mut b.stack;
        let old_len = stack.len();
        stack.resize(old_len + len, Handle::Null);
        return true;
    }

    // move the TOS to specified slot
    fn store(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let mut b = run.borrow_mut();
        let stack = &mut b.stack;
        assert!(a0 < stack.len() as u32);
        let tos = stack[stack.len() - 1].clone();
        stack[a0 as usize] = tos;
        stack.pop();
        return true;
    }

    // load the value specified at position a0 to TOS
    fn load(&mut self, a0: u32, run: &mut Runptr) -> bool {
        let data = run.borrow().stack[a0 as usize].clone();
        self.push_val(run, data);
        return true;
    }

    fn dot_access(&mut self, idx: u32, run: &mut Runptr) -> bool {
        let key = self.from_string_index(idx, run);
        let top = self.top0(run);
        self.pop(run);

        match top {
            Handle::Object(v) => {
                match v.borrow().get(&key) {
                    Option::Some(v) => {
                        self.push_val(run, v);
                        return true;
                    }
                    Option::None => {
                        self.error(format!(
                            "key {} is not found in object",
                            key.borrow().string
                        ));
                        return false;
                    }
                };
            }
            _ => {
                self.err("expect an object to perform dot operator");
                return false;
            }
        };
    }

    fn dot_store(&mut self, idx: u32, run: &mut Runptr) -> bool {
        let expr = self.top0(run);
        let obj = self.top1(run);
        let key = self.from_string_index(idx, run);

        self.pop_n(2, run);

        match obj {
            Handle::Object(v) => {
                v.borrow_mut().add(&key, expr);
                return true;
            }
            _ => {
                self.err("expect an object to perform dot operator assignment");
                return false;
            }
        };
    }

    fn array_index(&mut self, run: &mut Runptr) -> bool {
        let index = self.top0(run);
        let target = self.top1(run);
        self.pop_n(2, run);

        // allowing array index to be performed on following types:
        //   1) object
        //   2) array
        //   3) string
        match target {
            Handle::Object(v) => {
                let key = Conversion::to_string(run, &index);
                match key {
                    Option::Some(k) => {
                        match v.borrow().get(&k) {
                            Option::Some(vv) => {
                                self.push_val(run, vv);
                                return true;
                            }
                            _ => {
                                self.error(format!(
                                    "key {} is not found in object",
                                    k.borrow().string
                                ));
                                return false;
                            }
                        };
                    }
                    _ => {
                        self.err("key cannot be converted to string for object access");
                        return false;
                    }
                };
            }
            Handle::List(v) => {
                let idx = Conversion::to_index(&index, run);
                match idx {
                    Option::Some(k) => {
                        if k >= v.borrow().len() {
                            self.err("index out of bound");
                            return false;
                        }
                        self.push_val(run, v.borrow().index(k));
                        return true;
                    }
                    _ => {
                        self.err(
                            "index cannot be converted to valid index integer",
                        );
                        return false;
                    }
                };
            }
            Handle::Str(v) => {
                let idx = Conversion::to_index(&index, run);
                match idx {
                    Option::Some(k) => {
                        let borrow = v.borrow();
                        let mut chars = borrow.string.chars();
                        match chars.nth(k) {
                            Option::Some(c) => {
                                let c_str = run
                                    .borrow()
                                    .g
                                    .borrow_mut()
                                    .heap
                                    .new_string_handle(c.to_string());
                                self.push_val(run, c_str);
                                return true;
                            }
                            _ => {
                                self.err("index out of range for string index");
                                return false;
                            }
                        };
                    }
                    _ => {
                        self.err(
                            "index cannot be converted to valid index integer",
                        );
                        return false;
                    }
                };
            }
            _ => {
                self.err("invalid type for index operator");
                return false;
            }
        };
    }

    fn array_store(&mut self, run: &mut Runptr) -> bool {
        let expr = self.top0(run);
        let index = self.top1(run);
        let target = self.top2(run);
        self.pop_n(3, run);

        // allowing array index to be performed on following types:
        //   1) object
        //   2) array
        //
        // notes strings are immutable
        match target {
            Handle::Object(v) => {
                let key = Conversion::to_string(run, &index);
                match key {
                    Option::Some(k) => {
                        v.borrow_mut().add(&k, expr);
                        return true;
                    }
                    _ => {
                        self.err(
                            "key cannot be converted to string \
                                 for object component assignment",
                        );
                        return false;
                    }
                };
            }
            Handle::List(v) => {
                let idx = Conversion::to_index(&index, run);
                match idx {
                    Option::Some(k) => {
                        v.borrow_mut().assign(k as u32, expr);
                        return true;
                    }
                    _ => {
                        self.err(
                            "index cannot be converted to valid index integer",
                        );
                        return false;
                    }
                };
            }
            _ => {
                self.err("invalid type for index operator");
                return false;
            }
        };
    }

    // Calling convention materialization ======================================
    // call and return
    //
    // the call is been compiled into following sequences and this is our interp
    // calling frame for each calling operation
    //
    // [argN]
    // [argN-1]
    // ...
    // [arg0]
    // [target] <--- the function object we want to invoke, aliased with rcall
    //               register
    //
    // the return operation will learn how many arguments should be poped to
    // reach the [target] slot and it will need to resume the target slot and
    // then return back to the target. Notes the interpreter uses some native
    // stack to keep information, ie the interp will go to the top of the interp
    // loop to do the interpretation and keep the current PC etc on the stack.
    //
    // If yield happen the running information will be carried towards the Run
    // object itself, though we don't support yield/resume's style coroutine
    // for now but we do gonna support them

    // this function will setup the calling frame and extract the target function
    // from the stack and return back. The interp function will take care of the
    // rest to setup the new frame and enter the call of new function again.
    fn call(
        &mut self,
        arg_count: u32,
        run: &mut Runptr,
    ) -> Result<CallFunc, Verror> {
        let target = self.top(arg_count as usize, run);
        match target {
            Handle::Function(x) => {
                let exp_argcount = x.borrow().proto.argument_count;
                if exp_argcount != arg_count {
                    return Result::Err(Verror::from_str(format!(
                        "function argument mismatch, \
                            expect {} but got {}",
                        exp_argcount, arg_count
                    )));
                }
                return Result::Ok(CallFunc::Script(x));
            }
            Handle::NFunction(x) => {
                return Result::Ok(CallFunc::Native(x));
            }
            xx @ _ => {
                return Result::Err(Verror::from_str(format!(
                    "cannot invoke type {} as function",
                    handle_type_name(&xx)
                )));
            }
        };
    }

    fn ret_native(&mut self, arg_count: u32, run: &mut Runptr) {
        debug_assert!(handle_is_native_function(
            &self.top((arg_count + 1) as usize, run)
        ));
    }

    fn ret(&mut self, pop_n: u32, run: &mut Runptr) -> Handle {
        // before return instruction performed, we should have following stack
        // frame :
        //
        // [top]                  <-- return value
        // -------------
        // [local N]
        // [local N-1]
        // ....
        // [local 0]
        // -------------          <-- local variables
        // [arg N]
        // [arg N-1]
        // ....
        // [arg 0]
        // -------------          <-- argument, can be treated as local variables
        // [target]               <-- current calling functions
        //
        // the pop_n represents stack size of #local + #argument, so once we
        // go back, we should just move the return value out of the stack and
        // return it via ret's return value.

        let ret_val = self.top0(run);

        debug_assert!(handle_is_function(&self.top((pop_n + 1) as usize, run)));

        return ret_val;
    }

    // control flow transferring instruction.
    fn jump_false(&mut self, run: &mut Runptr) -> bool {
        let tos = self.top0(run);
        self.pop(run);
        return !Conversion::to_boolean(&tos, run);
    }

    // and jump, it takes the TOS as condition to check, if it is false, then
    // it perform the jumps and keep the value on the stack (mutated as boolean)
    // otherwise, it pop the value and then ignore the jump
    fn and(&mut self, run: &mut Runptr) -> bool {
        let cond = self.top0(run);
        let cond_boolean = Conversion::to_boolean(&cond, run);
        if !cond_boolean {
            self.replace(run, Handle::Boolean(false));
            return true;
        }
        self.pop(run);

        return false;
    }

    fn or(&mut self, run: &mut Runptr) -> bool {
        let cond = self.top0(run);
        let cond_boolean = Conversion::to_boolean(&cond, run);
        if cond_boolean {
            self.replace(run, Handle::Boolean(true));
            return true;
        }
        self.pop(run);
        return false;
    }

    fn ternary(&mut self, run: &mut Runptr) -> bool {
        let cond = self.top0(run);
        let cond_boolean = Conversion::to_boolean(&cond, run);
        self.pop(run);

        // a ternary jump jump when the condition evaluation fails, ie the jump
        // targets at the |else| branch inside of the ternary expression
        return !cond_boolean;
    }

    // global loading. for each run we allow user to intercept the global
    // and we have a shared global table for all run
    fn load_global(&mut self, i: u32, run: &mut Runptr) -> bool {
        let name = self.from_string_index(i, run);
        let dup_run = Rc::clone(run);

        // (0) finding the global from the run pointer
        {
            let v = dup_run.borrow().global.borrow().get(&name);
            match v {
                Option::Some(v) => {
                    self.push_val(run, v);
                    return true;
                }
                _ => (),
            };
        }

        // (1) finding out the global variable from the shared table
        {
            let v = dup_run.borrow().g.borrow().heap.global.borrow().get(&name);
            match v {
                Option::Some(v) => {
                    self.push_val(run, v);
                    return true;
                }
                _ => (),
            };
        }

        // (2) not found
        self.error(format!(
            "global variable {} is not found",
            name.borrow().string
        ));
        return false;
    }

    fn set_global(&mut self, i: u32, run: &mut Runptr) -> bool {
        let name = self.from_string_index(i, run);
        let top = self.top0(run);
        self.pop(run);
        run.borrow_mut().global.borrow_mut().add(&name, top);
        return true;
    }

    // builtins
    fn assert1(&mut self, run: &mut Runptr) -> bool {
        let top = self.top0(run);
        self.pop(run);
        return Conversion::to_boolean(&top, run);
    }

    fn assert2(&mut self, run: &mut Runptr) -> Option<String> {
        let msg = self.top0(run);
        let cond = self.top1(run);
        self.pop_n(2, run);
        if !Conversion::to_boolean(&cond, run) {
            return Option::Some(Conversion::debug_string(&msg, run));
        } else {
            return Option::None;
        }
    }

    fn halt(&mut self, run: &mut Runptr) -> Handle {
        let top = self.top0(run);
        self.pop(run);
        return top;
    }

    fn trace(&mut self, count: u32, run: &mut Runptr) -> Option<Verror> {
        let sinker = run.borrow().trace_sinker.clone();
        let x = sinker(run, count);
        self.pop_n(count as usize, run);
        x
    }

    // =====================================================================
    // error handling
    fn error(&mut self, msg: String) -> bool {
        self.error_msg = Option::Some(Verror { description: msg });
        return false;
    }

    fn err(&mut self, msg: &str) -> bool {
        self.error(msg.to_string())
    }

    fn interp_err(&mut self, e: Verror) -> bool {
        self.error_msg = Option::Some(e);
        return false;
    }

    fn iter_error(&mut self, e: Verror) -> bool {
        self.error_msg = Option::Some(e);
        return false;
    }

    // used to generate the final return from the interp function
    fn generate_return(
        &mut self,
        run: &mut Runptr,
        rt: ResultType,
    ) -> Result<Vresult, Verror> {
        match &self.error_msg {
            Option::Some(v) => {
                return Result::Err(Verror {
                    description: run.borrow().stacktrace(format!(
                        "execution error: {}",
                        v.description
                    )),
                });
            }
            _ => {
                // at least the stack has 2 values, one is the caller FunRef
                // and the other is the return value
                let v = run.borrow().stack.last().unwrap().clone();
                return Result::Ok(Vresult {
                    value: v,
                    result_type: rt,
                });
            }
        };
    }

    pub fn interp(
        &mut self,
        run: &mut Runptr,
        main: ProtoRc,
    ) -> Result<Vresult, Verror> {
        assert!(main.argument_count == 0);

        let main_func = run.borrow_mut().g.borrow_mut().heap.new_main(main);

        let gptr = Gptr::clone(&run.borrow().g);

        // (1) setup the calling frame for now, notes the |current| frame is
        //     been implicitly defined on the stack of interp
        let mut pc: usize = 0;
        let mut current = main_func;

        let mut stop: bool = false;
        let mut stop_reason = ResultType::Return;
        let mut new_frame: bool = false;

        // used to indicate the frame starting point, like x86's ebp register,
        // [ localN]
        //   ...
        // [ local0]
        // [ argN ]
        //   ...
        // [ arg0 ]    <----- offset,
        // [ caller ]
        //
        // Initially we will place the top level function's Function object on
        // the stack, so the offset should be 1 instead 0
        let mut offset: u32 = 1; // used to offset move operation

        // place the current calling function onto the run's stack, this will
        // generate a Frame marker on the stack for later usage.
        self.push_val(run, Handle::Function(Rc::clone(&current)));
        run.borrow_mut().enter_script_frame(Rc::clone(&current));

        // (1) starts to run the function's code
        while !stop {
            // workaround the borrow checker :(
            let cptr = Rc::clone(&current);
            let bc_len = cptr.borrow().proto.code.array.len();

            // internal interpretation loop, represent a execution frame and
            // been controlled by new_frame flag.
            while pc < bc_len {
                debug_assert!(!stop);
                // will have to do the copy, otherwise the GCMark will try to
                // borrow the current locked function, since bytecode array
                // belongs to the Function object which violates the borrow rule
                let bytecode = cptr.borrow().proto.code.array[pc].clone();
                pc += 1;

                // try gc here
                gptr.borrow_mut().heap.run_gc();

                if !match bytecode {
                    Bytecode::Add => self.add(run, pc),
                    Bytecode::Sub => self.sub(run, pc),
                    Bytecode::Mul => self.mul(run, pc),
                    Bytecode::Div => self.div(run, pc),
                    Bytecode::Mod => self.mod_(run, pc),
                    Bytecode::Pow => self.pow(run, pc),
                    Bytecode::Eq => self.eq(run, pc),
                    Bytecode::Ne => self.ne(run, pc),
                    Bytecode::Gt => self.gt(run, pc),
                    Bytecode::Ge => self.ge(run, pc),
                    Bytecode::Lt => self.lt(run, pc),
                    Bytecode::Le => self.le(run, pc),
                    Bytecode::Not => self.not(run, pc),
                    Bytecode::Neg => self.neg(run, pc),
                    Bytecode::Typeof => self.typeof_(run, pc),
                    Bytecode::Sizeof => self.sizeof(run, pc),

                    Bytecode::Boolean => self.boolean(run),

                    Bytecode::LoadInt(i) => self.load_int(i, run),
                    Bytecode::LoadReal(i) => self.load_real(i, run),
                    Bytecode::LoadString(i) => self.load_string(i, run),
                    Bytecode::ConStr(i) => self.con_str(i, run),
                    Bytecode::LoadFunction(i) => {
                        self.load_function(i, run, offset)
                    }
                    Bytecode::LoadNull => self.load_null(run),
                    Bytecode::LoadTrue => self.load_true(run),
                    Bytecode::LoadFalse => self.load_false(run),

                    Bytecode::LoadUpvalue(i) => self.load_upvalue(i, run),
                    Bytecode::SetUpvalue(i) => self.set_upvalue(i, run),

                    Bytecode::ListStart => self.list_start(run),
                    Bytecode::ListAdd(v) => self.list_add(v, run),

                    Bytecode::ObjectStart => self.object_start(run),
                    Bytecode::ObjectAdd(v) => self.object_add(v, run),

                    Bytecode::IteratorNew => self.iterator_new(run),
                    Bytecode::IteratorStart => self.iterator_start(run),
                    Bytecode::IteratorHas => self.iterator_has(run),
                    Bytecode::IteratorNext => self.iterator_next(run),
                    Bytecode::IteratorValue => self.iterator_value(run),

                    Bytecode::LoadGlobal(i) => self.load_global(i, run),
                    Bytecode::SetGlobal(i) => self.set_global(i, run),

                    Bytecode::Pop => self.pop(run),
                    Bytecode::PopN(i) => self.pop_n(i as usize, run),
                    Bytecode::Dup => self.dup(run),
                    Bytecode::Dup2 => self.dup2(run),
                    Bytecode::PushN(i) => {
                        let mut r = true;
                        if i != 0 {
                            r = self.push_n(i as usize, run);
                        }
                        r
                    }
                    Bytecode::Store(i) => self.store(i + offset, run),
                    Bytecode::Load(i) => self.load(i + offset, run),
                    Bytecode::DotAccess(i) => self.dot_access(i, run),
                    Bytecode::DotStore(i) => self.dot_store(i, run),
                    Bytecode::ArrayIndex => self.array_index(run),
                    Bytecode::ArrayStore => self.array_store(run),

                    // Code branching handling
                    Bytecode::And(cp) => {
                        if self.and(run) {
                            pc = cp as usize;
                        }
                        true
                    }
                    Bytecode::Or(cp) => {
                        if self.or(run) {
                            pc = cp as usize;
                        }
                        true
                    }
                    Bytecode::Ternary(cp) => {
                        if self.ternary(run) {
                            pc = cp as usize;
                        }
                        true
                    }
                    Bytecode::JumpFalse(cp) => {
                        if self.jump_false(run) {
                            pc = cp as usize;
                        }
                        true
                    }
                    Bytecode::LoopBack(cp) => {
                        pc = cp as usize;
                        true
                    }
                    Bytecode::Jump(cp) => {
                        pc = cp as usize;
                        true
                    }

                    // Calling handling
                    Bytecode::Call(i) => {
                        // update the current frame's information, notes, this
                        // is updated on demand whenever a call is issued.
                        run.borrow_mut().update_current_frame(pc, offset);

                        // needs to calculate offset
                        let off = run.borrow().stack.len() as u32 - i;

                        match self.call(i, run) {
                            Result::Err(e) => {
                                self.interp_err(e);
                                false
                            }
                            Result::Ok(result) => {
                                match result {
                                    CallFunc::Script(new_call) => {
                                        current = new_call;

                                        run.borrow_mut().enter_script_frame(
                                            FuncRef::clone(&current),
                                        );

                                        pc = 0;
                                        offset = off;
                                        new_frame = true;
                                        true
                                    }

                                    CallFunc::Native(nfunc) => {
                                        run.borrow_mut()
                                            .update_current_frame(pc, offset);

                                        // the native frame should be enterred
                                        // right at this stage without going to
                                        // outer of the interpretation loop.
                                        run.borrow_mut().enter_native_frame(
                                            NFuncRef::clone(&nfunc),
                                        );

                                        match nfunc.borrow_mut().invoke(i, run) {
                                            Result::Err(e) => {
                                                self.interp_err(e);
                                                false
                                            }
                                            Result::Ok(v) => {
                                                self.ret_native(i, run);
                                                match run
                                                    .borrow_mut()
                                                    .leave_native_frame()
                                                {
                                                    Option::Some(f) => {
                                                        pc = f.0;
                                                        current = f.1;
                                                        offset = f.2;
                                                    }
                                                    _ => stop = true,
                                                };

                                                // native call, no need to enter
                                                // another frame since we never
                                                // really swap out of the interp
                                                // frame but call into extension
                                                // directly
                                                self.push_val(run, v.value);
                                                true
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    Bytecode::Return(x) => {
                        let return_value = self.ret(x, run);
                        match run.borrow_mut().leave_script_frame(x) {
                            Option::Some(caller) => {
                                pc = caller.0;
                                current = caller.1;
                                offset = caller.2;
                            }
                            _ => stop = true,
                        };
                        self.push_val(run, return_value);
                        new_frame = true;
                        true
                    }

                    // Builtins
                    Bytecode::Halt => {
                        let val = self.halt(run);
                        self.push_val(run, val);
                        new_frame = true;
                        stop = true;
                        stop_reason = ResultType::Halt;
                        true
                    }

                    Bytecode::Assert1 => {
                        if !self.assert1(run) {
                            let lpos = current.borrow().proto.code.debug
                                [(pc - 1) as usize]
                                .clone();
                            self.error(format!(
                                "assertion failed at({}:{})!",
                                lpos.column, lpos.line
                            ));
                            false
                        } else {
                            true
                        }
                    }

                    Bytecode::Assert2 => match self.assert2(run) {
                        Option::Some(v) => {
                            let lpos = current.borrow().proto.code.debug
                                [(pc - 1) as usize]
                                .clone();
                            self.error(format!(
                                "asertion failed at({}:{})]: {}",
                                lpos.column, lpos.line, v
                            ));
                            false
                        }
                        _ => true,
                    },

                    Bytecode::Trace(x) => match self.trace(x, run) {
                        Option::Some(e) => {
                            self.interp_err(e);
                            false
                        }
                        _ => true,
                    },

                    _ => unreachable!(),
                } {
                    stop = true;
                    break;
                }

                // testify whether we should enter into a new frame, the script
                // call is literally been flatten out in terms frame enter and
                // leave, the information is been kept on the value stack directly
                // for future JIT frame extension
                if new_frame {
                    new_frame = false;
                    break;
                }
            }
        }

        run.borrow_mut().update_current_frame(pc, offset);
        let r = self.generate_return(run, stop_reason);
        run.borrow_mut().unwind_stack();

        return r;
    }

    pub fn do_interp(
        run: &mut Runptr,
        main: ProtoRc,
    ) -> Result<Vresult, Verror> {
        let mut e = Exec {
            error_msg: Option::None,
        };
        return e.interp(run, main);
    }
}

impl Util {
    fn must_int(v: &Handle) -> i64 {
        match v {
            Handle::Int(vv) => return *vv,
            _ => {
                unreachable!();
            }
        };
    }
    fn must_real(v: &Handle) -> f64 {
        match v {
            Handle::Real(vv) => return *vv,
            _ => {
                unreachable!();
            }
        };
    }
    fn must_number(v: &Handle) -> f64 {
        match v {
            Handle::Int(vv) => return *vv as f64,
            Handle::Real(vv) => return *vv,
            _ => {
                unreachable!();
            }
        };
    }
    fn must_boolean(v: &Handle) -> bool {
        match v {
            Handle::Boolean(vv) => return *vv,
            _ => {
                unreachable!();
            }
        };
    }

    #[allow(dead_code)]
    fn must_strref(v: &Handle) -> StrRef {
        match v {
            Handle::Str(vv) => return StrRef::clone(vv),
            _ => {
                unreachable!();
            }
        };
    }

    fn must_listref(v: &Handle) -> ListRef {
        match v {
            Handle::List(vv) => return ListRef::clone(vv),
            _ => {
                unreachable!();
            }
        };
    }

    fn must_objref(v: &Handle) -> ObjRef {
        match v {
            Handle::Object(vv) => return ObjRef::clone(vv),
            _ => {
                unreachable!();
            }
        };
    }

    fn must_funcref(v: &Handle) -> FuncRef {
        match v {
            Handle::Function(vv) => return FuncRef::clone(vv),
            _ => {
                unreachable!();
            }
        };
    }
    fn must_nfuncref(v: &Handle) -> NFuncRef {
        match v {
            Handle::NFunction(vv) => return NFuncRef::clone(vv),
            _ => {
                unreachable!();
            }
        };
    }
    fn must_iterref(v: &Handle) -> IterRef {
        match v {
            Handle::Iter(vv) => return IterRef::clone(vv),
            _ => {
                unreachable!();
            }
        };
    }

    fn upgrade_number(l: &Handle, r: &Handle) -> (f64, f64) {
        return (Util::must_number(&l), Util::must_number(&r));
    }
}

macro_rules! bin_arithmetic {
    ($name:ident, $op:tt) => {
        pub fn $name(
            l: Handle,
            r: Handle,
            run: &mut Runptr,
            bcpos: usize,
        ) -> Result<Handle, Verror> {
            let cref = run.borrow_mut().rcall_val();
            let mut rref = cref.borrow_mut();
            let fd = rref.fd.index_mut(bcpos);

            if handle_is_type_same(&l, &r) {
                match l {
                    Handle::Int(lv) => {
                        fd.binary(FType::Int, FType::Int);
                        return Result::Ok(Handle::Int(lv $op Util::must_int(&r)));
                    }
                    Handle::Real(lv) => {
                        fd.binary(FType::Real, FType::Real);
                        return Result::Ok(Handle::Real(
                            lv $op Util::must_real(&r),
                        ));
                    }
                    _ => {
                        fd.binary(FType::Unknown, FType::Unknown);
                        return Result::Err(Verror::from_str(format!(
                            "operator {} cannot work with type {} and {}",
                            stringify!($op),
                            handle_type_name(&l),
                            handle_type_name(&r)
                        )));
                    }
                };
            } else {
                if handle_is_number(&l) && handle_is_number(&r) {
                    fd.binary(FType::Number, FType::Number);
                    let (lv, rv) = Util::upgrade_number(&l, &r);
                    return Result::Ok(Handle::Real(lv $op rv));
                } else {
                    fd.binary( FType::Unknown, FType::Unknown);
                    return Result::Err(Verror::from_str(format!(
                        "operator {} cannot work with type {} and {}",
                        stringify!($op),
                        handle_type_name(&l),
                        handle_type_name(&r)
                    )));
                }
            }
        }
    };
}

macro_rules! bin_comparison {
    ($name:ident, $op:tt, $sop:tt) => {
        pub fn $name(
            l: Handle,
            r: Handle,
            run: &mut Runptr,
            bcpos: usize,
        ) -> Result<Handle, Verror> {
            let cref = run.borrow_mut().rcall_val();
            let mut rref = cref.borrow_mut();
            let fd = rref.fd.index_mut(bcpos);

            if handle_is_type_same(&l, &r) {
                match l {
                    Handle::Int(lv) => {
                        fd.binary(FType::Int, FType::Int);
                        return Result::Ok(Handle::Boolean(lv $op Util::must_int(&r)));
                    }
                    Handle::Real(lv) => {
                        fd.binary(FType::Real, FType::Real);
                        return Result::Ok(Handle::Boolean(
                            lv $op Util::must_real(&r),
                        ));
                    }
                    Handle::Boolean(lv) => {
                        fd.binary( FType::Boolean, FType::Boolean);
                        return Result::Ok(Handle::Boolean(lv $op Util::must_boolean(&r)));
                    }
                    Handle::Null => {
                        fd.binary(FType::Null, FType::Null);
                        return Result::Ok(Handle::Boolean(true));
                    }
                    Handle::Str(lv) => {
                        let lhs_borrow = lv.borrow();
                        let lhs = lhs_borrow.string.as_str();
                        let rhs_borrow = match &r {
                            Handle::Str(v) => v.borrow(),
                            _ => unreachable!(),
                        };
                        let rhs = rhs_borrow.string.as_str();
                        fd.binary(FType::Str, FType::Str);
                        return Result::Ok(Handle::Boolean(lhs.$sop(rhs)));
                    },
                    _ => (),
                };
            } else {
                if handle_is_number(&l) && handle_is_number(&r) {
                    fd.binary(FType::Number, FType::Number);
                    let (lv, rv) = Util::upgrade_number(&l, &r);
                    return Result::Ok(Handle::Boolean(lv $op rv));
                }
            }

            fd.binary( FType::Unknown, FType::Unknown);
            return Result::Err(Verror::from_str(format!(
                "operator {} cannot work with type {} and {}",
                stringify!($op),
                handle_type_name(&l),
                handle_type_name(&r)
            )));
        }
    };
}

impl Arithmetic {
    pub fn add(
        l: Handle,
        r: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        if handle_is_type_same(&l, &r) {
            match l {
                Handle::Int(lv) => {
                    fd.binary(FType::Int, FType::Int);
                    return Result::Ok(Handle::Int(lv + Util::must_int(&r)));
                }
                Handle::Real(lv) => {
                    fd.binary(FType::Real, FType::Real);
                    return Result::Ok(Handle::Real(lv + Util::must_real(&r)));
                }
                Handle::Str(lv) => {
                    fd.binary(FType::Str, FType::Str);
                    let mut o = String::from(&lv.borrow().string);
                    let borrow = match &r {
                        Handle::Str(v) => v.borrow(),
                        _ => unreachable!(),
                    };
                    let data = borrow.string.as_str();

                    o.push_str(data);
                    return Result::Ok(
                        run.borrow().g.borrow_mut().heap.new_string_handle(o),
                    );
                }
                _ => {
                    fd.binary(FType::Unknown, FType::Unknown);
                    return Result::Err(Verror::from_str(format!(
                        "operator + cannot work with type {} and {}",
                        handle_type_name(&l),
                        handle_type_name(&r)
                    )));
                }
            };
        } else {
            if handle_is_number(&l) && handle_is_number(&r) {
                fd.binary(FType::Number, FType::Number);
                let (lv, rv) = Util::upgrade_number(&l, &r);
                return Result::Ok(Handle::Real(lv + rv));
            } else {
                fd.binary(FType::Unknown, FType::Unknown);
                return Result::Err(Verror::from_str(format!(
                    "operator + cannot work with type {} and {}",
                    handle_type_name(&l),
                    handle_type_name(&r)
                )));
            }
        }
    }

    bin_arithmetic!(sub, -);
    bin_arithmetic!(mul, *);

    pub fn div(
        l: Handle,
        r: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        if handle_is_type_same(&l, &r) {
            match l {
                Handle::Int(lv) => {
                    let rv = Util::must_int(&r);
                    if rv == 0 {
                        return Result::Err(Verror::from_str(
                            "divide by 0".to_string(),
                        ));
                    }
                    fd.binary(FType::Int, FType::Int);
                    return Result::Ok(Handle::Int(lv / rv));
                }
                Handle::Real(lv) => {
                    fd.binary(FType::Real, FType::Real);
                    let rv = Util::must_real(&r);
                    return Result::Ok(Handle::Real(lv / rv));
                }
                _ => {
                    fd.binary(FType::Unknown, FType::Unknown);
                    return Result::Err(Verror::from_str(format!(
                        "operator / cannot work with type {} and {}",
                        handle_type_name(&l),
                        handle_type_name(&r)
                    )));
                }
            };
        } else {
            if handle_is_number(&l) && handle_is_number(&r) {
                fd.binary(FType::Number, FType::Number);
                let (lv, rv) = Util::upgrade_number(&l, &r);
                return Result::Ok(Handle::Real(lv / rv));
            } else {
                fd.binary(FType::Unknown, FType::Unknown);
                return Result::Err(Verror::from_str(format!(
                    "operator / cannot work with type {} and {}",
                    handle_type_name(&l),
                    handle_type_name(&r)
                )));
            }
        }
    }

    pub fn mod_(
        l: Handle,
        r: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        if handle_is_type_same(&l, &r) {
            match l {
                Handle::Int(lv) => {
                    let rv = Util::must_int(&r);
                    if rv == 0 {
                        return Result::Err(Verror::from_str(
                            "divide by 0".to_string(),
                        ));
                    }
                    fd.binary(FType::Int, FType::Int);
                    return Result::Ok(Handle::Int(lv % rv));
                }
                _ => {
                    fd.binary(FType::Unknown, FType::Unknown);
                    return Result::Err(Verror::from_str(format!(
                        "operator % cannot work with type {} and {}",
                        handle_type_name(&l),
                        handle_type_name(&r)
                    )));
                }
            };
        } else {
            fd.binary(FType::Unknown, FType::Unknown);
            return Result::Err(Verror::from_str(format!(
                "operator % cannot work with type {} and {}",
                handle_type_name(&l),
                handle_type_name(&r)
            )));
        }
    }

    pub fn pow(
        l: Handle,
        r: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        if handle_is_type_same(&l, &r) {
            match l {
                Handle::Int(lv) => {
                    fd.binary(FType::Int, FType::Int);
                    let rv = Util::must_int(&r);
                    return Result::Ok(Handle::Int(lv.pow(rv as u32)));
                }
                Handle::Real(lv) => {
                    fd.binary(FType::Real, FType::Real);
                    let rv = Util::must_real(&r);
                    return Result::Ok(Handle::Real(lv.powf(rv as f64)));
                }
                _ => (),
            };
        } else {
            if handle_is_number(&l) && handle_is_number(&r) {
                fd.binary(FType::Number, FType::Number);
                let (lv, rv) = Util::upgrade_number(&l, &r);
                return Result::Ok(Handle::Real(lv.powf(rv)));
            }
        }

        fd.binary(FType::Unknown, FType::Unknown);
        return Result::Err(Verror::from_str(format!(
            "pow needs both operand to be numbers, but got type {} and {}",
            handle_type_name(&l),
            handle_type_name(&r)
        )));
    }
}

impl Comparison {
    pub fn eq(
        l: Handle,
        r: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        if handle_is_type_same(&l, &r) {
            match &l {
                Handle::Int(lv) => {
                    fd.binary(FType::Int, FType::Int);
                    return Result::Ok(Handle::Boolean(
                        *lv == Util::must_int(&r),
                    ));
                }
                Handle::Real(lv) => {
                    fd.binary(FType::Real, FType::Real);
                    return Result::Ok(Handle::Boolean(
                        *lv == Util::must_real(&r),
                    ));
                }
                Handle::Boolean(lv) => {
                    fd.binary(FType::Boolean, FType::Boolean);
                    return Result::Ok(Handle::Boolean(
                        *lv == Util::must_boolean(&r),
                    ));
                }
                Handle::Null => {
                    fd.binary(FType::Null, FType::Null);
                    return Result::Ok(Handle::Boolean(true));
                }
                Handle::Str(lv) => {
                    fd.binary(FType::Str, FType::Str);
                    let lhs_borrow = lv.borrow();
                    let lhs = lhs_borrow.string.as_str();
                    let rhs_borrow = match &r {
                        Handle::Str(x) => x.borrow(),
                        _ => unreachable!(),
                    };
                    let rhs = rhs_borrow.string.as_str();

                    return Result::Ok(Handle::Boolean(lhs.eq(rhs)));
                }
                Handle::List(lv) => {
                    fd.binary(FType::List, FType::List);
                    return Result::Ok(Handle::Boolean(Rc::ptr_eq(
                        &lv,
                        &Util::must_listref(&r),
                    )));
                }
                Handle::Object(lv) => {
                    fd.binary(FType::Object, FType::Object);
                    return Result::Ok(Handle::Boolean(Rc::ptr_eq(
                        &lv,
                        &Util::must_objref(&r),
                    )));
                }
                Handle::Function(lv) => {
                    fd.binary(FType::Function, FType::Function);
                    return Result::Ok(Handle::Boolean(Rc::ptr_eq(
                        &lv,
                        &Util::must_funcref(&r),
                    )));
                }
                Handle::NFunction(lv) => {
                    fd.binary(FType::NFunction, FType::NFunction);
                    return Result::Ok(Handle::Boolean(Rc::ptr_eq(
                        &lv,
                        &Util::must_nfuncref(&r),
                    )));
                }
                Handle::Iter(lv) => {
                    fd.binary(FType::Iter, FType::Iter);
                    return Result::Ok(Handle::Boolean(Rc::ptr_eq(
                        &lv,
                        &Util::must_iterref(&r),
                    )));
                }
                Handle::CallFrame(_) => {
                    fd.binary(FType::Unknown, FType::Unknown);
                    return Result::Ok(Handle::Boolean(false));
                }
            };
        } else {
            if handle_is_number(&l) && handle_is_number(&r) {
                fd.binary(FType::Number, FType::Number);
                let (lv, rv) = Util::upgrade_number(&l, &r);
                return Result::Ok(Handle::Boolean(lv == rv));
            }
        }

        fd.binary(FType::Unknown, FType::Unknown);
        return Result::Err(Verror::from_str(format!(
            "operator == cannot work with type {} and {}",
            handle_type_name(&l),
            handle_type_name(&r)
        )));
    }

    pub fn ne(
        l: Handle,
        r: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        if handle_is_type_same(&l, &r) {
            match l {
                Handle::Int(lv) => {
                    fd.binary(FType::Int, FType::Int);
                    return Result::Ok(Handle::Boolean(
                        lv != Util::must_int(&r),
                    ));
                }
                Handle::Real(lv) => {
                    fd.binary(FType::Real, FType::Real);
                    return Result::Ok(Handle::Boolean(
                        lv != Util::must_real(&r),
                    ));
                }
                Handle::Boolean(lv) => {
                    fd.binary(FType::Boolean, FType::Boolean);
                    return Result::Ok(Handle::Boolean(
                        lv != Util::must_boolean(&r),
                    ));
                }
                Handle::Null => {
                    fd.binary(FType::Null, FType::Null);
                    return Result::Ok(Handle::Boolean(false));
                }
                Handle::Str(lv) => {
                    fd.binary(FType::Str, FType::Str);
                    let lhs_borrow = lv.borrow();
                    let lhs = lhs_borrow.string.as_str();
                    let rhs_borrow = match &r {
                        Handle::Str(v) => v.borrow(),
                        _ => unreachable!(),
                    };
                    let rhs = rhs_borrow.string.as_str();

                    return Result::Ok(Handle::Boolean(lhs.ne(rhs)));
                }
                Handle::List(lv) => {
                    fd.binary(FType::List, FType::List);
                    return Result::Ok(Handle::Boolean(!Rc::ptr_eq(
                        &lv,
                        &Util::must_listref(&r),
                    )));
                }
                Handle::Object(lv) => {
                    fd.binary(FType::Object, FType::Object);
                    return Result::Ok(Handle::Boolean(!Rc::ptr_eq(
                        &lv,
                        &Util::must_objref(&r),
                    )));
                }
                Handle::Function(lv) => {
                    fd.binary(FType::Function, FType::Function);
                    return Result::Ok(Handle::Boolean(!Rc::ptr_eq(
                        &lv,
                        &Util::must_funcref(&r),
                    )));
                }
                Handle::NFunction(lv) => {
                    fd.binary(FType::NFunction, FType::NFunction);
                    return Result::Ok(Handle::Boolean(!Rc::ptr_eq(
                        &lv,
                        &Util::must_nfuncref(&r),
                    )));
                }
                Handle::Iter(lv) => {
                    fd.binary(FType::Iter, FType::Iter);
                    return Result::Ok(Handle::Boolean(!Rc::ptr_eq(
                        &lv,
                        &Util::must_iterref(&r),
                    )));
                }
                Handle::CallFrame(_) => {
                    fd.binary(FType::Unknown, FType::Unknown);
                    return Result::Ok(Handle::Boolean(false));
                }
            };
        } else {
            if handle_is_number(&l) && handle_is_number(&r) {
                fd.binary(FType::Number, FType::Number);
                let (lv, rv) = Util::upgrade_number(&l, &r);
                return Result::Ok(Handle::Boolean(lv != rv));
            }
        }

        fd.binary(FType::Unknown, FType::Unknown);
        return Result::Err(Verror::from_str(format!(
            "operator != cannot work with type {} and {}",
            handle_type_name(&l),
            handle_type_name(&r)
        )));
    }

    bin_comparison!(lt, <, lt);
    bin_comparison!(le, <=, le);
    bin_comparison!(gt, >, gt);
    bin_comparison!(ge, >=, ge);
}

impl Conversion {
    // used for generating the assert failure string, which should never failed
    pub fn debug_string(h: &Handle, _: &mut Runptr) -> String {
        match h {
            Handle::Int(v) => {
                return v.to_string();
            }
            Handle::Real(v) => {
                return v.to_string();
            }
            Handle::Boolean(v) => {
                if *v {
                    return "true".to_string();
                } else {
                    return "false".to_string();
                }
            }
            Handle::Null => {
                return "null".to_string();
            }
            Handle::Str(v) => {
                return v.borrow().debug_info();
            }
            Handle::Function(v) => {
                return v.borrow().debug_info();
            }
            Handle::NFunction(v) => {
                return v.borrow().debug_info();
            }
            Handle::List(v) => {
                return v.borrow().debug_info();
            }
            Handle::Object(v) => {
                return v.borrow().debug_info();
            }
            Handle::Iter(v) => {
                return v.borrow().debug_info();
            }
            Handle::CallFrame(v) => {
                return v.to_string();
            }
        };
    }

    pub fn to_rust_string(h: &Handle) -> Option<String> {
        match h {
            Handle::Int(v) => {
                return Option::Some(v.to_string());
            }
            Handle::Real(v) => {
                return Option::Some(v.to_string());
            }
            Handle::Boolean(v) => {
                if *v {
                    return Option::Some("ture".to_string());
                } else {
                    return Option::Some("false".to_string());
                }
            }
            Handle::Null => {
                return Option::Some("null".to_string());
            }
            Handle::Str(v) => {
                return Option::Some(v.borrow().string.clone());
            }
            _ => return Option::None,
        };
    }

    pub fn to_string(run: &mut Runptr, h: &Handle) -> Option<StrRef> {
        let xx = match Conversion::to_rust_string(h) {
            Option::Some(s) => s,
            _ => return Option::None,
        };

        let strref = run.borrow().g.borrow_mut().heap.new_string(xx);
        return Option::Some(strref);
    }

    pub fn to_boolean(h: &Handle, r: &mut Runptr) -> bool {
        match h {
            Handle::Boolean(v) => return *v,
            Handle::Int(v) => return *v != 0,
            Handle::Real(v) => return *v != 0.0,
            Handle::Null => return false,
            Handle::Str(v) => return v.borrow().string != "",
            Handle::List(v) => return v.borrow().len() != 0,
            Handle::Object(v) => return v.borrow().len() != 0,
            Handle::Function(_) => return false,
            Handle::NFunction(_) => return false,
            Handle::Iter(itr) => return itr.borrow().has(r),
            Handle::CallFrame(_) => return false,
        };
    }

    pub fn to_index(h: &Handle, _: &mut Runptr) -> Option<usize> {
        match h {
            Handle::Int(v) => return Option::Some(*v as usize),
            Handle::Real(v) => return Option::Some(*v as usize),
            _ => return Option::None,
        };
    }
}

impl Unary {
    pub fn neg(
        v: Handle,
        run: &mut Runptr,
        bcpos: usize,
    ) -> Result<Handle, Verror> {
        let cref = run.borrow_mut().rcall_val();
        let mut rref = cref.borrow_mut();
        let fd = rref.fd.index_mut(bcpos);

        match &v {
            Handle::Int(vv) => {
                fd.unary(FType::Int);
                return Result::Ok(Handle::Int(-*vv));
            }
            Handle::Real(vv) => {
                fd.unary(FType::Real);
                return Result::Ok(Handle::Real(-*vv));
            }
            _ => {
                fd.unary(FType::Unknown);
                return Result::Err(Verror::from_str(format!(
                    "type {} cannot do negate operation",
                    handle_type_name(&v)
                )));
            }
        };
    }

    pub fn not(v: Handle, run: &mut Runptr, _: usize) -> Result<Handle, Verror> {
        return Result::Ok(Handle::Boolean(!Conversion::to_boolean(&v, run)));
    }

    pub fn typeof_(
        v: Handle,
        run: &mut Runptr,
        _: usize,
    ) -> Result<Handle, Verror> {
        return Result::Ok(
            run.borrow_mut()
                .g
                .borrow_mut()
                .heap
                .new_str_handle(handle_type_name(&v)),
        );
    }

    pub fn sizeof(
        v: Handle,
        _: &mut Runptr,
        _: usize,
    ) -> Result<Handle, Verror> {
        return Result::Ok(Handle::Int(handle_sizeof(&v) as i64));
    }
}

#[cfg(test)]
mod exec_tests {
    use super::*;
    use crate::heap::heap::GHeapConfig;
    use crate::syntax::parser::*;
    use std::cell::RefCell;

    fn runstr(code: &str) -> Result<Vresult, Verror> {
        let g = G::new(GHeapConfig::default());
        let mut run = {
            let dup_g = Rc::clone(&g);
            g.borrow_mut().heap.new_run(dup_g)
        };
        let main = match do_parse(g, code, "[tests]") {
            Result::Ok(v) => Rc::new(v),
            Result::Err(e) => {
                return Result::Err(Verror {
                    description: format!(
                        "compiler error({}:{}): {}",
                        e.column, e.line, e.description
                    ),
                });
            }
        };
        return Exec::do_interp(&mut run, main);
    }

    fn runstr_config(code: &str, cfg: GHeapConfig) -> Result<Vresult, Verror> {
        let g = G::new(cfg);
        let mut run = {
            let dup_g = Rc::clone(&g);
            g.borrow_mut().heap.new_run(dup_g)
        };
        let main = match do_parse(g, code, "[tests]") {
            Result::Ok(v) => Rc::new(v),
            Result::Err(e) => {
                return Result::Err(Verror {
                    description: format!(
                        "compiler error({}:{}): {}",
                        e.column, e.line, e.description
                    ),
                });
            }
        };
        return Exec::do_interp(&mut run, main);
    }

    fn dump_code(code: &str) {
        match do_parse(G::new(GHeapConfig::default()), code, "[test]") {
            Result::Ok(v) => {
                println!("DUMP CODE, BUG");
                println!("======================================");
                println!("{}", code);
                println!("======================================");
                println!("{}", v);
                println!("======================================");
            }
            Result::Err(e) => {
                println!(
                    "code compile error: {}@{}:{}",
                    e.description, e.line, e.column
                );
                println!("{}", code);
            }
        };
    }

    fn runstr_bool(code: &str) -> bool {
        match runstr(code) {
            Result::Ok(v) => {
                match v.value {
                    Handle::Boolean(vv) => return vv,
                    _ => {
                        println!(
                            "return value type: {}",
                            handle_type_name(&v.value)
                        );
                    }
                };
            }
            Result::Err(v) => {
                println!("failed: {}", v.description);
            }
        };

        dump_code(code);
        assert!(false);
        return true;
    }

    fn runstr_int(code: &str) -> i64 {
        match runstr(code) {
            Result::Ok(v) => {
                match v.value {
                    Handle::Int(vv) => return vv,
                    _ => {
                        println!(
                            "return value type: {}",
                            handle_type_name(&v.value)
                        );
                    }
                };
            }
            Result::Err(v) => {
                println!("failed: {}", v.description);
            }
        };

        dump_code(code);
        assert!(false);
        return 0;
    }

    fn runstr_real(code: &str) -> f64 {
        match runstr(code) {
            Result::Ok(v) => {
                match v.value {
                    Handle::Real(vv) => return vv,
                    _ => {
                        println!(
                            "return value type: {}",
                            handle_type_name(&v.value)
                        );
                    }
                };
            }
            Result::Err(v) => {
                println!("failed: {}", v.description);
            }
        };

        dump_code(code);
        assert!(false);
        return 0.0;
    }

    #[allow(dead_code)]
    fn runstr_null(code: &str) -> bool {
        match runstr(code) {
            Result::Ok(v) => {
                match v.value {
                    Handle::Null => return true,
                    _ => {
                        println!(
                            "return value type: {}",
                            handle_type_name(&v.value)
                        );
                    }
                };
            }
            Result::Err(v) => {
                println!("failed: {}", v.description);
            }
        };

        dump_code(code);
        assert!(false);
        return false;
    }

    fn runstr_str(code: &str) -> String {
        match runstr(code) {
            Result::Ok(v) => {
                match v.value {
                    Handle::Str(s) => return s.borrow().string.clone(),
                    _ => {
                        println!(
                            "return value type: {}",
                            handle_type_name(&v.value)
                        );
                    }
                };
            }
            Result::Err(v) => {
                println!("failed: {}", v.description);
            }
        };

        dump_code(code);
        assert!(false);
        return "".to_string();
    }

    fn runstr_fail(code: &str) -> bool {
        match runstr(code) {
            Result::Ok(_) => return false,
            Result::Err(v) => {
                println!("expected failure: {}", v.description);
                return true;
            }
        };
    }

    #[allow(dead_code)]
    fn runstr_ok(code: &str) -> bool {
        match runstr(code) {
            Result::Ok(_) => return true,
            Result::Err(v) => {
                println!("unexpected failure: {}", v.description);
                return false;
            }
        };
    }

    fn runstr_halt_int(code: &str) -> i64 {
        match runstr(code) {
            Result::Ok(v) => {
                if v.result_type == ResultType::Halt {
                    match v.value {
                        Handle::Int(i) => return i,
                        x @ _ => {
                            println!("invalid type {}", handle_type_name(&x));
                        }
                    }
                } else {
                    println!("invalid result type, not halt");
                }
            }
            Result::Err(v) => {
                println!("unexpected failure: {}", v.description);
            }
        };

        unreachable!();
    }

    #[test]
    fn test_folding() {
        {
            dump_code("let a = 1 + 2; return a; ");
            assert_eq!(runstr_int("let a = 1 + 2; return a;"), 3);
        }
        {
            dump_code("let a = 1 - 2; return a; ");
            assert_eq!(runstr_int("let a = 1 - 2; return a;"), -1);
        }
        {
            dump_code("let a = 1 * 2; return a; ");
            assert_eq!(runstr_int("let a = 1 * 2; return a;"), 2);
        }
        {
            dump_code("let a = 1 / 2; return a; ");
            assert_eq!(runstr_int("let a = 1 / 2; return a;"), 0);
        }
        {
            dump_code("let a = 1 ^ 2; return a; ");
            assert_eq!(runstr_int("let a = 1 ^ 2; return a;"), 1);
        }
        {
            dump_code("let a = 1 % 2; return a; ");
            assert_eq!(runstr_int("let a = 1 % 2; return a;"), 1 % 2);
        }
        {
            dump_code("let a = 1+2*3/2+4; return a; ");
            assert_eq!(
                runstr_int("let a = 1+2*3/2+4; return a;"),
                1 + 2 * 3 / 2 + 4
            );
        }
        {
            dump_code("let a = 'xx' + 'aa'; return a;");
            assert_eq!(
                runstr_int("let a = 'xx' + 'aa'; return a == 'xxaa' ? 1 : 0;"),
                1,
            );
        }
        {
            dump_code("let a = 1 > 100; return a;");
            assert_eq!(runstr_bool("let a = 1 > 100; return a;"), false);
        }
        {
            dump_code("let a = 100 > 100; return a;");
            assert_eq!(runstr_bool("let a = 100 >= 100; return a;"), true);
        }
        {
            dump_code("let a = 1 < 100; return a;");
            assert_eq!(runstr_bool("let a = 1 < 100; return a;"), true);
        }
        {
            dump_code("let a = 100 <= 100; return a;");
            assert_eq!(runstr_bool("let a = 100 <= 100; return a;"), true);
        }
        {
            dump_code("let a = 1 != 1; return a;");
            assert_eq!(runstr_bool("let a = 1 != 1; return a;"), false);
        }
        {
            dump_code("let a = 1 == 1; return a;");
            assert_eq!(runstr_bool("let a = 1 == 1; return a;"), true);
        }

        // unary folding
        {
            dump_code("let a = ---100; return a;");
            assert_eq!(runstr_int("let a = ---100; return a;"), -100);
        }
        {
            dump_code("let a = !!!true; return a;");
            assert_eq!(runstr_bool("let a = !!!true; return a;"), false);
        }
    }

    use std::fs::*;
    use std::path::*;
    use walkdir::WalkDir;

    // cargo test command. It will try to locate external testing files and
    // run them one by one.
    #[test]
    fn test_driver() {
        // (0) we will use cargo's environment variable to locate our position
        //     and then read testing file from external files
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("testing/resource");
        let path_str = d.display().to_string();

        println!("============================================");
        println!("located testing resource path: {}", path_str);
        println!("============================================");

        // (1) iterate through the folder recursively to hit all the testing
        //     file and read them in and verify the testing result
        let mut index = 0;
        let mut error = 0;
        let mut error_list = Vec::<(String, Verror)>::new();

        for entry in WalkDir::new(path_str) {
            let path = entry.as_ref().unwrap().path().clone();
            if path.is_file() {
                match path.extension() {
                    Option::Some(v) => {
                        if v.to_str().unwrap() != "cf" {
                            continue;
                        }
                    }
                    _ => (),
                };

                println!("============================================");
                println!("[TEST] {}. {}", index, path.display());
                index += 1;
                println!("============================================");

                // load the data into the memory
                let path_string = path.display().to_string();
                let data = read_to_string(path).unwrap();
                match runstr_config(&data, GHeapConfig::dbg(1, 1)) {
                    Result::Err(v) => {
                        dump_code(&data);
                        error += 1;
                        error_list.push((path_string, v));
                    }
                    _ => (),
                };
            }
        }

        println!("============================================");
        println!(
            "Total: {}; Passed: {}; Error: {}",
            index,
            (index - error),
            error
        );
        for x in error_list.iter() {
            println!("--------------------------------------------");
            println!("{} failed: {}", x.0, x.1.description);
            println!("--------------------------------------------");
        }
        println!("============================================");

        assert_eq!(error, 0);
    }

    #[test]
    fn test_global_var() {
        assert_eq!(
            runstr_int(
                r#"
g = 10;
b = g;
let x = 20;
return b + x;
"#
            ),
            30
        );
    }
    // upvalue testing, ie capturing lexical scope variable doesn't belong to
    // it but in its embeded function
    #[test]
    fn test_trace() {
        assert!(runstr_ok(
            r#"
trace "a", "b", "c", "d";
trace "Hello World";
trace "E", "hello world";
"#
        ));
    }

    #[test]
    fn test_assert() {
        assert!(runstr_fail(
            r#"
assert false, "yyy";
"#
        ));

        assert!(runstr_fail(
            r#"
assert false;
"#
        ));

        assert!(runstr_fail(
            r#"
assert true;
assert false;
"#
        ));

        assert!(runstr_fail(
            r#"
assert true;
assert false, "xxx";
"#
        ));
    }

    #[test]
    fn test_assert_nested() {
        assert!(runstr_fail(
            r#"
func a() {
    b();
}
func b() {
    c();
}
func c() {
    d();
}
func d() {
    assert false;
}

a();
"#
        ));
    }

    #[test]
    fn test_halt() {
        assert_eq!(
            runstr_halt_int(
                r#"
func a() {
    b();
}
func b() {
    c();
}
func c() {
    d();
}
func d() {
    halt 100;
}
d();
"#
            ),
            100
        );

        // automatically generate halt null
        assert!(runstr_null("let a = 100;"));
    }

    #[test]
    fn test_upval0() {
        assert_eq!(
            runstr_int(
                r#"
let uv = 0;
let vu = 1;
let foo = func() {
    let a = 10;
    let b = 20;
    return func() { return a + b; };
};

let x = foo()();
return x;
"#
            ),
            30
        );

        // nested
        assert_eq!(
            runstr_int(
                r#"
let uv = 0;
let vu = 1;
let foo = func() {
    let a = 10;
    return func() {
        let b = 20;
        return func() {
            let c = 30;
            return func() {
                let d = 40;
                return func() {
                    return a + b + c + d;
                };
            };
        };
    };
};

return foo()()()()();
"#
            ),
            10 + 20 + 30 + 40
        );

        // multiple level upvalue collapsing
        assert_eq!(
            runstr_int(
                r#"
let foo = func() {
    let a = 10;
    let b = 20;
    let c = 30;
    let d = 40;
    return func() {
        return func() {
            return func() {
                return func() {
                    return a + b + c + d;
                };
            };
        };
    };
};

return foo()()()()();
"#
            ),
            10 + 20 + 30 + 40
        );
    }

    #[test]
    fn test_upval1() {
        assert_eq!(
            runstr_int(
                r#"
let foo = func() {
    let a = [10];
    let b = {'c': 100};
    return func() {
        return func() {
            return func() {
                return func() {
                    return a[0] + b.c;
                };
            };
        };
    };
};

return foo()()()()();
"#
            ),
            110
        );
    }

    struct MyNative1 {
        gc: GC,
    }

    impl NFunc for MyNative1 {
        fn invoke(
            &mut self,
            argcount: u32,
            _: &mut Runptr,
        ) -> Result<Vresult, Verror> {
            if argcount != 0 {
                return Result::Err(Verror::from_str(
                    "fuck you, argument mismatch".to_string(),
                ));
            }
            return Result::Ok(Vresult::ret(Handle::Int(1)));
        }
    }

    impl HRef for MyNative1 {
        fn otype(&self) -> OType {
            OType::NFunction
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
            return "[my-native]".to_string();
        }
        fn sizeof(&self) -> usize {
            1
        }
    }

    #[test]
    fn test_native_call() {
        // calling native functions
        let g = G::new(GHeapConfig::default());
        let mut run = {
            let dup_g = Rc::clone(&g);
            g.borrow_mut().heap.new_run(dup_g)
        };
        let code = r#"
return foo();
        "#;

        let main = match do_parse(Gptr::clone(&g), code, "[tests]") {
            Result::Ok(v) => Rc::new(v),
            Result::Err(e) => {
                println!("parser error: {}", e.description);
                unreachable!();
            }
        };
        {
            let x = Rc::new(RefCell::new(MyNative1 { gc: GC::nil() }));
            let xx = Rc::clone(&x);
            g.borrow_mut().heap.watch_native_function(xx);
            run.borrow_mut().global_add(
                &g.borrow_mut().heap.new_str("foo"),
                Handle::NFunction(x),
            );
        }
        match Exec::do_interp(&mut run, main) {
            Result::Ok(v) => match v.value {
                Handle::Int(i) => {
                    assert_eq!(i, 1);
                    return;
                }
                _ => {
                    println!("type mismatch: {}", handle_type_name(&v.value));
                }
            },
            Result::Err(e) => {
                println!("execution error {}", e.description);
            }
        };
        unreachable!();
    }

    #[test]
    fn test_function_argument() {
        assert_eq!(
            runstr_int(
                r#"
func x0() {
    return 100;
}

func x1(a) {
    return a;
}

func x2(a, b) {
    return a + b;
}

return x0() + x1(100) + x2(100, 100);
"#
            ),
            400
        );
    }

    #[test]
    fn test_function_statement() {
        assert_eq!(
            runstr_int(
                r#"
func x0() {
    return 100;
}

func x1(a) {
    return a;
}

func x2(a, b) {
    return a + b;
}

func x3() {
    return {'a': [1, [2]]};
}

x0();
x1(100);
x2(100, 100);
x3().a[1][0];

return x0() + x1(100) + x2(100, 100);
"#
            ),
            400
        );
    }

    #[test]
    fn test_function_closure() {
        assert_eq!(
            runstr_int(
                r#"
let x = func() {
    return 10;
};

let xx = func(a) {
    return a;
};

let xxx = func(a, b) {
    return a + b;
};

return x() + xx(100) + xxx(100, 100);
"#
            ),
            310
        );

        assert_eq!(
            runstr_int(
                r#"
let x = (func() { return {'a': 100}; })().a;
return x;
"#
            ),
            100
        );
    }

    #[test]
    fn test_assignment_simpl() {
        assert_eq!(runstr_int("let a = 10; return a;"), 10);
        assert_eq!(runstr_int("let a = 0 - 1; return a;"), -1);
        assert_eq!(runstr_int("let a = 100 - 20 * 5; return a;"), 0);
        assert_eq!(runstr_int("let a = {'a':100}.a; return a;"), 100);
        assert_eq!(
            runstr_int("let a = {'a':{'a':{'b':100}}}.a.a.b; return a;"),
            100
        );
        assert_eq!(runstr_int("let a = [0][0]; return a;"), 0);
        assert_eq!(runstr_int("let a = [[[[0]]]][0][0][0][0]; return a;"), 0);
    }

    #[test]
    fn test_assignment_compl() {
        dump_code("let a =[1, 2]; a[1] = 100; return a[1];");
        assert_eq!(runstr_int("let a = [1, 2]; a[1] = 100; return a[1];"), 100);
        assert_eq!(
            runstr_int("let a = {}; a.b = 1; a['c'] = 2; return a.b + a.c;"),
            3
        );
        assert_eq!(runstr_int("let a = []; a[100] = 100; return a[100];"), 100);
        assert_eq!(
            runstr_int("let a = [[]]; a[0][100] = 100; return a[0][100];"),
            100
        );
        assert_eq!(
            runstr_int("let a = {'a':{'b':{}}}; a.a.b.c = 100; return a.a.b.c;"),
            100
        );
    }

    #[test]
    fn test_global_call() {
        assert_eq!(
            runstr_int(
                "func a() { return 100; } \
             func b() { return 200; } \
             a();
             b();
             return 100;"
            ),
            100
        );
        assert_eq!(
            runstr_int(
                "func a() { return {'a': 100}; } \
             func b() { return [100]; } \
             a().a; \
             b()[0]; \
             return 100;"
            ),
            100
        );
    }

    fn native_x(limit: i64, step: i64) -> i64 {
        let mut o = 0;
        let mut i = 0;
        while i < limit {
            i += 1;
            o += step;
        }
        return o;
    }

    fn native_xx(limit: i64, step: i64) -> i64 {
        let mut o = 0;
        let mut i = 0;
        while i < limit {
            if i % 2 == 0 {
                i += 1;
                continue;
            }
            o += step;
            i += 1;
        }
        return o;
    }

    #[test]
    fn test_forever_loop_break() {
        assert_eq!(
            runstr_int(
                r#"
func x(limit, step) {
    let o = 0;
    let idx = 0;
    for {
        if (idx >= limit) {
            break;
        }
        idx = idx + 1;
        o = o + step;
    }
    return o;
}
return x(100, 2);
"#
            ),
            native_x(100, 2)
        );
    }

    #[test]
    fn test_forever_loop_continue() {
        assert_eq!(
            runstr_int(
                r#"
func xx(limit, step) {
    let o = 0;
    let idx = 0;
    for {
        if (idx >= limit) {
            break;
        }
        idx = idx + 1;

        if idx % 2 == 0 {
            continue;
        }
        o = o + step;
    }
    return o;
}
return xx(100, 2);
"#
            ),
            native_xx(100, 2)
        );
    }

    // testing branch
    #[test]
    fn test_branch_only_if() {
        assert_eq!(
            runstr_int(
                r#"
let a = 100;
if (a == 0) {
    return 100;
}
return -100;
"#
            ),
            -100
        );

        // nesting
        assert_eq!(
            runstr_int(
                r#"
let a = 100;
if a > 0 {
    if a > 50 {
        if a > 80 {
            if a % 2 == 0 {
                return a;
            }
        }
    }
}
return -100;
"#
            ),
            100
        );
    }

    #[test]
    fn test_branch_if_else() {
        assert_eq!(
            runstr_int(
                r#"
let a = 100;
if (a == 0) {
    return 100;
} else {
    return -100;
}
"#
            ),
            -100
        );

        // nesting
        assert_eq!(
            runstr_int(
                r#"
let a = 100;
if a > 0 {
    if a > 50 {
        if a > 80 {
            if a % 2 == 1 {
                return a;
            } else {
                return -100;
            }
        }
    }
} 

"#
            ),
            -100
        );
    }

    #[test]
    fn test_branch_if_elif() {
        assert_eq!(
            runstr_int(
                r#"
let a = 100;
if a < 100 {
    return 1;
} elif a > 100 {
    return 2;
}

return a - 100;
"#
            ),
            0
        );

        assert_eq!(
            runstr_int(
                r#"
let a = 101;
if a < 100 {
    return 1;
} elif a > 100 {
    return 2;
}

return a - 100;
"#
            ),
            2
        );
    }

    #[test]
    fn test_branch0() {
        assert_eq!(
            runstr_int(
                r#"
let a = 100;
if a < 100 {
    return 1;
} elif a > 100 {
    return 2;
} else {
    return 3;
}

return a - 100;
"#
            ),
            3
        );

        assert_eq!(
            runstr_int(
                r#"
let a = 101;
if a < 100 {
    return 1;
} elif a > 100 {
    return 2;
} else {
    return 100;
}

return a - 100;
"#
            ),
            2
        );
    }

    #[test]
    fn test_basic_block() {
        assert_eq!(
            runstr_int(
                r#"
{
    let a = 10;
}
let a = 200;
return a;
"#
            ),
            200
        );
        assert_eq!(
            runstr_int(
                r#"
func x() {
    {
        let a = 0;
        if false {
            return a;
        }
    }
    let a = 20;
    return a;
}

return x();
"#
            ),
            20
        );
    }

    #[test]
    fn test_expr_str() {
        // concatenation, other operation is prohibited
        assert_eq!(runstr_str("return '';"), "");
        assert_eq!(runstr_str("return '' + 'a';"), "a");
        assert_eq!(runstr_str("return 'a'+ 'b';"), "ab");

        // comparison
        assert_eq!(runstr_bool("return '' == '';"), true);
        assert_eq!(runstr_bool("return '' != '';"), false);
        assert_eq!(runstr_bool("return '' >= '';"), true);
        assert_eq!(runstr_bool("return '' > '';"), false);
        assert_eq!(runstr_bool("return '' < '';"), false);
        assert_eq!(runstr_bool("return '' <= '';"), true);
        assert_eq!(runstr_bool("return 'abcd' == 'abcd';"), true);
        assert_eq!(runstr_bool("return 'abcde' != 'abcd';"), true);
    }

    #[test]
    fn test_str_iter() {
        // get the iterator of string
        assert_eq!(
            runstr_bool(
                r#"

func test() {
    let str = 'abcde';
    let idx = 0;
    for x in str {
        if (idx == 0 && x != 'a') return false;
        if (idx == 1 && x != 'b') return false;
        if (idx == 2 && x != 'c') return false;
        if (idx == 3 && x != 'd') return false;
        if (idx == 4 && x != 'e') return false;
        if (idx > 5) return false;
        idx = idx + 1;
    }
    return true;
}

return test();
    "#
            ),
            true
        );
    }

    #[test]
    fn test_expr_obj_empty() {
        assert_eq!(
            runstr_bool(
                r#"
func count(l) {
    let size = 0;
    for x in l {
        size = size + 1;
    }
    return size;
}
return count({}) == 0;
"#
            ),
            true
        );
    }

    #[test]
    fn test_expr_obj_primitive() {
        assert_eq!(
            runstr_bool(
                r#"
let obj = {
    "a" : 100,
    "b" : 200.0,
    "c" : true,
    "d" : false,
    "e" : null
};

let obj_a = obj.a;
let obj_b = obj["b"];
let obj_c = obj.c;
let obj_d = obj.d;
let obj_e = obj.e;

return obj_a == 100 &&
       obj_b == 200.0 &&
       obj_c == true &&
       obj_d == false &&
       obj_e == null;
"#
            ),
            true
        );
    }

    #[test]
    fn test_expr_obj_nested() {
        assert_eq!(
            runstr_bool(
                r#"
let obj = {
    "a" : {},
    "b" : {
        "d" : {
            "e" : "f"
        }
    }
};

func count(l) {
    let size = 0;
    for x in l {
        size = size + 1;
    }
    return size;
}

let obj_a = obj.a;
let obj_b = obj.b.d.e;

return count(obj_a) == 0 &&
       count(obj) == 2 &&
       obj_b == "f";
"#
            ),
            true
        );
    }

    #[test]
    fn test_expr_obj_equality() {
        assert_eq!(
            runstr_bool(
                r#"
let obj = {};
let obj_b = obj;
return obj_b == obj;
"#
            ),
            true
        );
        assert_eq!(
            runstr_bool(
                r#"
let obj = {};
return obj == {};
"#
            ),
            false
        );
    }

    #[test]
    fn test_expr_list_empty() {
        {
            let code = r#"
func count(l) {
    let size = 0;
    for x in l {
        size = size + 1;
    }
    return size;
}

return count([]) == 0;
        "#;
            assert_eq!(runstr_bool(code), true);
        }
    }

    #[test]
    fn test_expr_list_primitive_element() {
        let code = r#"

r = [1, true, false, null];

func count(l) {
    let size = 0;
    for x in l {
        size = size + 1;
    }
    return size;
}

return count(r) == 4 &&
       r[0] == 1 &&
       r[1] == true &&
       r[2] == false &&
       r[3] == null;
    "#;
        assert_eq!(runstr_bool(code), true);
    }

    #[test]
    fn test_expr_list_nested_list() {
        let code = r#"

func count(l) {
    let size = 0;
    for x in l {
        size = size + 1;
    }
    return size;
}

let r = [[1, 2.0], []];
let rr = r[0];
let rrr = r[1];
let r0 = count(r) == 2;
let r1 = count(r[0]) == 2;
let r2 = rr[0] == 1;
let r3 = rr[1] == 2.0;
let r4 = count(rrr) == 0;

return r0 && r1 && r2 && r3 && r4;

"#;
        assert_eq!(runstr_bool(code), true);
    }

    // testing list equality comparison
    #[test]
    fn test_expr_list_equality() {
        let code1 = r#"
let r0 = [];
let r1 = r0;
    return r0 == r1;
        "#;
        assert_eq!(runstr_bool(code1), true);

        let code2 = r#"
let r0 = [];
    return r0 == [];
        "#;
        assert_eq!(runstr_bool(code2), false);
    }

    #[test]
    fn test_expr_null() {
        assert_eq!(runstr_bool("return null != null;"), false);
        assert_eq!(runstr_bool("return null == null;"), true);
    }

    #[test]
    fn test_expr_bool() {
        // comparison, notes the boolean type doesn't participate in arithmetic
        // operation for now, which is stupid in my opinion.
        assert_eq!(runstr_bool("return true == true;"), true);
        assert_eq!(runstr_bool("return false== true;"), false);
        assert_eq!(runstr_bool("return false== false;"), true);
        assert_eq!(runstr_bool("return false != false;"), false);
        assert_eq!(runstr_bool("return true != false;"), true);

        // also stupid
        assert_eq!(runstr_bool("return true > true;"), false);
        assert_eq!(runstr_bool("return true>=true;"), true);
        assert_eq!(runstr_bool("return false > false;"), false);
        assert_eq!(runstr_bool("return false >= false;"), true);
        assert_eq!(runstr_bool("return false < false;"), false);
        assert_eq!(runstr_bool("return false <= false;"), true);
    }

    #[test]
    fn test_expr_int() {
        // integer number testing, mainly for testing the expression level
        assert_eq!(runstr_int("return 1;"), 1);
        assert_eq!(runstr_int("return 1+1;"), 2);
        assert_eq!(runstr_int("return 1-1;"), 0);
        assert_eq!(runstr_int("return 1*1;"), 1);
        assert_eq!(runstr_int("return 1/1;"), 1);
        assert_eq!(runstr_int("return 1+1*2;"), 3);
        assert_eq!(runstr_int("return 1+1/2;"), 1);
        assert_eq!(runstr_int("return 1+(1/1);"), 2);
        assert_eq!(runstr_int("return 1+(2/2);"), 2);
        assert_eq!(runstr_int("return -1;"), -1);
        assert_eq!(runstr_int("return -100;"), -100);

        // term/factor/power
        assert_eq!(runstr_int("return 1 * 2 + 3;"), 5);
        assert_eq!(runstr_int("return 1 + 2 * 3;"), 7);
        assert_eq!(runstr_int("return 1 + 2 * 3 ^ 2;"), 19);
        assert_eq!(runstr_int("return 1 + 2 ^ 3;"), 9);

        // unary
        assert_eq!(runstr_int("return 1 + -1;"), 0);
        assert_eq!(runstr_int("return !0 ? 1 : 0;"), 1);
        assert_eq!(runstr_int("return !1 ? 0 : 1;"), 1);

        // predecense
        assert_eq!(runstr_int("return 1 + 2 ^ 2;"), 5);
        assert_eq!(runstr_int("return 1 + 2 > 3 + 4 ? 0 : 1;"), 1);
        assert_eq!(runstr_int("return 1 + 2 > 3 == 1 < 3 ? 0 : 1;"), 1);
        assert_eq!(
            runstr_int("return 1 + 2 > (1+2 == 3 ? 1 : 100) ? 10 : -1;"),
            10
        );
        assert_eq!(runstr_int("return (((((((1)))))));"), 1);

        // equality testing
        assert_eq!(runstr_int("return 1 == 1 ? 0 : 1;"), 0);
        assert_eq!(runstr_int("return 1 != 1 ? 0 : 1;"), 1);
        assert_eq!(runstr_int("return 1 != 1 ? 0 : 1;"), 1);
        assert_eq!(runstr_int("return 1 == 1 ? 0 : 1;"), 0);

        // comparison
        assert_eq!(runstr_int("return 1 < 1 ? 0 : 1;"), 1);
        assert_eq!(runstr_int("return 1 <= 1 ? 0 : 1;"), 0);
        assert_eq!(runstr_int("return 1 > 1 ? 0 : 1;"), 1);
        assert_eq!(runstr_int("return 1 >= 1 ? 0 : 1;"), 0);
    }

    #[test]
    fn test_expr_real() {
        // notes almost the same as integer, but just use real number
        assert_eq!(runstr_real("return 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0+1.0;"), 2.0);
        assert_eq!(runstr_real("return 1.0-1.0;"), 0.0);
        assert_eq!(runstr_real("return 1.0*1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0/1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0+1.0*2.0;"), 3.0);
        assert_eq!(runstr_real("return 1.0+2.0/1.0;"), 3.0);
        assert_eq!(runstr_real("return 1.0+(1.0/1.0);"), 2.0);
        assert_eq!(runstr_real("return 1.0+(2.0/2.0);"), 2.0);
        assert_eq!(runstr_real("return -1.0;"), -1.0);
        assert_eq!(runstr_real("return -100.0;"), -100.0);

        // term/factor/power
        assert_eq!(runstr_real("return 1.0 * 2.0 + 3.0;"), 5.0);
        assert_eq!(runstr_real("return 1.0 + 2.0 * 3.0;"), 7.0);
        assert_eq!(runstr_real("return 1.0 + 2.0 * 3.0 ^ 2.0;"), 19.0);
        assert_eq!(runstr_real("return 1.0 + 2.0 ^ 3.0;"), 9.0);

        // unary
        assert_eq!(runstr_real("return 1.0 + -1.0;"), 0.0);
        assert_eq!(runstr_real("return !0.0 ? 1.0 : 0.0;"), 1.0);
        assert_eq!(runstr_real("return !1.0 ? 0.0 : 1.0;"), 1.0);

        // predecense
        assert_eq!(runstr_real("return 1.0 + 2.0 ^ 2.0;"), 5.0);
        assert_eq!(
            runstr_real("return 1.0 + 2.0 > 3.0 + 4.0 ? 0.0 : 1.0;"),
            1.0
        );
        assert_eq!(
            runstr_real("return 1.0 + 2.0 > 3.0 == 1.0 < 3.0 ? 0.0 : 1.0;"),
            1.0
        );
        assert_eq!(
            runstr_real("return 1.0 + 2.0 > (1.0+2.0 == 3.0 ? 1.0 : 100.0) ? 10.0 : -1.0;"),
            10.0
        );
        assert_eq!(runstr_real("return (((((((1.0)))))));"), 1.0);

        // equality testing
        assert_eq!(runstr_real("return 1.0 == 1.0 ? 0.0 : 1.0;"), 0.0);
        assert_eq!(runstr_real("return 1.0 != 1.0 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 != 1.0 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 == 1.0 ? 0.0 : 1.0;"), 0.0);

        // comparison
        assert_eq!(runstr_real("return 1.0 < 1.0 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 <= 1.0 ? 0.0 : 1.0;"), 0.0);
        assert_eq!(runstr_real("return 1.0 > 1.0 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 >= 1.0 ? 0.0 : 1.0;"), 0.0);

        // upgrading cases
        assert_eq!(runstr_real("return 1 + 1.0;"), 2.0);
        assert_eq!(runstr_real("return 1.0 + 1;"), 2.0);

        assert_eq!(runstr_real("return 1 - 1.0;"), 0.0);
        assert_eq!(runstr_real("return 1.0 - 1;"), 0.0);

        assert_eq!(runstr_real("return 1 * 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 * 1;"), 1.0);

        assert_eq!(runstr_real("return 1 / 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 / 1;"), 1.0);

        assert_eq!(runstr_real("return 1 > 2.0 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 > 2 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1 < 2.0 ? 0.0 : 1.0;"), 0.0);
        assert_eq!(runstr_real("return 1.0 < 2 ? 0.0 : 1.0;"), 0.0);

        assert_eq!(runstr_real("return 1 == 2.0 ? 0.0 : 1.0;"), 1.0);
        assert_eq!(runstr_real("return 1.0 == 2 ? 0.0 : 1.0;"), 1.0);

        assert_eq!(runstr_real("return 1 != 2.0 ? 0.0 : 1.0;"), 0.0);
        assert_eq!(runstr_real("return 1.0 != 2 ? 0.0 : 1.0;"), 0.0);

        // fpe doesn't generate exception for now
        assert_eq!(runstr_real("return 1/0.0;"), f64::INFINITY);
        assert_eq!(runstr_real("return 1.0 * (1/0.0);"), f64::INFINITY);
        assert_eq!(runstr_real("return -1.0/0.0;"), f64::NEG_INFINITY);
        assert!(runstr_real("return 0/0.0;").is_nan());
        assert!(runstr_real("return 0.0/0;").is_nan());
        assert!(runstr_real("return 0.0/0.0;").is_nan());
    }
}
