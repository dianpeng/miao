use crate::bc::bytecode::*;
use crate::jit::bbinfo::*;
use crate::jit::j::*;
use crate::jit::node::*;
use crate::object::object::*;
use bitvec::prelude::*;

use std::cell::RefCell;
use std::rc::Rc;

// A simulated stack, used to hold all the value during building.
type ValStack = Vec<Nref>;

struct OSRInfo {
    start: u32,
}

// Abstract interpretation of bytecode, tracking each BB's value stack and other
// stuffs.
struct Env {
    bbinfo_id: u32,
    stack: ValStack,

    // the CFG node of this Env, each Env has one cfg node correspondingly
    cfg: Nref,

    // loop iv that needs to be patched
    loop_iv: Vec<(u32, Nref)>,

    // some flags for verification
    visited: bool,
}

type Envptr = Rc<RefCell<Env>>;
type EnvList = Vec<Envptr>;

// Build a IR graph from the input bytecode. The output is a SSA sea of nodes
// style graph representation of the input. Both OSR and function level are
// building from here.
struct FBuilder {
    // Input
    function: FuncRef,
    arg_count: u32,

    // Others
    jit: Jitptr,
    graph: FGraphptr,

    // BBInfoSet, obtained from BBInfoBuilder
    bbinfo_set: BBInfoSet,

    // Building properties
    osr: Option<OSRInfo>,

    // Environment management
    env_list: EnvList,
    cur_env: Vec<u32>,
    env_from_bbinfo_id: Vec<u32>,

    return_list: Vec<Nref>,
    halt_list: Vec<Nref>,
}

impl Env {
    fn new_ptr(bbinfo_id: u32, cfg: Nref) -> Envptr {
        Envptr::new(RefCell::new(Env {
            bbinfo_id: bbinfo_id,
            stack: ValStack::new(),
            cfg: cfg,
            loop_iv: Vec::new(),
            visited: false,
        }))
    }
}

impl FBuilder {
    fn jptr(&self) -> Jitptr {
        return Jitptr::clone(&self.jit);
    }
    fn mptr(&self) -> Mpptr {
        return Mpptr::clone(&self.jit.borrow().mpool);
    }
    fn bc_at(&self, x: u32) -> Bytecode {
        return self.function.borrow().proto.code.array[x as usize].clone();
    }
    fn bc_max_size(&self) -> u32 {
        return self.function.borrow().proto.code.array.len() as u32;
    }

    // ====================================================================
    // Environment

    fn get_env(&self, bbinfo_id: u32) -> Envptr {
        let new_env_idx = self.env_from_bbinfo_id[bbinfo_id as usize];
        let new_env = self.env_list[new_env_idx as usize].clone();
        return new_env;
    }

    const SYNTH_AND_FALSE: u32 = 1;
    const SYNTH_OR_TRUE: u32 = 2;
    const SYNTH_NONE: u32 = 0;

    fn enter_env(&mut self, bbinfo_id: u32) -> bool {
        if self.has_cur_env() {
            let env = self.cur_env();
            let bbinfo =
                &self.bbinfo_set.bbinfo_list[env.borrow().bbinfo_id as usize];
            debug_assert!(bbinfo.is_succ(bbinfo_id));
        }

        // (0) do the duplication here, notes, the bbinfo_id pointed Env should
        //     already been created done, just need to lookup it
        let (_, new_env_idx) = {
            let new_env_idx = self.env_from_bbinfo_id[bbinfo_id as usize];
            let new_env = self.env_list[new_env_idx as usize].clone();
            if new_env.borrow().visited {
                return false;
            }
            debug_assert!(new_env.borrow().bbinfo_id == bbinfo_id);

            // Merging the environment's local stack, ie placing the PHI nodes
            // before we enter into the BB. The visiting of each BB is based on
            // pre order (ignore the loop back edge), so when a BB is been
            // visiting, its predecessor nodes should already have been visited.
            let pred_len =
                self.bbinfo_set.bbinfo_list[bbinfo_id as usize].pred.len();

            match pred_len {
                1 => {
                    let prev_env_id = {
                        let jump_edge = self.bbinfo_set.bbinfo_list
                            [bbinfo_id as usize]
                            .pred[0]
                            .clone();
                        debug_assert!(!jump_edge.is_loop_back());
                        jump_edge.bid
                    };
                    // Joining predecessor's block(env)'s stack value, in this
                    // case we just have one predecessor, so just duplicate
                    // the stack from that one.
                    let prev_env = self.get_env(prev_env_id);
                    debug_assert!(prev_env.borrow().visited);
                    new_env.borrow_mut().stack = prev_env.borrow().stack.clone();
                }
                _ => {
                    // Place PHI nodes to merge all the predecessor into one
                    // and then place them into the current(new_env)'s stack
                    let mut stk_size = 0;

                    // Used to indicate synth entry or not. If so the synth
                    // entry should place more value than we thought
                    let mut is_synth = Vec::<u32>::new();
                    is_synth.resize(pred_len, FBuilder::SYNTH_NONE);

                    // 0 for debugging purpose, ie verify that whether each
                    //   predecessor has same stack expression size, otherwise
                    //   it means bytecode has BUGs or we just have BUGs
                    {
                        let mut cur_stk = Option::<u32>::None;
                        let mut idx = 0;

                        for p in self.bbinfo_set.bbinfo_list[bbinfo_id as usize]
                            .pred
                            .iter()
                        {
                            let mut synth = 0;
                            let prev_env_id = {
                                match p.jtype {
                                    JumpType::LoopBack => {
                                        continue;
                                    }
                                    JumpType::AndFalse => {
                                        synth = FBuilder::SYNTH_AND_FALSE;
                                    }
                                    JumpType::OrTrue => {
                                        synth = FBuilder::SYNTH_OR_TRUE;
                                    }
                                    _ => (),
                                };
                                p.bid
                            };

                            is_synth[idx] = synth;
                            idx += 1;

                            let prev_env = self.get_env(prev_env_id);
                            debug_assert!(prev_env.borrow().visited);

                            let addition = if synth != 0 { 1 } else { 0 };
                            let stack_size = (prev_env.borrow().stack.len()
                                as u32)
                                + addition;

                            if let Option::Some(stk) = &cur_stk {
                                debug_assert!(*stk == stack_size);
                                stk_size = *stk;
                            } else {
                                cur_stk = Option::Some(stack_size);
                                stk_size = stack_size;
                            }
                        }
                    }

                    // 1 now we just start to place the PHIs and also perform
                    //   synthetic stack mutation

                    let mut stk_idx = 0;
                    let mut vstk = ValStack::new();
                    let bbinfo_bcfrom =
                        self.bbinfo_set.bbinfo_list[bbinfo_id as usize].bc_from;

                    while stk_idx < stk_size {
                        let mut phi =
                            self.mptr().borrow_mut().new_rv_phi(bbinfo_bcfrom);
                        let mut pred_idx = 0;

                        for p in self.bbinfo_set.bbinfo_list[bbinfo_id as usize]
                            .pred
                            .iter()
                        {
                            let prev_env = self.get_env(p.bid);

                            if p.jtype == JumpType::LoopBack {
                                // loopback edge, so the predecessor may not be
                                // visited yet, just place a placeholder into
                                // the PHIs value.
                                Node::add_phi_value(
                                    &mut phi,
                                    self.mptr()
                                        .borrow_mut()
                                        .new_placeholder(bbinfo_bcfrom),
                                    Nref::clone(&prev_env.borrow().cfg),
                                );

                                // If the variable is been treated as loop iv
                                // then it should be placed into the loop iv
                                // vector for later patch
                                if self.bbinfo_set.bbinfo_list
                                    [bbinfo_id as usize]
                                    .loop_iv_assignment
                                    [stk_idx as usize]
                                {
                                    new_env
                                        .borrow_mut()
                                        .loop_iv
                                        .push((stk_idx, Nref::clone(&phi)));
                                }
                            } else {
                                let l = prev_env.borrow().stack.len() as u32;
                                if stk_idx == l {
                                    debug_assert!(
                                        is_synth[pred_idx]
                                            != FBuilder::SYNTH_NONE
                                    );
                                    debug_assert!((stk_idx + 1) == l);

                                    if is_synth[pred_idx]
                                        == FBuilder::SYNTH_AND_FALSE
                                    {
                                        Node::add_phi_value(
                                            &mut phi,
                                            self.mptr()
                                                .borrow_mut()
                                                .new_rv_load_false(
                                                    bbinfo_bcfrom,
                                                ),
                                            prev_env.borrow().cfg.clone(),
                                        );
                                    } else {
                                        debug_assert!(
                                            is_synth[pred_idx]
                                                == FBuilder::SYNTH_OR_TRUE
                                        );

                                        Node::add_phi_value(
                                            &mut phi,
                                            self.mptr()
                                                .borrow_mut()
                                                .new_rv_load_true(bbinfo_bcfrom),
                                            prev_env.borrow().cfg.clone(),
                                        );
                                    }
                                } else {
                                    let cur_val = prev_env.borrow().stack
                                        [stk_idx as usize]
                                        .clone();
                                    let cur_cfg = prev_env.borrow().cfg.clone();
                                    Node::add_phi_value(
                                        &mut phi, cur_val, cur_cfg,
                                    );
                                }
                            }
                            pred_idx += 1;
                        }
                        debug_assert!(phi.borrow().value.len() > 0);

                        if phi.borrow().value.len() == 1 {
                            // If just one value can be added into the PHIs,
                            // then just get rid of the PHIs, here.
                            let v = phi.borrow().value[0].clone();
                            phi.borrow_mut().mark_dead();
                            vstk.push(v);
                        } else {
                            // Okay, place PHI into the value stack
                            vstk.push(phi);
                        }

                        stk_idx += 1;
                    }

                    new_env.borrow_mut().stack = vstk;
                }
            };

            (new_env, new_env_idx)
        };

        // (2) set up the current environment with the duplicated env just
        //     created
        self.cur_env.push(new_env_idx);

        return true;
    }

    // Some documentation of loop iv shape ----------------------------------
    //
    // 1) Before we enter into the main ir builder, the loop iv is been detected
    //    during the bbinfo collection phase.
    //
    // 2) We eagerly place PHI before we enter into the loop for those potential
    //    iv. After the operation, the PHI node is essentially just pending PHI
    //    ie the phi doesn't have its second input value.
    //
    // 3) After we finish IR building of the BB, the current stack should hold
    //    the mutated IV value for the PHI's second input value. Notes, if user
    //    write i = i, we should just rule out this situation
    //
    fn leave_env(&mut self) {
        // 1. Patch the loop iv assignment here, ie modification of the loop
        //    variables
        for (idx, phi) in self.cur_env().borrow().loop_iv.iter() {
            let current = self.stack_index(*idx);
            if Nref::ptr_eq(&current, &phi) {
                continue;
            } else {
                let mut x = Nref::clone(phi);
                Node::add_value(&mut x, current);
            }
        }

        self.cur_env.pop();
    }

    fn cur_env(&self) -> Envptr {
        let idx = *self.cur_env.last().unwrap();
        return Envptr::clone(&self.env_list[idx as usize].clone());
    }

    fn has_cur_env(&self) -> bool {
        return self.cur_env.len() != 0;
    }

    // Try to update effect if the target X has side effect and also try to
    // generate deoptimize entry in deoptimize table if X needs a deoptimization
    //
    // Each instruction may have following effect :
    //
    //   1) If an instruction is been marked as effect, then it has to be placed
    //      into the current BB(cfg node)'s effect list, since they must be
    //      ordered with other instruction that has effect.
    //
    //   2) If an instruction does generate side effect, then it will end up with
    //      a deoptimization entry, ie sparse snapshot of states of running
    //      program.
    //
    //   3) If an instruction requires deoptimization, then it will be lowered
    //      into instruction that will finally be linked to a snapshot of the
    //      program
    //
    fn check_effect_node(&mut self, x: &Nref) {
        let has_side_effect = x.borrow().has_side_effect();

        if has_side_effect {
            let bcid = x.borrow().bcid;
            // add the effect node into current BB's effect list
            {
                let cur_env = self.cur_env();
                Node::add_effect(&mut cur_env.borrow_mut().cfg, Nref::clone(x));
            }

            // snapshot the program as deoptimization entry
            self.add_deopt(bcid);
        }
    }

    // ====================================================================
    // Stack manipulation
    fn stack_len(&self) -> usize {
        return self.cur_env().borrow().stack.len();
    }
    fn stack_index(&self, idx: u32) -> Nref {
        return self.cur_env().borrow().stack[idx as usize].clone();
    }
    fn stack_store(&self, idx: u32, v: Nref) {
        self.cur_env().borrow_mut().stack[idx as usize] = v;
    }

    fn stack_top(&self, idx: u32) -> Nref {
        let slen = self.cur_env().borrow().stack.len() as u32;
        assert!(slen >= (idx + 1));
        let i = slen - idx - 1;
        return self.stack_index(i);
    }

    fn stack_top0(&self) -> Nref {
        return self.stack_top(0);
    }

    fn stack_top1(&self) -> Nref {
        return self.stack_top(1);
    }

    fn stack_pop(&self) {
        self.cur_env().borrow_mut().stack.pop();
    }
    fn stack_pop_n(&self, x: u32) {
        let mut i = 0;
        while i < x {
            self.cur_env().borrow_mut().stack.pop();
            i += 1;
        }
    }
    fn stack_push(&mut self, x: Nref) {
        self.cur_env().borrow_mut().stack.push(x);
    }

    // for documentation purpose, notes these value pops out the value
    // automatically
    fn una(&mut self) -> Nref {
        let tos = self.stack_top0();
        self.stack_pop();
        return tos;
    }
    fn lhs(&mut self) -> Nref {
        return self.una();
    }
    fn rhs(&mut self) -> Nref {
        return self.una();
    }
    fn tos(&mut self) -> Nref {
        return self.una();
    }

    // Special function that is been used to place value into TOS
    // Notes, it does following things in correct order :
    //
    //   1) place the value into the TOS
    //   2) check whether the TOS currently have effect or not, invoking
    //      check_effect_node in correct order
    fn output_tos(&mut self, n: Nref) {
        self.stack_push(Nref::clone(&n));
        self.check_effect_node(&n);
    }

    // ====================================================================
    // Deoptimization entry
    fn add_deopt(&mut self, bc: u32) {
        debug_assert!(self.graph.borrow().deopt_table[bc as usize].is_none());
        let mut snap = self.mptr().borrow_mut().new_snapshot(bc);
        let env = self.cur_env();
        let mut stk_idx = 0;
        for x in env.borrow().stack.iter() {
            let idx = self.mptr().borrow_mut().new_imm_index(stk_idx, bc);
            let val = Nref::clone(x);
            let restore =
                self.mptr().borrow_mut().new_restore_cell(idx, val, bc);
            Node::add_value(&mut snap, restore);
            stk_idx += 1;
        }
        self.graph.borrow_mut().deopt_table[bc as usize] = Option::Some(snap);
    }

    // ====================================================================
    // Expression(statement) nodes
    // ====================================================================
    // Arithmetic
    fn b_add(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_add(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_sub(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_sub(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_mul(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_mul(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_div(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_div(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_mod(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_mod(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_pow(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_pow(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }

    // -----------------------------------------------------------------------
    // Comparison
    fn b_eq(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_eq(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_ne(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_ne(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_gt(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_gt(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_ge(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_ge(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_lt(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_lt(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }
    fn b_le(&mut self, bcpos: u32) -> bool {
        let lhs = self.lhs();
        let rhs = self.rhs();
        let val = self.mptr().borrow_mut().new_rv_le(lhs, rhs, bcpos);
        self.output_tos(val);
        return true;
    }

    // unary
    fn b_not(&mut self, bcpos: u32) -> bool {
        let v = self.una();
        let val = self.mptr().borrow_mut().new_rv_not(v, bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_neg(&mut self, bcpos: u32) -> bool {
        let v = self.una();
        let val = self.mptr().borrow_mut().new_rv_neg(v, bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_boolean(&mut self, bcpos: u32) -> bool {
        let v = self.una();
        let val = self.mptr().borrow_mut().new_rv_to_boolean(v, bcpos);
        self.output_tos(val);
        return true;
    }

    // ConStr bytecode will translate the Bytecode::ConStr into a list of string
    // concate nation opcode ie RvConStr(lhs, rhs). Notes, we don't have HIR that
    // can concatenate multiple strings together
    fn b_con_str(&mut self, tt: u32, bcpos: u32) -> bool {
        // (0) loading the strings
        let stack_len = self.stack_len() as u32;
        assert!(stack_len >= tt);
        let mut current_index = stack_len - tt;
        let val = self.stack_index(current_index);
        let mut current = self.mptr().borrow_mut().new_rv_to_string(val, bcpos);

        current_index += 1;
        loop {
            if current_index == stack_len {
                break;
            }

            let opr = self.stack_index(current_index);
            let rhs = self.mptr().borrow_mut().new_rv_to_string(opr, bcpos);
            let lhs = current;
            current = self.mptr().borrow_mut().new_rv_con_str(lhs, rhs, bcpos);
            current_index += 1;
        }

        self.stack_pop_n(tt);
        self.output_tos(current);
        return true;
    }

    // loading
    fn b_load_int(&mut self, index: Index, bcpos: u32) -> bool {
        let idx = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let val = self.mptr().borrow_mut().new_rv_load_int(idx, bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_load_real(&mut self, index: Index, bcpos: u32) -> bool {
        let idx = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let val = self.mptr().borrow_mut().new_rv_load_real(idx, bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_load_string(&mut self, index: Index, bcpos: u32) -> bool {
        let idx = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let val = self.mptr().borrow_mut().new_rv_load_string(idx, bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_load_null(&mut self, bcpos: u32) -> bool {
        let val = self.mptr().borrow_mut().new_rv_load_null(bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_load_true(&mut self, bcpos: u32) -> bool {
        let val = self.mptr().borrow_mut().new_rv_load_true(bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_load_false(&mut self, bcpos: u32) -> bool {
        let val = self.mptr().borrow_mut().new_rv_load_false(bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_load_function(&mut self, index: Index, bcpos: u32) -> bool {
        let idx = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let val = self.mptr().borrow_mut().new_rv_load_function(idx, bcpos);
        self.output_tos(val);
        return true;
    }

    // List
    // TODO(dpeng): Optimize list to support hinting of the list size for reserve
    //   memory usage internally.
    fn b_list_create(&mut self, bcpos: u32) -> bool {
        let val = self.mptr().borrow_mut().new_rv_list_create(bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_list_add(&mut self, tt: u32, _: u32) -> bool {
        let mut list = self.stack_top(tt);
        debug_assert!(list.borrow().is_rv_list_create());

        let stack_len = self.stack_len() as u32;
        let mut start_index = stack_len - tt;
        loop {
            if start_index == stack_len {
                break;
            }
            let v = self.stack_index(start_index);
            Node::add_value(&mut list, v);

            start_index += 1;
        }
        self.stack_pop_n(tt);
        return true;
    }

    // Object
    fn b_object_create(&mut self, bcpos: u32) -> bool {
        let val = self.mptr().borrow_mut().new_rv_object_create(bcpos);
        self.output_tos(val);
        return true;
    }

    fn b_object_add(&mut self, tt: u32, _: u32) -> bool {
        let mut obj = self.stack_top(tt);
        debug_assert!(obj.borrow().is_rv_object_create());

        let stack_len = self.stack_len() as u32;
        let mut start_index = stack_len - tt * 2;
        loop {
            if start_index == stack_len {
                break;
            }
            let key = self.stack_index(start_index);
            let val = self.stack_index(start_index + 1);

            Node::add_value(&mut obj, key);
            Node::add_value(&mut obj, val);

            start_index += 2;
        }

        self.stack_pop_n(tt * 2);
        return true;
    }

    // Iterators
    fn b_iterator_new(&mut self, bcpos: u32) -> bool {
        let opr = self.una();
        let itr = self.mptr().borrow_mut().new_rv_iterator_new(opr, bcpos);
        self.output_tos(itr);
        return true;
    }
    fn b_iterator_has(&mut self, bcpos: u32) -> bool {
        let opr = self.stack_top0();
        let itr = self.mptr().borrow_mut().new_rv_iterator_has(opr, bcpos);
        self.check_effect_node(&itr);
        return true;
    }
    fn b_iterator_next(&mut self, bcpos: u32) -> bool {
        let opr = self.stack_top0();
        let itr = self.mptr().borrow_mut().new_rv_iterator_has(opr, bcpos);
        self.check_effect_node(&itr);
        return true;
    }
    fn b_iterator_value(&mut self, bcpos: u32) -> bool {
        let opr = self.stack_top0();
        let itr = self.mptr().borrow_mut().new_rv_iterator_next(opr, bcpos);
        self.output_tos(itr);
        return true;
    }

    // Globals
    fn b_load_global(&mut self, index: Index, bcpos: u32) -> bool {
        let index = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let ldg = self.mptr().borrow_mut().new_rv_load_global(index, bcpos);
        self.output_tos(ldg);
        return true;
    }
    fn b_set_global(&mut self, index: Index, bcpos: u32) -> bool {
        let index = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let value = self.una();
        let stg = self
            .mptr()
            .borrow_mut()
            .new_rv_set_global(index, value, bcpos);
        self.output_tos(stg);
        return true;
    }

    // Stack manipulation, result in no IR
    fn b_stk_pop(&mut self) -> bool {
        self.stack_pop();
        return true;
    }

    fn b_stk_pop_n(&mut self, n: u32) -> bool {
        self.stack_pop_n(n);
        return true;
    }

    fn b_stk_dup(&mut self) -> bool {
        let v = self.stack_top0();
        self.output_tos(v);
        return true;
    }
    fn b_stk_dup2(&mut self) -> bool {
        let v0 = self.stack_top1();
        let v1 = self.stack_top0();
        self.output_tos(v0);
        self.output_tos(v1);
        return true;
    }
    fn b_stk_load(&mut self, idx: u32) -> bool {
        let v = self.stack_index(idx);
        self.output_tos(v);
        return true;
    }
    fn b_stk_store(&mut self, idx: u32) -> bool {
        let v = self.stack_top0();
        self.stack_store(idx, v);
        return true;
    }
    fn b_stk_push_n(&mut self, idx: u32, bcpos: u32) -> bool {
        let mut i = 0;
        while i < idx {
            let v = self.mptr().borrow_mut().new_rv_load_null(bcpos);
            self.output_tos(v);
            i += 1;
        }
        return true;
    }

    // memory
    fn b_dot_load(&mut self, idx: Index, bcpos: u32) -> bool {
        let recv = self.una();
        let idx = self.mptr().borrow_mut().new_imm_index(idx, bcpos);
        let v = self.mptr().borrow_mut().new_dot_access(recv, idx, bcpos);
        self.output_tos(v);
        return true;
    }

    fn b_dot_store(&mut self, idx: Index, bcpos: u32) -> bool {
        let value = self.tos();
        let recv = self.tos();

        let idx = self.mptr().borrow_mut().new_imm_index(idx, bcpos);
        let v = self
            .mptr()
            .borrow_mut()
            .new_dot_store(recv, idx, value, bcpos);
        self.output_tos(v);
        return true;
    }

    fn b_index_store(&mut self, bcpos: u32) -> bool {
        let value = self.tos();
        let index = self.tos();
        let recv = self.tos();
        let v = self
            .mptr()
            .borrow_mut()
            .new_index_store(recv, index, value, bcpos);
        self.output_tos(v);
        return true;
    }

    fn b_index_load(&mut self, bcpos: u32) -> bool {
        let index = self.tos();
        let recv = self.tos();
        let v = self.mptr().borrow_mut().new_index_load(recv, index, bcpos);
        self.output_tos(v);
        return true;
    }

    // --------------------------------------------------------------
    // Builtin
    // --------------------------------------------------------------
    fn b_assert1(&mut self, bcpos: u32) -> bool {
        let cond = self.tos();
        let v = self.mptr().borrow_mut().new_rv_assert1(cond, bcpos);
        self.output_tos(v);
        return true;
    }

    fn b_assert2(&mut self, bcpos: u32) -> bool {
        let msg = self.tos();
        let cond = self.tos();
        let v = self.mptr().borrow_mut().new_rv_assert2(msg, cond, bcpos);
        self.output_tos(v);
        return true;
    }

    fn b_trace(&mut self, c: u32, bcpos: u32) -> bool {
        let stack_len = self.stack_len() as u32;
        let mut start_index = stack_len - c;
        let mut trace = self.mptr().borrow_mut().new_rv_trace(bcpos);
        self.check_effect_node(&trace);

        loop {
            if start_index == stack_len {
                break;
            }
            let v = self.stack_index(start_index);
            Node::add_value(&mut trace, v);
            start_index += 1;
        }
        self.stack_pop_n(c);
        return true;
    }

    fn b_typeof(&mut self, bcpos: u32) -> bool {
        let tos = self.una();
        let nd = self.mptr().borrow_mut().new_rv_typeof(tos, bcpos);
        self.check_effect_node(&nd);
        return true;
    }

    fn b_sizeof(&mut self, bcpos: u32) -> bool {
        let tos = self.una();
        let nd = self.mptr().borrow_mut().new_rv_sizeof(tos, bcpos);
        self.check_effect_node(&nd);
        return true;
    }

    fn b_halt(&mut self, bcpos: u32) -> bool {
        let tos = self.una();
        let nd = self.mptr().borrow_mut().new_rv_halt(tos, bcpos);
        self.check_effect_node(&nd);
        return true;
    }

    // upvalue
    fn b_load_upvalue(&mut self, index: Index, bcpos: u32) -> bool {
        let idx = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let nd = self.mptr().borrow_mut().new_rv_load_upvalue(idx, bcpos);
        self.output_tos(nd);
        return true;
    }
    fn b_store_upvalue(&mut self, index: Index, bcpos: u32) -> bool {
        let idx = self.mptr().borrow_mut().new_imm_index(index, bcpos);
        let value = self.tos();
        let nd = self
            .mptr()
            .borrow_mut()
            .new_rv_set_upvalue(idx, value, bcpos);
        self.check_effect_node(&nd);
        return true;
    }

    fn build_one_bc_in_bb(&mut self, bc: Bytecode, bcpos: u32) -> bool {
        return match bc {
            // arithmetic
            Bytecode::Add => self.b_add(bcpos),
            Bytecode::Sub => self.b_sub(bcpos),
            Bytecode::Mul => self.b_mul(bcpos),
            Bytecode::Div => self.b_div(bcpos),
            Bytecode::Mod => self.b_mod(bcpos),
            Bytecode::Pow => self.b_pow(bcpos),

            Bytecode::ConStr(cnt) => self.b_con_str(cnt, bcpos),

            // comparison
            Bytecode::Eq => self.b_eq(bcpos),
            Bytecode::Ne => self.b_ne(bcpos),
            Bytecode::Gt => self.b_gt(bcpos),
            Bytecode::Ge => self.b_ge(bcpos),
            Bytecode::Lt => self.b_lt(bcpos),
            Bytecode::Le => self.b_le(bcpos),

            // unary
            Bytecode::Not => self.b_not(bcpos),
            Bytecode::Neg => self.b_neg(bcpos),
            Bytecode::Boolean => self.b_boolean(bcpos),

            // literal loading
            Bytecode::LoadInt(i) => self.b_load_int(i, bcpos),
            Bytecode::LoadReal(i) => self.b_load_real(i, bcpos),
            Bytecode::LoadString(i) => self.b_load_string(i, bcpos),
            Bytecode::LoadNull => self.b_load_null(bcpos),
            Bytecode::LoadTrue => self.b_load_true(bcpos),
            Bytecode::LoadFalse => self.b_load_false(bcpos),
            Bytecode::LoadFunction(i) => self.b_load_function(i, bcpos),

            // lists
            Bytecode::ListStart => self.b_list_create(bcpos),
            Bytecode::ListAdd(c) => self.b_list_add(c, bcpos),

            // object
            Bytecode::ObjectStart => self.b_object_create(bcpos),
            Bytecode::ObjectAdd(c) => self.b_object_add(c, bcpos),

            // iterator
            Bytecode::IteratorNew => self.b_iterator_new(bcpos),
            Bytecode::IteratorStart => self.b_iterator_has(bcpos),
            Bytecode::IteratorHas => self.b_iterator_has(bcpos),
            Bytecode::IteratorNext => self.b_iterator_next(bcpos),
            Bytecode::IteratorValue => self.b_iterator_value(bcpos),

            // global
            Bytecode::LoadGlobal(i) => self.b_load_global(i, bcpos),
            Bytecode::SetGlobal(i) => self.b_set_global(i, bcpos),

            // stack
            Bytecode::Pop => self.b_stk_pop(),
            Bytecode::PopN(x) => self.b_stk_pop_n(x),
            Bytecode::Dup => self.b_stk_dup(),
            Bytecode::Dup2 => self.b_stk_dup2(),
            Bytecode::Load(x) => self.b_stk_load(x),
            Bytecode::Store(x) => self.b_stk_store(x),
            Bytecode::PushN(x) => self.b_stk_push_n(x, bcpos),

            // memory
            Bytecode::DotAccess(i) => self.b_dot_load(i, bcpos),
            Bytecode::DotStore(i) => self.b_dot_store(i, bcpos),
            Bytecode::ArrayIndex => self.b_index_load(bcpos),
            Bytecode::ArrayStore => self.b_index_store(bcpos),

            // builtins
            Bytecode::Assert1 => self.b_assert1(bcpos),
            Bytecode::Assert2 => self.b_assert2(bcpos),
            Bytecode::Trace(x) => self.b_trace(x, bcpos),
            Bytecode::Typeof => self.b_typeof(bcpos),
            Bytecode::Sizeof => self.b_sizeof(bcpos),

            // upvalue
            Bytecode::LoadUpvalue(i) => self.b_load_upvalue(i, bcpos),
            Bytecode::SetUpvalue(i) => self.b_store_upvalue(i, bcpos),

            // notes, Halt is not written here since it is a control flow
            // indicator, which is taken cared specially
            xx @ _ => {
                unreachable!();
            }
        };
    }

    fn build_bc_in_bb(&mut self) -> bool {
        let (mut bc_start, bc_end) = {
            let bbinfo = &self.bbinfo_set.bbinfo_list
                [self.cur_env().borrow().bbinfo_id as usize];

            (bbinfo.bc_from, bbinfo.bc_to)
        };

        while bc_start < bc_end {
            let bytecode = self.bc_at(bc_start);
            if !self.build_one_bc_in_bb(bytecode, bc_start) {
                return false;
            }
            bc_start += 1;
        }

        // =====================================================================
        // handle the last bytecode in the BB
        let bytecode = self.bc_at(bc_end);
        match bytecode {
            Bytecode::JumpFalse(_) => {
                let mut cfg = Nref::clone(&self.cur_env().borrow().cfg);
                Node::add_value(&mut cfg, self.una());
            }

            // The following And/Or bytecode's stack consistency is not
            // maintained correctly until the successor block been met for code
            // generation. The fix is basically modify the stack value
            Bytecode::And(_) => {
                let mut cfg = Nref::clone(&self.cur_env().borrow().cfg);
                Node::add_value(&mut cfg, self.una());
            }
            Bytecode::Or(_) => {
                let mut cfg = Nref::clone(&self.cur_env().borrow().cfg);
                Node::add_value(&mut cfg, self.una());
            }

            Bytecode::Ternary(_) => {
                let mut cfg = Nref::clone(&self.cur_env().borrow().cfg);
                Node::add_value(&mut cfg, self.una());
            }

            // halt and return instructions
            Bytecode::Halt | Bytecode::Return(_) => {
                let mut cfg = Nref::clone(&self.cur_env().borrow().cfg);
                Node::add_value(&mut cfg, self.una());
            }

            // do nothing for unconditional jump
            Bytecode::Jump(_) | Bytecode::LoopBack(_) => (),

            // natural jump
            _ => {
                // notes, it means this is a normal instruction and we should
                // interpret it regardlessly
                if !self.build_one_bc_in_bb(bytecode, bc_end) {
                    return false;
                }
            }
        };

        return true;
    }

    // Trying to build a basic block specified by the indicated bbinfo_id, this
    // will create a temporary Env on top of env_list and create the correspond
    // basic block in noder. It will generate the full IR graph in that block
    fn build_one_bb(&mut self, bbinfo_id: u32) -> bool {
        if self.enter_env(bbinfo_id) {
            self.cur_env().borrow_mut().visited = true;
            if !self.build_bc_in_bb() {
                return false;
            }
            self.leave_env();
        }
        return true;
    }

    // used to verify whether all the BB has been visited or not.
    fn get_bb_unvisited(&self) -> Option<u32> {
        let mut idx = 0;
        for xx in self.env_list.iter() {
            if !xx.borrow().visited {
                return Option::Some(idx);
            }
            idx += 1;
        }
        return Option::None; 
    } 

    fn verify_all_bb_visited(&self) -> bool { 
        return match self.get_bb_unvisited() {
            Option::Some(_) => false,
            _ => true,
        };
    }

    // Visiting each BB in pre-order. Notes, the pre-order is little bit more
    // complicated for graph than tree. The reason is the following graph
    //
    //  [A]----
    //   |    |
    //   |    |
    //  [B]   |
    //   |    |
    //   ----[C]
    //
    // The block C's visitation order may be earlier than B since A has a direct
    // edge link to C. For this case, we need to take extra steps to make sure
    // that B is always before C.
    //
    // To allow the graph visitation to maintain strict pre-order, we do the
    // following stuff.
    //
    //   1. Let q be a Queue
    //   2. Push first block into the q
    //   3. while q is not empty
    //     3.1 check top of the q's preds have been visited or not,
    //       3.1.1 if not, then push all the pred into the q and then lastly
    //             push top into the q
    //       3.1.2 otherwise, visit top and then pop from q
    //
    //  Notes, the above iteration will ignore the loop_back edge
    
    fn enqueue_bb_child(&self, wqueue: &mut Nidqueue, 
                        visited: &mut BitVec, child: &Option<JumpEdge>) {
        match child {
            Option::Some(x) => {
                if x.jtype != JumpType::LoopBack {
                    // checking pred of x whether they are all visited or not
                    for p in self.bbinfo_set.bbinfo_list[x.bid as usize].pred.iter() {
                        if !visited[p.bid as usize] {
                            wqueue.push_back(p.bid);
                        }
                    }
                    if !visited[x.bid as usize] {
                        wqueue.push_back(x.bid);
                    }
                }
            }
            _ => (),
        };
    }

    // Trying to build the full basic block. Assume all the BB has already been
    // linked and its just its content has not been emitted correctly
    fn build_bb(&mut self) -> bool {
        let mut wqueue = Nidqueue::new();
        let mut visited = BitVec::new();
        visited.resize(self.bbinfo_set.bbinfo_list.len(), false);
        wqueue.push_back(0);

        // Pre-order visit each BB
        while wqueue.len() != 0 {

            // The following part is kind of tricky. Since we are dealing with
            // a graph, potentially, a node can be pushed into the queue multiple
            // times due to following reasons :
            //   1) Its pred is not visited, so itself cannot be visited
            //
            //   2) Its in the queue, but haven't been visited, some other node
            //      that is been visited can reach this node as successor, so
            //      the node is not visited yet
            //
            // We have one invariants needs to keep, which is a node cannot be
            // visited if it has pred not been visited. So when we pop the node
            // out of the queue, we need to make sure that the pred is been
            // visited, if not, then repush the pred that is not been visited
            // into the queue along with the node just been poped and rerun the
            // queue pop logic again to find the node that met the requirements
            //
            // Essentially it is just a graph pre-order visiting algorithm
            let cur_idx = wqueue.pop_front().unwrap();
            if visited[cur_idx as usize] {
                continue;
            } else {
                let mut has_pred_not_visit = false;
                for p in self.bbinfo_set.bbinfo_list[cur_idx as usize].pred.iter() {
                    if !visited[p.bid as usize] {
                        has_pred_not_visit = true;
                        wqueue.push_back(p.bid);
                    }
                }
                if has_pred_not_visit {
                    wqueue.push_back(cur_idx);
                    continue;
                } else {
                    *visited.get_mut(cur_idx as usize).unwrap() = true;
                }
            }

            if !self.build_one_bb(cur_idx) {
                return false;
            }

            // Enqueue its lhs and rhs, ignore the jump back edge
            {
                let bb = &self.bbinfo_set.bbinfo_list[cur_idx as usize];

                // visiting order matters -----------------------------------
                // always visiting lhs first, it is false branch and then the
                // rhs branch which is the true branch
                self.enqueue_bb_child(&mut wqueue, &mut visited, &bb.lhs);
                self.enqueue_bb_child(&mut wqueue, &mut visited, &bb.rhs);
            }
        }

        debug_assert!(self.verify_all_bb_visited());
        return true;
    }

    // function to create cfg node for specified bb
    fn create_cfg_node(&mut self, bbinfo_id: u32) -> Nref {
        let (_, bc_to) = {
            let x = &self.bbinfo_set.bbinfo_list[bbinfo_id as usize];
            (x.bc_from, x.bc_to)
        };
        match self.bc_at(bc_to) {
            Bytecode::Ternary(_)
            | Bytecode::JumpFalse(_)
            | Bytecode::And(_)
            | Bytecode::Or(_) => {
                return self.mptr().borrow_mut().new_cfg_if_cmp(bc_to);
            }
            Bytecode::LoopBack(_) => {
                return self.mptr().borrow_mut().new_cfg_loop_back(bc_to);
            }
            Bytecode::Halt => {
                let x = self.mptr().borrow_mut().new_cfg_halt(bc_to);
                self.halt_list.push(Nref::clone(&x));
                return x;
            }
            Bytecode::Return(_) => {
                let x = self.mptr().borrow_mut().new_cfg_return(bc_to);
                self.return_list.push(Nref::clone(&x));
                return x;
            }
            _ => {
                return self.mptr().borrow_mut().new_cfg_jump(bc_to);
            }
        };
    }

    fn link_cfg_edge(&self, cur_cfg: &Nref, j: Option<JumpEdge>) {
        match j {
            Option::Some(je) => {
                let env = self.get_env(je.bid);
                let cfg = Nref::clone(&env.borrow().cfg);
                let mut cur = Nref::clone(cur_cfg);
                Node::add_control(&mut cur, cfg);
            }
            _ => (),
        };
    }

    // Prepare phase, ie create all the bb based on the bbinfo_set and then
    // link them together and put correct CFG node type
    fn setup_all_bb(&mut self) {
        debug_assert!(self.env_list.len() == 0);
        debug_assert!(self.cur_env.len() == 0);
        debug_assert!(self.env_from_bbinfo_id.len() == 0);

        let bb_size = self.bbinfo_set.bbinfo_list.len();

        // (0) create all the Env object according to the bbinfo_set, notes this
        //     pass doesn't actually link the BB for simplicity. The linking is
        //     been part the code generation phas in each basic block
        {
            self.env_from_bbinfo_id.resize(bb_size, 0);
            let mut bbinfo_id = 0;
            let bbinfo_size = self.bbinfo_set.bbinfo_list.len();

            while bbinfo_id < bbinfo_size {
                // notes this cfg_node just do a basic creation, we havent' link
                // them together and no value related to the cfg node for now
                let cfg_node = self.create_cfg_node(bbinfo_id as u32);

                self.env_list.push(Env::new_ptr(bbinfo_id as u32, cfg_node));
                self.env_from_bbinfo_id[bbinfo_id] =
                    (self.env_list.len() - 1) as u32;

                bbinfo_id += 1;
            }
        }

        // (1) linking all the nodes together based on the linking marker
        {
            for env in self.env_list.iter() {
                let lhs = self.bbinfo_set.bbinfo_list
                    [env.borrow().bbinfo_id as usize]
                    .lhs
                    .clone();

                let rhs = self.bbinfo_set.bbinfo_list
                    [env.borrow().bbinfo_id as usize]
                    .rhs
                    .clone();

                let cfg = Nref::clone(&env.borrow().cfg);
                self.link_cfg_edge(&cfg, lhs);
                self.link_cfg_edge(&cfg, rhs);
            }
        }
    }

    fn build(&mut self) -> bool {
        // (0) prepare the building, creating all the BB and Env object
        self.setup_all_bb();

        // (1) perform a Pre-order visit of the BB and perform the bytecode
        //     to IR transformation along with BB linking
        if !self.build_bb() {
            return false;
        }

        // (2) Finalize the graph with special node and linking, notes the
        //     current graph does not have in place deoptimization but the
        //     deoptimization entry is been put into external deoptimization
        //     table.
        {
            let first_bb = Nref::clone(&self.env_list[0].borrow().cfg);
            let mut start = Nref::clone(&self.graph.borrow().cfg_start);
            let end = Nref::clone(&self.graph.borrow().cfg_end);

            // linking the start directly to the first_bb
            Node::add_control(&mut start, first_bb);

            // add return region
            if self.return_list.len() != 0 {
                let mut merge = self
                    .mptr()
                    .borrow_mut()
                    .new_cfg_merge_return(self.bc_max_size());

                Node::add_control(&mut merge, Nref::clone(&end));

                for xx in self.return_list.iter_mut() {
                    Node::add_control(xx, Nref::clone(&merge));
                }
            }

            // add halt region
            if self.halt_list.len() != 0 {
                let mut merge = self
                    .mptr()
                    .borrow_mut()
                    .new_cfg_merge_halt(self.bc_max_size());

                Node::add_control(&mut merge, Nref::clone(&end));

                for xx in self.halt_list.iter_mut() {
                    Node::add_control(xx, Nref::clone(&merge));
                }
            }
        }

        self.jit
            .borrow_mut()
            .graph_list
            .push(FGraphptr::clone(&self.graph));

        return true;
    }

    fn new(
        fref: FuncRef,
        acount: u32,
        osr: Option<OSRInfo>,
        jit: Jitptr,
    ) -> FBuilder {
        let bbinfo_set = BBInfoBuilder::build(FuncRef::clone(&fref), acount);
        let graph = FGraph::new(
            &mut jit.borrow_mut().mpool,
            FuncRef::clone(&fref),
            acount,
        );

        FBuilder {
            function: fref,
            arg_count: acount,
            jit: jit,
            graph: graph,
            bbinfo_set: bbinfo_set,
            osr: osr,
            env_list: EnvList::new(),
            cur_env: Vec::new(),
            env_from_bbinfo_id: Vec::new(),
            return_list: Vec::new(),
            halt_list: Vec::new(),
        }
    }

    pub fn build_func(fref: FuncRef, acount: u32, jit: Jitptr) -> bool {
        let mut x = FBuilder::new(fref, acount, Option::None, jit);
        return x.build();
    }
}

// --------------------------------------------------------------------------
// Testing

#[cfg(test)]
mod fbuilder_tests {
    use super::*;
    use crate::heap::heap::*;
    use crate::jit::j::*;
    use crate::jit::node_print::*;
    use crate::syntax::parser::*;

    // ----------------------------------------------------------------------
    // basic
    //
    // printing the code with its bytecode and also its graph representation
    fn print_code(code: &str) -> Option<(String, String)> {
        let g = G::new(GHeapConfig::default());

        // (0) doing the parsing, now the code has been transferred into the
        //     bytecode sequenecs
        let proto = match do_parse(Gptr::clone(&g), code, "[tests]") {
            Result::Ok(v) => ProtoRc::new(v),
            Result::Err(e) => {
                println!(
                    "parsing failed: {}@{} {}",
                    e.column, e.line, e.description
                );
                return Option::None;
            }
        };

        let code_string = proto.code.dump();
        println!("{}", code_string);

        // (1) creating the function object, since we don't have any runtime
        //     information, so just doing nothing at all here.
        let func = g.borrow_mut().heap.new_function(proto, HandleList::new());

        // (2) building the graph
        let jit = Jit::new();
        assert!(FBuilder::build_func(func, 0, Jitptr::clone(&jit)));

        // (3) printing into string
        let graph_string = print_graph(
            &jit.borrow().graph_list[0].borrow().cfg_start,
            jit.borrow().max_node_id(),
        );

        return Option::Some((code_string, graph_string));
    }

    #[test]
    fn basic() {
        match print_code(
            r#"
let a = 10;
if a > 10 {
    a = 20;
} elif (a == 200) {
    a = 30;
} elif (a == 40) {
    a = -10;
} elif (a == 20) {
    a -= 20;
} else {
    a = 10;
}

return a + 10;
            "#,
        ) {
            Option::Some((a, b)) => {
                println!("========================================");
                println!("            bytecode");
                println!("========================================");
                println!("{}", a);

                println!("========================================");
                println!("            graph");
                println!("========================================");
                println!("{}", b);
            }

            _ => (),
        };
    }
}
