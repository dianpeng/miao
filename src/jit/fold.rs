// Folding pass, the basic local expression optimization pass

use crate::jit::iter::*;
use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

pub enum FoldMode {
    Tolerate,
    BailoutWhenError,
}

pub struct FoldPass {
    j: Jitptr,
    f: FGraphptr,
}

impl FoldPass {
    // -------------------------------------------------------------------------
    // Simple fold pass, which just does direct constant folding or other
    // optimization and also reorder/reassociate the expression to allow second
    // pass to perform optimization
    fn simple_fold(&mut self) -> PassResult {
        let bb = self.f.borrow().cfg_start.clone();
        let max_id = self.j.borrow().max_node_id();

        for cur in NodePOIter::new(&bb, max_id) {
            if cur.borrow().is_dead() {
                continue;
            }

            match cur.borrow().op.op {
                Opcode::RvAdd
                | Opcode::RvSub
                | Opcode::RvMul
                | Opcode::RvDiv
                | Opcode::RvMod
                | Opcode::RvPow => {
                    self.arith_pass1(Nref::clone(cur));
                }

                Opcode::RvEq
                | Opcode::RvNe
                | Opcode::RvGt
                | Opcode::RvGe
                | Opcode::RvLt
                | Opcode::RvLe => {
                    self.comp_pass1(Nref::clone(cur));
                }

                _ => (),
            };
        }
    }

    // =========================================================================
    // Arithmetic operation related folding optimization

    fn arith_simple_fold(
        n: &mut Nref,
        l: &mut Nref,
        r: &mut Nref,
    ) -> bool {
        let op = n.borrow().op.op.clone();

        if l.borrow().is_imm_int() && r.borrow().is_imm_int() {
            let lv = l.borrow().imm.upgrade_to_int();
            let rv = r.borrow().imm.upgrade_to_int();

            let ov = match op {
                Opcode::RvAdd => lv + rv,
                Opcode::RvSub => lv - rv,
                Opcode::RvMul => lv * rv,
                Opcode::RvDiv => {
                    if rv == 0 {
                        Node::replace(
                            n,
                            self.mptr()
                                .borrow_mut()
                                .new_trap_div_zero(n.bc.clone()),
                        );
                        n.mark_dead();
                        return;
                    }

                    lv / rv
                }
                Opcode::RvMod => {
                    if rv == 0 {
                        Node::replace(
                            n,
                            self.mptr()
                                .borrow_mut()
                                .new_trap_div_zero(n.bc.clone()),
                        );
                        n.mark_dead();
                        return;
                    }

                    lv % rv
                }
                Opcode::RvPow => lv.pow(rv),
            };

            let out = self.mptr().borrow_mut().new_imm_i64(ov, n.bc.clone());

            Node::replace(n, out);
            n.mark_dead();
            return true;
        }

        if (l.borrow().is_imm_real() && r.borrow().is_imm_real())
            || (l.borrow().is_imm_num() && r.borrow().is_imm_num())
        {
            let lv = l.borrow().imm.upgrade_to_real();
            let rv = r.borrow().imm.upgrade_to_real();
            let ov = match op {
                Opcode::RvAdd => lv + rv,
                Opcode::RvSub => lv - rv,
                Opcode::RvMul => lv * rv,
                Opcode::RvDiv => lv / rv,
                Opcode::RvMod => lv % rv,
                Opcode::RvPow => lv.pow(rv),
            };
            let out = self.mptr().borrow_mut().new_imm_f64(ov, n.bc.clone());
            Node::replace(n, out);
            n.mark_dead();
            return true;
        }

        if op == Opcode::RvAdd
            && l.borrow().is_imm_str()
            && r.borrow().is_imm_str()
        {
            let mut s = String::new();
            s.push_str(l.borrow().imm.to_str());
            s.push_str(r.borrow().imm.to_str());

            let out = self.mptr().borrow_mut().new_imm_string(s, n.bc.clone());
            Node::replace(n, out);
            n.mark_dead();
            return true;
        }

        return false;
    }

    // Integer related arithmetic optimization other than simple constant fold
    // 
    // The work that it tries to perform are as following :
    //
    //   1) (1+x) => (x+1), ie constant will be put in RHS instead of LHS, this
    //      is for future constant folding
    //
    //   2) (x+1) + y => (x+y) + 1, if both x and y are integer
    //
    //   3) Identity tests, assume none constant node is integer
    //      (x+0) (0+x) => x
    //      (x-0) (0-x) => x/-x
    //      (x*1) (1*x) => x
    //
    fn arith_int_optimize(&mut n, &mut lhs, &mut rhs) -> bool {
        return false;
    }

    fn arith_pass1(&mut self, n: Nref) {
        debug_assert!(n.borrow().is_value_binary());

        let lhs = n.borrow().lhs();
        let rhs = n.borrow().rhs();

        // 0. check whether we have simple constant folding opportunity or not
        if self.arith_simple_fold(&mut n, &mut lhs, &mut rhs) {
            return;
        }

        // 1. if constant folding doesn't work, then try integer reorder
        if self.arith_int_optimize(&mut n, &mut lhs, &mut rhs) {
            return;
        }

        return;
    }
}
