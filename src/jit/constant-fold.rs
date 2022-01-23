use crate::jit::iter::*;
use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

pub enum FoldMode {
    Tolerate,
    BailoutWhenError,
}

// Simple constant folding, notes fancier expression optimization will be done
// when the lower pass finished.
pub struct ConstantFoldPass {
    j: Jitptr,
    f: FGraphptr,
}

impl ConstantFoldPass {
    // Helpers
    fn upgrade_to_i64(imm: &Imm) -> i64 {
        return match imm {
            Imm::Index(v) => v as i64,
            Imm::ImmU32(v) => v as i64,
            Imm::ImmU16(v) => v as i64,
            Imm::ImmU8(v) => v as i64,

            Imm::ImmI32(v) => v as i64,
            Imm::ImmI16(v) => v as i64,
            Imm::ImmI8(v) => v as i64,
            Imm::ImmI64(v) => v,

            _ => unreachable!(),
        };
    }

    fn upgrade_to_real(imm: &Imm) -> f64 {
        return match imm {
            Imm::Index(v) => v as f64,
            Imm::ImmU32(v) => v as f64,
            Imm::ImmU16(v) => v as f64,
            Imm::ImmU8(v) => v as f64,

            Imm::ImmI32(v) => v as f64,
            Imm::ImmI16(v) => v as f64,
            Imm::ImmI8(v) => v as f64,
            Imm::ImmI64(v) => v as f64,

            _ => unreachable!(),
        };
    }

    fn imm_to_str(imm: &Imm) -> &str {
        return match imm {
            Imm::ImmString(x) => &x,
            _ => unreachable!(),
        };
    }

    // =========================================================================
    // Arithmetic operation related folding optimization
    fn arith_constant_fold(n: &mut Nref, l: &mut Nref, r: &mut Nref) -> bool {
        let op = n.borrow().op.op.clone();

        if l.borrow().is_imm_int() && r.borrow().is_imm_int() {
            let lv = ConstantFoldPass::upgrade_to_i64(&l.borrow().imm);
            let rv = ConstantFoldPass::upgrade_to_i64(&r.borrow().imm);

            let ov = match op {
                Opcode::RvAdd => lv + rv,
                Opcode::RvSub => lv - rv,
                Opcode::RvMul => lv * rv,
                xx @ Opcode::RvDiv | Opcode::RvMod => {
                    if rv == 0 {
                        Node::replace_and_dispose(
                            n,
                            self.mptr()
                                .borrow_mut()
                                .new_trap_div_zero(n.bc.clone()),
                        );
                        return;
                    }

                    if xx == Opcode::RvDiv {
                        lv / rv
                    } else {
                        lv % rv
                    }
                }
                Opcode::RvPow => lv.pow(rv),
            };

            let out = self.mptr().borrow_mut().new_imm_i64(ov, n.bc.clone());

            Node::replace_and_dispose(n, out);
            return true;
        }

        if (l.borrow().is_imm_real() && r.borrow().is_imm_real())
            || (l.borrow().is_imm_num() && r.borrow().is_imm_num())
        {
            let lv = ConstantFoldPass::upgrade_to_real(&l.borrow().imm);
            let rv = ConstantFoldPass::upgrade_to_real(&r.borrow().imm);

            let ov = match op {
                Opcode::RvAdd => lv + rv,
                Opcode::RvSub => lv - rv,
                Opcode::RvMul => lv * rv,
                Opcode::RvDiv => lv / rv,
                Opcode::RvMod => lv % rv,
                Opcode::RvPow => lv.pow(rv),
            };
            let out = self.mptr().borrow_mut().new_imm_f64(ov, n.bc.clone());

            Node::replace_and_dispose(n, out);
            return true;
        }

        if op == Opcode::RvAdd
            && l.borrow().is_imm_str()
            && r.borrow().is_imm_str()
        {
            let mut s = String::new();
            s.push_str(ConstantFoldPass::imm_to_str(&l.borrow().imm));
            s.push_str(ConstantFoldPass::imm_to_str(&r.borrow().imm));

            let out = self.mptr().borrow_mut().new_imm_string(s, n.bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }

        return false;
    }

    // =========================================================================
    // Comparison operation related folding optimization
    fn comp_constant_fold(n: &mut Nref, l: &mut Nref, r: &mut Nref) -> bool {}
}
