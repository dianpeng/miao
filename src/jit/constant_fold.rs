use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

pub enum FoldMode {
    Tolerate,
    BailoutWhenError,
}

// Simple constant folding, notes fancier expression optimization will be done
// when the lower pass finished.
pub struct ConstantFold {
    j: Jitptr,
    f: FGraphptr,
}

impl ConstantFold {
    // Helpers
    fn mptr(&self) -> Mpptr {
        return self.j.borrow().mpool.clone();
    }

    fn upgrade_to_i64(imm: &Imm) -> i64 {
        return match imm {
            Imm::Index(v) => *v as i64,
            Imm::ImmU32(v) => *v as i64,
            Imm::ImmU16(v) => *v as i64,
            Imm::ImmU8(v) => *v as i64,

            Imm::ImmI32(v) => *v as i64,
            Imm::ImmI16(v) => *v as i64,
            Imm::ImmI8(v) => *v as i64,
            Imm::ImmI64(v) => *v,

            _ => unreachable!(),
        };
    }

    fn upgrade_to_real(imm: &Imm) -> f64 {
        return match imm {
            Imm::Index(v) => *v as f64,
            Imm::ImmU32(v) => *v as f64,
            Imm::ImmU16(v) => *v as f64,
            Imm::ImmU8(v) => *v as f64,

            Imm::ImmI32(v) => *v as f64,
            Imm::ImmI16(v) => *v as f64,
            Imm::ImmI8(v) => *v as f64,
            Imm::ImmI64(v) => *v as f64,

            _ => unreachable!(),
        };
    }

    fn imm_to_str(imm: &Imm) -> &str {
        return match imm {
            Imm::ImmStr(x) => &x,
            _ => unreachable!(),
        };
    }

    fn imm_conv_to_boolean(imm: &Imm) -> Option<bool> {
        return match imm {
            Imm::ImmU32(v) => Option::Some(*v != 0),
            Imm::ImmU16(v) => Option::Some(*v != 0),
            Imm::ImmU8(v) => Option::Some(*v != 0),
            Imm::ImmI64(v) => Option::Some(*v != 0),
            Imm::ImmI32(v) => Option::Some(*v != 0),
            Imm::ImmI16(v) => Option::Some(*v != 0),
            Imm::ImmI8(v) => Option::Some(*v != 0),
            Imm::ImmF64(v) => Option::Some(*v != 0.0),
            Imm::ImmBoolean(b) => Option::Some(*b),
            Imm::ImmNull => Option::Some(false),
            _ => Option::None,
        };
    }

    fn imm_conv_to_string(imm: &Imm) -> Option<String> {
        return match imm {
            Imm::ImmU32(v) => Option::Some(v.to_string()),
            Imm::ImmU16(v) => Option::Some(v.to_string()),
            Imm::ImmU8(v) => Option::Some(v.to_string()),
            Imm::ImmI64(v) => Option::Some(v.to_string()),
            Imm::ImmI32(v) => Option::Some(v.to_string()),
            Imm::ImmI16(v) => Option::Some(v.to_string()),
            Imm::ImmI8(v) => Option::Some(v.to_string()),
            Imm::ImmF64(v) => Option::Some(v.to_string()),
            Imm::ImmBoolean(b) => Option::Some(if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }),
            Imm::ImmNull => Option::Some("null".to_string()),
            _ => Option::None,
        };
    }

    // =========================================================================
    // Arithmetic operation related folding optimization
    fn arith_constant_fold(
        &self,
        n: &mut Nref,
        l: &mut Nref,
        r: &mut Nref,
    ) -> bool {
        let op = n.borrow().op.op.clone();

        if l.borrow().is_imm_int() && r.borrow().is_imm_int() {
            let lv = ConstantFold::upgrade_to_i64(&l.borrow().imm);
            let rv = ConstantFold::upgrade_to_i64(&r.borrow().imm);

            let ov = match op {
                Opcode::RvAdd => lv + rv,
                Opcode::RvSub => lv - rv,
                Opcode::RvMul => lv * rv,
                xx @ Opcode::RvDiv | xx @ Opcode::RvMod => {
                    if rv == 0 {
                        let bc = n.borrow().bc.clone();
                        Node::replace_and_dispose(
                            n,
                            self.mptr().borrow_mut().new_error_div_zero(bc),
                        );
                        return true;
                    }

                    if xx == Opcode::RvDiv {
                        lv / rv
                    } else {
                        lv % rv
                    }
                }
                Opcode::RvPow => {
                    if rv < 0 {
                        let bc = n.borrow().bc.clone();
                        Node::replace_and_dispose(
                            n,
                            self.mptr()
                                .borrow_mut()
                                .new_error_pow_negative_int(bc),
                        );
                        return true;
                    }
                    lv.pow(rv as u32)
                }
                _ => unreachable!(),
            };

            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_i64(ov, n.borrow().bc.clone());

            Node::replace_and_dispose(n, out);
            return true;
        }

        if (l.borrow().is_imm_real() && r.borrow().is_imm_real())
            || (l.borrow().is_imm_num() && r.borrow().is_imm_num())
        {
            let lv = ConstantFold::upgrade_to_real(&l.borrow().imm);
            let rv = ConstantFold::upgrade_to_real(&r.borrow().imm);

            let ov = match op {
                Opcode::RvAdd => lv + rv,
                Opcode::RvSub => lv - rv,
                Opcode::RvMul => lv * rv,
                Opcode::RvDiv => lv / rv,
                Opcode::RvMod => lv % rv,
                Opcode::RvPow => lv.powf(rv),
                _ => unreachable!(),
            };
            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_f64(ov, n.borrow().bc.clone());

            Node::replace_and_dispose(n, out);
            return true;
        }

        if op == Opcode::RvAdd
            && l.borrow().is_imm_str()
            && r.borrow().is_imm_str()
        {
            let mut s = String::new();
            s.push_str(ConstantFold::imm_to_str(&l.borrow().imm));
            s.push_str(ConstantFold::imm_to_str(&r.borrow().imm));

            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_str(s, n.borrow().bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }

        return false;
    }

    // =========================================================================
    // Comparison operation related folding optimization
    fn eq_constant_fold(
        &self,
        n: &mut Nref,
        l: &mut Nref,
        r: &mut Nref,
    ) -> bool {
        if l.borrow().is_imm() && r.borrow().is_imm() {
            let bval = l.borrow().imm == r.borrow().imm;
            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_boolean(bval, n.borrow().bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }
        return false;
    }

    fn ne_constant_fold(
        &self,
        n: &mut Nref,
        l: &mut Nref,
        r: &mut Nref,
    ) -> bool {
        if l.borrow().is_imm() && r.borrow().is_imm() {
            let bval = l.borrow().imm != r.borrow().imm;
            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_boolean(bval, n.borrow().bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }
        return false;
    }

    fn comp_constant_fold(
        &self,
        n: &mut Nref,
        l: &mut Nref,
        r: &mut Nref,
    ) -> bool {
        let op = n.borrow().op.op.clone();

        // handle ne/eq sperately since ne/eq works for all types for now
        match &op {
            Opcode::RvNe => {
                return self.ne_constant_fold(n, l, r);
            }
            Opcode::RvEq => {
                return self.eq_constant_fold(n, l, r);
            }
            _ => (),
        };

        if l.borrow().is_imm_int() && r.borrow().is_imm_int() {
            let l_int = ConstantFold::upgrade_to_i64(&l.borrow().imm);
            let r_int = ConstantFold::upgrade_to_i64(&r.borrow().imm);
            let bval = match &op {
                Opcode::RvLt => l_int < r_int,
                Opcode::RvLe => l_int <= r_int,
                Opcode::RvGt => l_int > r_int,
                Opcode::RvGe => l_int >= r_int,
                _ => unreachable!(),
            };

            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_boolean(bval, n.borrow().bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }

        if l.borrow().is_imm_num() && r.borrow().is_imm_num() {
            let l_int = ConstantFold::upgrade_to_real(&l.borrow().imm);
            let r_int = ConstantFold::upgrade_to_real(&r.borrow().imm);
            let bval = match &op {
                Opcode::RvLt => l_int < r_int,
                Opcode::RvLe => l_int <= r_int,
                Opcode::RvGt => l_int > r_int,
                Opcode::RvGe => l_int >= r_int,
                _ => unreachable!(),
            };

            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_boolean(bval, n.borrow().bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }

        // str and str
        if l.borrow().is_imm_str() && r.borrow().is_imm_str() {
            let l_str = ConstantFold::imm_to_str(&l.borrow().imm).to_string();
            let r_str = ConstantFold::imm_to_str(&r.borrow().imm).to_string();

            let bval = match &op {
                Opcode::RvLt => l_str.lt(&r_str),
                Opcode::RvLe => l_str.le(&r_str),
                Opcode::RvGt => l_str.gt(&r_str),
                Opcode::RvGe => l_str.ge(&r_str),
                _ => unreachable!(),
            };
            let out = self
                .mptr()
                .borrow_mut()
                .new_imm_boolean(bval, n.borrow().bc.clone());
            Node::replace_and_dispose(n, out);
            return true;
        }

        return false;
    }

    // =========================================================================
    // Unary constant folding
    fn unary_neg(&self, n: &mut Nref, una: &mut Nref) -> bool {
        let negate_val = match &una.borrow().imm {
            Imm::ImmU32(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-(*v as i64), n.borrow().bc.clone()),

            Imm::ImmU16(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-(*v as i64), n.borrow().bc.clone()),

            Imm::ImmU8(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-(*v as i64), n.borrow().bc.clone()),

            Imm::ImmI64(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-*v, n.borrow().bc.clone()),

            Imm::ImmI32(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-(*v as i64), n.borrow().bc.clone()),

            Imm::ImmI16(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-(*v as i64), n.borrow().bc.clone()),

            Imm::ImmI8(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_i64(-(*v as i64), n.borrow().bc.clone()),

            Imm::ImmF64(v) => self
                .mptr()
                .borrow_mut()
                .new_imm_f64(-*v, n.borrow().bc.clone()),

            _ => return false,
        };

        Node::replace_and_dispose(n, negate_val);
        return true;
    }

    fn node_to_boolean(&self, n: &mut Nref) -> Option<bool> {
        return match ConstantFold::imm_conv_to_boolean(&n.borrow().imm) {
            Option::Some(b) => Option::Some(b),
            _ => {
                if n.borrow().op.the_type.is_function()
                    || n.borrow().op.the_type.is_nfunction()
                {
                    Option::Some(false)
                } else {
                    Option::None
                }
            }
        };
    }

    fn unary_not(&self, n: &mut Nref, una: &mut Nref) -> bool {
        // Try to get the value from its imm, if not then try to decide its
        // type, ie if it is a function or nfunction, it so we can learn that
        // its value must be false.

        let bval = match self.node_to_boolean(una) {
            Option::Some(b) => b,
            _ => return false,
        };

        let out = self
            .mptr()
            .borrow_mut()
            .new_imm_boolean(!bval, n.borrow().bc.clone());
        Node::replace_and_dispose(n, out);
        return true;
    }

    fn unary_to_string(&self, n: &mut Nref, una: &mut Nref) -> bool {
        let sval = match ConstantFold::imm_conv_to_string(&una.borrow().imm) {
            Option::Some(s) => s,
            _ => return false,
        };
        let out = self
            .mptr()
            .borrow_mut()
            .new_imm_str(sval, n.borrow().bc.clone());
        Node::replace_and_dispose(n, out);
        return true;
    }

    fn unary_to_boolean(&self, n: &mut Nref, una: &mut Nref) -> bool {
        let bval = match self.node_to_boolean(una) {
            Option::Some(b) => b,
            _ => return false,
        };

        let out = self
            .mptr()
            .borrow_mut()
            .new_imm_boolean(bval, n.borrow().bc.clone());
        Node::replace_and_dispose(n, out);
        return true;
    }

    fn unary_constant_fold(&self, n: &mut Nref, una: &mut Nref) -> bool {
        let op = n.borrow().op.op.clone();
        match &op {
            Opcode::RvNeg => {
                return self.unary_neg(n, una);
            }

            Opcode::RvNot => {
                return self.unary_not(n, una);
            }

            Opcode::RvToString => {
                return self.unary_to_string(n, una);
            }

            Opcode::RvToBoolean => {
                return self.unary_to_boolean(n, una);
            }

            _ => unreachable!(),
        };
    }
}

impl NodePass for ConstantFold {
    fn run_node(&mut self, mut node: Nref) -> PassResult {
        let op = node.borrow().op.op.clone();
        match op {
            Opcode::RvAdd
            | Opcode::RvSub
            | Opcode::RvMul
            | Opcode::RvDiv
            | Opcode::RvMod
            | Opcode::RvPow => {
                let mut lhs = node.borrow().lhs();
                let mut rhs = node.borrow().rhs();

                self.arith_constant_fold(&mut node, &mut lhs, &mut rhs);
            }

            Opcode::RvEq
            | Opcode::RvNe
            | Opcode::RvLt
            | Opcode::RvLe
            | Opcode::RvGt
            | Opcode::RvGe => {
                let mut lhs = node.borrow().lhs();
                let mut rhs = node.borrow().rhs();

                self.comp_constant_fold(&mut node, &mut lhs, &mut rhs);
            }

            Opcode::RvNot
            | Opcode::RvNeg
            | Opcode::RvToString
            | Opcode::RvToBoolean => {
                let mut una = node.borrow().una();
                self.unary_constant_fold(&mut node, &mut una);
            }

            _ => (),
        };

        return PassResult::OK;
    }
}
