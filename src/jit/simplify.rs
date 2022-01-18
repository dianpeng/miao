use crate::jit::iter::*;
use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

// This is just a general purpose simplification after the graph been generated
// Currently the IR generation will just have many infeasible and redundancy in
// the generated IR node. This one provides simplifcation for all sorts of stuff
// and also allow future folding to take place.

// 1) Phi simplification
// 2) Simplify RvLoadXXX into actual loaded value

struct RvSimplify {
    j: Jitptr,
    f: FGraphptr,
}

impl NodePass for RvSimplify {
    fn run_node(&mut self, cur: Nref) -> PassResult {
        debug_assert!(!cur.borrow().is_dead());

        match cur.borrow().op.op {
            // Phi
            Opcode::RvPhi => {
                self.simplify_phi(Nref::clone(&cur));
            }

            // RvLoad
            Opcode::RvLoadInt => {
                self.simplify_ld_int(Nref::clone(&cur));
            }
            Opcode::RvLoadReal => {
                self.simplify_ld_real(Nref::clone(&cur));
            }
            Opcode::RvLoadString => {
                self.simplify_ld_string(Nref::clone(&cur));
            }
            Opcode::RvLoadNull => {
                self.simplify_ld_null(Nref::clone(&cur));
            }
            Opcode::RvLoadTrue => {
                self.simplify_ld_true(Nref::clone(&cur));
            }
            Opcode::RvLoadFalse => {
                self.simplify_ld_false(Nref::clone(&cur));
            }
            _ => (),
        };

        return PassResult::OK;
    }
}

impl RvSimplify {
    pub fn new(j: Jitptr, f: FGraphptr) -> RvSimplify {
        RvSimplify { j: j, f: f }
    }

    fn mptr(&self) -> Mpptr {
        return self.j.borrow().mpool.clone();
    }

    fn simplify_phi(&mut self, mut x: Nref) {
        if !Node::simplify_phi(&mut x) {
            debug_assert!(x.borrow().is_phi());
            if x.borrow().value.len() == 2 {
                debug_assert!(x.borrow().cfg.len() == 2);

                // if both value of the phi are the same, then just replace phi
                // to be the value it has
                if Nref::ptr_eq(&x.borrow().value[0], &x.borrow().value[1]) {
                    let rep = x.borrow().value[0].clone();
                    Node::replace(&mut x, rep);
                    x.borrow_mut().mark_dead();
                }
            }
        }
    }

    fn simplify_ld_int(&mut self, mut x: Nref) {
        debug_assert!(x.borrow().value.len() == 1);
        let imm = x.borrow().value[0].borrow().imm.clone();
        if let Imm::Index(idx) = imm {
            let new_node = self.mptr().borrow_mut().new_imm_i64(
                self.f.borrow().func.borrow().proto.code.load_int(idx),
                x.borrow().bc.clone(),
            );
            Node::replace(&mut x, new_node);
            x.borrow_mut().mark_dead();
        } else {
            unreachable!();
        }
    }

    fn simplify_ld_real(&mut self, mut x: Nref) {
        debug_assert!(x.borrow().value.len() == 1);
        let imm = x.borrow().value[0].borrow().imm.clone();
        if let Imm::Index(idx) = imm {
            let new_node = self.mptr().borrow_mut().new_imm_f64(
                self.f.borrow().func.borrow().proto.code.load_real(idx),
                x.borrow().bc.clone(),
            );
            Node::replace(&mut x, new_node);
            x.borrow_mut().mark_dead();
        } else {
            unreachable!();
        }
    }

    fn simplify_ld_true(&mut self, mut x: Nref) {
        debug_assert!(x.borrow().value.len() == 0);
        let new_node =
            self.mptr().borrow_mut().new_imm_true(x.borrow().bc.clone());
        Node::replace(&mut x, new_node);
        x.borrow_mut().mark_dead();
    }

    fn simplify_ld_false(&mut self, mut x: Nref) {
        debug_assert!(x.borrow().value.len() == 0);
        let new_node = self
            .mptr()
            .borrow_mut()
            .new_imm_false(x.borrow().bc.clone());

        Node::replace(&mut x, new_node);
        x.borrow_mut().mark_dead();
    }

    fn simplify_ld_null(&mut self, mut x: Nref) {
        debug_assert!(x.borrow().value.len() == 0);
        let new_node =
            self.mptr().borrow_mut().new_imm_null(x.borrow().bc.clone());
        Node::replace(&mut x, new_node);
        x.borrow_mut().mark_dead();
    }

    fn simplify_ld_string(&mut self, mut x: Nref) {
        debug_assert!(x.borrow().value.len() == 1);
        let imm = x.borrow().value[0].borrow().imm.clone();
        if let Imm::Index(idx) = imm {
            let new_node = self.mptr().borrow_mut().new_imm_string(
                self.f.borrow().func.borrow().proto.code.load_str(idx),
                x.borrow().bc.clone(),
            );
            Node::replace(&mut x, new_node);
            x.borrow_mut().mark_dead();
        } else {
            unreachable!();
        }
    }
}
