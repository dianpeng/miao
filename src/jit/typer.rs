use crate::ic::feedback::*;
use crate::ic::ftype::*;
use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

// Typer process.
//
//   This process simply does a progressive typping of the IR graph to catch
//   up situation where one operation site doesn't have enough herusitic to
//   guess the type of a value but another places does
//
//   Example as following :
//
//   let i = 0;
//   let v = global_a;
//   let out = 0;
//
//   for i < 100000 {
//     i += 1;
//
//     if i <= 5 {
//       out += v + 10; // v's type cannot be guessed since it is only been
//                      // executed 5 times
//     }
//
//     out += v + 20; // here, the add operation will be executed multiple
//                    // times to learn the type of v herusitically, so v's
//                    // type can be inferred here
//   }
//
//   Since the graph is SSA styled, so v will not be modified without either
//   using memory or runtime API, which is rarely happened from user's viewpoint,
//   and rest of the explicit modification will be catched up by the IR. Anyway,
//   the type_hint propagation will be meaningful for us.
//
//   Therefore, the v's type is essentially mostly not modified, therefore, v's
//   type should be propagated to the v+10 sites if applicable. Additionally,
//   the type should be consistent, which means from different site, the type
//   observation should be always the same or unknown.

#[allow(dead_code)]
pub struct Typer {
    j: Jitptr,
    f: FGraphptr,
}

impl NodePass for Typer {
    fn run_node(&mut self, cur: Nref) -> PassResult {
        debug_assert!(!cur.borrow().is_dead());
        let op_code = cur.borrow().op.op.clone();

        match op_code {
            Opcode::RvAdd
            | Opcode::RvSub
            | Opcode::RvMul
            | Opcode::RvDiv
            | Opcode::RvMod
            | Opcode::RvPow => {
                self.arithmetic_typer(cur);
            }

            Opcode::RvEq
            | Opcode::RvNe
            | Opcode::RvLt
            | Opcode::RvLe
            | Opcode::RvGt
            | Opcode::RvGe => {
                self.comparison_typer(cur);
            }

            Opcode::RvNeg
            | Opcode::RvNot
            | Opcode::RvToString
            | Opcode::RvToBoolean => {
                self.unary_typer(cur);
            }

            _ => (),
        };

        return PassResult::OK;
    }
}

impl Typer {
    // Check whether the recorded feedback type matches collected type via typer
    // this is mainly used to detect type mismatch which will later recorded as
    fn type_match(feedback: &FType, collected: &MainType) -> bool {
        if *collected == MainType::Unknown {
            return true;
        }

        let should_type = MainType::map_ftype(feedback);
        return should_type == *collected;
    }

    // perform type propagation.
    //   1. the feedback type is observed from the feedback vector
    //   2. and the type_hint is the existed type for the verify node
    //
    // The logic is simple:
    //   1. if the type_hint exists, ie not Unknown, then we set the feedback
    //      type to the type_hint
    //
    //   2. If the type_hint contains valid type, then we do verification to
    //      check whether the type hint is consistent or not.

    fn type_propagate(feedback: &FType, type_hint: &mut MainType) -> bool {
        if *type_hint == MainType::Unknown {
            *type_hint = MainType::map_ftype(feedback);
            return true;
        } else {
            return Typer::type_match(feedback, type_hint);
        }
    }

    fn feedback_at(&self, x: u32) -> Feedback {
        return self.f.borrow().func.borrow().fd.index(x as usize).clone();
    }

    fn binary_lhs_at(&self, n: &Nref) -> Option<FType> {
        let f = self.feedback_at(n.borrow().bc.bc);
        return match f {
            Feedback::Binary(b) => Option::Some(b.lhs_type),
            _ => Option::None,
        };
    }

    fn binary_rhs_at(&self, n: &Nref) -> Option<FType> {
        let f = self.feedback_at(n.borrow().bc.bc);
        return match f {
            Feedback::Binary(b) => Option::Some(b.rhs_type),
            _ => Option::None,
        };
    }

    fn unary_at(&self, n: &Nref) -> Option<FType> {
        let f = self.feedback_at(n.borrow().bc.bc);
        return match f {
            Feedback::Unary(b) => Option::Some(b.opr_type),
            _ => Option::None,
        };
    }

    // -------------------------------------------------------------------------
    //
    // typping process.
    //
    //   Generally, typping works in PO and it tries to decorate the node for
    //   whatever information it gathers from the runtime and rules been applied
    //   accordingly.

    fn unary_typer(&mut self, n: Nref) -> Option<()> {
        debug_assert!(n.borrow().is_value_unary());

        let una = n.borrow().una();
        let una_feedback = self.unary_at(&n)?;

        let tp = {
            let mut t = una.borrow().type_hint.clone();
            if !Typer::type_propagate(&una_feedback, &mut t) {
                return Option::None;
            }
            una.borrow_mut().type_hint = t.clone();
            t
        };

        let out_type = match n.borrow().op.op {
            Opcode::RvNeg => tp.clone(),
            Opcode::RvNot => MainType::Boolean,
            Opcode::RvToString => MainType::Str,
            Opcode::RvToBoolean => MainType::Boolean,
            _ => MainType::Unknown,
        };

        n.borrow_mut().type_hint = out_type;
        return Option::Some(());
    }

    fn arithmetic_typer(&mut self, n: Nref) -> Option<()> {
        debug_assert!(n.borrow().is_value_binary());

        let lhs = n.borrow().lhs();
        let rhs = n.borrow().rhs();

        let lhs_feedback = self.binary_lhs_at(&n)?;
        let rhs_feedback = self.binary_rhs_at(&n)?;

        // (0) using feedback type to validate the type of its operand and do
        //     type propagation accordingly
        let lhs_type = {
            let mut t = lhs.borrow().type_hint.clone();
            if !Typer::type_propagate(&lhs_feedback, &mut t) {
                return Option::None;
            }
            lhs.borrow_mut().type_hint = t.clone();
            t
        };

        let rhs_type = {
            let mut t = rhs.borrow().type_hint.clone();
            if !Typer::type_propagate(&rhs_feedback, &mut t) {
                return Option::None;
            }
            rhs.borrow_mut().type_hint = t.clone();
            t
        };

        // (1) reason about the arithmetic result's type based on the lhs/rhs
        //     type
        let out_type = match n.borrow().op.op {
            Opcode::RvAdd
            | Opcode::RvSub
            | Opcode::RvMul
            | Opcode::RvDiv
            | Opcode::RvPow => match (&lhs_type, &rhs_type) {
                (MainType::Int, MainType::Int) => MainType::Int,
                (MainType::Real, MainType::Real) => MainType::Real,
                (MainType::Int, MainType::Real)
                | (MainType::Real, MainType::Int) => MainType::Real,

                (MainType::Str, MainType::Str) => {
                    if n.borrow().op.op == Opcode::RvAdd {
                        MainType::Str
                    } else {
                        MainType::Unknown
                    }
                }
                _ => MainType::Unknown,
            },

            _ => MainType::Unknown,
        };

        n.borrow_mut().type_hint = out_type;
        return Option::Some(());
    }

    fn comparison_typer(&mut self, n: Nref) -> Option<()> {
        debug_assert!(n.borrow().is_value_binary());

        let lhs = n.borrow().lhs();
        let rhs = n.borrow().rhs();

        let lhs_feedback = self.binary_lhs_at(&n)?;
        let rhs_feedback = self.binary_rhs_at(&n)?;

        // (0) using feedback type to validate the type of its operand and do
        //     type propagation accordingly
        let lhs_type = {
            let mut t = lhs.borrow().type_hint.clone();
            if !Typer::type_propagate(&lhs_feedback, &mut t) {
                return Option::None;
            }
            lhs.borrow_mut().type_hint = t.clone();
            t
        };

        let rhs_type = {
            let mut t = rhs.borrow().type_hint.clone();
            if !Typer::type_propagate(&rhs_feedback, &mut t) {
                return Option::None;
            }
            rhs.borrow_mut().type_hint = t.clone();
            t
        };

        // (1) reason about the arithmetic result's type based on the lhs/rhs
        //     type
        let out_type = match n.borrow().op.op {
            Opcode::RvEq
            | Opcode::RvNe
            | Opcode::RvLt
            | Opcode::RvLe
            | Opcode::RvGt
            | Opcode::RvGe => {
                match (&lhs_type, &rhs_type) {
                    // Can work with eq/ne/lt/le/gt/ge
                    (MainType::Int, MainType::Int)
                    | (MainType::Int, MainType::Real)
                    | (MainType::Real, MainType::Int)
                    | (MainType::Real, MainType::Real)
                    | (MainType::Str, MainType::Str) => MainType::Boolean,

                    // Can work with eq/ne
                    (MainType::List, MainType::List)
                    | (MainType::Object, MainType::Object)
                    | (MainType::Function, MainType::Function)
                    | (MainType::NFunction, MainType::NFunction)
                    | (MainType::Iter, MainType::Iter)
                    | (MainType::Null, MainType::Null)
                    | (MainType::Boolean, MainType::Boolean) => {
                        if n.borrow().op.op == Opcode::RvEq
                            || n.borrow().op.op == Opcode::RvNe
                        {
                            MainType::Boolean
                        } else {
                            MainType::Unknown
                        }
                    }

                    _ => MainType::Unknown,
                }
            }

            _ => MainType::Unknown,
        };

        n.borrow_mut().type_hint = out_type;
        return Option::Some(());
    }
}
