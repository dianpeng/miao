use crate::ic::feedback::*;
use crate::ic::ftype::*;
use crate::jit::iter::*;
use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

// Our type feedback will only be related to the operation but not the value. The
// typer will propagate the feedback collected type back to the value itself. It
// allows for type propagation if certain type observer site is not capable of
// collecting the type.
//
//   Example as following :
//
//    feedback(+)
//
//      [+] ----> [A] <------ [-] ------- [C]
//       |
//       |                 feedback(-)
//      [B]
//
//  If feedback(+) somehow is not able to collect enough type feedback, but
//  feedback(-) can, so A can still be decorated with speculative type guess
//  which later on turns into a guard instruction for speculative code generation
//
// This typer pass just provides a possible type pass for later lower pass.

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

        return match feedback {
            FType::Int => *collected == MainType::Int,
            FType::Real => *collected == MainType::Real,
            FType::Boolean => *collected == MainType::Boolean,
            FType::Null => *collected == MainType::Null,
            FType::Str => *collected == MainType::Str,
            FType::List => *collected == MainType::List,
            FType::Object => *collected == MainType::Object,
            FType::Function => *collected == MainType::Function,
            FType::NFunction => *collected == MainType::NFunction,
            FType::Iter => *collected == MainType::Iter,
            FType::Unknown => *collected == MainType::Unknown,
        };
    }

    fn type_propagate(feedback: &FType, n: &mut Nref) -> bool {
        if !n.borrow().the_type.is_unknown() {
            return true;
        }
        n.borrow_mut().the_type = JType::map_ftype(feedback);
        return true;
    }

    fn feedback_at(&self, x: u32) -> Feedback {
        return self.f.borrow().func.borrow().fd.index(x as usize).clone();
    }

    fn unary_typer(&mut self, n: Nref) {}

    fn arithmetic_typer(&mut self, n: Nref) {
        debug_assert!(n.borrow().is_value_binary());

        let mut lhs = n.borrow().lhs();
        let mut rhs = n.borrow().rhs();

        let lhs_type = lhs.borrow().the_type.clone();
        let rhs_type = rhs.borrow().the_type.clone();

        let mut out_mtype = MainType::Unknown;
        let mut out_stype = SubType::Invalid;

        // (0) try to reason the type from the lhs and rhs value
        match n.borrow().op.op {
            // arithmetic operations, type is directly deduced
            Opcode::RvAdd
            | Opcode::RvSub
            | Opcode::RvMul
            | Opcode::RvDiv
            | Opcode::RvPow => {
                match (&lhs_type.main, &rhs_type.main) {
                    (MainType::Int, MainType::Int) => {
                        out_mtype = MainType::Int;
                        out_stype = SubType::I64;
                    }

                    (MainType::Real, MainType::Real) => {
                        out_mtype = MainType::Real;
                        out_stype = SubType::F64;
                    }

                    (MainType::Str, MainType::Str) => {
                        if n.borrow().op.op == Opcode::RvAdd {
                            out_mtype = MainType::Str;
                            out_stype = SubType::Invalid;
                        }
                    }

                    (MainType::Int, MainType::Real)
                    | (MainType::Real, MainType::Int) => {
                        out_mtype = MainType::Real;
                        out_stype = SubType::F64;
                    }
                    _ => (),
                };
            }

            _ => (),
        };

        // (1) try feedback vector to collect runtime feedback, notes this
        //     value may be conflicted with existed reasoning.
        let feedback_type = self.feedback_at(n.borrow().bc.bc);

        match feedback_type {
            Feedback::Binary(f) => {
                if !Typer::type_match(&f.lhs_type, &lhs_type.main)
                    || !Typer::type_match(&f.rhs_type, &rhs_type.main)
                {
                    out_mtype = MainType::Unknown;
                    out_stype = SubType::Invalid;
                } else {
                    if out_mtype == MainType::Unknown {
                        debug_assert!(out_stype == SubType::Invalid);

                        // Propagate the types to its LHS and RHS if applicable
                        if Typer::type_propagate(&f.lhs_type, &mut lhs)
                            && Typer::type_propagate(&f.rhs_type, &mut rhs)
                        {
                            match (f.lhs_type, f.rhs_type) {
                                (FType::Int, FType::Int) => {
                                    out_mtype = MainType::Int;
                                    out_stype = SubType::I64;
                                }

                                (FType::Real, FType::Real) => {
                                    out_mtype = MainType::Int;
                                    out_stype = SubType::I64;
                                }

                                (FType::Str, FType::Str) => {
                                    if n.borrow().op.op == Opcode::RvAdd {
                                        out_mtype = MainType::Str;
                                        out_stype = SubType::Invalid;
                                    }
                                }

                                (FType::Int, FType::Real)
                                | (FType::Real, FType::Int) => {
                                    out_mtype = MainType::Real;
                                    out_stype = SubType::F64;
                                }

                                _ => (),
                            };
                        }
                    }
                }
            }
            _ => (),
        };

        // Lastly assign the type back to the current nodes
        n.borrow_mut().the_type = JType {
            main: out_mtype,
            sub: out_stype,
        };
    }

    fn comparison_typer(&mut self, n: Nref) {
        debug_assert!(n.borrow().is_value_binary());

        let mut lhs = n.borrow().lhs();
        let mut rhs = n.borrow().rhs();

        let lhs_type = lhs.borrow().the_type.clone();
        let rhs_type = rhs.borrow().the_type.clone();

        let mut out_mtype = MainType::Unknown;
        let mut out_stype = SubType::Invalid;

        // (0) try to reason the type from the lhs and rhs value
        match n.borrow().op.op {
            // comparison operations
            Opcode::RvEq
            | Opcode::RvNe
            | Opcode::RvLt
            | Opcode::RvLe
            | Opcode::RvGt
            | Opcode::RvGe => {
                match (&lhs_type.main, &rhs_type.main) {
                    // Can work with eq/ne/lt/le/gt/ge
                    (MainType::Int, MainType::Int)
                    | (MainType::Int, MainType::Real)
                    | (MainType::Real, MainType::Int)
                    | (MainType::Real, MainType::Real)
                    | (MainType::Str, MainType::Str) => {
                        out_mtype = MainType::Boolean;
                        out_stype = SubType::Invalid;
                    }

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
                            out_mtype = MainType::Boolean;
                            out_stype = SubType::Invalid;
                        }
                    }

                    _ => (),
                };
            }

            _ => (),
        };

        // (1) try feedback vector to collect runtime feedback, notes this
        //     value may be conflicted with existed reasoning.
        let feedback_type = self.feedback_at(n.borrow().bc.bc);

        match feedback_type {
            Feedback::Binary(f) => {
                if !Typer::type_match(&f.lhs_type, &lhs_type.main)
                    || !Typer::type_match(&f.rhs_type, &rhs_type.main)
                {
                    out_mtype = MainType::Unknown;
                    out_stype = SubType::Invalid;
                } else {
                    if out_mtype == MainType::Unknown {
                        debug_assert!(out_stype == SubType::Invalid);

                        // Propagate the types to its LHS and RHS if applicable
                        if Typer::type_propagate(&f.lhs_type, &mut lhs)
                            && Typer::type_propagate(&f.rhs_type, &mut rhs)
                        {
                            match (f.lhs_type, f.rhs_type) {
                                (FType::Int, FType::Int)
                                | (FType::Int, FType::Real)
                                | (FType::Real, FType::Int)
                                | (FType::Real, FType::Real)
                                | (FType::Str, FType::Str) => {
                                    out_mtype = MainType::Boolean;
                                    out_stype = SubType::Invalid;
                                }

                                // Can work with eq/ne
                                (FType::List, FType::List)
                                | (FType::Object, FType::Object)
                                | (FType::Function, FType::Function)
                                | (FType::NFunction, FType::NFunction)
                                | (FType::Iter, FType::Iter)
                                | (FType::Null, FType::Null)
                                | (FType::Boolean, FType::Boolean) => {
                                    if n.borrow().op.op == Opcode::RvEq
                                        || n.borrow().op.op == Opcode::RvNe
                                    {
                                        out_mtype = MainType::Boolean;
                                        out_stype = SubType::Invalid;
                                    }
                                }

                                _ => (),
                            };
                        }
                    }
                }
            }
            _ => (),
        };

        // Lastly assign the type back to the current nodes
        n.borrow_mut().the_type = JType {
            main: out_mtype,
            sub: out_stype,
        };
    }
}
