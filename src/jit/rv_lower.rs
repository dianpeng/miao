use crate::jit::j::*;
use crate::jit::node::*;
use crate::jit::pass::*;

// Lower Rval tier instruction. This file is the main lower pass that will tries
// to lower the Rv graph as much as possible. This pass should be executed after
// the typer pass.

// -- ==========================================================================
//
// Shared common function for creation of each value nodes
//
// [RvXXX] can be replaced speculatively by feedback type system to be
//
//    [Guard XXX] --> [RvXXX]
//
// With guard instruction, we are capable of unboxing the boxed value into the
// unboxed primitive value for later usage.
//
// Polymorphic operation is basically perform a builtin call that directs the
// function to a builtin runtime call. The builtin call is able to generate
// trap that will be handled by the generated call stub.

struct RvLower {
    j: Jitptr,
    f: FGraphptr,
}

impl RvLower {
    // ------------------------------------------------------------------------
    // common functions
    fn lhs_type_hint(&self, n: &Nref) -> FType {
        match self.f.borrow().func.borrow().fd.index(n.borrow().bc.bc) {
            Binary(x) => {
                return x.lhs_type.clone();
            }
            _ => return FType::Unknown,
        };
    }

    fn rhs_type_hint(&self, n: &Nref) -> FType {
        match self.f.borrow().func.borrow().fd.index(n.borrow().bc.bc) {
            Binary(x) => {
                return x.rhs_type.clone();
            }
            _ => return FType::Unknown,
        };
    }

    fn una_type_hint(&self, n: &Nref) -> FType {
        match self.f.borrow().func.borrow().fd.index(n.borrow().bc.bc) {
            Unary(x) => {
                return x.opr_type.clone();
            }
            _ => return FType::Unkonwn,
        }
    }

    // ------------------------------------------------------------------------
    // Builtin call lowering.
    //
    //  The builin call is similar as type guard, each builtin call should have
    //  a guard_if_not_trap to allow later lower of the guard after scheduling
    //  for each instruction. The guard_if_not_trap will turns into a control
    //  flow which allow deoptimization from the current frame back to the interp
    //  frame.
    fn lower_builtin_call_pure(&self, call: Nref) -> Nref {
        debug_assert!(call.borrow().is_builtin_call());
        let bc = call.borrow().bc.clone();
        return self.mptr().new_guard_if_not_trap(call, bc);
    }

    fn lower_binary_op_with_builtin(
        &self,
        lhs: Nref,
        rhs: Nref,
        call: BuiltinCall,
        bc: BcCtx,
    ) -> Nref {
        let builtin_call = self.lower.do_builtin_call2(lhs, rhs, bc);
        Node::replace_and_dispose(&mut x, builtin_call);
    }

    fn lower_unary_op_with_builtin(
        &self,
        x: Nref,
        call: BuiltinCall,
        bc: BcCtx,
    ) -> Nref {
        let builtin_call = self.lower.do_builtin_call1(x, bc);
        Node::replace_and_dispose(&mut x, builtin_call);
    }

    // ------------------------------------------------------------------------
    // new a type guard of the input node n, and tries to convert it into the
    // output type T
    fn new_guard(&mut self, n: Nref, hint: FType) -> Option<Nref> {
        let bc_ctx = n.borrow().bc.clone();

        // (0) check the hint type is available or not, if so, then generates
        //     the gaurd based on the hint
        match hint {
            FType::Int => {
                return self.mptr().borrow_mut().new_guard_int(n, bc_ctx);
            }

            FType::Real => {
                return self.mptr().borrow_mut().new_guard_real(n, bc_ctx);
            }

            FType::Boolean => {
                return self.mptr().borrow_mut().new_guard_boolean(n, bc_ctx);
            }

            FType::Null => {
                return self.mptr().borrow_mut().new_guard_null(n, bc_ctx);
            }

            FType::Str => {
                return self.mptr().borrow_mut().new_guard_str(n, bc_ctx);
            }

            FType::List => {
                return self.mptr().borrow_mut().new_guard_list(n, bc_ctx);
            }

            FType::Object => {
                return self.mptr().borrow_mut().new_guard_object(n, bc_ctx);
            }

            FType::Function => {
                return self.mptr().borrow_mut().new_guard_function(n, bc_ctx);
            }

            FType::NFunction => {
                return self.mptr().borrow_mut().new_guard_nfunction(n, bc_ctx);
            }

            FType::Iter => {
                return self.mptr().borrow_mut().new_guard_iter(n, bc_ctx);
            }
            _ => (),
        };

        // (1) Okay, type hint doesn't give us hint, let's check typer's
        //     conclusion

        match &n.borrow().type_hint {
            MainType::Int => {
                return self.mptr().borrow_mut().new_guard_int(n, bc_ctx);
            }

            MainType::Real => {
                return self.mptr().borrow_mut().new_guard_real(n, bc_ctx);
            }

            MainType::Boolean => {
                return self.mptr().borrow_mut().new_guard_boolean(n, bc_ctx);
            }

            MainType::Null => {
                return self.mptr().borrow_mut().new_guard_null(n, bc_ctx);
            }

            MainType::Str => {
                return self.mptr().borrow_mut().new_guard_str(n, bc_ctx);
            }

            MainType::List => {
                return self.mptr().borrow_mut().new_guard_list(n, bc_ctx);
            }

            MainType::Object => {
                return self.mptr().borrow_mut().new_guard_object(n, bc_ctx);
            }

            MainType::Function => {
                return self.mptr().borrow_mut().new_guard_function(n, bc_ctx);
            }

            MainType::NFunction => {
                return self.mptr().borrow_mut().new_guard_nfunction(n, bc_ctx);
            }

            MainType::Iter => {
                return self.mptr().borrow_mut().new_guard_iter(n, bc_ctx);
            }
        };

        return Option::None;
    }

    // -------------------------------------------------------------------------
    // The input value n must be a typped value, ie with a guard
    fn new_unbox_from_guard(&mut self, n: Nref) -> Nref {
        debug_assert!(n.borrow().is_guard_type());

        let bc = n.borrow().bc.clone();

        match n.borrow().op.op {
            Opcode::GuardInt => {
                return self.mptr().borrow_mut().new_unbox_i64(n, bc);
            }
            Opcode::GuardReal => {
                return self.mptr().borrow_mut().new_unbox_f64(n, bc);
            }
            Opcode::GuardBoolean => {
                return self.mptr().borrow_mut().new_unbox_boolean(n, bc);
            }
            Opcode::GuardNull => {
                return self.mptr().borrow_mut().new_unbox_null(n, bc);
            }
            Opcode::GuardStr => {
                return self.mptr().borrow_mut().new_unbox_str(n, bc);
            }
            Opcode::GuardList => {
                return self.mptr().borrow_mut().new_unbox_list(n, bc);
            }
            Opcode::GuardObject => {
                return self.mptr().borrow_mut().new_unbox_object(n, bc);
            }
            Opcode::GuardFunction => {
                return self.mptr().borrow_mut().new_unbox_function(n, bc);
            }
            Opcode::GuardNFunction => {
                return self.mptr().borrow_mut().new_unbox_nfunction(n, bc);
            }
            Opcode::GuardIter => {
                return self.mptr().borrow_mut().new_unbox_iter(n, bc);
            }

            _ => unreachable!(),
        };
    }

    // new a boxed value from the unboxed value given. The Nref specified must be
    // an unboxed value, otherwise crashed.
    fn new_box_value(&mut self, n: Nref) -> Nref {
        debug_assert!(n.borrow().is_unbox_value());
        let bc = n.borrow().bc.clone();

        match n.borrow().the_type.main {
            MainType::Int => {
                return self.mptr().borrow_mut().new_box_i64(n, bc);
            }
            MainType::Real => {
                return self.mptr().borrow_mut().new_box_f64(n, bc);
            }
            MainType::Boolean => {
                return self.mptr().borrow_mut().new_box_boolean(n, bc);
            }
            MainType::Null => {
                return self.mptr().borrow_mut().new_box_null(n, bc);
            }

            MainType::Str => {
                return self.mptr().borrow_mut().new_box_str(n, bc);
            }
            MainType::List => {
                return self.mptr().borrow_mut().new_box_list(n, bc);
            }
            MainType::Object => {
                return self.mptr().borrow_mut().new_box_object(n, bc);
            }
            MainType::Function => {
                return self.mptr().borrow_mut().new_box_function(n, bc);
            }
            MainType::NFunction => {
                return self.mptr().borrow_mut().new_box_nfunction(n, bc);
            }
            MainType::Iter => {
                return self.mptr().borrow_mut().new_box_iter(n, bc);
            }

            _ => unreachable!(),
        };
    }

    // -------------------------------------------------------------------------
    fn new_unbox_guard(&mut self, n: Nref, hint: FType) -> Option<Nref> {
        if let Option::Some(nn) = self.new_guard(n, hint) {
            return self.new_unbox_from_guard(n);
        }
        return Option::None;
    }

    // ------------------------------------------------------------------------
    // String operations
    //
    //   1) String concate
    //     1.1) UnboxStrCon
    //
    //   2) String comparison
    //
    //     2.1) CmpStrEq
    //     2.2) CmpStrNe
    //     2.3) CmpStrLt
    //     2.4) CmpStrLe
    //     2.5) CmpStrGt
    //     2.6) CmpStrGe

    fn new_str_opts_string_con(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Nref {
        return self.mptr().borrow_mut().new_unbox_str_conn(lhs, rhs, bc);
    }

    // ------------------------------------------------------------------------
    // arithmetic operation
    //
    // Each arithmetic operation will generate correct type based on the input
    // binary arguments, and if it cannot find approriate type then it will
    // returns None
    fn new_unbox_add(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return Option::Some(
                self.mptr().borrow_mut().new_i64_add(lhs, rhs, bc),
            );
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return Option::Some(
                self.mptr().borrow_mut().new_f64_add(lhs, rhs, bc),
            );
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return Option::Some(self.new_str_opts_string_con(lhs, rhs, bc));
        }

        return Option::None;
    }

    fn new_unbox_sub(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_sub(lhs, rhs, bc);
        }
        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_sub(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_mul(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_mul(lhs, rhs, bc);
        }
        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_mul(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_div(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_div(lhs, rhs, bc);
        }
        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_div(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_mod(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_mod(lhs, rhs, bc);
        }
        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_mod(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_pow(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_pow(lhs, rhs, bc);
        }
        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_pow(lhs, rhs, bc);
        }

        return Option::None;
    }

    // ------------------------------------------------------------------------
    // comparison
    fn new_unbox_cmp_eq(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_eq(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_eq(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_boolean()
            && rhs.borrow().the_type.is_boolean()
        {
            return self.mptr().borrow_mut().new_boolean_eq(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return self.mptr().borrow_mut().new_str_eq(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_null() && rhs.borrow().the_type_is_null() {
            return self.mptr().borrow_mut().new_null_eq(lhs, rhs, bc);
        }

        // heap types
        return self.mptr().borrow_mut().new_ptr_eq(lhs, rhs, bc);
    }

    fn new_unbox_cmp_ne(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_ne(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_f64_ne(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_boolean()
            && rhs.borrow().the_type.is_boolean()
        {
            return self.mptr().borrow_mut().new_boolean_ne(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return self.mptr().borrow_mut().new_str_ne(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_null() && rhs.borrow().the_type_is_null() {
            return self.mptr().borrow_mut().new_null_ne(lhs, rhs, bc);
        }

        // heap types
        return self.mptr().borrow_mut().new_ptr_ne(lhs, rhs, bc);
    }

    fn new_unbox_cmp_lt(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_lt(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_i64_lt(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return self.mptr().borrow_mut().new_str_lt(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_cmp_le(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_le(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_i64_le(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return self.mptr().borrow_mut().new_str_le(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_cmp_gt(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_gt(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_i64_gt(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return self.mptr().borrow_mut().new_str_gt(lhs, rhs, bc);
        }

        return Option::None;
    }

    fn new_unbox_cmp_ge(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Option<Nref> {
        if lhs.borrow().the_type.is_int() && rhs.borrow().the_type.is_int() {
            return self.mptr().borrow_mut().new_i64_ge(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_real() && rhs.borrow().the_type.is_real() {
            return self.mptr().borrow_mut().new_i64_ge(lhs, rhs, bc);
        }

        if lhs.borrow().the_type.is_str() && rhs.borrow().the_type.is_str() {
            return self.mptr().borrow_mut().new_str_ge(lhs, rhs, bc);
        }

        return Option::None;
    }
}

// -- ==========================================================================
//
// Arithmetic RvXXX lower
//
//   The basic RvXXX lower is as following, each RvXXX arithmetic operation
//   should already have feedback type, with that, we can tries to lower the
//   operand. Each operands lower will be lowered into type guard and then
//   tries to perform the operation
//
//
//   [RvAdd] ---- [A]
//      |
//      |
//      |
//     [B]
//
//   will be lowered into following sequences
//
//       [IntAdd] ----- [UnboxInt] ---- [Guard Int] ---- [A]
//          |  |
//          |  |
//          |  +------- [UnboxInt] ---- [Guard Int] ---- [B]
//          |
//          |
//       [BoxInt]
//
struct RvArithmeticLower {
    lower: RvLower,
}

type TryLowerR = Option<()>;

impl RvArithmetciLower {
    fn try_lower_add_sub_mul_pow_with_feedback(&mut self, x: Nref) -> TryLowerR {
        debug_assert!(x.borrow().is_value_binary_arith());

        let lhs = x.borrow().lhs();
        let rhs = x.borrow().rhs();

        let lhs_type = self.lower.lhs_type_hint(&x);
        let rhs_type = self.lower.rhs_type_hint(&x);

        let unbox_lhs = self.lower.new_unbox_guard(lhs, lhs_type)?;
        let unbox_rhs = self.lower.new_unbox_guard(rhs, rhs_type)?;

        let unbox_op = match x.borrow().op {
            Opcode::RvAdd => {
                self.lower
                    .new_unbox_add(unbox_lhs, unbox_rhs, x.bc.clone())?
            }
            Opcode::RvSub => {
                self.lower
                    .new_unbox_sub(unbox_lhs, unbox_rhs, x.bc.clone())?
            }

            Opcode::RvMul => {
                self.lower
                    .new_unbox_mul(unbox_lhs, unbox_rhs, x.bc.clone())?
            }

            Opcode::RvPow => {
                self.lower
                    .new_unbox_pow(unbox_lhs, unbox_rhs, x.bc.clone())?
            }
            _ => unreachable!(),
        };

        let box_val = self.lower.new_box_value(unbox_op);

        Node::replace_and_dispose(&mut x, box_val);
        return Option::Some(());
    }

    fn try_lower_div_mod_with_feedback(&mut self, x: Nref) -> TryLowerR {
        debug_assert!(x.borrow().is_value_binary_arith());

        let lhs = x.borrow().lhs();
        let rhs = x.borrow().rhs();

        let lhs_type = self.lower.lhs_type_hint(&x);
        let rhs_type = self.lower.rhs_type_hint(&x);

        let unbox_lhs = self.lower.new_unbox_guard(lhs, lhs_type)?;

        let unbox_rhs = self.lower.new_unbox_guard(rhs, rhs_type)?;

        // generate code for checking whether rhs is zero or not.
        let checked_unbox_rhs = self.lower.new_guard_not_zero(unbox_rhs);

        let unbox_div = match x.borrow().op.op {
            Opcode::RvDiv => self.lower.new_unbox_div(
                unbox_lhs,
                checked_unbox_rhs,
                x.bc.clone(),
            )?,

            Opcode::RvMod => self.lower.new_unbox_mod(
                unbox_lhs,
                checked_unbox_rhs,
                x.bc.clone(),
            )?,

            _ => unreachable!(),
        };

        let box_val = self.lower.new_box_value(unbox_div);

        Node::replace_and_dispose(&mut x, box_val);
        return Option::Some(());
    }

    fn lower_with_builtin(&mut self, x: Nref) {
        let lhs = x.borrow().lhs();
        let rhs = x.borrow().rhs();

        let flag = match &x.borrow().op.op {
            Opcode::RvAdd => BuiltinCall::ArithAdd,
            Opcode::RvSub => BuiltinCall::ArithSub,
            Opcode::RvMul => BuiltinCall::ArithMul,
            Opcode::RvDiv => BuiltinCall::ArithDiv,
            Opcode::RvMod => BuiltinCall::ArithMod,
            Opcode::RvPow => BuiltinCall::ArithPow,
            _ => unreachable!(),
        };

        self.lower.lower_binary_op_with_builtin(
            lhs,
            rhs,
            flag,
            x.borrow().bc.clone(),
        );
    }

    fn lower(&mut self, x: Nref) {
        match x.borrow().op.op {
            Opcode::RvAdd | Opcode::RvSub | Opcode::RvMul | Opcode::RvPow => {
                if self
                    .try_lower_add_sub_mul_pow_with_feedback(Nref::clone(&x))
                    .is_some()
                {
                    return;
                }
            }

            Opcode::RvDiv | Opcode::RvMod => {
                if self
                    .try_lower_div_mod_with_feedback(Nref::clone(&x))
                    .is_some()
                {
                    return;
                }
            }

            _ => {
                unreachable!();
            }
        };

        self.lower_with_builtin(x);
    }
}

// -- ==========================================================================
//
// Comparison RvXXX lower
//
// Assume A, B are both integer in type feedback, then the following Rv sub-graph
//
//  [RvEq] ----- [A]
//    |
//    +--------- [B]
//
//  will be lowered into following sub-graph
//
//  [CmpIntEq] ---- [UnboxInt] -- [Guard Int] --- [A]
//     | |
//     | +--------- [UnboxInt] -- [Guard Int] --- [B]
//     |
//     +------------[BoxBool]
//
//
// Later on CmpIntEq can be directly lowered into instruction like X86-64's cmp
// test which simply just modify flag registers and turns into a boolean value by
// issuing a ccmov instruction
struct RvComparisonLower {
    lower: RvLower,
}

impl RvComparisonLower {
    fn try_lower_with_feedback(&mut self, x: Nref) -> TryLowerR {
        debug_assert!(x.borrow().is_value_binary_arith());

        let op_code = x.borrow().op.op.clone();

        let lhs = x.borrow().lhs();
        let rhs = x.borrow().rhs();
        let lhs_type_hint = self.lower.lhs_type_hint(&lhs);
        let rhs_type_hint = self.lower.rhs_type_hint(&rhs);

        let unbox_lhs = self.lower.new_unbox_guard(lhs, lhs_type_hint)?;
        let unbox_rhs = self.lower.new_unbox_guard(rhs, rhs_type_hint)?;

        let unbox_cmp = match op_code {
            Opcode::RvEq => self.lower.new_unbox_cmp_eq(
                unbox_lhs,
                unbox_rhs,
                x.borrow().bc.clone(),
            )?,

            Opcode::RvNe => self.lower.new_unbox_cmp_ne(
                unbox_lhs,
                unbox_rhs,
                x.borrow().bc.clone(),
            )?,

            Opcode::RvLt => self.lower.new_unbox_cmp_lt(
                unbox_lhs,
                unbox_rhs,
                x.borrow().bc.clone(),
            )?,

            Opcode::RvLe => self.lower.new_unbox_cmp_le(
                unbox_lhs,
                unbox_rhs,
                x.borrow().bc.clone(),
            )?,

            Opcode::RvGt => self.lower.new_unbox_cmp_gt(
                unbox_lhs,
                unbox_rhs,
                x.borrow().bc.clone(),
            )?,

            Opcode::RvLe => self.lower.new_unbox_cmp_le(
                unbox_lhs,
                unbox_rhs,
                x.borrow().bc.clone(),
            )?,
        };

        let box_val = self.lower.new_box_value(unbox_cmp);

        Node::replace_and_dispose(&mut x, box_val);
        return Option::Some(());
    }

    fn lower_with_builtin(&mut self, x: Nref) {
        let lhs = x.borrow().lhs();
        let rhs = x.borrow().rhs();

        let flag = match &x.borrow().op.op {
            Opcode::RvEq => BuiltinCall::CompEq,
            Opcode::RvNe => BuiltinCall::CompNe,
            Opcode::RvLt => BuiltinCall::CompLt,
            Opcode::RvLe => BuiltinCall::CompLe,
            Opcode::RvGt => BuiltinCall::CompGt,
            Opcode::RvGe => BuiltinCall::CompGe,
            _ => unreachable!(),
        };

        self.lower.lower_binary_op_with_builtin(
            lhs,
            rhs,
            flag,
            x.borrow().bc.clone(),
        );
    }

    fn lower(&mut self, x: Nref) {
        match x.borrow().op.op {
            Opcode::RvEq
            | Opcode::RvNe
            | Opcode::RvLt
            | Opcode::RvLe
            | Opcode::RvGt
            | Opcode::RvGe => {
                if self
                    .try_lower_add_sub_mul_pow_with_feedback(Nref::clone(&x))
                    .is_some()
                {
                    return;
                }
            }

            _ => {
                unreachable!();
            }
        };

        self.lower_with_builtin(x);
    }
}

// -- ==========================================================================
//
// Unary operator lower
//
//   RvToString
//   RvToBoolean
//   RvNot
//   RvNeg
//
// 1) RvToString --> will have to be lowered into builtin function call
//
// 2) RvToBoolean --> will be lowered based on the guard, ie what type it is
//
//    [IntToBoolean] ---- [GuardInt] --- [A]
//         |
//         |
//         |
//     [BoxBool]
//
// 3) RvNot will be lowered with the help of unboxed conversion
//
//    [IntToBoolean] ---- [GuardInt] --- [A]
//         |
//         |
//    [FlipBoolean]
//         |
//         |
//      [BoxBool]
//
// 4) RvNeg will be lowered with the help of unboxed conversion as well
//
//    [UnboxInt] ---- [GuardInt] --- [A]
//         |
//         |
//      [IntNeg]
//         |
//         |
//      [BoxInt]
//
//
//    [UnboxReal] ---- [GuardReal] --- [A]
//         |
//         |
//     [RealNeg]
//         |
//         |
//     [BoxReal]

struct RvUnaryLower {
    lower: RvLower,
}

impl RvUnaryLower {
    // lower the RvToBoolean
    //
    // speculative lower, if lower failed, fallback to unary builtin
    fn try_lower_rv_to_boolean_with_feedback(&mut self, n: Nref) -> TryLowerR {
        debug_assert!(n.borrow().op.op == Opcode::RvToBoolean);

        let val = n.borrow().una();
        let val_feedback = self.lower.una_type_hint(&n);
        let unboxed_val = self.lower.new_unbox_guard(val, val_feedback)?;
        let to_boolean = self.lower.new_to_boolean(unboxed, n.bc.clone())?;
        let boxed_val = self.lower.new_box_value(to_boolean);

        Node::replace_and_dispose(&mut n, boxed_val);
        return TryLowerR::Some(());
    }

    // lower the RvNeg
    //
    // Speculative lower, if lower failed, fallback to unary builtin
    fn try_lower_rv_neg_with_feedback(&mut self, n: Nref) -> TryLowerR {
        debug_assert!(n.borrow().op.op == Opcode::RvNeg);

        let val = n.borrow().una();
        let val_feedback = self.lower.una_type_hint(&n);
        let unboxed_val = self.lower.new_unbox_guard(val, val_feedback)?;
        let negate_val = self.lower.new_negate(unboxed_val);
        let boxed_val = self.lower.new_box_value(negate_val);

        Node::replace_and_dispose(&mut n, boxed_val);
        return TryLowerR::Some(());
    }

    // lower the RvNot
    //
    // Speculative lower, if lower failed, fallback to unary builtin
    fn try_lower_rv_not_with_feedback(&mut self, n: Nref) -> TryLowerR {
        debug_assert!(n.borrow().op.op == Opcode::RvNot);

        let val = n.borrow().una();
        let val_feedback = self.lower.una_type_hint(&n);
        let unboxed_val = self.lower.new_unbox_guard(val, val_feedback)?;
        let to_boolean = self.lower.new_to_boolean(unboxed, n.bc.clone())?;
        let flip_boolean =
            self.lower.new_flip_boolean(to_boolean, n.bc.clone())?;
        let boxed_val = self.lower.new_box_value(flip_boolean);

        Node::replace_and_dispose(&mut n, boxed_val);
        return TryLowerR::Some(());
    }

    // Fallback lower
    fn lower_with_builtin(&mut self, n: Nref) {
        let flag = match n.borrow().op.op {
            Opcode::RvToString => BuiltinCall::UnaToString,
            Opcode::RvToBoolean => BuiltinCall::UnaToBoolean,
            Opcode::RvNeg => BuiltinCall::UnaNeg,
            Opcode::RvNot => BuiltinCall::UnaNot,
            _ => unreachable!(),
        };

        let v = n.borrow().una();

        self.lower
            .lower_unary_op_with_builtin(v, flag, n.bc.clone());
    }

    fn lower(&mut self, x: Nref) {
        match x.borrow().op.op {
            Opcode::RvToBoolean => {
                if self.try_lower_rv_to_boolean_with_feedback(Nref::clone(&x)) {
                    return;
                }
            }

            Opcode::RvNeg => {
                if self.try_lower_rv_neg_with_feedback(Nref::clone(&x)) {
                    return;
                }
            }

            Opcode::RvNot => {
                if self.try_lower_rv_not_with_feedback(Nref::clone(&x)) {
                    return;
                }
            }
            Opcode::RvToString => (),

            _ => {
                unreachable!();
            }
        };

        self.lower_with_builtin(x);
    }
}

// -- ==========================================================================
// String Concatenation Lowering
//
//   RvConStr operator can take any number of inputs, up to now, we don't support
//   ConStr type internally for our string representation. So all the string
//   concatenation will be lowered as builtin call

struct RvStrConLower;

impl RvStrConLower {
    fn lower(&mut self, x: Nref) {
        debug_assert!(x.borrow().op.op == Opcode::RvConStr);

        // generate any arguments size (N-ary) function call's con_str
        let call = self.lower.do_builtin_call_nary(
            x.borrow().value.clone(),
            BuiltinCall::ConStr,
            x.bc.clone(),
        );

        Node::replace_and_dispose(&mut x, call);
    }
}
