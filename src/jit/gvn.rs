use crate::jit::node::*;
use crate::jit::pass::*;

use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;

// Global Value Numbering
//
// for us GVN is easy to perform since we know that any node without side effect
// can be part of GVN, and then we just do this to dedup sub-expression, that's
// it. Each node's hash will be implemented locally here and just use rust's
// hash set will be fine

struct GVNRcd {
    node: Nref,
}

impl Hash for GVNRcd {
    fn hash<H>(&self, h: &mut H)
    where
        H: Hasher,
    {
        h.write_u64(calc_node_hash(&self.node).unwrap());
        h.finish();
    }
}

impl PartialEq for GVNRcd {
    fn eq(&self, other: &Self) -> bool {
        return eq_node(&self.node, &other.node).unwrap();
    }
}

impl Eq for GVNRcd {}

impl GVN {
    fn try_dedup(&mut self, n: Nref) -> Option<Nref> {
        if bailout_from_gvn(&n) {
            return Option::None;
        }
        match self.h.get(&GVNRcd {
            node: Nref::clone(&n),
        }) {
            Option::Some(vv) => {
                // the node do exist, then just returns that nodes
                return Option::Some(Nref::clone(&vv.node));
            }
            _ => (),
        };

        // do the insertion
        assert!(self.h.insert(GVNRcd {
            node: Nref::clone(&n)
        }));
        return Option::None;
    }
}

pub struct GVN {
    h: HashSet<GVNRcd>,
}

impl NodePass for GVN {
    fn run_node(&mut self, mut n: Nref) -> PassResult {
        match self.try_dedup(Nref::clone(&n)) {
            Option::Some(v) => {
                // de-duplicate the node here
                Node::replace_and_dispose(&mut n, v);
            }
            _ => (),
        };

        return PassResult::OK;
    }
}

// Private implementation for GVN ----------------------------------------------
// check whether the node N should be part of GVN or not. Notes even if the node
// pass this test it does not mean the node should be part of GVN
fn should_gvn(n: &Nref) -> bool {
    if n.borrow().op.tier == OpTier::Bval
        || n.borrow().op.tier == OpTier::Rval
        || n.borrow().op.tier == OpTier::Guard
        || n.borrow().op.tier == OpTier::Trap
        || n.borrow().op.tier == OpTier::Error
        || n.borrow().op.tier == OpTier::Mid
        || n.borrow().op.tier == OpTier::Low
        || n.borrow().op.tier == OpTier::Imm
    {
        debug_assert!(!n.borrow().has_cfg());
        debug_assert!(!n.borrow().has_effect());
        return true;
    }
    return false;
}

// use this to quickly testify whether a node can be part of GVN, this is not
// performant at all.
fn bailout_from_gvn(n: &Nref) -> bool {
    if !should_gvn(n) {
        return true;
    }

    debug_assert!(!n.borrow().has_cfg());
    debug_assert!(!n.borrow().has_effect());

    for v in n.borrow().value.iter() {
        if !should_gvn(v) {
            return true;
        }
    }
    return false;
}

// -----------------------------------------------------------------------------
// Equality of nodes
// -----------------------------------------------------------------------------
fn eq_node(lhs: &Nref, rhs: &Nref) -> Option<bool> {
    if !should_gvn(lhs) || !should_gvn(rhs) {
        return Option::None;
    }

    debug_assert!(!lhs.borrow().has_cfg());
    debug_assert!(!rhs.borrow().has_effect());

    let lhs_op = lhs.borrow().op.op.clone();
    let rhs_op = rhs.borrow().op.op.clone();

    if lhs_op != rhs_op {
        return Option::Some(false);
    }

    if lhs.borrow().value.len() != rhs.borrow().value.len() {
        return Option::Some(false);
    }

    let len = lhs.borrow().value.len();
    for i in 0..len {
        if !eq_node(&lhs.borrow().value[i], &rhs.borrow().value[i])? {
            return Option::Some(false);
        }
    }

    // notes, immediate number should be taken care specifically
    if lhs.borrow().imm != rhs.borrow().imm {
        return Option::Some(false);
    }

    return Option::Some(true);
}

// -----------------------------------------------------------------------------
// Hash of nodes
// -----------------------------------------------------------------------------

fn calc_node_binary_hash(n: &Nref) -> Option<u64> {
    let lhs = n.borrow().lhs();
    let rhs = n.borrow().rhs();

    let lhs_hash = calc_node_hash(&lhs)?;
    let rhs_hash = calc_node_hash(&rhs)?;

    let mut hasher = DefaultHasher::new();

    hasher.write_u64(lhs_hash);
    hasher.write_u64(rhs_hash);
    hasher.write(n.borrow().op.name.as_bytes());

    return Option::Some(hasher.finish());
}

fn calc_node_unary_hash(n: &Nref) -> Option<u64> {
    let una = n.borrow().una();
    let una_hash = calc_node_hash(&una)?;

    let mut hasher = DefaultHasher::new();

    hasher.write_u64(una_hash);
    hasher.write(n.borrow().op.name.as_bytes());

    return Option::Some(hasher.finish());
}

fn calc_node_nary_hash(n: &Nref) -> Option<u64> {
    let mut hasher = DefaultHasher::new();
    hasher.write(n.borrow().op.name.as_bytes());
    for v in n.borrow().value.iter() {
        let vv = calc_node_hash(v)?;
        hasher.write_u64(vv);
    }
    return Option::Some(hasher.finish());
}

fn calc_node_guard_hash(n: &Nref) -> Option<u64> {
    debug_assert!(n.borrow().is_guard());
    debug_assert!(n.borrow().value_len() == 1);

    return calc_node_unary_hash(n);
}

fn calc_node_trap_hash(n: &Nref) -> Option<u64> {
    debug_assert!(n.borrow().is_trap());
    debug_assert!(n.borrow().value_len() == 1);

    return calc_node_unary_hash(n);
}

fn calc_node_error_hash(n: &Nref) -> Option<u64> {
    debug_assert!(n.borrow().is_error());
    debug_assert!(n.borrow().value_len() == 1);

    return calc_node_unary_hash(n);
}

fn calc_node_imm_hash(n: &Nref) -> Option<u64> {
    let mut hasher = DefaultHasher::new();
    match &n.borrow().imm {
        Imm::Index(v) => hasher.write_u32(*v),
        Imm::ImmU32(v) => hasher.write_u32(*v),
        Imm::ImmU16(v) => hasher.write_u16(*v),
        Imm::ImmU8(v) => hasher.write_u8(*v),
        Imm::ImmI64(v) => hasher.write_i64(*v),
        Imm::ImmI32(v) => hasher.write_i32(*v),
        Imm::ImmI16(v) => hasher.write_i16(*v),
        Imm::ImmI8(v) => hasher.write_i8(*v),
        Imm::ImmF64(v) => hasher.write(v.to_string().as_bytes()),
        Imm::ImmBoolean(v) => hasher.write(v.to_string().as_bytes()),
        Imm::ImmNull => hasher.write(b"null"),
        Imm::ImmStr(v) => hasher.write(v.as_bytes()),

        _ => {
            unreachable!();
        }
    };

    return Option::Some(hasher.finish());
}

// Calculating the hash of node, if the node's hash is not able to be calculated
// ie, control flow graph or node has side effect, then it just returns None
fn calc_node_hash(n: &Nref) -> Option<u64> {
    if n.borrow().op.side_effect {
        return Option::None;
    }
    if !n.borrow().is_value() {
        return Option::None;
    }

    // anything that has effect or control flow component will NOT be part of the
    // GVN
    if n.borrow().has_cfg() || n.borrow().has_effect() {
        return Option::None;
    }

    match n.borrow().op.op {
        // =====================================================================
        // Rval tier IR nodes GVN hash

        // arithmetic parts
        Opcode::RvAdd
        | Opcode::RvSub
        | Opcode::RvMul
        | Opcode::RvDiv
        | Opcode::RvMod
        | Opcode::RvPow => {
            return calc_node_binary_hash(n);
        }

        // string comparison
        Opcode::RvConStr => {
            return calc_node_nary_hash(n);
        }

        // comparison parts
        Opcode::RvLt
        | Opcode::RvLe
        | Opcode::RvGt
        | Opcode::RvGe
        | Opcode::RvEq
        | Opcode::RvNe => {
            return calc_node_binary_hash(n);
        }

        // unary parts
        Opcode::RvNot
        | Opcode::RvNeg
        | Opcode::RvToString
        | Opcode::RvToBoolean => {
            return calc_node_unary_hash(n);
        }

        // =====================================================================
        // Mid iter IR GVN hash
        Opcode::I64Add
        | Opcode::F64Add
        | Opcode::I64Sub
        | Opcode::F64Sub
        | Opcode::I64Mul
        | Opcode::F64Mul
        | Opcode::I64Div
        | Opcode::F64Div
        | Opcode::I64Mod
        | Opcode::F64Mod
        | Opcode::I64Pow
        | Opcode::F64Pow => {
            return calc_node_binary_hash(n);
        }

        Opcode::I64Eq
        | Opcode::I64Ne
        | Opcode::I64Lt
        | Opcode::I64Le
        | Opcode::I64Gt
        | Opcode::I64Ge => {
            return calc_node_binary_hash(n);
        }

        Opcode::F64Eq
        | Opcode::F64Ne
        | Opcode::F64Lt
        | Opcode::F64Le
        | Opcode::F64Gt
        | Opcode::F64Ge => {
            return calc_node_binary_hash(n);
        }

        Opcode::BooleanEq
        | Opcode::BooleanNe
        | Opcode::NullEq
        | Opcode::NullNe => {
            return calc_node_binary_hash(n);
        }

        // Unary
        Opcode::I64ToBoolean
        | Opcode::F64ToBoolean
        | Opcode::StrToBoolean
        | Opcode::ListToBoolean
        | Opcode::ObjectToBoolean
        | Opcode::IterToBoolean
        | Opcode::I64Negate
        | Opcode::F64Negate
        | Opcode::FlipBoolean => {
            return calc_node_unary_hash(n);
        }

        // =====================================================================
        // Reset tiers
        //
        //   1. Immediate
        //   2. Guard
        //   3. Trap
        //   4. Error
        //
        _ => {
            match n.borrow().op.tier {
                OpTier::Guard => {
                    return calc_node_guard_hash(n);
                }
                OpTier::Trap => {
                    return calc_node_trap_hash(n);
                }
                OpTier::Error => {
                    return calc_node_error_hash(n);
                }
                OpTier::Imm => {
                    return calc_node_imm_hash(n);
                }

                _ => (),
            };
        }
    };

    return Option::None;
}
