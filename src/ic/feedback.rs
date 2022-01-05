// For each bytecode in the ProtoRc, we also have an additional information
// called feedback for that bytecode, which is used to record the type information
// observe during the runtime execution. This information is needed for us to
// generate speculative optimization code.
use crate::ic::ftype::*;

// A saturated counter, ie it will not grow unboundly and will not make rust
// panic at overflow
#[derive(Clone)]
pub struct TCounter(u8);

#[derive(Clone)]
pub struct BinaryFeedback {
    pub lhs_cnt: TCounter,
    pub rhs_cnt: TCounter,
    pub lhs_bailout: TCounter,
    pub rhs_bailout: TCounter,
    pub lhs_type: FType,
    pub rhs_type: FType,
}

#[derive(Clone)]
pub struct UnaryFeedback {
    pub opr_type: FType,
    pub opr_cnt: TCounter,
    pub opr_bailout: TCounter,
}

#[derive(Clone)]
pub enum Feedback {
    Binary(BinaryFeedback),
    Unary(UnaryFeedback),
    NA,
}

pub struct FeedbackList(Vec<Feedback>);

// If the type changes more than 20 times, then we mark it pernently unknown
const THRESHOLD: u8 = 8;

fn hit_one(t: &mut FType, tcnt: &mut TCounter, bcnt: &mut TCounter, c: FType) {
    if bcnt.val() > THRESHOLD {
        return;
    }

    if *t != c {
        if bcnt.inc() == THRESHOLD {
            bcnt.inc();
            *t = FType::Unknown;
            return;
        }
        *t = c;
        tcnt.reset();
    } else {
        tcnt.inc();
    }
}

impl BinaryFeedback {
    pub fn new_one(l: FType, r: FType) -> BinaryFeedback {
        BinaryFeedback {
            lhs_cnt: TCounter::one(),
            rhs_cnt: TCounter::one(),
            lhs_bailout: TCounter::zero(),
            rhs_bailout: TCounter::zero(),
            lhs_type: l,
            rhs_type: r,
        }
    }

    pub fn hit(&mut self, l: FType, r: FType) {
        hit_one(
            &mut self.lhs_type,
            &mut self.lhs_cnt,
            &mut self.lhs_bailout,
            l,
        );

        hit_one(
            &mut self.rhs_type,
            &mut self.rhs_cnt,
            &mut self.rhs_bailout,
            r,
        );
    }
}

impl UnaryFeedback {
    pub fn new_one(v: FType) -> UnaryFeedback {
        UnaryFeedback {
            opr_type: v,
            opr_cnt: TCounter::one(),
            opr_bailout: TCounter::zero(),
        }
    }
    pub fn hit(&mut self, x: FType) {
        hit_one(
            &mut self.opr_type,
            &mut self.opr_cnt,
            &mut self.opr_bailout,
            x,
        );
    }
}

impl TCounter {
    fn val(&self) -> u8 {
        return self.0;
    }
    fn reset(&mut self) {
        self.0 = 0;
    }
    fn inc(&mut self) -> u8 {
        if self.0 != 255 {
            self.0 += 1;
        }
        return self.0;
    }
    fn zero() -> TCounter {
        TCounter(0)
    }
    fn one() -> TCounter {
        TCounter(1)
    }
}

impl Feedback {
    pub fn binary(&mut self, l: FType, r: FType) {
        if let Feedback::Binary(ref mut x) = self {
            x.hit(l, r);
        } else {
            *self = Feedback::Binary(BinaryFeedback::new_one(l, r));
        }
    }
    pub fn unary(&mut self, v: FType) {
        if let Feedback::Unary(ref mut x) = self {
            x.hit(v);
        } else {
            *self = Feedback::Unary(UnaryFeedback::new_one(v));
        }
    }
}

impl FeedbackList {
    pub fn new(sz: u32) -> FeedbackList {
        let mut x = Vec::<Feedback>::new();
        x.resize(sz as usize, Feedback::NA);
        FeedbackList(x)
    }
    pub fn ensure(&mut self, sz: u32) {
        self.0.resize(sz as usize, Feedback::NA);
    }
    pub fn index_mut(&mut self, idx: usize) -> &mut Feedback {
        &mut self.0[idx]
    }
    pub fn index(&self, idx: usize) -> &Feedback {
        &self.0[idx]
    }

    // pos is realy just index same as the bytecode. the FeedbackList is owned
    // by each Function object to store the runtime feedback information for
    // sure it is not that performant :(
    pub fn binary(&mut self, pos: u32, l: FType, r: FType) {
        if let Feedback::Binary(ref mut x) = &mut self.0[pos as usize] {
            x.hit(l, r);
        } else {
            self.0[pos as usize] =
                Feedback::Binary(BinaryFeedback::new_one(l, r));
        }
    }
    pub fn unary(&mut self, pos: u32, t: FType) {
        if let Feedback::Unary(ref mut x) = &mut self.0[pos as usize] {
            x.hit(t);
        } else {
            self.0[pos as usize] = Feedback::Unary(UnaryFeedback::new_one(t));
        }
    }
}
