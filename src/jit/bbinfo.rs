use crate::bc::bytecode::*;
use crate::object::object::*;
use bitvec::prelude::*;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq)]
pub enum JumpType {
    LoopBack,
    CondFalse,
    CondTrue,
    // two special cases for synthetic value normalization
    AndFalse,
    OrTrue,
    Uncond,
}

#[derive(Debug, Clone, PartialEq)]
pub struct JumpEdge {
    pub jtype: JumpType,
    pub bid: u32,
}

#[derive(Clone)]
pub struct BBInfo {
    // -------------------------------------------------------------------------
    pub bc_from: u32,
    pub bc_to: u32,
    pub id: u32,

    // -------------------------------------------------------------------------
    // branch information, each bb can jump to at most 2 BB due to the condition
    // jump, other situation is not possible since we don't have goto
    pub lhs: Option<JumpEdge>,
    pub rhs: Option<JumpEdge>,

    // -------------------------------------------------------------------------
    // Predecessor information, ie which BB jumps into this block
    pub pred: Vec<JumpEdge>,

    // -------------------------------------------------------------------------
    // Variable informations

    // The variable that has been accessed both inside and outside of the loop,
    // ie a loop induction variable
    pub loop_iv_assignment: BitVec,
}

pub struct LoopRange(Vec<(u32, u32)>);

pub struct BBInfoSet {
    // lists of BBInfo's, notes the bb's id is the index of the vector as well
    pub bbinfo_list: Vec<BBInfo>,

    // index of BBInfo's, all sortted with ascending order
    pub bc_from_map_bbinfo: Vec<Option<u32>>,
    pub bc_to_map_bbinfo: Vec<Option<u32>>,

    // bytecode range for loop body, ie the bytecode position of LoopBack until
    // it reaches a LoopBack. Good to use for furture loop analysis
    pub loop_range: LoopRange,
}

impl JumpEdge {
    fn new(j: JumpType, id: u32) -> JumpEdge {
        JumpEdge { jtype: j, bid: id }
    }
    fn new_uncond(id: u32) -> JumpEdge {
        JumpEdge {
            jtype: JumpType::Uncond,
            bid: id,
        }
    }
    fn new_true(id: u32) -> JumpEdge {
        JumpEdge {
            jtype: JumpType::CondTrue,
            bid: id,
        }
    }
    fn new_false(id: u32) -> JumpEdge {
        JumpEdge {
            jtype: JumpType::CondFalse,
            bid: id,
        }
    }
    fn new_loop_back(id: u32) -> JumpEdge {
        JumpEdge {
            jtype: JumpType::LoopBack,
            bid: id,
        }
    }
    fn new_and_false(id: u32) -> JumpEdge {
        JumpEdge {
            jtype: JumpType::AndFalse,
            bid: id,
        }
    }
    fn new_or_true(id: u32) -> JumpEdge {
        JumpEdge {
            jtype: JumpType::OrTrue,
            bid: id,
        }
    }

    pub fn is_loop_back(&self) -> bool {
        return self.jtype == JumpType::LoopBack;
    }
}

impl LoopRange {
    pub fn new() -> LoopRange {
        LoopRange(Vec::new())
    }

    pub fn add(&mut self, x: (u32, u32)) {
        self.0.push(x);
    }
    pub fn len(&self) -> usize {
        return self.0.len();
    }

    // Checking whether the 2 ranges have following overlapping :
    // --------------------------------------------------------
    // Case 1:
    //
    //     [ ----- range 1 ----- ]
    //            [ ----- range 2 ----]
    //
    // --------------------------------------------------------
    // Case 2:
    //
    //     [ ----- range 1 ----- ]
    // [ ----- range 2 -----]
    //
    // --------------------------------------------------------
    // Notes entirely included or unoverlapped are fine.
    //   1) Unoverlapped range means parallel loop;
    //   2) Entirely included situation means nested loop
    fn is_range_overlap(lhs: &(u32, u32), rhs: &(u32, u32)) -> bool {
        // situation 1:
        // [ -- range 1 -- ]
        //                   [ -- range 2 -- ]
        if lhs.1 <= rhs.0 {
            return true;
        }

        // situation 2:
        // [ -- range 2 -- ]
        //                   [ -- range 1 -- ]
        if lhs.0 >= rhs.1 {
            return true;
        }

        // situation 3:
        // [ ----        range 1      ---- ]
        //          [ -- range 2 -- ]
        if lhs.0 <= rhs.0 && lhs.1 >= rhs.1 {
            return true;
        }

        // situation 4:
        // [ ----        range 2      ---- ]
        //          [ -- range 1 -- ]
        if rhs.0 <= lhs.0 && rhs.1 >= lhs.1 {
            return true;
        }

        // rest situation are invalid here.
        return false;
    }

    // finding out 2 ranges that resides inside of the LoopRange has invalid
    // overlapping. This typically indicates code bug.
    fn find_loop_range_overlap(&self) -> Option<(u32, u32)> {
        let mut i = 0;
        let len = self.0.len() as u32;

        while i < len {
            let mut j = i + 1;
            while j < len {
                if LoopRange::is_range_overlap(
                    &self.0[i as usize],
                    &self.0[j as usize],
                ) {
                    return Option::Some((i, j));
                }

                j += 1;
            }

            i += 1;
        }

        return Option::None;
    }

    // used to verify that the loop range contains no overlap ranged, for us
    // this is impossible since we don't have none reducable graph since we
    // don't allow arbitary jump with goto statement
    pub fn assert_no_overlap(&self) {
        match self.find_loop_range_overlap() {
            Option::Some(_) => assert!(false),
            _ => (),
        };
    }
}

impl BBInfo {
    pub fn new_default(id: u32, from: u32) -> BBInfo {
        BBInfo {
            bc_from: from,
            bc_to: 0,
            id: id,
            lhs: Option::None,
            rhs: Option::None,
            pred: Vec::new(),
            loop_iv_assignment: BitVec::new(),
        }
    }

    pub fn new_range(id: u32, from: u32, to: u32) -> BBInfo {
        BBInfo {
            bc_from: from,
            bc_to: to,
            id: id,
            lhs: Option::None,
            rhs: Option::None,
            pred: Vec::new(),
            loop_iv_assignment: BitVec::new(),
        }
    }

    // Convinient methods
    pub fn is_succ(&self, x: u32) -> bool {
        return match &self.lhs {
            Option::Some(v) => v.bid == x,
            _ => false,
        } || match &self.rhs {
            Option::Some(v) => v.bid == x,
            _ => false,
        };
    }

    pub fn is_pred(&self, x: u32) -> bool {
        for y in self.pred.iter() {
            if y.bid == x {
                return true;
            }
        }
        return false;
    }
}

impl BBInfoSet {
    pub fn new() -> BBInfoSet {
        BBInfoSet {
            bbinfo_list: Vec::new(),
            bc_from_map_bbinfo: Vec::new(),
            bc_to_map_bbinfo: Vec::new(),
            loop_range: LoopRange::new(),
        }
    }

    pub fn at(&self, i: u32) -> &BBInfo {
        return &self.bbinfo_list[i as usize];
    }
    pub fn at_mut(&mut self, i: u32) -> &mut BBInfo {
        return &mut self.bbinfo_list[i as usize];
    }

    // printing the BBInfoSet as dot graph for visulization
}

// ------------------------------------------------------------------------
// Basic Block Creation
//
// This pass deals with few pre-stage thing before starting the main IR graph
// construction and we try to keep this process clean and simple.
//
// 1) BB creation
//    It recognizes all the BB and also generates link between. Additionally
//    links are categorized
//
// 2) Loop induction variable recognition
//    It tries to learn all the loop iv in each BB for later loop IV generation

pub struct BBInfoBuilder {
    function: FuncRef,
    arg_count: u32,
    push_n: u32,
    out: BBInfoSet,
}

impl BBInfoBuilder {
    fn new(fref: FuncRef, acnt: u32) -> BBInfoBuilder {
        let fbytecode = fref.borrow().proto.code.array[0].clone();
        let push_n = match fbytecode {
            Bytecode::PushN(v) => v,
            _ => {
                unreachable!();
            }
        };

        BBInfoBuilder {
            function: fref,
            arg_count: acnt,
            push_n: push_n,
            out: BBInfoSet::new(),
        }
    }

    fn bc_at(&self, idx: u32) -> Bytecode {
        return self.function.borrow().proto.code.array[idx as usize].clone();
    }

    fn bb_at(&self, idx: u32) -> &BBInfo {
        return &self.out.bbinfo_list[idx as usize];
    }

    fn bb_at_mut(&mut self, idx: u32) -> &mut BBInfo {
        return &mut self.out.bbinfo_list[idx as usize];
    }
    // The stack offset starting for each temporary variables, the parser will
    // essentially hoist all the local variable, regardless whether it is in
    // a nested lexical scope or not to the beginning of the function.
    fn tmp_var_size(&self) -> usize {
        return (self.push_n + self.arg_count + 1) as usize;
    }

    // testing a index is a local variable or not. The local variable access
    // have a simple pattern, as Load/Store. A store will essentially mutate
    // a local variable and a load will just use the local variable
    fn is_tmp_var(&self, idx: u32) -> bool {
        return idx < self.tmp_var_size() as u32;
    }

    // ----------------------------------------------------------------------
    // Basic Block Discovery Algorithm
    //
    //   The algorithm has been splitted into 2 passes,
    //
    //   1) Pass one is to identify all the BB, a BB is been recognized as we
    //      scanning the bytecode array. Until we hit a control transfer bytecode
    //      or we hit the start point of another already discoveried BB, then
    //      we learn there's a BB.
    //
    //   2) Pass two is to be used for linking the BB's edge. Including its
    //      predecessor and successor. Each BB can have multiple predecessor,
    //      but can at most have 2 successor (due to the bytecode). Additionally
    //      each jump edge will be labeled with additional information for later
    //      IR construction.

    // build a BB starting from the specific code position until it hits a
    // jump instruction, returns the jump instruction's position or end of
    // bytecode
    fn scan_until_jump(&self, pc: u32) -> u32 {
        let mut bc_idx = pc;
        let bc_len = self.function.borrow().proto.code.len() as u32;

        // Check situation that the a single BB just have one jump, this is
        // possible when generating loop rotation header.
        {
            let bytecode = self.bc_at(bc_idx);
            match bytecode {
                Bytecode::JumpFalse(_)
                | Bytecode::LoopBack(_)
                | Bytecode::Jump(_)
                | Bytecode::And(_)
                | Bytecode::Or(_)
                | Bytecode::Ternary(_)
                | Bytecode::Return(_)
                | Bytecode::Halt => {
                    // This BB just have one single instruction which does a
                    // control flow transfer
                    return bc_idx;
                }
                _ => (),
            };
        }

        bc_idx += 1;

        while bc_idx < bc_len {
            // (0) If the bc_idx points to an already existed BB, then just stop
            //     here, since we should not let the BB overlap with other BB.
            match &self.out.bc_from_map_bbinfo[bc_idx as usize] {
                Option::Some(_) => {
                    debug_assert!(bc_idx - 1 >= pc);
                    return bc_idx - 1;
                }
                _ => (),
            };

            // (1) Check the bytecode, if the bytecode is a control flow transfer
            //     one then, we end up with a BB.
            let bytecode = self.bc_at(bc_idx);
            match bytecode {
                Bytecode::JumpFalse(_)
                | Bytecode::LoopBack(_)
                | Bytecode::Jump(_)
                | Bytecode::And(_)
                | Bytecode::Or(_)
                | Bytecode::Ternary(_)
                | Bytecode::Return(_)
                | Bytecode::Halt => {
                    return bc_idx;
                }
                _ => (),
            };

            bc_idx += 1;
        }

        // Currently, we should never reach the end of bytecode since at least
        // one halt will be inserted at the end of each proto's bytecode array
        unreachable!();
    }

    // Helper to add a bb from the specified CodePos as starting point. Notes
    // this function take care of the loop jump, ie jump back stuff if needed.
    fn add_bbinfo(&mut self, cp: u32) -> (u32, bool) {
        // (1) check whether existed or not, if so we just need to nothing but
        //     return the existed block's id
        match &self.out.bc_from_map_bbinfo[cp as usize] {
            Option::Some(id) => {
                return (*id, false);
            }
            _ => (),
        };

        // (2) check whether jumps into the middle of a existed BB or overlapped
        //     with the existed blocks.
        //
        //     2 situations are needed to be considered :
        //
        //     1) the cp points to the middle of an existed block, so this
        //        block needs to be splitted
        //
        //     2) the cp pointed range includes the existed blocks, this is
        //        covered by the scan_until_jump which will consider the existed
        //        BB instead of just bytecodes
        //
        let mut new_range = Option::<(u32, u32, u32)>::None;
        for bb in self.out.bbinfo_list.iter_mut() {
            if bb.bc_from < cp && bb.bc_to >= cp {
                // the jump jumps into the middle of an already existed block,
                // so split the block right at the position of cp
                let new_end = bb.bc_to;
                new_range = Option::Some((cp, new_end, bb.id));
                break;
            }
        }

        // (3) if we need to add an extra BB, then do it
        match new_range {
            Option::Some((start, end, prev_id)) => {
                let idx = self.out.bbinfo_list.len() as u32;
                self.out
                    .bbinfo_list
                    .push(BBInfo::new_range(idx, start, end));

                // setup index for current current BB
                self.out.bc_from_map_bbinfo[start as usize] = Option::Some(idx);
                self.out.bc_to_map_bbinfo[end as usize] = Option::Some(idx);

                // setup index for previous BB
                self.out.bbinfo_list[prev_id as usize].bc_to = cp - 1;
                self.out.bc_to_map_bbinfo[(cp - 1) as usize] =
                    Option::Some(prev_id);

                return (idx, false);
            }
            _ => (),
        };

        // (4) needs to create a new BB
        {
            let idx = self.out.bbinfo_list.len() as u32;
            self.out.bbinfo_list.push(BBInfo::new_default(idx, cp));
            self.out.bc_from_map_bbinfo[cp as usize] = Option::Some(idx);
            return (idx, true);
        }
    }

    // Pass one, discovery of all the BB. Notes this pass doesn't generate link
    // but just recognize all the BB and store them into bbinfo_lists. Additionally
    // all the bc_from/bc_to mapping index are setup.
    fn discovery_bb(&mut self) {
        let mut bc_idx;

        let bc_len = self.function.borrow().proto.code.len();
        self.out.bc_from_map_bbinfo.resize(bc_len, Option::None);
        self.out.bc_to_map_bbinfo.resize(bc_len, Option::None);

        // A worker queue keep track of the unfinshed task/job
        let mut wqueue = VecDeque::<u32>::new();
        self.add_bbinfo(0);
        wqueue.push_back(0);

        while wqueue.len() != 0 {
            let bb_idx = wqueue.pop_front().unwrap();
            bc_idx = self.out.bbinfo_list[bb_idx as usize].bc_from;

            let last_jump = self.scan_until_jump(bc_idx);

            // the last bytecode
            let bytecode = self.bc_at(last_jump);

            self.out.bbinfo_list[bb_idx as usize].bc_to = last_jump;
            self.out.bc_to_map_bbinfo[(last_jump) as usize] =
                Option::Some(bb_idx);

            match bytecode {
                Bytecode::JumpFalse(cp) => {
                    // false branch
                    {
                        let (idx, is_new) = self.add_bbinfo(cp);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }

                    // true branch
                    {
                        let (idx, is_new) = self.add_bbinfo(last_jump + 1);

                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }
                }

                Bytecode::Jump(cp) => {
                    let (idx, is_new) = self.add_bbinfo(cp);
                    if is_new {
                        wqueue.push_back(idx);
                    }
                }

                Bytecode::LoopBack(cp) => {
                    let (idx, is_new) = self.add_bbinfo(cp);
                    if is_new {
                        wqueue.push_back(idx);
                    }

                    debug_assert!(cp < last_jump);
                    self.out.loop_range.add((cp, last_jump));
                }

                Bytecode::And(cp) => {
                    // false jump
                    {
                        let (idx, is_new) = self.add_bbinfo(cp);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }

                    // true jump
                    {
                        let (idx, is_new) = self.add_bbinfo(last_jump + 1);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }
                }
                Bytecode::Or(cp) => {
                    // true jump
                    {
                        let (idx, is_new) = self.add_bbinfo(cp);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }

                    // false jump
                    {
                        let (idx, is_new) = self.add_bbinfo(last_jump + 1);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }
                }
                Bytecode::Ternary(cp) => {
                    // false jump
                    {
                        let (idx, is_new) = self.add_bbinfo(cp);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }
                    // true jump
                    {
                        let (idx, is_new) = self.add_bbinfo(last_jump + 1);
                        if is_new {
                            wqueue.push_back(idx);
                        }
                    }
                }

                // Terminted block, maybe we should mark it with extra tag for
                // future analysis
                Bytecode::Return(_) | Bytecode::Halt => (),

                // Notes, it is possible to see other situations, ie the natural
                // jump because of the block is been splitted, if so the last
                // bytecode is not a control flow instruction
                _ => {
                    let (_, is_new) = self.add_bbinfo(last_jump + 1);

                    // the block must already been existed otherwise
                    // scan_until_jump will return us a real jump instruction
                    debug_assert!(!is_new);
                }
            };
        }
    }

    fn link_edge(
        &mut self,
        current_bid: u32,
        target_bc: u32,
        jump_type: JumpType,
    ) -> u32 {
        match &self.out.bc_from_map_bbinfo[target_bc as usize] {
            Option::Some(v) => {
                self.out.bbinfo_list[*v as usize]
                    .pred
                    .push(JumpEdge::new(jump_type, current_bid));
                return *v;
            }
            _ => unreachable!(),
        };
    }

    // Pass 2, perform the BB linking and jump edge labelling
    fn bb_link_edge(&mut self) {
        let len = self.out.bbinfo_list.len() as u32;
        let mut id = 0;

        while id < len {
            let cur_bc = self.bb_at(id).bc_to;
            let last_bytecode = self.bc_at(cur_bc);
            let ft_bc = cur_bc + 1;

            match last_bytecode {
                Bytecode::JumpFalse(bc_pos) | Bytecode::Ternary(bc_pos) => {
                    {
                        let target =
                            self.link_edge(id, bc_pos, JumpType::CondFalse);

                        self.bb_at_mut(id).lhs =
                            Option::Some(JumpEdge::new_false(target));
                    }

                    {
                        let target =
                            self.link_edge(id, ft_bc, JumpType::CondTrue);

                        self.bb_at_mut(id).rhs =
                            Option::Some(JumpEdge::new_true(target));
                    }
                }

                Bytecode::And(bc_pos) => {
                    // false branch
                    {
                        let target =
                            self.link_edge(id, bc_pos, JumpType::AndFalse);
                        self.bb_at_mut(id).lhs =
                            Option::Some(JumpEdge::new_and_false(target));
                    }

                    // true branch
                    {
                        let target =
                            self.link_edge(id, ft_bc, JumpType::CondTrue);
                        self.bb_at_mut(id).rhs =
                            Option::Some(JumpEdge::new_true(target));
                    }
                }

                Bytecode::Or(bc_pos) => {
                    // false branch
                    {
                        let target =
                            self.link_edge(id, ft_bc, JumpType::CondFalse);
                        self.bb_at_mut(id).lhs =
                            Option::Some(JumpEdge::new_false(target));
                    }

                    // true branch
                    {
                        let target =
                            self.link_edge(id, bc_pos, JumpType::OrTrue);
                        self.bb_at_mut(id).rhs =
                            Option::Some(JumpEdge::new_or_true(target));
                    }
                }

                Bytecode::LoopBack(bc_pos) => {
                    let target = self.link_edge(id, bc_pos, JumpType::LoopBack);
                    self.bb_at_mut(id).lhs =
                        Option::Some(JumpEdge::new_loop_back(target));
                }

                Bytecode::Jump(cp) => {
                    let target = self.link_edge(id, cp, JumpType::Uncond);
                    self.bb_at_mut(id).lhs =
                        Option::Some(JumpEdge::new_uncond(target));
                }

                Bytecode::Return(_) | Bytecode::Halt => (),

                _ => {
                    let target = self.link_edge(id, ft_bc, JumpType::Uncond);
                    self.bb_at_mut(id).lhs =
                        Option::Some(JumpEdge::new_uncond(target));
                }
            };

            id += 1;
        }
    }

    fn build_bb(&mut self) {
        self.discovery_bb();
        self.bb_link_edge();
    }

    // -------------------------------------------------------------------------
    // Algorithm part 2
    //
    //   Identify loop induction variable inside of a BB which is contained by
    //   loop range.

    // analyze the bytecode lies in range between [start, end), the [start, end)
    // forms a valid basic block
    fn do_loop_iv_assignment_in_range(&self, start: u32, end: u32) -> BitVec {
        let mut idx = start;
        let mut assignment = BitVec::new();
        assignment.resize(self.tmp_var_size(), false);

        while idx < end {
            // Only care about instruction that performs mutation of variables
            // currently it is Store, which will be used to indicate a local
            // variable, ie tmp var, mutation operation.
            let bytecode = self.bc_at(idx);
            match bytecode {
                Bytecode::Store(idx) => {
                    if self.is_tmp_var(idx) {
                        *assignment.get_mut(idx as usize).unwrap() = true;
                    }
                }
                _ => (),
            };
            idx += 1;
        }

        return assignment;
    }

    fn get_bb_from_range(&self, start: u32, end: u32) -> Vec<u32> {
        let mut o = Vec::<u32>::new();

        // Notes, the bb_from_map_bbinfo is naturally sorted with bytecode pos
        // in ascending order
        let mut curbc = start;
        loop {
            match &self.out.bc_from_map_bbinfo[curbc as usize] {
                Option::Some(bb_idx) => {
                    let bb = &self.out.bbinfo_list[*bb_idx as usize];
                    if bb.bc_to <= end {
                        o.push(bb.id);
                        if bb.bc_to == end {
                            break;
                        } else {
                            curbc = bb.bc_to + 1;
                        }
                    } else {
                        // We should never have such cases
                        unreachable!();
                    }
                }
                _ => {
                    curbc += 1;
                }
            };
        }

        return o;
    }

    fn do_loop_iv_assignment(&mut self) {
        // self.out.loop_range.assert_no_overlap();

        let mut visited = bitvec![u32, Msb0;];
        visited.resize(self.out.bbinfo_list.len(), false);

        for (start, end) in self.out.loop_range.0.iter() {
            let bb_list = self.get_bb_from_range(*start, *end);
            for bb_id in bb_list.iter() {
                if !visited[*bb_id as usize] {
                    *visited.get_mut(*bb_id as usize).unwrap() = true;

                    let (from, to) = {
                        let bb = &mut self.out.bbinfo_list[*bb_id as usize];
                        assert!(bb.loop_iv_assignment.len() == 0);
                        (bb.bc_from, bb.bc_to)
                    };

                    let assignment =
                        self.do_loop_iv_assignment_in_range(from, to);

                    self.out.bbinfo_list[*bb_id as usize].loop_iv_assignment =
                        assignment;
                }
            }
        }
    }

    pub fn build(func: FuncRef, arg_count: u32) -> BBInfoSet {
        let mut x = BBInfoBuilder::new(func, arg_count);

        // (0) perform basic block analyze
        x.build_bb();

        // (1) perform loop iv assignment
        x.do_loop_iv_assignment();

        // (2) pass the output
        return x.out;
    }
}
