/*
// Used to generate the comparison loop.
    //
    // Precondition: the lhs and rhs must be unboxed strref, ie string pointer
    fn new_str_comparison_loop(
        &mut self,
        lhs: Nref,
        rhs: Nref,
        bc: BcCtx,
    ) -> Nref {
        // 1. let lhs_len = load_string_length(lhs);
        let lhs_len = self
            .mptr()
            .borrow_mut()
            .new_str_len(Nref::clone(&lhs), bc.clone());

        // 2. let lhs_len = load_string_length(lhs);
        let rhs_len = self
            .mptr()
            .borrow_mut()
            .new_str_len(Nref::clone(&rhs), bc.clone());

        // 3. cnt = min(lhs_len, rhs_len)
        let cnt = self.mptr().borrow_mut().new_i64_min(
            Nref::clone(&lhs_len),
            Nref::clone(&rhs_len),
            bc.clone(),
        );

        // 4. output = (lhs_len - rhs_len)
        let output_length_diff =
            self.mptr()
                .borrow_mut()
                .new_i64_sub(lhs_len, rhs_len, bc.clone());

        // 5. addressing
        let lhs_addr = self
            .mptr()
            .borrow_mut()
            .new_str_addr_ptr(Nref::clone(&lhs), bc.clone());

        let rhs_addr = self
            .mptr()
            .borrow_mut()
            .new_str_addr_ptr(Nref::clone(&rhs), bc.clone());

        #[rustfmt::skip]
        // Generate the loop body for comparison here.
        //
        //
        //               |---------------- [I64Eq] ----------- [cnt]
        //               |                    |
        //            [IfCmp]                 +--------------- [I64(0)]
        //               |
        //        +------+------------+
        //        |                   |
        //      False               True
        //        |                   |
        //        |                [Loop]---+
        //        |                   |     |
        //        |                   |     |
        //        |                [IfCmp] ----- [I64Eq]  ----------- [I64(0)]
        //        |                   |     |        |
        //        |                   |     |        |
        //    [LoopMerge] -[False]----+-+   |        |
        //        |                     |   |    [I64Sub] ---------- [LoadStrI64]
        //        |                     |   |        |
        //      [False] ----------- [IfCmp]-|--+     +-------------- [LoadStrI64]
        //                              |   |  |
        //                              |   |  |
        //               [cnt] ---- [I64Eq] |  |
        //                              |   |  |
        //                              |   +-[Loopback]
        //                          [LoopIV]
        //

        let mut loop_merge = self.mptr().borrow_mut().new_cfg_jump(bc.clone());

        {
            // cfg blocks
            //
            // 0) pre loop check, ie the if check to testify whether cnt is 0
            //    or not
            let mut pre_loop_check =
                self.mptr().borrow_mut().new_cfg_if_cmp(bc.clone());
            let mut pre_loop_false =
                self.mptr().borrow_mut().new_cfg_jump(bc.clone());

            // 1) comparison check, ie checking whether the comparison of a code
            //    point is 0 or not. notes comp_check is the loop header itself.
            let mut comp_check =
                self.mptr().borrow_mut().new_cfg_if_cmp(bc.clone());
            let mut comp_check_false =
                self.mptr().borrow_mut().new_cfg_jump(bc.clone());

            // 2) loop iv check, ie checking whether the loop should terminate
            let mut loop_check =
                self.mptr().borrow_mut().new_cfg_if_cmp(bc.clone());
            let mut loop_false =
                self.mptr().borrow_mut().new_cfg_jump(bc.clone());

            // 3) loop back branch, jumps back to the loop headers.
            let mut loop_back =
                self.mptr().borrow_mut().new_cfg_loop_back(bc.clone());

            // 4) loop iv
            let mut iv = {
                let one = self.mptr().borrow_mut().new_imm_i64(1, bc.clone());
                let zero = self.mptr().borrow_mut().new_imm_i64(0, bc.lone());

                self.new_loop_iv_i64(
                    zero,
                    Nref::clone(&pre_loop_check),
                    one,
                    Nref::clone(&loop_check),
                    bc.clone(),
                )
            };

            // link the cfg's edge correctly
            {
                // pre loop check
                //   1. false branch goes into the loop
                //   2. true branch goes into the loop itself
                {
                    Node::add_control(
                        &mut pre_loop_check,
                        Nref::clone(&pre_loop_false),
                    );
                    Node::add_control(
                        &mut pre_loop_check,
                        Nref::clone(&comp_check),
                    );
                }

                // Loop body
                //   1. false branch jumps to the loop merge
                //   2. true branch goes into the loop back
                {
                    Node::add_control(
                        &mut comp_check,
                        Nref::clone(&comp_check_false),
                    );
                    Node::add_control(&mut comp_check, Nref::clone(&loop_check));
                }

                // Loop iv
                {
                    Node::add_control(&mut loop_check, Nref::clone(&loop_false));
                    Node::add_control(&mut loop_check, Nref::clone(&loop_back));
                }

                // Loop back
                //   1. false branch jumps to the loop merge
                //   2. true branch goes into the loop body for next iteration
                {
                    Node::add_control(&mut loop_back, Nref::clone(&comp_check));
                }

                // linking all the false branch back to the merge
                {
                    Node::add_control(
                        &mut pre_loop_false,
                        Nref::clone(&loop_merge),
                    );
                    Node::add_control(
                        &mut comp_check_false,
                        Nref::clone(&loop_merge),
                    );
                    Node::add_control(&mut loop_false, Nref::clone(&loop_merge));
                }
            }

            // linking the if_cmp's condition one by one
            {
                // pre loop check condition
                {
                    let zero =
                        self.mptr().borrow_mut().new_imm_i64(0, bc.clone());
                    let cond_ne = self.mptr().borrow_mut().new_i64_eq(
                        cnt,
                        zero,
                        bc.clone(),
                    );
                    let boxed_val = self.new_box_value(cond_ne, bc.clone());

                    Node::add_value(&mut pre_loop_check, boxed_val);
                }

                // loop body
                {
                    let lhs_char = self.mptr().borrow_mut().new_ptr_deref_u32(
                        Nref::clone(&lhs_addr),
                        Nref::clone(&iv),
                        bc.clone(),
                    );

                    let rhs_char = self.mptr().borrow_mut().new_ptr_deref_u32(
                        Nref::clone(&rhs_addr),
                        Nref::clone(&iv),
                        bc.clone(),
                    );

                    let char_diff = self.mptr().borrow_mut().new_u32_diff(
                        lhs_char,
                        rhs_char,
                        bc.clone());

                    let cond = self.mptr().borrow_mut().
                }
            }
        }
    }
*/
