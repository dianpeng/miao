use crate::miao::bytecode::*;
use crate::miao::lexer::*;
use std::cell::RefCell;
use std::rc::Rc;

// THE parser. We implement a recursive descent parser with directly yielding of
// bytecode instruction, some trick is been performed to allow one pass parasing
// possible.

#[derive(Clone)]
pub struct Perror {
    pub description: String,
    pub line: u32,
    pub column: u32,
}

#[derive(PartialEq, Eq)]
enum ScopeType {
    FuncStart,
    Basic,
    Loop,
    Branch,
}

enum Symbol {
    Local(u32),
    Upvalue(u32),
    Global(u32),
}

struct UpvalueEntry {
    name: String,
    index: u32,
    is_stack: bool,
}

// Okay, the BytecodeArray is hard to return since lexical scope is currently
// been held in a RefCell, and we cannot return yet another RefCell dependent on
// a temporary MutRef, so we need to return something that can be shared, ie Rc.
type BytecodeArrayPtr = Rc<RefCell<BytecodeArray>>;

struct LexicalScope {
    scope_type: ScopeType, // which type of scope
    local: Vec<String>,
    max_local: u32,
    stack_offset: u32,

    // loop related objects
    continue_list: Vec<Label>,
    break_list: Vec<Label>,
    return_list: Vec<Label>,

    upvalue: Option<Vec<UpvalueEntry>>, // available when scope_type is FuncStart

    // to work around rust's borrow rules, we need to place the whole Prototype
    // member directly here since we want the bytecode array of the Prototype to
    // be a RefCell since we need to return its mutable reference directly in
    // functions
    bc: Option<BytecodeArrayPtr>,
    arg_count: Option<u32>,
    hint: Option<String>,
    func_prolog: Option<Label>,
}

impl LexicalScope {
    fn new(t: ScopeType, ml: u32, off: u32) -> LexicalScope {
        let mut r = LexicalScope {
            scope_type: t,
            local: Vec::<String>::new(),
            max_local: ml,
            stack_offset: off,
            continue_list: Vec::<Label>::new(),
            break_list: Vec::<Label>::new(),
            return_list: Vec::<Label>::new(),
            upvalue: Option::None,
            bc: Option::None,
            arg_count: Option::None,
            hint: Option::None,
            func_prolog: Option::None,
        };

        if r.scope_type == ScopeType::FuncStart {
            r.upvalue = Option::Some(Vec::<UpvalueEntry>::new());

            let bc_ptr = Rc::new(RefCell::new(BytecodeArray::new()));
            r.func_prolog =
                Option::Some(bc_ptr.borrow_mut().label_d(Lpos::init()));

            // initialize the function related stuff
            r.bc = Option::Some(bc_ptr);
            r.arg_count = Option::Some(0);
            r.hint = Option::Some(String::new());
        }

        return r;
    }

    fn is_loop(&self) -> bool {
        return self.scope_type == ScopeType::Loop;
    }
    #[allow(dead_code)]
    fn is_basic(&self) -> bool {
        return self.scope_type == ScopeType::Basic;
    }
    fn is_func_start(&self) -> bool {
        return self.scope_type == ScopeType::FuncStart;
    }
    #[allow(dead_code)]
    fn is_branch(&self) -> bool {
        return self.scope_type == ScopeType::Branch;
    }

    fn find_local(&self, name: &str) -> Option<u32> {
        match self.local.iter().position(|x| x == name) {
            Some(v) => return Option::Some(v as u32 + self.stack_offset),
            _ => return Option::None,
        };
    }

    fn find_upvalue(&self, name: &str) -> Option<u32> {
        match &self.upvalue {
            Some(v) => {
                match v.iter().position(|x| x.name == name) {
                    Some(vv) => return Option::Some(vv as u32),
                    _ => return Option::None,
                };
            }
            _ => return Option::None,
        };
    }

    fn finish_prototype(&mut self, p: &mut Prototype) {
        assert!(self.is_func_start());

        // NOTES(dpeng): the self.bc is invalid to be used anymore, and it
        // has been stolen and to be part of the P already. Use p.code instead

        // patch all return instructions
        for r in self.return_list.iter_mut() {
            // notes, when our function return, it should pops out all the local
            // and also the argument as well. the max_local only covers local
            // variable instead of the arguments, for return it needs to account
            // for the argument count as well here.
            let return_size = self.max_local + self.arg_count.unwrap();
            r.emit(&mut p.code, Bytecode::Return(return_size));
        }

        // emit prolog
        self.func_prolog
            .as_ref()
            .unwrap()
            .emit(&mut p.code, Bytecode::PushN(self.max_local));

        p.argument_count = self.arg_count.unwrap();
        p.hint = self.hint.as_ref().unwrap().to_string();

        for x in self.upvalue.as_ref().unwrap().iter() {
            p.upvalue.push(Upval {
                index: x.index,
                is_stack: x.is_stack,
            });
        }
    }

    fn define_upvalue(&mut self, upvalue_entry: UpvalueEntry) -> u32 {
        assert!(self.is_func_start());

        let mut idx: u32 = 0;
        for x in self.upvalue.as_ref().unwrap().iter() {
            if x.name == upvalue_entry.name {
                return idx;
            }
            idx += 1;
        }

        idx = self.upvalue.as_ref().unwrap().len() as u32;
        self.upvalue.as_mut().unwrap().push(upvalue_entry);
        return idx;
    }

    fn define_local_internal(
        &mut self,
        name: String,
        update: bool,
    ) -> Option<u32> {
        match self.local.iter().position(|x| *x == name) {
            Some(_) => return None,
            _ => (),
        };
        let idx = self.local.len() as u32;
        self.local.push(name);
        if update {
            self.max_local += 1;
        }
        return Some(idx + self.stack_offset);
    }
    fn define_local(&mut self, name: String) -> Option<u32> {
        self.define_local_internal(name, true)
    }
    fn define_arg(&mut self, name: String) -> Option<u32> {
        self.define_local_internal(name, false)
    }
    fn local_len(&self) -> u32 {
        return self.local.len() as u32;
    }
    fn get_stack_offset(&self) -> u32 {
        return self.local_len() + self.stack_offset;
    }
}

type ScopeList = Vec<RefCell<LexicalScope>>;

struct Parser {
    l: Lexer,
    token: Token, // current token, if error, this token points to Invalid
    dbg: Lpos,    // current token's position, used for debugging purpose
    err: Option<Perror>,
    scope_list: ScopeList,

    // some fields mostly used to workaround with rust's strict type system
    cur_bc: Option<BytecodeArrayPtr>,
}

impl Parser {
    pub fn new(source: &str) -> Parser {
        Parser {
            l: Lexer::new(source),
            token: Token::Invalid,
            dbg: Lpos::nil(),
            err: Option::None,
            scope_list: ScopeList::new(),
            cur_bc: Option::None,
        }
    }

    // entry function, to start to parse the specified input. If parsing succeeded,
    // then the function will return a top level most Prototype object, otherwise
    // it will return an Perror to indicate the error.
    pub fn parse(&mut self, comment: String) -> Result<Prototype, Perror> {
        self.enter_func_scope("[top-level]".to_string(), Vec::<String>::new());

        // parsing the global statement until we are done
        if !self.parse_global() {
            return Result::Err(self.err.as_ref().unwrap().clone());
        }

        // populate the bytecodes
        let bytecode = self.current_func_start().bc.as_ref().unwrap().take();
        let mut p = Prototype::new_top_level(bytecode, comment);

        self.current().finish_prototype(&mut p);

        self.leave();
        return Result::Ok(p);
    }

    fn has_error(&self) -> bool {
        match self.err {
            None => false,
            Some(_) => true,
        }
    }

    // =========================================================================
    // Symbols
    // =========================================================================
    // define local symbol, ie variable starts with let
    fn define_local(&mut self, n: String) -> Option<u32> {
        return self.current().define_local(n);
    }

    fn collapse_upvalue(
        &mut self,
        where_to_start: usize,
        idx: u32,
        n: &str,
    ) -> u32 {
        let end = self.scope_list.len();
        let mut prev_index = idx;
        let mut is_stack = true;
        let mut wts = where_to_start;

        // notes, where_to_start points to the scope's index (not reverse order),
        // where it has a value as local variable, and the inner most function
        // scope needs to collapse this local variable as upvalue in frames
        // that exists in between.
        while wts != end {
            // this loop is to find out the next funcion start scope from the
            // current position
            loop {
                wts += 1;
                if wts == end {
                    assert!(!is_stack);
                    return prev_index;
                }

                if self.scope_list[wts].borrow().is_func_start() {
                    break;
                }
            }

            // now let's just collapse the value into this function's upvalue
            // scopes
            prev_index = self.scope_list[wts].borrow_mut().define_upvalue(
                UpvalueEntry {
                    name: n.to_string(),
                    index: prev_index,
                    is_stack: is_stack,
                },
            );

            is_stack = false;
        }

        assert!(is_stack);
        return prev_index;
    }

    fn resolve_symbol(&mut self, n: &str) -> Symbol {
        // (0) searching the scope from the current scopes backwards, until we
        //     hit nothing or we hit the inner most scope which is a function
        //     scope. If we find it in the current range, then it must be a
        //     local symbol
        let mut last_func_start: usize = self.scope_list.len();

        for scope in self.scope_list.iter().rev() {
            last_func_start -= 1;

            // 1) try to get it from local table
            match scope.borrow().find_local(n) {
                Some(v) => return Symbol::Local(v),
                _ => (),
            };

            // 2) try to get it from upvalue table
            match scope.borrow().find_upvalue(n) {
                Some(v) => return Symbol::Upvalue(v),
                _ => (),
            };

            if scope.borrow().is_func_start() {
                break;
            }
        }

        // (1) now searching continuously from the higher level of function
        //     closure scope. For this we will need to mark it as upvalue
        //     and return the index of upvalue. Notes finding_index + 1 points
        //     the scope index where it has a local variable can be lifted as
        //     upvalue, this is to avoid usize substraction overflow
        let mut finding_index: usize = self.scope_list.len();
        let mut embedded_local_index: Option<u32> = Option::None;

        if last_func_start != 0 {
            // skip the previous scope
            last_func_start -= 1;

            for scope in self.scope_list.iter().rev() {
                if (finding_index - 1) <= last_func_start {
                    // try to find it as local variable
                    match scope.borrow().find_local(n) {
                        Some(v) => {
                            // collapse the upvalue from the scope positioned at
                            // finding_index until the top scope, and insert that
                            // piece of value into the scope's upvalue table
                            embedded_local_index = Option::Some(v);
                            break;
                        }
                        _ => (),
                    };
                }

                finding_index -= 1;
            }
        }

        match embedded_local_index {
            Some(v) => {
                return Symbol::Upvalue(self.collapse_upvalue(
                    finding_index - 1,
                    v,
                    n,
                ));
            }
            _ => (),
        };

        // (2) try to get it from global scope since it must be a global index
        //     or we end up with a runtime error
        let name_index = self.bc().string_index(n.to_string());
        return Symbol::Global(name_index);
    }

    // =========================================================================
    // Lexical Scope
    // =========================================================================
    fn current(&self) -> std::cell::RefMut<LexicalScope> {
        assert!(self.scope_list.len() != 0);
        let idx = self.scope_list.len() - 1;
        return self.scope_list[idx].borrow_mut();
    }

    fn current_func_start(&self) -> std::cell::RefMut<LexicalScope> {
        let len = self.scope_list.len() as u32;
        let mut idx = len;
        for x in self.scope_list.iter().rev() {
            idx -= 1;
            if x.borrow().is_func_start() {
                break;
            }
        }

        assert!(idx < len);
        return self.scope_list[idx as usize].borrow_mut();
    }

    fn current_loop(&self) -> std::cell::RefMut<LexicalScope> {
        let len = self.scope_list.len() as u32;
        let mut idx = len;
        for x in self.scope_list.iter().rev() {
            idx -= 1;
            if x.borrow().is_loop() {
                break;
            }
        }

        assert!(idx < len);
        return self.scope_list[idx as usize].borrow_mut();
    }

    // check whether the current scope allows loop control statement, ie break
    // continue statement or not
    fn allow_loop_control(&self) -> bool {
        for x in self.scope_list.iter().rev() {
            if x.borrow().is_func_start() {
                break;
            }
            if x.borrow().is_loop() {
                return true;
            }
        }
        return false;
    }

    fn set_cur_bc(&mut self) {
        if self.current().is_func_start() {
            let rc = Rc::clone(self.current().bc.as_ref().unwrap());
            self.cur_bc = Option::Some(rc);
        }
    }

    fn restore_cur_bc(&mut self) {
        let rc = Rc::clone(self.current_func_start().bc.as_ref().unwrap());
        self.cur_bc = Option::Some(rc);
    }

    fn enter_func_scope(&mut self, hint: String, arg_list: Vec<String>) {
        let mut scope = LexicalScope::new(ScopeType::FuncStart, 0, 0);
        scope.hint = Option::Some(hint);
        scope.arg_count = Option::Some(arg_list.len() as u32);
        for x in arg_list.iter() {
            scope.define_arg(x.to_string());
        }
        self.scope_list.push(RefCell::new(scope));
        self.set_cur_bc();
    }

    fn enter_loop_scope(&mut self) {
        let ml = self.current().max_local;
        let off = self.current().get_stack_offset();
        self.scope_list.push(RefCell::new(LexicalScope::new(
            ScopeType::Loop,
            ml,
            off,
        )));
    }

    fn enter_branch_scope(&mut self) {
        let ml = self.current().max_local;
        let off = self.current().get_stack_offset();
        self.scope_list.push(RefCell::new(LexicalScope::new(
            ScopeType::Branch,
            ml,
            off,
        )));
    }

    fn enter_basic_scope(&mut self) {
        let ml = self.current().max_local;
        let off = self.current().get_stack_offset();
        self.scope_list.push(RefCell::new(LexicalScope::new(
            ScopeType::Basic,
            ml,
            off,
        )));
    }

    fn leave(&mut self) {
        let is_func = self.current().is_func_start();
        let max_local = self.current().max_local;
        self.scope_list.pop();

        // the max local update doesn't work across function boundary
        if !is_func {
            let mut x = self.current();
            if x.max_local < max_local {
                x.max_local = max_local;
            }
        }

        if is_func && self.scope_list.len() != 0 {
            self.restore_cur_bc();
        }
    }

    fn emit_return(&mut self, l: Label) {
        self.current_func_start().return_list.push(l);
    }

    fn bc(&self) -> std::cell::RefMut<BytecodeArray> {
        // FIXME(dpeng): This function needs to be optimized little bit to avoid
        // the loop. For now it is okay, and we can just keep a reference to the
        // pushed scope but issue is the bytecode array is in the prototype and
        // the prototype may not be existed and the borrow checker will complain
        return self.cur_bc.as_ref().unwrap().borrow_mut();
    }

    // helper for jumping code
    fn emit_jump_false_here(&self, l: &mut Label) {
        let mut x = self.bc();
        l.jump_false_here(&mut *x);
    }
    fn emit_jump_here(&self, l: &mut Label) {
        let mut x = self.bc();
        l.jump_here(&mut *x);
    }
    fn emit_loop_jump_here(&self, l: &mut Label) {
        let mut x = self.bc();
        l.loop_jump_here(&mut *x);
    }
    fn emit_and_here(&self, l: &mut Label) {
        let mut x = self.bc();
        l.and_here(&mut *x);
    }
    fn emit_or_here(&self, l: &mut Label) {
        let mut x = self.bc();
        l.or_here(&mut *x);
    }
    fn emit_ternary_here(&self, l: &mut Label) {
        let mut x = self.bc();
        l.ternary_here(&mut *x);
    }

    // emit helpers
    fn emit_int(&mut self, v: i64) {
        let idx = self.bc().int_index(v);
        self.bc().emit_d(Bytecode::LoadInt(idx), self.dbg());
    }

    fn emit_real(&mut self, v: f64) {
        let idx = self.bc().real_index(v);
        self.bc().emit_d(Bytecode::LoadReal(idx), self.dbg());
    }

    fn emit_str(&mut self, v: String) {
        let idx = self.bc().string_index(v);
        self.bc().emit_d(Bytecode::LoadString(idx), self.dbg());
    }

    // loop control related helper functions
    //
    // patch the continue list. make sure the current active scope is loop scope
    // and it will just patch all the pending jumps in continue_list to jump here
    fn patch_continue_list(&mut self) {
        assert!(self.current().is_loop());
        for x in self.current().continue_list.iter_mut() {
            self.emit_jump_here(x);
        }
    }

    fn patch_break_list(&mut self) {
        assert!(self.current().is_loop());
        for x in self.current().break_list.iter_mut() {
            self.emit_jump_here(x);
        }
    }

    // =========================================================================
    // Error
    // =========================================================================
    fn set_error(&mut self, msg: String) -> bool {
        let (col, line) = self.l.get_pos();
        let err = Perror {
            description: msg,
            line: line as u32,
            column: col as u32,
        };
        self.err = Option::Some(err);
        return false;
    }

    fn set_err(&mut self, msg: &str) -> bool {
        return self.set_error(msg.to_string());
    }
    fn err(&mut self, e: Lerror) -> bool {
        return self.set_error(format!("scanner error: {}", e.description));
    }
    fn grammar_error(&mut self, msg: &str) -> bool {
        return self.set_err(msg);
    }
    fn grammar_error_string(&mut self, msg: String) -> bool {
        return self.set_error(msg);
    }

    // =========================================================================
    // Lexer helper
    // =========================================================================
    fn next(&mut self) -> Token {
        match self.l.next() {
            Ok(v) => {
                self.token = v;
            }
            Err(e) => {
                self.err(e);
                self.token = Token::Invalid;
            }
        };

        self.dbg = self.l.get_lpos();
        return self.token.clone();
    }

    // generate debug position for later reference
    fn dbg(&self) -> Lpos {
        self.dbg.clone()
    }

    fn expect_next(&mut self, tk: Token) -> bool {
        if std::mem::discriminant(&self.next()) != std::mem::discriminant(&tk) {
            self.grammar_error_string(format!(
                "expect token {:?}, but got {:?}",
                tk, self.token
            ));
            return false;
        }
        return true;
    }

    fn expect_current(&mut self, tk: Token) -> bool {
        if std::mem::discriminant(&self.token) != std::mem::discriminant(&tk) {
            self.grammar_error_string(format!(
                "expect token {:?}, but got {:?}",
                tk, self.token
            ));
            return false;
        }
        return true;
    }

    // =========================================================================
    // Parser Subroutine
    // =========================================================================

    // global := global_statement*
    fn parse_global(&mut self) -> bool {
        self.next();

        while !self.has_error() && !self.l.eof() {
            match &self.token {
                Token::Func => {
                    if !self.parse_global_function_define_statement() {
                        return false;
                    }
                }
                _ => {
                    if !self.parse_statement() {
                        return false;
                    }
                }
            }
        }

        if self.has_error() {
            return false;
        }

        // lastly always, generate a return for the global code. the script do
        // allow user to return something in global scope.
        self.bc().emit_d(Bytecode::LoadNull, self.dbg());
        self.bc().emit_d(Bytecode::Halt, self.dbg());
        return true;
    }

    fn parse_global_function_define_statement(&mut self) -> bool {
        if !self.expect_next(Token::Id("".to_string())) {
            return false;
        }

        // used for prototype debugging purpose
        let hint: String;

        let global_index = match &self.token {
            Token::Id(v) => {
                hint = format!("global => {}", v);
                self.bc().string_index(v.to_string())
            }
            _ => {
                assert!(false);
                return false;
            }
        };
        self.next();

        let prototype_index = match self.parse_prototype(hint) {
            Some(v) => v,
            _ => {
                return false;
            }
        };

        // allow user optionally have a semicolon
        match &self.token {
            Token::Semicolon => {
                self.next();
            }
            _ => (),
        };

        self.bc()
            .emit_d(Bytecode::LoadFunction(prototype_index), self.dbg());

        self.bc()
            .emit_d(Bytecode::SetGlobal(global_index), self.dbg());

        return true;
    }

    // parse a prototype into a prototype object been stored in the current
    // prototype's prototype table, the current token can be anything. The
    // function is supposed to check whether the current one starts with '('
    fn parse_prototype(&mut self, hint: String) -> Option<Index> {
        match &self.token {
            Token::LPar => (),
            _ => {
                self.grammar_error("expect '(' for a function definition");
                return Option::None;
            }
        };

        // (0) argument list parsing and code generation parts
        let mut arg_list = Vec::<String>::new();
        loop {
            self.next();
            match &self.token {
                Token::RPar => {
                    self.next();
                    break;
                }
                Token::Comma => {
                    continue;
                }
                Token::Id(id) => {
                    match arg_list.iter().position(|x| *x == *id) {
                        Some(_) => {
                            self.grammar_error("duplicate argument");
                            return Option::None;
                        }
                        _ => (),
                    };
                    arg_list.push(id.to_string());
                }
                _ => {
                    self.grammar_error("expect identifier as argument");
                    return Option::None;
                }
            };
        }

        let proto;

        // (1) Prototype body parsing
        {
            self.enter_func_scope(hint, arg_list);

            if !self.parse_block_statement() {
                return Option::None;
            }

            // always add a return null at the end of the function, otherwise
            // the function will not return if user doesn't write return
            // explicitly
            self.bc().emit_d(Bytecode::LoadNull, self.dbg());
            {
                let l = self.bc().label_d(self.dbg());
                self.emit_return(l);
            }

            let bytecode =
                self.current_func_start().bc.as_ref().unwrap().take();

            let mut current_p = Prototype::new(bytecode);

            // body part is done, so set up the upvalue array in the protocol
            self.current().finish_prototype(&mut current_p);

            // lastly register the current prototype to be ours
            assert!(self.current().is_func_start());

            self.leave();

            proto = current_p;
        }

        return Option::Some(self.bc().proto_index(proto));
    }

    // parse block statement contains a statement list
    fn parse_block_statement(&mut self) -> bool {
        match &self.token {
            Token::LBra => (),
            _ => {
                self.grammar_error("expect a '{' to start block of statement");
                return false;
            }
        };
        self.next();

        loop {
            match &self.token {
                Token::RBra => {
                    self.next();
                    break;
                }
                _ => {
                    if !self.parse_statement() {
                        return false;
                    }
                }
            };
        }

        return true;
    }

    fn parse_statement(&mut self) -> bool {
        match self.token.clone() {
            Token::LBra => {
                self.enter_basic_scope();
                if !self.parse_block_statement() {
                    return false;
                }
                self.leave();
            }
            Token::Let => {
                if !self.parse_local_define_statement() {
                    return false;
                }
            }
            Token::Id(id) => {
                if !self.parse_id_prefix_statement(id) {
                    return false;
                }
            }
            Token::If => {
                if !self.parse_if_statement() {
                    return false;
                }
            }
            Token::For => {
                if !self.parse_for_statement() {
                    return false;
                }
            }
            Token::Return => {
                if !self.parse_return_statement() {
                    return false;
                }
            }
            Token::Continue => {
                if !self.parse_continue_statement() {
                    return false;
                }
            }
            Token::Break => {
                if !self.parse_break_statement() {
                    return false;
                }
            }

            // builtin statement
            Token::Assert => {
                if !self.parse_assert_statement() {
                    return false;
                }
            }
            Token::Halt => {
                if !self.parse_halt_statement() {
                    return false;
                }
            }
            Token::Trace => {
                if !self.parse_trace_statement() {
                    return false;
                }
            }
            _ => {
                // treat rest of statement as expression and generate them
                // then generate a pop.
                if !self.parse_expression() {
                    return false;
                }
                if !self.expect_current(Token::Semicolon) {
                    return false;
                }
                self.next();
                self.bc().emit_d(Bytecode::Pop, self.dbg());
            }
        };

        return true;
    }

    // Notes, parse_block do not enter any scopes
    fn parse_block(&mut self) -> bool {
        match &self.token {
            Token::LBra => {
                return self.parse_block_statement();
            }
            _ => (),
        };

        if !self.parse_statement() {
            return false;
        }
        return true;
    }

    fn parse_local_define_statement(&mut self) -> bool {
        if !self.expect_next(Token::Id("".to_string())) {
            return false;
        }

        let id = match &self.token {
            Token::Id(v) => v.to_string(),
            _ => {
                self.grammar_error(
                    "unexpected token, expect identifier to be part of local \
                        definition statement",
                );
                return false;
            }
        };

        // allocate a stack position to store our local variables
        let stack_pos = match self.define_local(id) {
            Some(v) => v,
            _ => {
                self.grammar_error("duplicate local variable define");
                return false;
            }
        };

        match self.next() {
            Token::Assign => {
                self.next();
                if !self.parse_expression() {
                    return false;
                }
                self.bc().emit_d(Bytecode::Store(stack_pos), self.dbg());
            }
            _ => {
                self.bc().emit_d(Bytecode::LoadNull, self.dbg());
                self.bc().emit_d(Bytecode::Store(stack_pos), self.dbg());
            }
        };

        // lastly check the ';' after each statement
        if !self.expect_current(Token::Semicolon) {
            return false;
        }
        self.next();

        return true;
    }

    // help to generate corresponding symbol. For a symbol, we have following
    // ways to introduce it into our prototype
    //
    // 1) let xxx = expression; ==> introduce the xxx as local variable
    // 2) xxx = expression; ==> if xxx is found as an upvalue, then introuce it
    //                          into scope's upvalue
    // 3) lastly, the xxx will be introduced as global variable
    fn load_symbol(&mut self, n: String) -> bool {
        match self.resolve_symbol(&n) {
            Symbol::Local(index) => {
                self.bc().emit_d(Bytecode::Load(index), self.dbg());
            }
            Symbol::Global(index) => {
                self.bc().emit_d(Bytecode::LoadGlobal(index), self.dbg());
            }
            Symbol::Upvalue(index) => {
                self.bc().emit_d(Bytecode::LoadUpvalue(index), self.dbg());
            }
        };
        return true;
    }

    fn parse_suffix_expression(&mut self) -> bool {
        loop {
            match &self.token {
                Token::LSqr => {
                    self.next();
                    if !self.parse_expression() {
                        return false;
                    }
                    if !self.expect_current(Token::RSqr) {
                        return false;
                    }
                    self.next();
                    self.bc().emit_d(Bytecode::ArrayIndex, self.dbg());
                }
                Token::Dot => {
                    match self.next() {
                        Token::Id(id) => {
                            let idx = self.bc().string_index(id);
                            self.bc()
                                .emit_d(Bytecode::DotAccess(idx), self.dbg());
                            self.next();
                        }
                        _ => break,
                    };
                }
                Token::LPar => {
                    // function in our language, so the load_symbol already have
                    // the function been placed at the stack, so we need to do
                    // few things to invoke the call. The call takes an argument
                    // to indicate the argument # which allows the call to locate
                    // the target, on the stack
                    let mut arg_size: u32 = 0;
                    match self.next() {
                        Token::RPar => {
                            self.next();
                        }
                        _ => loop {
                            if !self.parse_expression() {
                                return false;
                            }
                            arg_size += 1;

                            match &self.token {
                                Token::Comma => {
                                    self.next();
                                }
                                Token::RPar => {
                                    self.next();
                                    break;
                                }
                                _ => {
                                    self.grammar_error(
                                        "expect ',' or ')' in function call \
                                        argument list ",
                                    );
                                    return false;
                                }
                            };
                        },
                    };

                    self.bc().emit_d(Bytecode::Call(arg_size), self.dbg());
                }
                _ => break,
            }
        }

        return true;
    }

    // notes, this function assume the Id(string) been consumed already, and it
    // will not try to move the Id token to the next.
    fn parse_id_prefix_start_id(&mut self, n: String) -> bool {
        if !self.load_symbol(n) {
            return false;
        }
        return self.parse_suffix_expression();
    }

    // this is the most ambiguious statement to be parsed and we are having a
    // one pass parser which we need to generate bytecode on the fly without
    // needing to construct any intermediate representation like AST. The
    // following situation needs to be identified :
    //
    //
    //   1) id prefix expression, ie no assignment involve
    //      a()
    //      a.b
    //      a["b"]
    //      a().b["c"]().d.e.f.g()
    //      etc ....
    //
    //      essentially . and []/() becomes operator on the expression until
    //      we hit an unknown one
    //
    //   2) assignment
    //
    //      simple variable assignment like:
    //
    //      a = 10
    //      b = 20
    //
    //      complicated variable assignment to assign component of compound
    //      data structure,
    //
    //      a[10] = 1000
    //      a[1][2][3][4][5] = 1000
    //      a.b.c.d.e[2].f = 2000
    //
    //      we need to generate special instruction to fullfill the assignment.
    //      Basically we will need to use IndexStore and DotStore, but before
    //      generating these instructions, we need to lookahead at least when
    //      we hit the '=' to decide whether it is a situation 1) or 2). So if
    //      we do our job like using parse_id_prefix_start_id then we definitly
    //      gonna end up with a DotAccess or ArrayIndex instruction already in
    //      the code buffer. Then we can do some trick here by removing the last
    //      instruction.

    fn parse_id_prefix_statement(&mut self, name: String) -> bool {
        self.next();

        // (0) we start to do the job by using parse_id_prefix_start_id(...)
        if !self.parse_id_prefix_start_id(name) {
            return false;
        }

        // (1) check the current token, if the current token is an assign,
        //     it means either way it is a id assignment or complicated assign,
        //     and we will have to rework the instruction been generated here.
        match self.token.clone() {
            tk @ (Token::Assign |
                  // aggregate assignment
                  Token::AssignSub  |
                  Token::AssignAdd  |
                  Token::AssignMul  |
                  Token::AssignDiv  |
                  Token::AssignMod) => {

                // rewriting the already generated bytecode
                let last_bc = self.bc().pop_last();

                // notes only [] or . prefixed experssion requires dup, other
                // doesn't have dup since they are one shot loading
                let mut should_dup_tos = false;
                let store_bytecode = match &last_bc {
                    // simple assignment
                    Bytecode::Load(x) => Bytecode::Store(*x),
                    Bytecode::LoadGlobal(x) => Bytecode::SetGlobal(*x),
                    Bytecode::LoadUpvalue(x) => Bytecode::SetUpvalue(*x),
                    // complicated assignment, ie ending up with DotAccess or
                    // ArrayIndex
                    Bytecode::DotAccess(x) => {
                        should_dup_tos = true;
                        Bytecode::DotStore(*x)
                    }
                    Bytecode::ArrayIndex => {
                        should_dup_tos = true;
                        Bytecode::ArrayStore
                    }
                    _ => {
                        // other statement simply cannot follow a expression
                        // to be assigned
                        self.grammar_error("the left hand side expression \
                                           cannot be assigned to");
                        return false;
                    }
                };
                self.next();

                // Handle the assignment and aggregate assignment in 2 different
                // way
                //
                // 1) for assignment,
                //    this is trivial, just parsing the expression on the rhs
                //    of the assignment statement and finally generate the
                //    store bytecode
                //
                // 2) for agg statement,
                //    2.1 we need to duplicate the lhs expression result on the
                //        stack, since we already remove the last instruction,
                //        we can 1) generate a dup to dup the TOS; 2) re-apply
                //        the instruction just been poped out, then at least
                //        on TOS it is the expression of lhs just been evaluated
                //
                //    2.2 and then parse the expression
                //
                //    2.3 generate the store

                if Lexer::is_agg_assign(&tk) {
                    // generate duplication
                    if should_dup_tos {
                        self.bc().emit_d(Bytecode::Dup, self.dbg());
                    }
                    self.bc().emit_d(last_bc, self.dbg());
                }

                // now parsing the assignment expression
                if !self.parse_expression() {
                    return false;
                }

                // tos needs to be combined by the operators
                match &tk {
                    Token::AssignAdd => self.bc().emit_d(Bytecode::Add, self.dbg()),
                    Token::AssignSub => self.bc().emit_d(Bytecode::Sub, self.dbg()),
                    Token::AssignMul => self.bc().emit_d(Bytecode::Mul, self.dbg()),
                    Token::AssignDiv => self.bc().emit_d(Bytecode::Div, self.dbg()),
                    Token::AssignMod => self.bc().emit_d(Bytecode::Mod, self.dbg()),
                    _ => (),
                };

                // lastly store the symbol based on the store bytecode
                self.bc().emit_d(store_bytecode, self.dbg());
            }

            _ => {
                // If the current token is not a assignment, so it is a expression
                // level statement, then we need to pop the evaluated result
                self.bc().emit_d(Bytecode::Pop, self.dbg());
            }
        };

        if !self.expect_current(Token::Semicolon) {
            return false;
        }
        self.next();
        return true;
    }

    fn parse_condition(&mut self) -> bool {
        if !self.parse_expression() {
            return false;
        }
        self.bc().emit_d(Bytecode::Boolean, self.dbg());
        return true;
    }

    // this function returns a pair of label, user needs to patch these 2
    // label later on.
    // 1) label1 -> points to the condition testify afterwards, ie needs to
    //              be patched to jump to next branch when condition fails
    // 2) label2 -> points to block after the branch's body, ie needs to jump
    //              to the end of branch tree
    fn parse_branch_block(
        &mut self,
        prev: Option<Label>,
    ) -> Option<(Label, Label)> {
        match prev {
            Some(mut v) => self.emit_jump_false_here(&mut v),
            _ => (),
        };

        if !self.parse_condition() {
            return Option::None;
        }
        // condition part, this condition may fail, so we need to jump to the
        // next elif/else chain if has one.
        let l1 = self.bc().label_d(self.dbg());

        // branch body compilation part
        {
            self.enter_branch_scope();
            if !self.parse_block() {
                return Option::None;
            }
            self.leave();
        }

        return Option::Some((l1, self.bc().label_d(self.dbg())));
    }

    fn parse_if_statement(&mut self) -> bool {
        self.next();
        let mut prev_label = Option::None;
        let mut final_patch = Vec::<Label>::new();

        // (0) first if block compilation
        match self.parse_branch_block(prev_label) {
            Some(v) => {
                prev_label = Option::Some(v.0);
                final_patch.push(v.1);
            }
            None => return false,
        };

        // (1) elif chain
        while self.token == Token::Elif {
            self.next();
            match self.parse_branch_block(prev_label) {
                Some(v) => {
                    prev_label = Option::Some(v.0);
                    final_patch.push(v.1);
                }
                None => return false,
            };
        }

        // (2) check the else
        match prev_label {
            Some(mut v) => self.emit_jump_false_here(&mut v),
            _ => (),
        };

        if self.token == Token::Else {
            self.next();
            self.enter_branch_scope();
            if !self.parse_block() {
                return false;
            }
            self.leave();
        }

        // finally patch all the jump block to merge into the current control
        // flow block
        for x in final_patch.iter_mut() {
            self.emit_jump_here(x);
        }

        return true;
    }

    // for statement comes into 2 flavors, all are been compiled into shape
    // like this :
    //
    //    if (condition) {
    // loop_start:
    //      loop_body
    //       ....
    //
    //       if (condition) goto loop_start
    //    }
    //
    //  In compiler theory, this is called loop rotation. The condition is based
    //  on certain abstract model, ie iterator. For object that supports iterating
    //  we allow user to return back a special object called iterator, and the
    //  runtime will just iterate object based on the returned iterator.
    //
    //  condition will just be iterator.start(); the last goto will become just
    //  iterator.has_next(); and then if the has_next is true we will generate
    //  code to invoke next() and put the value into the variable specified before
    //  the loop
    //
    //  For forever loop, we handle it in a special way

    fn parse_forever_loop(&mut self) -> bool {
        let loop_header_codepos = self.bc().code_pos();

        if !self.parse_block_statement() {
            return false;
        }

        self.patch_continue_list();
        self.bc()
            .emit_d(Bytecode::Jump(loop_header_codepos), self.dbg());
        self.patch_break_list();

        self.leave();
        return true;
    }

    fn parse_for_iterator_new(&mut self, id: String) -> Option<u32> {
        // we should never fail here, since the local is defined in the
        // current scope and the current scope is just newly allocated.
        let id_index = self.define_local(id).unwrap();

        if !self.expect_next(Token::In) {
            return Option::None;
        }
        self.next();

        if !self.parse_expression() {
            return Option::None;
        }

        // replace current expression into a iterator to support the
        // for in protocols
        self.bc().emit_d(Bytecode::IteratorNew, self.dbg());
        return Option::Some(id_index);
    }

    fn parse_for_statement(&mut self) -> bool {
        self.enter_loop_scope();

        // parsing optional for condition
        let lv_index = match self.next() {
            Token::Let => match self.next() {
                Token::Id(id) => match self.parse_for_iterator_new(id) {
                    Option::Some(v) => v,
                    Option::None => return false,
                },
                _ => {
                    self.grammar_error("unexpected token in loop/for.");
                    return false;
                }
            },
            Token::Id(id) => match self.parse_for_iterator_new(id) {
                Option::Some(v) => v,
                Option::None => return false,
            },
            Token::LBra => {
                return self.parse_forever_loop();
            }
            _ => {
                self.grammar_error("unexpected token in loop/for.");
                return false;
            }
        };

        // Except for forever loop, which is a specialized loop case, the
        // normal iteration loop will be compiled as following
        //
        //       ---------------------
        //       |   iterator_start  |
        //       |    loop_jump L:m  |
        //       ---------------------
        //                |
        //                |
        //                |
        //                |
        //               \-/
        //       L: body              <---- start of the loop body
        //       |------------------|
        //       |                  |
        //       |     loop body    |
        //       |                  |
        //       |------------------|
        //                |
        //                |
        //                |
        //               \-/
        //       |------------------|
        //       |  iterator_next   |
        //       |    loop_jump L:m |
        //       |    jump L:body   |
        //       |  iterator_next   |
        //       |    refresh iv    |
        //       |                  |
        //       | L:m              |
        //       |  new basic block |
        //       |------------------|
        //
        //      iv = induction variable
        //
        // perform a loop rotation, since we issue a sort of like annotated
        // bytecode to mark the starting point of the loop for future JIT
        // compilation, and we avoid this cost for rest of the iteration of
        // loop by performing loop rotation.

        // the first iteration's loop condition testing code. Notes iterator
        // start is same as iterator next, and it is only used for annotation
        // of the loop rotation's header, or loop's real header
        self.bc().emit_d(Bytecode::IteratorStart, self.dbg());
        let mut loop_rotation_label = self.bc().label_d(self.dbg());

        // setup the loop induction variable
        // notes this part of the code has been duplicated from the loop header
        // to loop tail, since we do rotate the loop.
        self.bc().emit_d(Bytecode::IteratorValue, self.dbg());
        self.bc().emit_d(Bytecode::Store(lv_index), self.dbg());

        // The rest of the body part, except for the first iteration, rest
        // iteration will perform testing at the tail of the loop and then
        // jump back to this position to run the loop body. Notes, we purposely
        // omit the code for refreshing the loop induction variable since the
        // refreshing has been duplicated inside of the loop tail
        let loop_header_codepos = self.bc().code_pos();

        // Then we enter into the loop body compilation here.
        if !self.parse_block() {
            return false;
        }

        // Do the loop condition check and jump back if needed
        // the loop tail is been compiled as following :
        // ----------------------------------------------------------------------
        //  1) iterator_has  ;; get the iterator's next method call and check
        //                   ;; whether we can continue
        //
        //  2) loop_jump     ;; directly jump out of the tail part if we cannot
        //                   ;; continue;
        //
        //  3) iterator_next ;; get the iterator's next value
        //
        //  4) mov @tos, iv_index ;; refreshing index's value
        //
        //  5) jump 1)       ;; jump back to the label 1)
        // ----------------------------------------------------------------------

        // patching the continue list to be here, since they should just jump to
        // the loop tail to testify the next iteration's validity
        self.patch_continue_list();

        // update the iterator and return whether the current position is valid
        // or not, if it is not return false; otherwise true
        self.bc().emit_d(Bytecode::IteratorNext, self.dbg());

        // jump out
        let mut loop_tail_label = self.bc().label_d(self.dbg());

        // get the next value of iterator
        self.bc().emit_d(Bytecode::IteratorValue, self.dbg());

        // move the value to the lv_index
        self.bc().emit_d(Bytecode::Store(lv_index), self.dbg());

        // jump to loop header for next iteration
        self.bc()
            .emit_d(Bytecode::Jump(loop_header_codepos), self.dbg());

        // Okay, if we reach here it means the real end of the loop, ie the SSA
        // format's merge node after the loop. Now, we just join all the out of
        // body jump to this position
        self.emit_loop_jump_here(&mut loop_rotation_label);
        self.emit_loop_jump_here(&mut loop_tail_label);

        // Since we are in the loop body, we need to patch all the pending loop
        // control operations, ie continue, break etc ...
        self.patch_break_list();

        // Lastly, we have a iterator on TOS, just pops it we dont need it
        self.bc().emit_d(Bytecode::Pop, self.dbg());

        // Leave the current scope
        self.leave();

        return true;
    }

    fn parse_return_statement(&mut self) -> bool {
        match self.next() {
            Token::Semicolon => {
                self.bc().emit_d(Bytecode::LoadNull, self.dbg());
            }
            _ => {
                if !self.parse_expression() {
                    return false;
                }
                if !self.expect_current(Token::Semicolon) {
                    return false;
                }
                self.next();
            }
        };

        let l = self.bc().label_d(self.dbg());
        self.emit_return(l);
        return true;
    }

    fn parse_continue_statement(&mut self) -> bool {
        if !self.allow_loop_control() {
            return self.grammar_error(
                "invalid loop control statement, continue/break must be in a \
                    loop or loop derived scope",
            );
        }
        let l = self.bc().label_d(self.dbg());
        self.current_loop().continue_list.push(l);
        if !self.expect_next(Token::Semicolon) {
            return false;
        }
        self.next();
        return true;
    }

    fn parse_break_statement(&mut self) -> bool {
        if !self.allow_loop_control() {
            return self.grammar_error(
                "invalid loop control statement, continue/break must be in a \
                    loop or loop derived scope",
            );
        }
        let l = self.bc().label_d(self.dbg());
        self.current_loop().break_list.push(l);
        if !self.expect_next(Token::Semicolon) {
            return false;
        }
        self.next();
        return true;
    }

    // builtin statement, function as keyword
    fn parse_assert_statement(&mut self) -> bool {
        // assert allows 2 forms:
        //   1) assert expression;
        //   2) assert expression, expression(msg);
        self.next();
        if !self.parse_expression() {
            return false;
        }

        match &self.token {
            Token::Comma => {
                self.next();
                if !self.parse_expression() {
                    return false;
                }
                self.bc().emit_d(Bytecode::Assert2, self.dbg());
                if !self.expect_current(Token::Semicolon) {
                    return false;
                }
                self.next();
            }

            Token::Semicolon => {
                self.bc().emit_d(Bytecode::Assert1, self.dbg());
                self.next();
            }

            _ => {
                self.grammar_error("unexpected assert grammar");
                return false;
            }
        };

        return true;
    }

    fn parse_halt_statement(&mut self) -> bool {
        self.next();
        if !self.parse_expression() {
            return false;
        }
        self.bc().emit_d(Bytecode::Halt, self.dbg());
        if !self.expect_current(Token::Semicolon) {
            return false;
        }
        self.next();
        return true;
    }

    fn parse_trace_statement(&mut self) -> bool {
        self.next();

        // parse a comma separated expression list until we hit semicolon
        let mut count = 0;
        loop {
            if !self.parse_expression() {
                return false;
            }
            count += 1;

            match &self.token {
                Token::Comma => {
                    self.next();
                }
                Token::Semicolon => {
                    self.next();
                    break;
                }
                _ => {
                    self.grammar_error(
                        "unexpected trace grammar, expect a ',' or ';'",
                    );
                    return false;
                }
            }
        }

        if count == 0 {
            self.grammar_error(
                "trace statement expects at least one expression",
            );
            return false;
        }

        self.bc().emit_d(Bytecode::Trace(count), self.dbg());
        return true;
    }

    // ------------------------------------------------------------------------
    // Expression parsing -----------------------------------------------------
    // ------------------------------------------------------------------------
    fn parse_expression(&mut self) -> bool {
        return self.parse_ternary();
    }

    fn parse_ternary(&mut self) -> bool {
        if !self.parse_logic() {
            return false;
        }

        // checking whether we have ternary requirements or not
        match &self.token {
            Token::Question => {
                let mut ternary = self.bc().label_d(self.dbg());

                self.next();
                if !self.parse_expression() {
                    return false;
                }
                let mut jump_out = self.bc().label_d(self.dbg());

                if !self.expect_current(Token::Colon) {
                    return false;
                }
                self.next();

                self.emit_ternary_here(&mut ternary);
                if !self.parse_expression() {
                    return false;
                }
                self.emit_jump_here(&mut jump_out);
            }
            _ => return true,
        };

        return true;
    }

    // logic parsing should confine the circular break logic, so essentially
    // these logic expression will be turned into a small control flow blocks
    // like if-elif-else chain
    fn parse_logic(&mut self) -> bool {
        if !self.parse_equality() {
            return false;
        }
        let mut or_list = Vec::<Label>::new(); // used by OR to short cut
        let mut and_list = Vec::<Label>::new(); // used by AND to short cut

        loop {
            let bytecode = match self.token {
                Token::Or => {
                    or_list.push(self.bc().label_d(self.dbg()));
                    Bytecode::Boolean
                }
                Token::And => {
                    and_list.push(self.bc().label_d(self.dbg()));
                    Bytecode::Boolean
                }
                _ => break,
            };

            self.next();

            if !self.parse_equality() {
                return false;
            }

            self.bc().emit_d(bytecode, self.dbg());
        }

        for x in or_list.iter_mut() {
            self.emit_or_here(x);
        }

        for x in and_list.iter_mut() {
            self.emit_and_here(x);
        }

        return true;
    }

    // simple abstraction
    //
    // notes, to avoid shit in rust, we just use very simple integer to denote
    const EQUALITY: u32 = 0;
    const COMPARISON: u32 = 1;
    const TERM: u32 = 2;
    const FACTOR: u32 = 3;
    const POWER: u32 = 4;

    fn match_binary_token(&mut self, t: u32) -> Option<Bytecode> {
        return if t == Parser::COMPARISON {
            match &self.token {
                Token::Gt => Option::Some(Bytecode::Gt),
                Token::Ge => Option::Some(Bytecode::Ge),
                Token::Lt => Option::Some(Bytecode::Lt),
                Token::Le => Option::Some(Bytecode::Le),
                _ => Option::None,
            }
        } else if t == Parser::TERM {
            match &self.token {
                Token::Add => Option::Some(Bytecode::Add),
                Token::Sub => Option::Some(Bytecode::Sub),
                _ => Option::None,
            }
        } else if t == Parser::FACTOR {
            match &self.token {
                Token::Mul => Option::Some(Bytecode::Mul),
                Token::Div => Option::Some(Bytecode::Div),
                Token::Mod => Option::Some(Bytecode::Mod),
                _ => Option::None,
            }
        } else if t == Parser::POWER {
            match &self.token {
                Token::Pow => Option::Some(Bytecode::Pow),
                _ => Option::None,
            }
        } else if t == Parser::EQUALITY {
            match &self.token {
                Token::Eq => Option::Some(Bytecode::Eq),
                Token::Ne => Option::Some(Bytecode::Ne),
                _ => Option::None,
            }
        } else {
            self.grammar_error("unexpected binary operator here");
            return Option::None;
        };
    }

    fn parse_binary(&mut self, next: fn(&mut Parser) -> bool, t: u32) -> bool {
        if !next(self) {
            return false;
        }
        loop {
            let bytecode = match self.match_binary_token(t) {
                Some(v) => v,
                _ => break,
            };
            self.next();

            if !next(self) {
                return false;
            }
            self.bc().emit_d(bytecode, self.dbg());
        }
        return !self.has_error();
    }

    // Euqality/Comparison/Factor/Power/Term
    fn parse_equality(&mut self) -> bool {
        return self.parse_binary(Parser::parse_comparison, Parser::EQUALITY);
    }
    fn parse_comparison(&mut self) -> bool {
        return self.parse_binary(Parser::parse_term, Parser::COMPARISON);
    }
    fn parse_term(&mut self) -> bool {
        return self.parse_binary(Parser::parse_factor, Parser::TERM);
    }
    fn parse_factor(&mut self) -> bool {
        return self.parse_binary(Parser::parse_power, Parser::FACTOR);
    }
    fn parse_power(&mut self) -> bool {
        return self.parse_binary(Parser::parse_unary, Parser::POWER);
    }

    // unary
    fn parse_unary(&mut self) -> bool {
        let mut bc = Option::<Bytecode>::None;

        match &self.token {
            Token::Sub => {
                bc = Option::Some(Bytecode::Neg);
                self.next();
            }
            Token::Not => {
                bc = Option::Some(Bytecode::Not);
                self.next();
            }
            Token::Typeof => {
                bc = Option::Some(Bytecode::Typeof);
                self.next();
            }
            Token::Sizeof => {
                bc = Option::Some(Bytecode::Sizeof);
                self.next();
            }
            _ => (),
        };

        if !self.parse_primary() {
            return false;
        }

        match bc {
            Some(v) => {
                self.bc().emit_d(v, self.dbg());
            }
            _ => (),
        }
        return true;
    }

    fn parse_primary(&mut self) -> bool {
        match &self.token {
            Token::Id(n) => {
                let x = n.to_string();
                self.next();
                return self.parse_id_prefix_start_id(x);
            }
            _ => {
                if !self.parse_atomic() {
                    return false;
                }
                if !self.parse_suffix_expression() {
                    return false;
                }
                return true;
            }
        };
    }

    // parsing string template, we just doing concatenation on the value
    // stack
    fn parse_string_template(&mut self) -> bool {
        let mut count = 0;
        self.next();

        loop {
            match self.token.clone() {
                Token::StrTempEnd => {
                    self.next();
                    break;
                }

                Token::Text(data) => {
                    self.emit_str(data);
                    self.next();
                }

                Token::ExprStart => {
                    self.next();
                    if !self.parse_expression() {
                        return false;
                    }
                    if !self.expect_current(Token::RBra) {
                        return false;
                    }
                    self.l.template_finish_expr();
                    self.next();
                }
                _ => {
                    return self.grammar_error(&format!(
                        "unexpected token {:?} in string template",
                        self.token
                    ));
                }
            };

            count += 1;
        }

        if count == 0 {
            // no string on the stack, so we just emit a dummy string
            self.emit_str("".to_string());
        } else {
            // okay, emit ConStr to concate all counted string on the stack
            self.bc().emit_d(Bytecode::ConStr(count), self.dbg());
        }

        assert!(!self.l.lex_template());
        return true;
    }

    fn parse_atomic(&mut self) -> bool {
        match self.token.clone() {
            Token::Int(v) => {
                self.emit_int(v);
                self.next();
            }
            Token::Real(v) => {
                self.emit_real(v);
                self.next();
            }
            Token::True => {
                self.bc().emit_d(Bytecode::LoadTrue, self.dbg());
                self.next();
            }
            Token::False => {
                self.bc().emit_d(Bytecode::LoadFalse, self.dbg());
                self.next();
            }
            Token::Null => {
                self.bc().emit_d(Bytecode::LoadNull, self.dbg());
                self.next();
            }
            Token::Str(v) => {
                self.emit_str(v);
                self.next();
            }
            Token::StrTempStart => {
                return self.parse_string_template();
            }
            Token::LSqr => return self.parse_list(),
            Token::LBra => return self.parse_object(),
            Token::Func => return self.parse_closure(),
            Token::LPar => {
                self.next();
                if !self.parse_expression() {
                    return false;
                }
                if !self.expect_current(Token::RPar) {
                    return false;
                }
                self.next();
            }
            _ => {
                return self
                    .grammar_error(&format!("unknown token {:?}", self.token));
            }
        };

        return true;
    }

    fn parse_list(&mut self) -> bool {
        self.next();
        self.bc().emit_d(Bytecode::ListStart, self.dbg());
        let mut c = 0;

        while self.token != Token::RSqr {
            if !self.parse_expression() {
                return false;
            }
            c += 1;
            match &self.token {
                Token::Comma => self.next(),
                _ => break,
            };
        }
        self.bc().emit_d(Bytecode::ListAdd(c), self.dbg());

        self.next();
        return true;
    }

    fn parse_object(&mut self) -> bool {
        self.next();
        self.bc().emit_d(Bytecode::ObjectStart, self.dbg());
        let mut c = 0;

        while self.token != Token::RBra {
            if !self.parse_expression() {
                return false;
            }
            if !self.expect_current(Token::Colon) {
                return false;
            }
            self.next();
            if !self.parse_expression() {
                return false;
            }

            c += 1;
            match &self.token {
                Token::Comma => self.next(),
                _ => break,
            };
        }

        self.bc().emit_d(Bytecode::ObjectAdd(c), self.dbg());

        self.next();
        return true;
    }

    fn parse_closure(&mut self) -> bool {
        self.next();
        match self.parse_prototype("[closure]".to_string()) {
            Some(v) => self.bc().emit_d(Bytecode::LoadFunction(v), self.dbg()),
            _ => return false,
        };
        return true;
    }
}

pub fn do_parse(s: &str, comment: &str) -> Result<Prototype, Perror> {
    let mut p = Parser::new(s);
    return p.parse(comment.to_string());
}

// Notes, for simplicity, the testing of parser is been merged into the executor.
// this is easier for us since otherwise we have to manually write the bytecode
// to verify the parsing result which is not easy to maintain. Traditional way
// will be parsing the source into AST and write compact representation of AST
// for verification, commonly using S-expression to do this task. For us, this
// is not the case and verify with execution is good enough
