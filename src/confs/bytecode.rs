// Bytecode of our VM
//
// the bytecode assembls a simple stack based VM for simplicity
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

use crate::confs::lexer::Lpos;

pub type Index = u32;
pub type CodePos = u32;

#[derive(Clone, Debug, PartialEq)]
pub enum Bytecode {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,

    Not,
    Neg,
    Boolean,

    // literal constant loading
    LoadInt(Index),
    LoadReal(Index),
    LoadString(Index),
    LoadFunction(Index),
    LoadNull,
    LoadTrue,
    LoadFalse,

    // Upvalue
    LoadUpvalue(u32),
    SetUpvalue(u32),

    // list
    ListStart,
    ListAdd(u32),

    // Object
    ObjectStart,
    ObjectAdd(u32),

    // Iterator
    IteratorNew,
    IteratorStart,
    IteratorHas,
    IteratorNext,
    IteratorValue,

    // Global
    LoadGlobal(Index),
    SetGlobal(Index),

    // Stack manipulation
    Pop,
    PopN(u32),
    Dup,
    Load(u32),
    Store(u32),
    PushN(u32),

    // Access
    // dot accessor
    DotAccess(u32),
    DotStore(u32),

    // [] accessor
    ArrayIndex,
    ArrayStore,

    // invocation
    Call(u32),

    // Control flow
    JumpFalse(CodePos),
    LoopJump(CodePos),
    Jump(CodePos),

    // Used for logic operation/expression
    And(CodePos),
    Or(CodePos),
    Ternary(CodePos),

    // Return the value on the stack
    Return(u32),

    // Builtins
    Assert1,
    Assert2,
    Trace(u32),
    Typeof,
    Sizeof,
    Halt,

    // Others
    Label,
}

#[derive(Default)]
pub struct BytecodeArray {
    pub array: Vec<Bytecode>,
    pub debug: Vec<Lpos>,

    int_table: Vec<i64>,
    real_table: Vec<f64>,
    string_table: Vec<String>,
    proto_table: Vec<ProtoRc>,
}

pub struct Upval {
    pub index: u32,
    pub is_stack: bool,
}

// The assets been compiled from textual function definition. ie the "code"
pub struct Prototype {
    pub code: BytecodeArray,
    pub argument_count: u32,
    pub hint: String,

    // upvalue array, used to hint how to fill up the upvalue array which is
    // used as clousre's capture internally
    pub upvalue: Vec<Upval>,
}

pub type ProtoRc = Rc<Prototype>;

impl fmt::Display for Prototype {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "prototype ({}:{})\n", self.hint, self.argument_count)
            .unwrap();
        write!(
            f,
            "====================\n{}\n====================\n",
            self.code.dump()
        )
        .unwrap();
        let mut idx = 0;
        for x in self.upvalue.iter() {
            write!(f, "upval({}) => {}/{}\n", idx, x.index, x.is_stack)
                .unwrap();
            idx += 1;
        }
        write!(f, "\n")
    }
}

impl Prototype {
    pub fn new(bc: BytecodeArray) -> Prototype {
        Prototype {
            code: bc,
            argument_count: 0,
            hint: "".to_string(),
            upvalue: Vec::<Upval>::new(),
        }
    }
    pub fn new_top_level(bc: BytecodeArray, hint: String) -> Prototype {
        Prototype {
            code: bc,
            argument_count: 0,
            hint: hint,
            upvalue: Vec::<Upval>::new(),
        }
    }
    pub fn debug_info(&self) -> String {
        return format!(
            "{}({}:{})",
            self.hint,
            self.argument_count,
            self.upvalue.len()
        );
    }
}

pub struct Label {
    pos: CodePos,
}

impl Label {
    pub fn jump_false(&self, ba: &mut BytecodeArray, arg: CodePos) {
        ba.patch(self.pos, Bytecode::JumpFalse(arg));
    }
    pub fn jump_false_here(&self, ba: &mut BytecodeArray) {
        self.jump_false(ba, ba.code_pos());
    }
    pub fn jump(&self, ba: &mut BytecodeArray, arg: CodePos) {
        ba.patch(self.pos, Bytecode::Jump(arg));
    }
    pub fn jump_here(&self, ba: &mut BytecodeArray) {
        self.jump(ba, ba.code_pos());
    }
    pub fn loop_jump(&self, ba: &mut BytecodeArray, arg: CodePos) {
        ba.patch(self.pos, Bytecode::LoopJump(arg));
    }
    pub fn loop_jump_here(&self, ba: &mut BytecodeArray) {
        self.loop_jump(ba, ba.code_pos());
    }
    pub fn and(&self, ba: &mut BytecodeArray, arg: CodePos) {
        ba.patch(self.pos, Bytecode::And(arg));
    }
    pub fn and_here(&self, ba: &mut BytecodeArray) {
        self.and(ba, ba.code_pos());
    }
    pub fn or(&self, ba: &mut BytecodeArray, arg: CodePos) {
        ba.patch(self.pos, Bytecode::Or(arg));
    }
    pub fn or_here(&self, ba: &mut BytecodeArray) {
        self.or(ba, ba.code_pos());
    }
    pub fn ternary(&self, ba: &mut BytecodeArray, arg: CodePos) {
        ba.patch(self.pos, Bytecode::Ternary(arg));
    }
    pub fn ternary_here(&self, ba: &mut BytecodeArray) {
        self.ternary(ba, ba.code_pos());
    }

    pub fn emit(&self, ba: &mut BytecodeArray, bc: Bytecode) {
        ba.patch(self.pos, bc);
    }
}

impl BytecodeArray {
    pub fn new() -> BytecodeArray {
        BytecodeArray {
            array: Vec::<Bytecode>::new(),
            debug: Vec::<Lpos>::new(),
            int_table: Vec::<i64>::new(),
            real_table: Vec::<f64>::new(),
            string_table: Vec::<String>::new(),
            proto_table: Vec::<ProtoRc>::new(),
        }
    }

    // Function to emit each bytecodes type
    pub fn emit(&mut self, b: Bytecode) {
        self.array.push(b);
        self.debug.push(Lpos::nil());
    }

    pub fn emit_d(&mut self, b: Bytecode, p: Lpos) {
        self.array.push(b);
        self.debug.push(p);
    }

    // Function to mutate the recently added instruction
    pub fn pop_last(&mut self) -> Bytecode {
        self.array.pop().unwrap()
    }

    // Function used to do patching, ie temporary reserve a slot at the current
    // position and then later on do patching accordingly. This is mainly used
    // in jumpping code.
    pub fn code_pos(&self) -> CodePos {
        return self.array.len() as CodePos;
    }

    pub fn len(&self) -> usize {
        return self.array.len();
    }

    pub fn label(&mut self) -> Label {
        let pos = self.code_pos();
        self.array.push(Bytecode::Label);
        return Label { pos: pos };
    }

    pub fn label_d(&mut self, d: Lpos) -> Label {
        self.debug.push(d);
        self.label()
    }

    pub fn patch(&mut self, pos: CodePos, bc: Bytecode) {
        assert!(pos < self.array.len() as CodePos);
        self.array[pos as usize] = bc;
    }

    pub fn int_index(&mut self, v: i64) -> Index {
        match self.int_table.iter().position(|&x| x == v) {
            Some(index) => return index as Index,
            _ => (),
        };

        let idx = self.int_table.len() as Index;
        self.int_table.push(v);
        return idx;
    }

    pub fn real_index(&mut self, v: f64) -> Index {
        match self.real_table.iter().position(|&x| x == v) {
            Some(index) => return index as Index,
            _ => (),
        };

        let idx = self.real_table.len() as Index;
        self.real_table.push(v);
        return idx;
    }

    pub fn str_index(&mut self, s: &str) -> Index {
        return self.string_index(s.to_string());
    }

    pub fn string_index(&mut self, s: String) -> Index {
        let ref_s = &s;
        match self.string_table.iter().position(|x| *x == *ref_s) {
            Some(index) => return index as Index,
            _ => (),
        };

        let idx = self.string_table.len() as Index;
        self.string_table.push(s);
        return idx;
    }

    pub fn proto_index(&mut self, p: Prototype) -> Index {
        let proto_rc = Rc::new(p);
        let idx = self.proto_table.len() as Index;
        self.proto_table.push(proto_rc);
        return idx;
    }

    pub fn load_int(&self, idx: u32) -> i64 {
        return self.int_table[idx as usize];
    }
    pub fn load_real(&self, idx: u32) -> f64 {
        return self.real_table[idx as usize];
    }
    pub fn load_str(&self, idx: u32) -> String {
        return self.string_table[idx as usize].clone();
    }
    pub fn load_proto(&self, idx: u32) -> ProtoRc {
        return Rc::clone(&self.proto_table[idx as usize]);
    }

    fn dump_table_iter<T: IntoIterator>(&self, t: &str, itr: T) -> String
    where
        <T as IntoIterator>::Item: std::fmt::Display,
    {
        let mut o = Vec::<String>::new();
        let mut idx = 0;
        o.push(format!("[table] {}", t));
        for x in itr {
            o.push(format!("{}. {}", idx, x));
            idx += 1;
        }
        o.push(format!("--------------------------"));
        return o.join("\n");
    }

    pub fn dump(&self) -> String {
        let mut o = Vec::<String>::new();

        o.push(self.dump_table_iter("int", self.int_table.iter()));
        o.push(self.dump_table_iter("real", self.real_table.iter()));
        o.push(self.dump_table_iter("string", self.string_table.iter()));
        o.push(self.dump_table_iter("prototype", self.proto_table.iter()));

        let mut idx = 0;
        for code in self.array.iter() {
            o.push(format!("{}. {}", idx, code));
            idx += 1;
        }
        return o.join("\n");
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

// ------------------------------------------------------------------------
//   TESTING
// ------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    fn match_ba(bc: &BytecodeArray, tar: Vec<Bytecode>) {
        assert_eq!(bc.array.len(), tar.len());

        let mut idx = 0;
        for x in bc.array.iter() {
            assert_eq!(*x, tar[idx]);
            idx += 1;
        }
    }

    #[test]
    fn test_int_table() {
        let mut ba = BytecodeArray::new();

        {
            assert_eq!(ba.int_table.len(), 0);
            assert_eq!(ba.int_index(0), 0);
            assert_eq!(ba.int_index(10), 1);
            assert_eq!(ba.int_index(100), 2);
            assert_eq!(ba.int_index(0), 0);
            assert_eq!(ba.int_index(10), 1);
            assert_eq!(ba.int_index(100), 2);
            assert_eq!(ba.int_table.len(), 3);
        }
    }

    #[test]
    fn test_real_table() {
        let mut ba = BytecodeArray::new();

        {
            assert_eq!(ba.real_table.len(), 0);

            assert_eq!(ba.real_index(0.1), 0);
            assert_eq!(ba.real_index(10.1), 1);
            assert_eq!(ba.real_index(100.1), 2);
            assert_eq!(ba.real_index(0.1), 0);
            assert_eq!(ba.real_index(10.1), 1);
            assert_eq!(ba.real_index(100.1), 2);

            assert_eq!(ba.real_table.len(), 3);
        }
    }

    #[test]
    fn test_string_table() {
        let mut ba = BytecodeArray::new();
        {
            assert_eq!(ba.string_table.len(), 0);
            assert_eq!(ba.str_index("Hello"), 0);
            assert_eq!(ba.str_index("World"), 1);
            assert_eq!(ba.str_index(""), 2);

            assert_eq!(ba.str_index("Hello"), 0);
            assert_eq!(ba.str_index("World"), 1);
            assert_eq!(ba.str_index(""), 2);
        }

        assert_eq!(ba.string_table.len(), 3);
    }

    #[test]
    fn test_label_jump() {
        let mut ba = BytecodeArray::new();
        ba.emit(Bytecode::Add);
        assert_eq!(ba.code_pos(), 1);

        let label = ba.label();
        assert_eq!(ba.code_pos(), 2);

        ba.emit(Bytecode::Sub);
        assert_eq!(ba.code_pos(), 3);

        label.jump(&mut ba, 0 as CodePos);
        assert_eq!(ba.len(), 3);

        match_ba(&ba, vec![Bytecode::Add, Bytecode::Jump(0), Bytecode::Sub]);
    }

    #[test]
    fn test_label_jump_false() {
        let mut ba = BytecodeArray::new();
        ba.emit(Bytecode::Add);
        assert_eq!(ba.code_pos(), 1);

        let label = ba.label();
        assert_eq!(ba.code_pos(), 2);

        ba.emit(Bytecode::Sub);
        assert_eq!(ba.code_pos(), 3);

        label.jump_false(&mut ba, 0 as CodePos);
        assert_eq!(ba.len(), 3);

        match_ba(
            &ba,
            vec![Bytecode::Add, Bytecode::JumpFalse(0), Bytecode::Sub],
        );
    }

    #[test]
    fn test_emit1() {
        let mut ba = BytecodeArray::new();
        assert_eq!(0, ba.len());
        match_ba(&ba, vec![]);

        {
            ba.emit(Bytecode::Add);
            assert_eq!(ba.code_pos(), 1);

            ba.emit(Bytecode::Sub);
            assert_eq!(ba.code_pos(), 2);

            ba.emit(Bytecode::Div);
            assert_eq!(ba.code_pos(), 3);

            ba.emit(Bytecode::Mul);
            assert_eq!(ba.code_pos(), 4);

            ba.emit(Bytecode::Pow);
            assert_eq!(ba.code_pos(), 5);

            match_ba(
                &ba,
                vec![
                    Bytecode::Add,
                    Bytecode::Sub,
                    Bytecode::Div,
                    Bytecode::Mul,
                    Bytecode::Pow,
                ],
            );
        }
    }
}
