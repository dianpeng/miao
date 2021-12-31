use std::cmp;

#[derive(Clone)]
pub struct Lerror {
    pub description: String,
    pub column: u32,
    pub line: u32,
}

// lexer position, used to recover from the bytecode the actual source code for
// diagnostic information.
#[derive(Clone)]
pub struct Lpos {
    pub column: u32,
    pub line: u32,
}

impl Lpos {
    pub fn from_lerror(x: &Lerror) -> Lpos {
        Lpos {
            column: x.column,
            line: x.line,
        }
    }

    pub fn init() -> Lpos {
        Lpos { column: 1, line: 1 }
    }

    // nil, used as placeholder to indicate there's no such information provided
    // here
    pub fn nil() -> Lpos {
        Lpos { column: 0, line: 0 }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Invalid,
    Eof,

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    // Comparison
    Lt,
    Le,
    Gt,
    Ge,

    // Equality
    Eq,
    Ne,

    // Logic
    And,
    Or,

    // Assign
    Assign,

    // Aggregation Assign
    AssignAdd,
    AssignSub,
    AssignMul,
    AssignDiv,
    AssignMod,
    AssignPow,

    // unary
    Not,

    // Punctuation
    Question,
    Comma,
    Dot,
    Semicolon,
    Colon,
    LSqr,
    RSqr,
    LBra,
    RBra,
    LPar,
    RPar,

    // Keywords
    Func,     // func
    If,       // if
    Else,     // else
    Elif,     // elif
    For,      // for
    Return,   // return
    Break,    // break
    Continue, // continue
    Let,      // let
    In,       // in

    // Builints
    Assert, // assert
    Halt,   // halt
    Trace,  // trace

    // Constant
    Str(String),
    True,  // true
    False, // false
    Null,  // null
    Real(f64),
    Int(i64),
    Id(String),
}

type Lresult = Result<Token, Lerror>;

fn mk_result(tk: Token) -> Lresult {
    return Ok(tk);
}

fn mk_err(desc: String, col: u32, line: u32) -> Lresult {
    return Err(Lerror {
        description: desc,
        column: col,
        line: line,
    });
}

pub struct Lexer {
    source: String,
    cursor: u32,
    offset: u32,
    pub prev_token: Token,
}

impl Lexer {
    pub fn new(src: &str) -> Lexer {
        return Lexer {
            source: String::from(src),
            cursor: 0,
            offset: 0,
            prev_token: Token::Invalid,
        };
    }

    // -------------------------------------------------------------------------
    // methods
    fn cp_at(&self, idx: u32) -> (Option<char>, u32) {
        let barr = self.source.as_bytes();
        let len = self.source.as_bytes().len() as u32;
        let end = cmp::min(idx + 4, len);

        // sentry character when we scan pass the final boundary instead of
        // returning an error which means decoding error
        if idx == end {
            return (Option::Some('\0'), 0);
        }

        let slice = &barr[idx as usize..end as usize];
        let mut itr = slice.iter().cloned();
        return match utf8_decode::decode(&mut itr) {
            None => (Option::None, 0),
            Some(v) => match v {
                Ok(v) => {
                    let len = v.len_utf8() as u32;
                    (Option::Some(v), len)
                }
                Err(_) => (Option::None, 0),
            },
        };
    }

    fn next_cp(&mut self) -> Option<char> {
        let (c, offset) = self.cp_at(self.cursor);
        self.offset = offset;
        return c;
    }

    fn peek_cp(&self, offset: u32) -> Option<char> {
        let (c, _) = self.cp_at(offset);
        return c;
    }

    fn peek_cur(&mut self) -> Option<char> {
        return self.peek_cp(self.cursor);
    }

    fn peek_next(&mut self) -> Option<char> {
        return self.peek_cp(self.cursor + self.offset);
    }

    // used to check whether the lookahead character is matched, if so returns
    // true and consume it otherwise not. Notes, if we cannot decode, we will
    // return false here and the unconsumed token will be consumed next time and
    // will raise an exception typically
    fn check_next(&mut self, lookahead: char) -> bool {
        match self.peek_next() {
            Some(v) => {
                if v == lookahead {
                    self.mv_more(v.len_utf8() as u32);
                    return true;
                }
                return false;
            }
            _ => return false,
        };
    }

    fn mv(&mut self) {
        self.cursor += self.offset;
        self.offset = 0;
    }

    fn mv_more(&mut self, offset: u32) {
        self.cursor += self.offset + offset;
        self.offset = 0;
    }

    fn putback(&mut self) {
        self.offset = 0;
    }

    fn t(&mut self, t: Token) -> Lresult {
        self.prev_token = t;
        return mk_result(self.prev_token.clone());
    }

    fn tt(&mut self, lookahead: char, t: Token) -> Lresult {
        self.mv();
        match self.next_cp() {
            Some(v) => {
                if v == lookahead {
                    self.prev_token = t;
                    return mk_result(self.prev_token.clone());
                }
            }
            _ => (),
        };
        return self.err(&format!("expect {}", lookahead));
    }

    pub fn eof(&self) -> bool {
        return self.cursor >= self.source.as_bytes().len() as u32;
    }

    fn mv_eof(&mut self) -> bool {
        self.mv();
        return self.eof();
    }

    fn p2(&mut self, p: char, tk1: Token, tk2: Token) -> Lresult {
        let result = match self.peek_next() {
            None => {
                return self.err("invalid utf8");
            }
            Some(v) => v,
        };

        if result == p {
            self.mv_more(result.len_utf8() as u32);
            return self.t(tk2);
        } else {
            return self.t(tk1);
        }
    }

    fn pos(&self) -> (u32, u32) {
        let mut c = 0;
        let mut line = 1;
        let mut col = 1;
        while c <= self.cursor {
            match self.peek_cp(c) {
                Some(v) => {
                    if v == '\n' {
                        line += 1;
                        col = 1;
                    } else {
                        col += 1;
                    }
                    c += v.len_utf8() as u32;
                }
                _ => return (0, 0),
            }
        }

        return (col, line);
    }

    // error handling
    fn err(&mut self, err: &str) -> Lresult {
        let (column, line) = self.pos();
        return mk_err(String::from(err), column, line);
    }

    fn eescp(&mut self) -> Lresult {
        return self.err("unknown string literal escape sequences");
    }

    fn eutf8(&mut self) -> Lresult {
        return self.err("invalid utf8");
    }

    // Comment lexing ----------------------------------------------------------
    fn do_cmt_line(&mut self) -> Option<Lresult> {
        while !self.mv_eof() {
            match self.next_cp() {
                Some(v) => {
                    if v == '\n' {
                        return Option::None;
                    }
                    continue;
                }
                _ => {
                    return Option::Some(self.eutf8());
                }
            };
        }

        return Option::None;
    }

    fn cmt_sharp(&mut self) -> Option<Lresult> {
        return self.do_cmt_line();
    }

    fn cmt_line(&mut self) -> Option<Lresult> {
        return self.do_cmt_line();
    }

    // comment handling in block way, ie finished with */
    // no nesting comment is allowed
    fn cmt_block(&mut self) -> Option<Lresult> {
        while !self.mv_eof() {
            match self.next_cp() {
                Some(v) => {
                    if v == '*' {
                        if self.check_next('/') {
                            return Option::None;
                        }
                    }
                }
                _ => return Option::Some(self.eutf8()),
            };
        }
        return Option::Some(
            self.err("comment block not closed properly with */"),
        );
    }

    // Number lexing -----------------------------------------------------------
    // Trying to lexing number from the byte sequences
    // the number has some leading integer parts, afterwards, we gonna see either
    // . or Ee for representation
    fn rd_ints(
        &mut self,
        at_least: usize,
        buf: &mut String,
    ) -> Option<Lresult> {
        let mut read_count: usize = 0;
        while !self.mv_eof() {
            match self.next_cp() {
                Some(v) => {
                    if v.is_digit(10) {
                        read_count += 1;
                        buf.push(v);
                        continue;
                    }

                    self.putback();
                    return Option::None;
                }
                _ => return Option::Some(self.eutf8()),
            };
        }

        if at_least < read_count {
            return Option::Some(self.err("expect more digits in number"));
        } else {
            return Option::None;
        }
    }

    fn num(&mut self, c: char) -> Lresult {
        let mut has_dot = false;
        let mut has_e = false;
        let mut buf = String::new();
        buf.push(c);

        // (0) reading in the leading integer parts
        match self.rd_ints(1, &mut buf) {
            Some(v) => return v,
            _ => (),
        };

        // (1) check whether we have dot or e|E
        match self.peek_cur() {
            Some(v) => {
                if v == '.' {
                    self.mv_more(1);

                    has_dot = true;
                    buf.push('.');
                    match self.rd_ints(1, &mut buf) {
                        Some(v) => return v,
                        _ => (),
                    };
                }
            }
            _ => (),
        };
        match self.peek_cur() {
            Some(v) => {
                if v == 'e' || v == 'E' {
                    self.mv_more(1);

                    has_e = true;
                    buf.push(v);
                    match self.rd_ints(1, &mut buf) {
                        Some(v) => return v,
                        _ => (),
                    };
                }
            }
            _ => (),
        };

        // (2) based on the symbol we saw during parsing to decide how to box
        //     the number.
        if !has_e {
            if has_dot {
                match buf.parse::<f64>() {
                    Ok(v) => {
                        return self.t(Token::Real(v));
                    }
                    _ => return self.err("cannot parse number"),
                };
            } else {
                match buf.parse::<i64>() {
                    Ok(v) => {
                        return self.t(Token::Int(v));
                    }
                    _ => return self.err("cannot parse number"),
                };
            }
        } else {
            match buf.parse::<f64>() {
                Ok(v) => {
                    return self.t(Token::Real(v));
                }
                _ => return self.err("cannot parse number"),
            };
        }
    }

    // String lexing ------------------------------------------------------------
    fn str(&mut self, ending: char) -> Lresult {
        let mut buf = String::new();

        while !self.mv_eof() {
            match self.next_cp() {
                Some(v) => {
                    if v == ending {
                        self.mv();
                        return self.t(Token::Str(buf));
                    } else if v == '\\' {
                        match self.peek_next() {
                            Some(vv) => match vv {
                                't' => buf.push('\t'),
                                'n' => buf.push('\n'),
                                'r' => buf.push('\r'),
                                '\'' => buf.push('\''),
                                '\"' => buf.push('\"'),
                                '\\' => buf.push('\\'),
                                _ => return self.eescp(),
                            },
                            _ => return self.eutf8(),
                        }
                        self.mv_more(1);
                    } else {
                        buf.push(v);
                    }
                }
                _ => return self.eutf8(),
            };
        }

        return self.err("string not closed properly");
    }

    // Keyword lexing------------------------------------------------------------
    fn maybe_id(&mut self, leading: char) -> Lresult {
        if leading != '_' && !leading.is_ascii_alphabetic() {
            return self.err("unknown token");
        }

        let mut buf = String::new();
        buf.push(leading);

        while !self.mv_eof() {
            match self.next_cp() {
                Some(v) => {
                    if v != '_' && !v.is_ascii_alphanumeric() {
                        self.putback();
                        return self.t(Token::Id(buf));
                    }
                    buf.push(v);
                }
                _ => return self.eutf8(),
            };
        }
        return self.t(Token::Id(buf));
    }

    fn try_match_keyword(
        &mut self,
        target: &[char],
        tk: Token,
    ) -> Option<Lresult> {
        let mut cur = self.cursor;
        let mut idx: usize = 0;
        let len = self.source.as_bytes().len() as u32;

        while cur < len && idx < target.len() {
            match self.peek_cp(cur) {
                Some(v) => {
                    if target[idx] != v {
                        return Option::None;
                    }
                    cur += v.len_utf8() as u32;
                    idx += 1;
                }
                _ => return Option::Some(self.eutf8()),
            };
        }

        if idx == target.len() {
            if cur < len {
                match self.peek_cp(cur) {
                    Some(v) => {
                        if v == '_' || v.is_ascii_alphanumeric() {
                            return Option::None;
                        }
                    }

                    _ => {
                        self.cursor = cur;
                        self.offset = 0;
                        return Option::Some(self.eutf8());
                    }
                }
            }

            self.cursor = cur;
            self.offset = 0;
            return Option::Some(self.t(tk));
        } else {
            return Option::None;
        }
    }

    fn keyword_or_id(&mut self, leading: char) -> Lresult {
        let keywords: [(&[char], Token); 16] = [
            (&['t', 'r', 'u', 'e'], Token::True),
            (&['f', 'a', 'l', 's', 'e'], Token::False),
            (&['f', 'u', 'n', 'c'], Token::Func),
            (&['i', 'f'], Token::If),
            (&['l', 'e', 't'], Token::Let),
            (&['i', 'n'], Token::In),
            (&['e', 'l', 's', 'e'], Token::Else),
            (&['e', 'l', 'i', 'f'], Token::Elif),
            (&['c', 'o', 'n', 't', 'i', 'n', 'u', 'e'], Token::Continue),
            (&['b', 'r', 'e', 'a', 'k'], Token::Break),
            (&['r', 'e', 't', 'u', 'r', 'n'], Token::Return),
            (&['f', 'o', 'r'], Token::For),
            (&['n', 'u', 'l', 'l'], Token::Null),
            // builtins
            (&['h', 'a', 'l', 't'], Token::Halt),
            (&['a', 's', 's', 'e', 'r', 't'], Token::Assert),
            (&['t', 'r', 'a', 'c', 'e'], Token::Trace),
        ];

        for (slice, token) in keywords.iter() {
            match self.try_match_keyword(slice, token.clone()) {
                Some(v) => return v,
                _ => (),
            };
        }

        return self.maybe_id(leading);
    }

    // Get the current position for diagnostic usage
    pub fn get_pos(&self) -> (u32, u32) {
        return self.pos();
    }

    pub fn get_lpos(&self) -> Lpos {
        let (c, l) = self.pos();
        Lpos { line: l, column: c }
    }

    // Trying to decode the next
    pub fn next(&mut self) -> Lresult {
        while !self.mv_eof() {
            match self.next_cp() {
                Some(v) => {
                    // token main loops
                    match v {
                        '\n' | '\r' | '\t' | ' ' => (),

                        '+' => {
                            return self.p2('=', Token::Add, Token::AssignAdd)
                        }
                        '-' => {
                            return self.p2('=', Token::Sub, Token::AssignSub)
                        }
                        '*' => {
                            return self.p2('=', Token::Mul, Token::AssignMul)
                        }
                        '/' => {
                            // parsing comment or division
                            match self.peek_next() {
                                None => {
                                    return self.eutf8();
                                }
                                Some(vv) => {
                                    if vv == '/' {
                                        self.mv_more(1);
                                        match self.cmt_line() {
                                            Some(v) => return v,
                                            _ => (),
                                        };
                                        continue;
                                    }
                                    if vv == '*' {
                                        self.mv_more(1);
                                        match self.cmt_block() {
                                            Some(v) => return v,
                                            _ => (),
                                        };
                                        continue;
                                    }
                                    if vv == '=' {
                                        self.mv_more(1);
                                        return self.t(Token::AssignDiv);
                                    }
                                    return self.t(Token::Div);
                                }
                            };
                        }
                        '%' => {
                            return self.p2('=', Token::Mod, Token::AssignMod)
                        }
                        '|' => return self.tt('|', Token::Or),
                        '&' => return self.tt('&', Token::And),
                        '^' => return self.t(Token::Pow),
                        '<' => return self.p2('=', Token::Lt, Token::Le),
                        '>' => return self.p2('=', Token::Gt, Token::Ge),
                        '=' => return self.p2('=', Token::Assign, Token::Eq),
                        '!' => return self.p2('=', Token::Not, Token::Ne),
                        '?' => return self.t(Token::Question),
                        ',' => return self.t(Token::Comma),
                        '.' => return self.t(Token::Dot),
                        ':' => return self.t(Token::Colon),
                        ';' => return self.t(Token::Semicolon),
                        '(' => return self.t(Token::LPar),
                        ')' => return self.t(Token::RPar),
                        '[' => return self.t(Token::LSqr),
                        ']' => return self.t(Token::RSqr),
                        '{' => return self.t(Token::LBra),
                        '}' => return self.t(Token::RBra),
                        cc @ '0'..='9' => {
                            self.mv();
                            return self.num(cc);
                        }
                        '"' => {
                            self.mv();
                            return self.str('"');
                        }
                        '\'' => {
                            self.mv();
                            return self.str('\'');
                        }
                        '#' => {
                            match self.cmt_sharp() {
                                Some(v) => return v,
                                _ => (),
                            };
                        }
                        cc @ _ => {
                            return self.keyword_or_id(cc);
                        }
                    }
                }
                _ => return self.eutf8(),
            }
        }

        return self.t(Token::Eof);
    }

    // other utility
    pub fn is_agg_assign(tk: &Token) -> bool {
        match tk {
            Token::AssignAdd
            | Token::AssignSub
            | Token::AssignMul
            | Token::AssignDiv
            | Token::AssignMod => return true,
            _ => return false,
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn match_tokens(input: &str, token: &[Token]) {
        let mut lexer = Lexer::new(input);
        let mut idx: usize = 0;

        while !lexer.eof() {
            match lexer.next() {
                Ok(v) => assert_eq!(v, token[idx]),
                Err(e) => {
                    println!("{}", e.description);
                    assert!(false);
                }
            };
            idx += 1;
            assert!(idx <= token.len());
        }
    }

    #[test]
    fn test_lexer_basic() {
        let mapping = [
            ("+", [Token::Add, Token::Eof]),
            ("-", [Token::Sub, Token::Eof]),
            ("*", [Token::Mul, Token::Eof]),
            ("/", [Token::Div, Token::Eof]),
            ("^", [Token::Pow, Token::Eof]),
            ("<=", [Token::Le, Token::Eof]),
            ("<", [Token::Lt, Token::Eof]),
            (">", [Token::Gt, Token::Eof]),
            (">=", [Token::Ge, Token::Eof]),
            ("==", [Token::Eq, Token::Eof]),
            ("!=", [Token::Ne, Token::Eof]),
            ("!", [Token::Not, Token::Eof]),
            ("=", [Token::Assign, Token::Eof]),
            ("?", [Token::Question, Token::Eof]),
            (",", [Token::Comma, Token::Eof]),
            (":", [Token::Colon, Token::Eof]),
            (";", [Token::Semicolon, Token::Eof]),
            (".", [Token::Dot, Token::Eof]),
            ("[", [Token::LSqr, Token::Eof]),
            ("]", [Token::RSqr, Token::Eof]),
            ("{", [Token::LBra, Token::Eof]),
            ("}", [Token::RBra, Token::Eof]),
            ("(", [Token::LPar, Token::Eof]),
            (")", [Token::RPar, Token::Eof]),
            ("func", [Token::Func, Token::Eof]),
            ("func ", [Token::Func, Token::Eof]),
            ("funcd", [Token::Id("funcd".to_string()), Token::Eof]),
            ("true", [Token::True, Token::Eof]),
            ("true ", [Token::True, Token::Eof]),
            ("true_", [Token::Id("true_".to_string()), Token::Eof]),
            ("false", [Token::False, Token::Eof]),
            ("false ", [Token::False, Token::Eof]),
            ("false_", [Token::Id("false_".to_string()), Token::Eof]),
            ("if", [Token::If, Token::Eof]),
            ("if ", [Token::If, Token::Eof]),
            ("if_", [Token::Id("if_".to_string()), Token::Eof]),
            ("else", [Token::Else, Token::Eof]),
            ("else ", [Token::Else, Token::Eof]),
            ("else_", [Token::Id("else_".to_string()), Token::Eof]),
            ("elif", [Token::Elif, Token::Eof]),
            ("elif ", [Token::Elif, Token::Eof]),
            ("elif_", [Token::Id("elif_".to_string()), Token::Eof]),
            ("return", [Token::Return, Token::Eof]),
            ("return ", [Token::Return, Token::Eof]),
            ("return_", [Token::Id("return_".to_string()), Token::Eof]),
            ("continue", [Token::Continue, Token::Eof]),
            ("continue ", [Token::Continue, Token::Eof]),
            (
                "continue_",
                [Token::Id("continue_".to_string()), Token::Eof],
            ),
            ("break", [Token::Break, Token::Eof]),
            ("break ", [Token::Break, Token::Eof]),
            ("break_", [Token::Id("break_".to_string()), Token::Eof]),
            ("for", [Token::For, Token::Eof]),
            ("for ", [Token::For, Token::Eof]),
            ("for_", [Token::Id("for_".to_string()), Token::Eof]),
            ("null", [Token::Null, Token::Eof]),
            ("null", [Token::Null, Token::Eof]),
            ("null_", [Token::Id("null_".to_string()), Token::Eof]),
        ];

        for (input, expect) in mapping.iter() {
            match_tokens(input, expect);
        }
    }

    #[test]
    fn test_lexer_id() {
        let mapping = [
            ("a", [Token::Id("a".to_string()), Token::Eof]),
            ("_", [Token::Id("_".to_string()), Token::Eof]),
            ("_a", [Token::Id("_a".to_string()), Token::Eof]),
            ("a_", [Token::Id("a_".to_string()), Token::Eof]),
            ("_1", [Token::Id("_1".to_string()), Token::Eof]),
            ("a1", [Token::Id("a1".to_string()), Token::Eof]),
            ("a ", [Token::Id("a".to_string()), Token::Eof]),
            ("_ ", [Token::Id("_".to_string()), Token::Eof]),
            ("_a ", [Token::Id("_a".to_string()), Token::Eof]),
            ("a_ ", [Token::Id("a_".to_string()), Token::Eof]),
            ("_1 ", [Token::Id("_1".to_string()), Token::Eof]),
            ("a1 ", [Token::Id("a1".to_string()), Token::Eof]),
        ];
        for (input, expect) in mapping.iter() {
            match_tokens(input, expect);
        }
    }

    #[test]
    fn test_lexer_comment_line() {
        let mapping = [
            ("#this is a comment", [Token::Eof]),
            ("#this is a comment\n", [Token::Eof]),
            ("//this is a comment", [Token::Eof]),
            ("//this is a comment\n", [Token::Eof]),
        ];

        for (input, expect) in mapping.iter() {
            match_tokens(input, expect);
        }
    }

    #[test]
    fn test_lexer_comment_block() {
        let mapping = [
            ("/* a comment */", [Token::Eof]),
            ("/* a comment */\n\n\n#A comment", [Token::Eof]),
        ];

        for (input, expect) in mapping.iter() {
            match_tokens(input, expect);
        }
    }

    #[test]
    fn test_lexer_num() {
        {
            let mapping = [
                ("1", [Token::Int(1), Token::Eof]),
                ("1.1", [Token::Real(1.1), Token::Eof]),
                ("1e1", [Token::Real(1e1), Token::Eof]),
                ("1.0e1", [Token::Real(1.0e1), Token::Eof]),
            ];

            for (input, expect) in mapping.iter() {
                match_tokens(input, expect);
            }
        }
        {
            let mapping = [
                ("1;", [Token::Int(1), Token::Semicolon, Token::Eof]),
                ("1.1;", [Token::Real(1.1), Token::Semicolon, Token::Eof]),
            ];

            for (input, expect) in mapping.iter() {
                match_tokens(input, expect);
            }
        }
    }

    #[test]
    fn test_lexer_whitespace() {
        let mapping = [
            ("+   -", [Token::Add, Token::Sub, Token::Eof]),
            ("+\n -", [Token::Add, Token::Sub, Token::Eof]),
            ("+\r\t-", [Token::Add, Token::Sub, Token::Eof]),
            ("+\r\t-", [Token::Add, Token::Sub, Token::Eof]),
        ];
        for (input, expect) in mapping.iter() {
            match_tokens(input, expect);
        }
    }

    #[test]
    fn test_lexer_quoted_string() {
        let mapping = [
            ("\'\'", [Token::Str("".to_string()), Token::Eof]),
            ("\"\"", [Token::Str("".to_string()), Token::Eof]),
            ("'a'", [Token::Str("a".to_string()), Token::Eof]),
            ("\"a\"", [Token::Str("a".to_string()), Token::Eof]),
            ("'abc'", [Token::Str("abc".to_string()), Token::Eof]),
            ("'\\''", [Token::Str("'".to_string()), Token::Eof]),
            ("'\\n'", [Token::Str("\n".to_string()), Token::Eof]),
            ("'\\\\'", [Token::Str("\\".to_string()), Token::Eof]),
        ];

        for (input, expect) in mapping.iter() {
            match_tokens(input, expect);
        }
    }
}
