

use crate::source_ast::{Scope, Stmt};

#[derive(Hash, PartialEq, Eq)]
pub enum Var {
    Vreg(i64),
    Stack(i64),
    Global(String),
    NamedSource(String, Scope),
    NamedTmp(String, i64)
}

enum AtomicExp {
    Ident(Var),
    Num(i64)
}

enum TestOp {
    Lt,
    Gt,
    Eq
}

struct Test {
    exp1 : AtomicExp,
    op : TestOp,
    exp2 : AtomicExp,
}

enum NextBlock {
    Return(Option<Var>),
    Next(usize),
    Branch(Test, usize, usize)
}

pub struct CFGEntry {
    bnum : usize,
    elems : usize,
    next : NextBlock,
    started : bool,
    finished : bool
}

pub fn build_cfg(ast : &[Stmt]) -> Vec<CFGEntry>{
    let mut block_num : usize = 0;
    let mut blocks : Vec<CFGEntry> = Vec::new();

    blocks
}
