

use crate::{source_ast::{Scope, Stmt, Exp, ID, TypedExp, scope_to_string}, tokens::{OP, UOP}};

#[derive(Hash, PartialEq, Eq, Clone)]
pub enum Var {
    Vreg(i64),
    Stack(i64),
    Global(String),
    NamedSource(String, Scope),
    NamedTmp(String, usize)
}

#[derive(Clone, Eq, PartialEq)]
pub enum AtomicExp {
    Ident(Var),
    Num(i64)
}

#[derive(Clone, Eq, PartialEq)]
pub enum BlockElem {
    AssignOp(Var, AtomicExp, OP, AtomicExp),
    AssignAtom(Var, AtomicExp),
    Ld(Var, Var, AtomicExp),
    St(Var, AtomicExp, AtomicExp),
    Call(Option<Var>, String, Vec<AtomicExp>),
    BoundCheck(AtomicExp, AtomicExp),
    NullCheck(Var)
}

#[derive(Clone, Eq, PartialEq)]
pub enum TestOp {
    Lt,
    Gt,
    Eq
}

#[derive(Clone, Eq, PartialEq)]
pub struct Test {
    pub exp1 : AtomicExp,
    pub op : TestOp,
    pub exp2 : AtomicExp,
}

#[derive(Clone, Eq, PartialEq)]
pub enum NextBlock {
    Return(Option<Var>),
    Next(usize),
    Branch(Test, usize, usize)
}

#[derive(Clone, Eq, PartialEq)]
pub struct CFGEntry {
    pub bnum : usize,
    pub elems : Vec<BlockElem>,
    pub next : NextBlock,
    pub started : bool,
    pub finished : bool
}

pub fn var_to_string(v : &Var) -> String {
    match v {
        Var::Vreg(r) => format!("reg_{}", r),
        Var::Stack(n) => format!("stack_{}", n),
        Var::Global(g) => format!("g_{}", g),
        Var::NamedSource(st, sc) => format!("ns_{}_{}", st, scope_to_string(&Some(*sc))),
        Var::NamedTmp(s, n) => format!("tmp_{}_{}", s, n),
    }
}

pub fn atomic_to_string(exp : &AtomicExp) -> String {
    match exp {
        AtomicExp::Ident(id) => var_to_string(id),
        AtomicExp::Num(n) => format!("{n}"),
    }
}

pub fn test_op_to_str(op : TestOp) -> &'static str {
    match op {
        TestOp::Lt => "<",
        TestOp::Gt => ">",
        TestOp::Eq => "==",
    }
}

pub fn test_to_string(test : &Test) -> String {
    format!("{} {} {}", atomic_to_string(&test.exp1), test_op_to_str(test.op.clone()), atomic_to_string(&test.exp2))
}

fn atomic_op_to_test_op(op : &OP) -> TestOp {
    match op {
        OP::Lt => TestOp::Lt,
        OP::Eq => TestOp::Eq,
        OP::Gt => TestOp::Gt,
        _ => panic!("unexpected op")
    }
}

fn id_to_var(x : &ID) -> Var {
    match x {
        ID::Source(_, None) => panic!("unscoped source"),
        ID::Source(x, Some(sc)) => Var::NamedSource(x.clone(), *sc),
        ID::Temp(x, i) => Var::NamedTmp(x.clone(), *i),
    }
}

fn exp_to_atomic(e : &TypedExp) -> AtomicExp{
    match &e.exp {
        Exp::Ident(id, es) if es.len() == 0 => AtomicExp::Ident(id_to_var(id)),
        Exp::Num(n) => AtomicExp::Num(*n),
        Exp::Bool(b) => AtomicExp::Num(*b as i64),
        _ => panic!("non attomic exp found")
    }
}

fn flat_e_to_assign(x : &ID, e: &TypedExp) -> Vec<BlockElem> {
    let v = id_to_var(x);
    match &e.exp {
        Exp::Ident(id, es) if es.len() == 0 => {
            vec![BlockElem::AssignAtom(v, AtomicExp::Ident(id_to_var(id)))]
        },
        Exp::Ident(id, es) if es.len() == 1 => {
            let tmp_var = Var::NamedTmp("BS".to_string(), 0);
            let ae = exp_to_atomic(es.first().unwrap());
            let mut blk = Vec::new();
            blk.push(BlockElem::NullCheck(id_to_var(id)));
            blk.push(BlockElem::Ld(tmp_var.clone(), id_to_var(id), AtomicExp::Num(0)));
            blk.push(BlockElem::BoundCheck(ae.clone(), AtomicExp::Ident(tmp_var.clone())));
            match ae {
                AtomicExp::Num(n) => {
                    blk.push(BlockElem::Ld(v, id_to_var(id), AtomicExp::Num((n + 1) * 8)))
                }
                AtomicExp::Ident(_) => {
                    blk.push(BlockElem::AssignOp(tmp_var.clone(), ae.clone(), OP::Plus, AtomicExp::Num(1)));
                    blk.push(BlockElem::AssignOp(tmp_var.clone(), AtomicExp::Ident(tmp_var.clone()), OP::Lshift, AtomicExp::Num(3)));
                    blk.push(BlockElem::Ld(v, id_to_var(id), AtomicExp::Ident(tmp_var)));
                },
            }
            blk
        },
        Exp::Ident(_, _) => panic!("non flat array access"),
        Exp::Num(n) => vec![BlockElem::AssignAtom(v, AtomicExp::Num(*n))],
        Exp::Bool(b) => vec![BlockElem::AssignAtom(v, AtomicExp::Num(*b as i64))],
        Exp::Op(ae1, op, ae2) => vec![BlockElem::AssignOp(v, exp_to_atomic(ae1), *op, exp_to_atomic(ae2))],
        Exp::Uop(UOP::Not, ae) => vec![BlockElem::AssignOp(v, exp_to_atomic(ae), OP::Eq, AtomicExp::Num(0))],
        Exp::Array(es) => vec![BlockElem::Call(Some(v), format!("allocate{}", es.len()), es.iter().map(|e| exp_to_atomic(e)).collect())],
        Exp::Call(f, es) => {
            let name = match f {
                ID::Source(s, _) => s,
                ID::Temp(s, _) => s,
            };
            vec![BlockElem::Call(Some(v), name.clone(), es.iter().map(|e| exp_to_atomic(e)).collect())]
        }
    }
}

fn flat_exp_to_test (e : &Exp) -> Test {
    match e {
        Exp::Ident(id, inds) if inds.len() == 0 => {
            Test{exp1: AtomicExp::Ident(id_to_var(id)), op: TestOp::Eq, exp2: AtomicExp::Num(1)}
        }
        Exp::Call(_, _) => panic!("call in test"),
        Exp::Num(_) => panic!("num in test"),
        Exp::Bool(b) => Test{exp1: AtomicExp::Num((*b) as i64), op: TestOp::Eq, exp2: AtomicExp::Num(1)},
        Exp::Op(id1, op, id2) => Test{exp1: exp_to_atomic(&**id1), op: atomic_op_to_test_op(op), exp2: exp_to_atomic(&**id2)},
        Exp::Uop(UOP::Not, id) => Test{exp1: exp_to_atomic(&**id), op: TestOp::Eq, exp2: AtomicExp::Num(0)},
        Exp::Array(_) => panic!("array in test"),
        Exp::Ident(_, _) => panic!("array index in test")
    }
}

fn make_cfg_entry(bnum : usize, elems : Vec<BlockElem>, next : NextBlock) -> CFGEntry {
    CFGEntry{ bnum: bnum, elems: elems, next: next, started: false, finished: false }
}

fn build_cfg(ast : &[Stmt], block_num: &mut usize, head_block : usize, next_block : &NextBlock) -> Vec<CFGEntry> {
    let mut blocks : Vec<CFGEntry> = Vec::new();
    let mut wip_bnum = head_block;
    let mut wip_elems : Vec<BlockElem> = Vec::new();
    for stmt in ast {
        match stmt {
            Stmt::Assign(id, es, e) if es.len() == 0 => {
                wip_elems.append(&mut flat_e_to_assign(&id, &e));
            }
            Stmt::Assign(id, es, e) if es.len() == 1 => {
                let idx = exp_to_atomic(es.first().unwrap());
                let tmp_var = Var::NamedTmp("BS".to_string(), 0);
                let get_len = BlockElem::Ld(tmp_var.clone(), id_to_var(&id), AtomicExp::Num(0));
                wip_elems.push(BlockElem::NullCheck(id_to_var(&id)));
                wip_elems.push(get_len);
                wip_elems.push(BlockElem::BoundCheck(idx.clone(), AtomicExp::Ident(tmp_var.clone())));
                match idx {
                    AtomicExp::Num(n) => {
                        wip_elems.push(BlockElem::St(id_to_var(&id), AtomicExp::Num((n + 1) * 8), exp_to_atomic(&e)));
                    },
                    AtomicExp::Ident(_) => {
                        wip_elems.push(BlockElem::AssignOp(tmp_var.clone(), idx.clone(), OP::Plus, AtomicExp::Num(1)));
                        wip_elems.push(BlockElem::AssignOp(tmp_var.clone(), idx, OP::Times, AtomicExp::Num(8)));
                        wip_elems.push(BlockElem::St(id_to_var(&id), AtomicExp::Ident(tmp_var), exp_to_atomic(&e)))
                    },
                }
            }
            Stmt::Assign(id, es, e) => panic!("multi dimensional array"),
            Stmt::Stmts(ss) => {
                blocks.push(make_cfg_entry(wip_bnum, wip_elems, NextBlock::Next(*block_num)));
                let stmt_blk = *block_num;
                *block_num += 1;
                wip_bnum = *block_num;
                *block_num += 1;
                let mut sub_blocks = build_cfg(ss, block_num, stmt_blk, &NextBlock::Next(wip_bnum));
                blocks.append(&mut sub_blocks);
                wip_elems = Vec::new();
            },
            Stmt::DoWhile(s0, e, s1) => {
                let s0_b = *block_num;
                *block_num += 1;
                let s1_b = *block_num;
                *block_num += 1;
                blocks.push(make_cfg_entry(wip_bnum, wip_elems, NextBlock::Next(s0_b)));
                wip_bnum = *block_num;
                *block_num += 1;
                let mut header = if let Stmt::Stmts(stmts0) = &**s0 {
                    build_cfg(&stmts0, block_num, s0_b, &NextBlock::Branch(flat_exp_to_test(&e.exp), s1_b, wip_bnum))
                }
                else {
                    build_cfg(&(vec![*s0.clone()]), block_num, s0_b, &NextBlock::Branch(flat_exp_to_test(&e.exp), s1_b, wip_bnum))
                };

                let mut body = if let Stmt::Stmts(stmts1) = &**s1 {
                     build_cfg(&stmts1, block_num, s1_b, &NextBlock::Branch(flat_exp_to_test(&e.exp), s1_b, wip_bnum))
                }
                else {
                    build_cfg(&(vec![*s1.clone()]), block_num, s1_b, &NextBlock::Branch(flat_exp_to_test(&e.exp), s1_b, wip_bnum))
                };
                
                blocks.append(&mut header);
                blocks.append(&mut body);
                wip_elems = Vec::new();

            },
            Stmt::Ite(e, s0, s1) => {
                let s0_b = *block_num;
                *block_num += 1;
                let s1_b = *block_num;
                *block_num += 1;
                blocks.push(make_cfg_entry(wip_bnum, wip_elems, NextBlock::Branch(flat_exp_to_test(&e.exp), s0_b, s1_b)));
                wip_bnum = *block_num;
                *block_num += 1;
                let mut then_blk = if let Stmt::Stmts(stmts0) = &**s0 {
                    build_cfg(&stmts0, block_num, s0_b, &NextBlock::Next(wip_bnum))
                }
                else {
                    build_cfg(&(vec![*s0.clone()]), block_num, s0_b, &NextBlock::Next(wip_bnum))
                };

                let mut else_blk = if let Stmt::Stmts(stmts1) = &**s1 {
                     build_cfg(&stmts1, block_num, s1_b, &NextBlock::Next(wip_bnum))
                }
                else {
                    build_cfg(&(vec![*s1.clone()]), block_num, s1_b, &NextBlock::Next(wip_bnum))
                };

                blocks.append(&mut then_blk);
                blocks.append(&mut else_blk);
                wip_elems = Vec::new();
                
            },
            Stmt::In(x) => {
                wip_elems.push(BlockElem::Call(Some(id_to_var(&x)), String::from("input"), Vec::new()));
            },
            Stmt::Out(x) => {
                wip_elems.push(BlockElem::Call(None, String::from("output"), vec![AtomicExp::Ident(id_to_var(&x))]));
            },
            Stmt::Loc(s0, _) => {
                let s0_b = *block_num;
                *block_num += 1;
                blocks.push(make_cfg_entry(wip_bnum, wip_elems, NextBlock::Next(s0_b)));
                wip_bnum = *block_num;
                *block_num += 1;
                let mut blk = build_cfg(&(vec![*s0.clone()]), block_num, s0_b, &NextBlock::Next(wip_bnum));
                blocks.append(&mut blk);
                wip_elems = Vec::new();
            },
            Stmt::Return(Some(id)) => {
                blocks.push(make_cfg_entry(wip_bnum, wip_elems, NextBlock::Return(Some(id_to_var(&id)))));
                wip_bnum = *block_num;
                *block_num += 1;
                wip_elems = Vec::new();
            }
            Stmt::Return(None) => {
                blocks.push(make_cfg_entry(wip_bnum, wip_elems, NextBlock::Return(None)));
                wip_bnum = *block_num;
                *block_num += 1;
                wip_elems = Vec::new();
            }

        }
    }
    blocks.push(make_cfg_entry(wip_bnum, wip_elems, next_block.clone()));
    
    blocks
}



fn clean_ast(cfg : Vec<CFGEntry>) -> Vec<CFGEntry> {
    cfg
}

pub fn ast_to_cfg(ast : &[Stmt]) -> Vec<CFGEntry> {
    let mut counter: usize = 1;
    let cfg = build_cfg(&ast, &mut counter, 0, &NextBlock::Return(None));
    println!("{}", counter);
    let cfg2 = clean_ast(cfg);
    println!("{}", cfg2.len());
    cfg2
}

