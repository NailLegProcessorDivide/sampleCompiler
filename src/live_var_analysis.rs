use std::collections::HashSet;

use crate::{
    block_structure::{AtomicExp, BlockElem, CFGEntry, NextBlock, Test, Var},
    tokens::OP,
};

#[derive(Clone, Eq, PartialEq)]
pub struct CFGAnnot {
    pub gen: HashSet<Var>,
    pub kill: HashSet<Var>,
    pub live_in: HashSet<Var>,
    pub live_exit: HashSet<Var>,
}

impl CFGAnnot {
    fn new() -> CFGAnnot {
        CFGAnnot {
            gen: HashSet::new(),
            kill: HashSet::new(),
            live_in: HashSet::new(),
            live_exit: HashSet::new(),
        }
    }
}

fn add_gen(e: &AtomicExp, gen: &mut HashSet<Var>) {
    if let AtomicExp::Ident(v) = e {
        gen.insert(v.clone());
    }
}

fn analyse_block(b: &Vec<BlockElem>) -> CFGAnnot {
    let mut out = CFGAnnot::new();
    //go from top to bottom of block
    for e in b.iter().rev() {
        match e {
            BlockElem::AssignOp(i, a1, _, a2) => {
                out.gen.remove(i);
                add_gen(&a1, &mut out.gen);
                add_gen(&a2, &mut out.gen);
                out.kill.insert(i.clone());
            }
            BlockElem::AssignAtom(i, a) => {
                out.gen.remove(i);
                add_gen(&a, &mut out.gen);
                out.kill.insert(i.clone());
            }
            BlockElem::Ld(i, j, a) => {
                out.gen.remove(i);
                add_gen(&a, &mut out.gen);
                out.gen.insert(j.clone());
                out.kill.insert(i.clone());
            }
            BlockElem::St(i, a1, a2) => {
                out.gen.remove(i);
                add_gen(&a1, &mut out.gen);
                add_gen(&a2, &mut out.gen);
            }
            BlockElem::Call(res, _, aes) => {
                if let Some(r) = res {
                    out.gen.remove(r);
                    out.kill.insert(r.clone());
                }
                for a in aes {
                    add_gen(a, &mut out.gen);
                }
            }
            BlockElem::BoundCheck(a1, a2) => {
                add_gen(&a1, &mut out.gen);
                add_gen(&a2, &mut out.gen);
            }
            BlockElem::NullCheck(v) => {
                out.gen.insert(v.clone());
            }
        }
    }
    out
}

fn init_live_exit(globals: &HashSet<Var>, mut a: CFGAnnot, next: NextBlock) -> CFGAnnot {
    match next {
        NextBlock::Return(r) => {
            a.live_exit = globals.clone();
            if let Some(rv) = r {
                a.live_exit.insert(rv);
            }
            a
        }
        NextBlock::Next(_) => a,
        NextBlock::Branch(_, _, _) => a,
    }
}

//
fn find_preds<'a, 'b>(
    bnum: usize,
    cfg: &'a Vec<&(&'b CFGEntry, CFGAnnot)>,
) -> (
    Vec<(&'b CFGEntry, &'a CFGAnnot)>,
    Vec<(&'b CFGEntry, &'a CFGAnnot)>,
) {
    cfg.iter()
        .map(|(e, a)| (*e, a))
        .partition(|(e, _)| match e.next {
            NextBlock::Return(_) => true,
            NextBlock::Next(i) => i == bnum,
            NextBlock::Branch(_, i1, i2) => bnum == i1 || bnum == i2,
        })
}

fn find_block<'a, 'b>(
    bnum: usize,
    cfg: &'a Vec<(&'b CFGEntry, CFGAnnot)>,
) -> (&'b CFGEntry, &'a CFGAnnot) {
    for (blk, anot) in cfg {
        if (*blk).bnum == bnum {
            return (*blk, anot);
        }
    }
    panic!("searching for non existent block '{bnum}'")
}

fn find_block_mut<'a, 'b>(
    bnum: usize,
    cfg: &'a mut Vec<(&'b CFGEntry, CFGAnnot)>,
) -> (&'b CFGEntry, &'a mut CFGAnnot) {
    for (blk, anot) in cfg {
        if (*blk).bnum == bnum {
            return (*blk, anot);
        }
    }
    panic!("searching for non existent block '{bnum}'")
}

fn get_block_preds<'a, 'b>(
    bnum: usize,
    cfg: &'a Vec<(&'b CFGEntry, CFGAnnot)>,
) -> Vec<(&'b CFGEntry, &'a CFGAnnot)> {
    cfg.iter()
        .map(|(e, a)| (*e, a))
        .filter(|(e, a)| match e.next {
            NextBlock::Return(_) => false,
            NextBlock::Next(i) => i == bnum,
            NextBlock::Branch(_, i1, i2) => bnum == i1 || bnum == i2,
        })
        .collect::<Vec<_>>()
}

fn proc<'a, 'b>(
    mut cfg: Vec<(&'a CFGEntry, CFGAnnot)>,
    globals: &'b HashSet<Var>,
) -> Vec<(&'a CFGEntry, CFGAnnot)> {
    let mut worklist = cfg
        .iter()
        .filter(|(blk, _)| matches!((*blk).next, NextBlock::Return(_)))
        .map(|(e, a)| (*e).bnum)
        .collect::<Vec<_>>();
    while worklist.len() > 0 {
        let bnum = worklist.pop().unwrap();
        let blk = find_block(bnum, &cfg);
        let out = match &blk.0.next {
            NextBlock::Return(_) => globals.clone(),
            NextBlock::Next(n) => find_block(*n, &cfg).1.live_in.clone(),
            NextBlock::Branch(_, n1, n2) => find_block(*n1, &cfg)
                .1
                .live_in
                .union(&find_block(*n2, &cfg).1.live_in)
                .cloned()
                .collect(),
        };
        let lin = find_block(bnum, &cfg)
            .1
            .gen
            .union(&(&blk.1.live_exit - &blk.1.kill))
            .cloned()
            .collect::<HashSet<_>>();
        if lin != blk.1.live_in {
            find_block_mut(bnum, &mut cfg).1.live_in = lin;
            for (e, _) in get_block_preds(bnum, &cfg) {
                worklist.push(e.bnum)
            }
        }
        find_block_mut(bnum, &mut cfg).1.live_exit = out;
    }
    cfg
}

fn pure_op(op: OP) -> bool {
    op == OP::Div
}

pub fn lva<'a>(globals: &HashSet<Var>, cfg: &'a Vec<CFGEntry>) -> Vec<(&'a CFGEntry, CFGAnnot)> {
    let init_list: Vec<(&CFGEntry, CFGAnnot)> = cfg
        .iter()
        .map(|e| {
            (
                e,
                init_live_exit(globals, analyse_block(&e.elems), e.next.clone()),
            )
        })
        .collect();
    proc(init_list, globals)
}

fn remove_unuesd_writes_block(entry: &mut CFGEntry, annot: &CFGAnnot) {
    let mut live = annot.live_exit.clone();
    let mut to_delete = Vec::new();
    for (index, elem) in entry.elems.iter().enumerate().rev() {
        match elem {
            BlockElem::AssignOp(i, a1, op, a2) if live.contains(i) || !pure_op(*op) => {
                live.remove(i);
                add_gen(a1, &mut live);
                add_gen(a2, &mut live);
            }
            BlockElem::AssignAtom(i, a) if live.contains(i) => {
                live.remove(i);
                add_gen(a, &mut live);
            }
            BlockElem::Ld(i, j, a) if live.contains(i) => {
                live.remove(i);
                live.insert(j.clone());
                add_gen(a, &mut live);
            }
            BlockElem::St(i, a1, a2) => {
                live.insert(i.clone());
                add_gen(a1, &mut live);
                add_gen(a2, &mut live);
            }
            BlockElem::Call(i, _, aes) => {
                if let Some(v) = i {
                    live.remove(v);
                }
                for a in aes {
                    add_gen(a, &mut live);
                }
            }
            BlockElem::BoundCheck(a1, a2) => {
                add_gen(a1, &mut live);
                add_gen(a2, &mut live);
            }
            BlockElem::NullCheck(v) => {
                live.insert(v.clone());
            }
            _ => {
                to_delete.push(index);
            }
        }
    }
    //O(N^2) but blocks shouldnt be too large
    for index in to_delete {
        entry.elems.remove(index);
    }
}

pub fn remove_unuesd_writes<'a>(cfg: Vec<(&'a mut CFGEntry, &'a CFGAnnot)>) {
    for (entry, annot) in cfg {
        remove_unuesd_writes_block(entry, annot);
    }
}
