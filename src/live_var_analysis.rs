use std::collections::HashSet;

use crate::block_structure::{AtomicExp, BlockElem, CFGEntry, NextBlock, Test, Var};

#[derive(Clone, Eq, PartialEq)]
pub struct CFGAnnot {
    pub gen: HashSet<Var>,
    pub kill: HashSet<Var>,
    pub live_exit: HashSet<Var>,
}

fn add_gen(e: &AtomicExp, gen: &mut HashSet<Var>) {
    if let AtomicExp::Ident(v) = e {
        gen.insert(v.clone());
    }
}

fn analyse_block(b: &Vec<BlockElem>) -> CFGAnnot {
    let mut out = CFGAnnot {
        gen: HashSet::new(),
        kill: HashSet::new(),
        live_exit: HashSet::new(),
    };
    for e in b {
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
        NextBlock::Branch(Test { exp1, op: _, exp2 }, _, _) => {
            add_gen(&exp1, &mut a.live_exit);
            add_gen(&exp2, &mut a.live_exit);
            a
        }
    }
}

fn find_preds<'a, 'b>(
    bnum: usize,
    cfg: &'a Vec<(&'b CFGEntry, CFGAnnot)>,
) -> (Vec<(&'b CFGEntry, CFGAnnot)>, Vec<(&'b CFGEntry, CFGAnnot)>) {
    cfg.iter().cloned().partition(|(e, _)| match e.next {
        NextBlock::Return(_) => true,
        NextBlock::Next(i) => i == bnum,
        NextBlock::Branch(_, i1, i2) => bnum == i1 || bnum == i2,
    })
}

fn do_one(cfg: Vec<(&CFGEntry, CFGAnnot)>) -> Vec<(&CFGEntry, CFGAnnot)> {
    let mut worklist = cfg;
    let mut finished = Vec::new();
    while worklist.len() > 0 {
        let (entry, annot) = worklist.pop().unwrap();
        let live_entry = annot.gen.union(&(&annot.live_exit - &annot.kill));
        let (updates, worklist) = find_preds(entry.bnum, &worklist);
        finished.push((entry, annot));
        let (possible_updates, finished) = find_preds(entry.bnum, &finished);
    }
    finished
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
    do_one(init_list)
}
