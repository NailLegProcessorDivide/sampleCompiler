use crate::block_structure::{BlockElem, CFGEntry};

fn shrink_imm_elem(e: BlockElem) -> Vec<BlockElem> {
    match e {
        BlockElem::AssignOp(_, _, _, _) => todo!(),
        BlockElem::AssignAtom(_, _) => todo!(),
        BlockElem::Ld(_, _, _) => todo!(),
        BlockElem::St(_, _, _) => todo!(),
        BlockElem::Call(_, _, _) => todo!(),
        BlockElem::BoundCheck(_, _) => todo!(),
        BlockElem::NullCheck(_) => todo!(),
    }
}

pub fn shrink_imm(cfg: Vec<CFGEntry>) -> Vec<CFGEntry> {
    let mut new_cfg = Vec::new();
    for entry in cfg {
        let elems = entry
            .elems
            .iter()
            .map(|x| shrink_imm_elem(x.clone()))
            .flatten()
            .collect();

        let new_ent = CFGEntry {
            bnum: entry.bnum,
            elems: elems,
            next: entry.next.clone(),
            started: false,
            finished: false,
        };
        new_cfg.push(new_ent);
    }
    new_cfg
}
