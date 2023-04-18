use crate::block_structure::CFGEntry;

pub fn reg_alloc(blocks: Vec<&mut CFGEntry>) -> (Vec<&mut CFGEntry>, u64) {
    (blocks, 0)
}
