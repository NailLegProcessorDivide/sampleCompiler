// use crate::block_structure::CFGEntry;

// pub enum Instruction_aa64 {

// }

// enum Condition{
//     L,
//     LE,
//     G,
//     GE,
//     E,
//     NE,
// }

// enum AsmTransition_aa64<'a> {
//     Cont,
//     J(usize),
//     JCCont(Condition, CmpArgs<'a>),
//     JC(Condition, CmpArgs<'a>, usize),
//     RetV(&'a Var),
//     Ret,
// }

// fn compile_block<'a>((block, trans) : &(&'a CFGEntry, AsmTransition)) -> Vec<Instruction_aa64<'a>> {
//     let mut insts = block.elems.iter().map(|elem| compile_elem(elem)).flatten().collect();
//     insts
// }

// fn order_cfg<'a>(cfg : &[&'a CFGEntry]) -> Vec<(&'a CFGEntry, AsmTransition_aa64<'a>)> {
//     cfg.iter().map(|e| (*e, match &e.next {
//         NextBlock::Return(Some(v)) => AsmTransition_x64::RetV(v),
//         NextBlock::Next(b) => AsmTransition_x64::J(*b),
//         NextBlock::Return(None) => AsmTransition_x64::Ret,
//         NextBlock::Branch(Test { exp1, op: TestOp::Lt, exp2}, b1, b2) => AsmTransition_x64::JC(Condition::L, CmpArgs {a: exp1, b: exp2, target: *b1}, *b2),
//         NextBlock::Branch(Test { exp1, op: TestOp::Eq, exp2}, b1, b2) => AsmTransition_x64::JC(Condition::E, CmpArgs {a: exp1, b: exp2, target: *b1}, *b2),
//         NextBlock::Branch(Test { exp1, op: TestOp::Gt, exp2}, b1, b2) => AsmTransition_x64::JC(Condition::G, CmpArgs {a: exp1, b: exp2, target: *b1}, *b2),
//     })).collect()
// }

// pub fn cfg_to_aa64<'a>(cfg : &[&'a CFGEntry]) -> Vec<Instruction_aa64<'a>> {
//     let block_order = order_cfg(cfg);
//     block_order.iter().map(|block| compile_block(block)).flatten().collect()
// }
