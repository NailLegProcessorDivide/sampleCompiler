use std::fmt;

use regex::internal::Inst;

use crate::{
    block_structure::{AtomicExp, BlockElem, CFGEntry, NextBlock, Test, TestOp, Var},
    source_ast::scope_to_string,
    tokens::{show_op, OP, UOP},
};

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Reg {
    RDI,
    RSI,
    RDX,
    RCX,
    R8,
    R9,
    RBX,
    R12,
    R13,
    R14,
    R15,
    RBP,
    R10,
    R11,
    RAX,
    RSP,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Reg::RDI => "rdi",
                Reg::RSI => "rsi",
                Reg::RDX => "rdx",
                Reg::RCX => "rcx",
                Reg::R8 => "r8",
                Reg::R9 => "r9",
                Reg::RBX => "rbx",
                Reg::R12 => "r12",
                Reg::R13 => "r13",
                Reg::R14 => "r14",
                Reg::R15 => "r15",
                Reg::RBP => "rbp",
                Reg::R10 => "r10",
                Reg::R11 => "r11",
                Reg::RAX => "rax",
                Reg::RSP => "rsp",
            }
        )
    }
}

const reg_order: [Reg; 15] = [
    Reg::RDI,
    Reg::RSI,
    Reg::RDX,
    Reg::RCX,
    Reg::R8,
    Reg::R9,
    Reg::RBX,
    Reg::R12,
    Reg::R13,
    Reg::R14,
    Reg::R15,
    Reg::RBP,
    Reg::R10,
    Reg::R11,
    Reg::RAX,
];
const temp0: Reg = Reg::R10;
const temp1: Reg = Reg::R11;
const temp2: Reg = Reg::RAX;

fn isize_to_i32(i: isize) -> i32 {
    if i >= (i32::MIN as isize) && i <= (i32::MAX as isize) {
        i as i32
    } else {
        todo!()
    }
}

pub enum Displacement {
    ConcreteDisp(i64),
    LabelDisp(String),
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum RegGlob<'a> {
    VReg(Reg),
    VGlob(&'a str),
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Addr<'a> {
    base: RegGlob<'a>,
    index: Option<(Reg, u8)>,
    disp: i32,
}

impl fmt::Display for Addr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let p1 = match self.base {
            RegGlob::VReg(r) => format!("QWORD [{}", r),
            RegGlob::VGlob(g) => format!("QWORD [{}", g),
        };
        let p2 = if let Some((r, s)) = self.index {
            format!(" + {} * {}", r, s)
        } else {
            "".to_string()
        };
        write!(f, "{}{} + {}]", p1, p2, self.disp)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum RegAddr<'a> {
    Reg(Reg),
    Addr(Addr<'a>),
}

impl fmt::Display for RegAddr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegAddr::Reg(r) => write!(f, "{}", r),
            RegAddr::Addr(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum RegAddrImm64<'a> {
    Reg(Reg),
    Addr(Addr<'a>),
    Imm(i64),
}

impl fmt::Display for RegAddrImm64<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegAddrImm64::Reg(r) => write!(f, "{}", r),
            RegAddrImm64::Addr(a) => write!(f, "{}", a),
            RegAddrImm64::Imm(i) => write!(f, "{}", i),
        }
    }
}

pub enum RegImm64 {
    Reg(Reg),
    Imm(i64),
}

impl fmt::Display for RegImm64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegImm64::Reg(r) => write!(f, "{}", r),
            RegImm64::Imm(i) => write!(f, "{}", i),
        }
    }
}

pub enum RegAddrImm32<'a> {
    Reg(Reg),
    Addr(Addr<'a>),
    Imm(i32),
}

impl fmt::Display for RegAddrImm32<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegAddrImm32::Reg(r) => write!(f, "{}", r),
            RegAddrImm32::Addr(a) => write!(f, "{}", a),
            RegAddrImm32::Imm(i) => write!(f, "{}", i),
        }
    }
}

pub enum RegImm32 {
    Reg(Reg),
    Imm(i32),
}

impl fmt::Display for RegImm32 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RegImm32::Reg(r) => write!(f, "{}", r),
            RegImm32::Imm(i) => write!(f, "{}", i),
        }
    }
}

pub enum Binop<'a> {
    RR(RegAddr<'a>, RegImm32),
    RA(Reg, Addr<'a>),
}

impl fmt::Display for Binop<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Binop::RR(r1, r2) => write!(f, "{}, {}", r1, r2),
            Binop::RA(r, a) => write!(f, "{}, {}", r, a),
        }
    }
}

pub enum Movop<'a> {
    RR(RegAddr<'a>, RegImm64),
    RA(Reg, Addr<'a>),
}

impl fmt::Display for Movop<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Movop::RR(r1, r2) => write!(f, "{}, {}", r1, r2),
            Movop::RA(r, a) => write!(f, "{}, {}", r, a),
        }
    }
}

pub enum Shiftop<'a> {
    ACL(RegAddr<'a>),
    AI(RegAddr<'a>, u8),
}

impl fmt::Display for Shiftop<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Shiftop::ACL(r) => write!(f, "{}, cl", r),
            Shiftop::AI(r, i) => write!(f, "{}, {}", r, i),
        }
    }
}

fn cfg_to_real_stack_index(i: isize) -> i32 {
    let ind = if i >= 0 {
        i
    } else {
        i - 2 //skip stack frame for stack parameters
    } * 8;
    isize_to_i32(ind)
}

//binary instruction
pub enum BinInst {
    Add,
    And,
    Cmp,
    Imul,
    Or,
    Sub,
    Xor,
}

impl fmt::Display for BinInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinInst::Add => "add",
                BinInst::And => "and",
                BinInst::Cmp => "cmp",
                BinInst::Imul => "imul",
                BinInst::Or => "or",
                BinInst::Sub => "sub",
                BinInst::Xor => "xor",
            }
        )
    }
}

pub enum UnInst {
    Inc,
    Dec,
}

impl fmt::Display for UnInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnInst::Inc => "inc",
                UnInst::Dec => "dec",
            }
        )
    }
}

pub enum ShiftInst {
    Shl,
    Sar, //no need for unsigned right
}

impl fmt::Display for ShiftInst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ShiftInst::Shl => "shl",
                ShiftInst::Sar => "sar",
            }
        )
    }
}

pub enum Instruction_x64<'a> {
    ZLabel(usize),
    ZBinInst(BinInst, Binop<'a>),
    ZShift(ShiftInst, Shiftop<'a>),
    Lea(Reg, Addr<'a>),
    ZUnInst(UnInst, RegAddr<'a>),
    Zmov(Movop<'a>),
    Call(&'a str),
    Push(Reg),
    Pop(Reg),
    Branch(Condition, usize),
    Jmp(usize),
    Xchg(Reg, Reg),
    Ret,
}

enum JmpX64<'a> {
    J(usize),
    JC(Condition, CmpArgs<'a>),
}

enum X64BlockElem<'a> {
    Label(usize),
    Binop(OP, RegAddr<'a>, RegAddrImm64<'a>),
    Trinop(OP, RegAddr<'a>, RegAddrImm64<'a>, RegAddrImm64<'a>),
    Monop(UOP, RegAddr<'a>),
    Mov(RegAddr<'a>, RegAddrImm64<'a>),
    BoundCheck(RegAddrImm64<'a>, RegAddr<'a>),
    Branch(JmpX64<'a>),
    Call(&'a str),
    Push(RegAddrImm64<'a>),
    Pop(Reg),
    Ret,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct CmpArgs<'a> {
    a: RegAddrImm64<'a>,
    b: RegAddrImm64<'a>,
    target: usize,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Condition {
    L,
    LE,
    G,
    GE,
    E,
    NE,
}

fn cond_to_branch(c: Condition) -> &'static str {
    match c {
        Condition::L => "jl",
        Condition::LE => "jle",
        Condition::G => "jg",
        Condition::GE => "jge",
        Condition::E => "je",
        Condition::NE => "jne",
    }
}

enum AsmTransitionX64<'a> {
    Cont,
    J(usize),
    JCCont(Condition, CmpArgs<'a>),
    JC(Condition, CmpArgs<'a>, usize),
    RetV(&'a Var),
    Ret,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum OpType {
    Binop,
    Shift,
    Div,
}

fn inst_to_string(inst: &Instruction_x64) -> String {
    match inst {
        Instruction_x64::ZLabel(i) => format!(".block{}:", i),
        Instruction_x64::ZBinInst(b, a) => format!("  {} {}", b, a),
        Instruction_x64::ZShift(s, a) => format!("  {} {}", s, a),
        Instruction_x64::Lea(r, a) => format!("  lea {}, {}", r, a),
        Instruction_x64::ZUnInst(u, r) => format!("  {} {}", u, r),
        Instruction_x64::Zmov(r) => format!("  mov {}", r),
        Instruction_x64::Call(f) => format!("  call {}", f),
        Instruction_x64::Push(r) => format!("  push {}", r),
        Instruction_x64::Pop(r) => format!("  pop {}", r),
        Instruction_x64::Branch(c, t) => format!("  {} .block{}", cond_to_branch(*c), t),
        Instruction_x64::Jmp(t) => format!("  jmp .block{}", t),
        Instruction_x64::Xchg(r1, r2) => format!("  xchg {}, {}", r1, r2),
        Instruction_x64::Ret => "  ret".to_string(),
    }
}

pub fn asm_to_string(asm: &[Instruction_x64]) -> String {
    asm.iter()
        .map(|inst| inst_to_string(inst) + "\n")
        .collect::<Vec<_>>()
        .join("")
}

fn op_to_inst_type(op: OP) -> OpType {
    match op {
        OP::Plus | OP::Minus | OP::Times | OP::BitOr | OP::BitAnd => OpType::Binop,
        OP::Div => OpType::Div,
        OP::Lshift => OpType::Shift,
        _ => panic!("unsupported op in CFG {}", show_op(&op)),
    }
}

fn op_to_bininst(op: OP) -> BinInst {
    match op {
        OP::Plus => BinInst::Add,
        OP::Minus => BinInst::Sub,
        OP::Times => BinInst::Imul,
        OP::BitOr => BinInst::Or,
        OP::BitAnd => BinInst::And,
        _ => panic!("unsupported op {}", show_op(&op)),
    }
}

fn var_to_reg_addr(v: &Var) -> RegAddr {
    match v {
        Var::Vreg(r) => RegAddr::Reg(reg_order[*r]),
        Var::Stack(i) => RegAddr::Addr(Addr {
            base: RegGlob::VReg(Reg::RSP),
            index: None,
            disp: cfg_to_real_stack_index(*i),
        }),
        Var::Global(g) => RegAddr::Addr(Addr {
            base: RegGlob::VGlob(g),
            index: None,
            disp: 0,
        }),
        _ => panic!(""),
    }
}

fn var_to_reg_addr_imm64(v: &Var) -> RegAddrImm64 {
    match v {
        Var::Vreg(r) => RegAddrImm64::Reg(reg_order[*r]),
        Var::Stack(i) => RegAddrImm64::Addr(Addr {
            base: RegGlob::VReg(Reg::RSP),
            index: None,
            disp: cfg_to_real_stack_index(*i),
        }),
        Var::Global(g) => RegAddrImm64::Addr(Addr {
            base: RegGlob::VGlob(g),
            index: None,
            disp: 0,
        }),
        _ => panic!(""),
    }
}

fn ae_to_reg_addr_imm64(ae: &AtomicExp) -> RegAddrImm64 {
    match ae {
        AtomicExp::Ident(Var::Vreg(r)) => RegAddrImm64::Reg(reg_order[*r]),
        AtomicExp::Ident(Var::Global(g)) => RegAddrImm64::Addr(Addr {
            base: RegGlob::VGlob(g),
            index: None,
            disp: 0,
        }),
        AtomicExp::Ident(Var::Stack(i)) => RegAddrImm64::Addr(Addr {
            base: RegGlob::VReg(Reg::RSP),
            index: None,
            disp: cfg_to_real_stack_index(*i),
        }),
        AtomicExp::Num(n) => RegAddrImm64::Imm(*n),
        AtomicExp::Ident(Var::NamedSource(s, sc)) => panic!(
            "untransformed variable layout ns {} {}",
            s,
            scope_to_string(&Some(*sc))
        ),
        AtomicExp::Ident(Var::NamedTmp(s, sz)) => {
            panic!("untransformed variable layout nt {} {}", s, *sz)
        }
        _ => panic!("untransformed variable layout"),
    }
}

fn order_cfg<'a>(cfg: &[&'a CFGEntry]) -> Vec<(&'a CFGEntry, AsmTransitionX64<'a>)> {
    cfg.iter()
        .map(|e| {
            (
                *e,
                match &e.next {
                    NextBlock::Return(Some(v)) => AsmTransitionX64::RetV(v),
                    NextBlock::Next(b) => AsmTransitionX64::J(*b),
                    NextBlock::Return(None) => AsmTransitionX64::Ret,
                    NextBlock::Branch(
                        Test {
                            exp1,
                            op: TestOp::Lt,
                            exp2,
                        },
                        b1,
                        b2,
                    ) => AsmTransitionX64::JC(
                        Condition::L,
                        CmpArgs {
                            a: ae_to_reg_addr_imm64(exp1),
                            b: ae_to_reg_addr_imm64(exp2),
                            target: *b1,
                        },
                        *b2,
                    ),
                    NextBlock::Branch(
                        Test {
                            exp1,
                            op: TestOp::Eq,
                            exp2,
                        },
                        b1,
                        b2,
                    ) => AsmTransitionX64::JC(
                        Condition::E,
                        CmpArgs {
                            a: ae_to_reg_addr_imm64(exp1),
                            b: ae_to_reg_addr_imm64(exp2),
                            target: *b1,
                        },
                        *b2,
                    ),
                    NextBlock::Branch(
                        Test {
                            exp1,
                            op: TestOp::Gt,
                            exp2,
                        },
                        b1,
                        b2,
                    ) => AsmTransitionX64::JC(
                        Condition::G,
                        CmpArgs {
                            a: ae_to_reg_addr_imm64(exp1),
                            b: ae_to_reg_addr_imm64(exp2),
                            target: *b1,
                        },
                        *b2,
                    ),
                },
            )
        })
        .collect()
}

fn push_adjust_reg_addr_imm64(rai: RegAddrImm64, pushes: usize) -> RegAddrImm64 {
    match rai {
        RegAddrImm64::Addr(Addr { base: RegGlob::VReg(Reg::RSP), index, disp }) => {
            RegAddrImm64::Addr(Addr { base: RegGlob::VReg(Reg::RSP), index, disp: disp + (pushes * 8) as i32})
        },
        _ => rai,
    }
}

fn cfg_block_to_pres<'a>(
    cfg: &'a CFGEntry,
    transition: &AsmTransitionX64<'a>,
    stack_space: usize,
) -> Vec<X64BlockElem<'a>> {
    let mut block: Vec<X64BlockElem<'a>> = Vec::new();
    block.push(X64BlockElem::Label(cfg.bnum));
    if cfg.bnum == 0 {
        block.push(X64BlockElem::Push(RegAddrImm64::Reg(Reg::RBX)));
        block.push(X64BlockElem::Push(RegAddrImm64::Reg(Reg::R12)));
        block.push(X64BlockElem::Push(RegAddrImm64::Reg(Reg::R13)));
        block.push(X64BlockElem::Push(RegAddrImm64::Reg(Reg::R14)));
        block.push(X64BlockElem::Push(RegAddrImm64::Reg(Reg::R15)));
        block.push(X64BlockElem::Binop(
            OP::Minus,
            RegAddr::Reg(Reg::RSP),
            RegAddrImm64::Imm((stack_space * 8 + 8) as i64),
        ));
    }
    let mut elems = &cfg.elems[..];
    while elems.len() > 0 {
        match elems {
            [BlockElem::AssignOp(a, AtomicExp::Ident(b), op, c), es @ ..] if a == b => {
                block.push(X64BlockElem::Binop(
                    *op,
                    var_to_reg_addr(a),
                    ae_to_reg_addr_imm64(c),
                ));
                elems = es;
            }
            [BlockElem::AssignOp(a, b, op, c), es @ ..] => {
                block.push(X64BlockElem::Trinop(
                    *op,
                    var_to_reg_addr(a),
                    ae_to_reg_addr_imm64(b),
                    ae_to_reg_addr_imm64(c),
                ));
                elems = es;
            }
            [BlockElem::AssignAtom(v, ae), es @ ..] => {
                block.push(X64BlockElem::Mov(
                    var_to_reg_addr(v),
                    ae_to_reg_addr_imm64(ae),
                ));
                elems = es;
            }
            [BlockElem::Ld(v, v2, index), es @ ..] => {
                block.push(X64BlockElem::Trinop(
                    OP::Plus,
                    RegAddr::Reg(temp0),
                    var_to_reg_addr_imm64(v2),
                    ae_to_reg_addr_imm64(index),
                ));
                block.push(X64BlockElem::Mov(
                    var_to_reg_addr(v),
                    RegAddrImm64::Addr(Addr {
                        base: RegGlob::VReg(temp0),
                        index: None,
                        disp: 0,
                    }),
                ));
                elems = es;
            }
            [BlockElem::St(v, index, o), es @ ..] => {
                block.push(X64BlockElem::Trinop(
                    OP::Plus,
                    RegAddr::Reg(temp0),
                    var_to_reg_addr_imm64(v),
                    ae_to_reg_addr_imm64(index),
                ));
                block.push(X64BlockElem::Mov(
                    RegAddr::Addr(Addr {
                        base: RegGlob::VReg(temp0),
                        index: None,
                        disp: 0,
                    }),
                    ae_to_reg_addr_imm64(o),
                ));
                elems = es;
            }
            [BlockElem::Call(rv, func, params), es @ ..] => {
                let saved_regs = 6;
                for i in 0..saved_regs {
                    block.push(X64BlockElem::Push(RegAddrImm64::Reg(reg_order[i])));
                }
                let mut pushes = saved_regs;
                if params.len() > saved_regs && params.len() & 1 == 1 {
                    block.push(X64BlockElem::Binop(
                        OP::Minus,
                        RegAddr::Reg(Reg::RSP),
                        RegAddrImm64::Imm(8),
                    ));
                    pushes += 1;
                }
                if params.len() > saved_regs {
                    for i in 0..(params.len() - saved_regs) {
                        block.push(X64BlockElem::Push(push_adjust_reg_addr_imm64(ae_to_reg_addr_imm64(
                            &params[params.len() - i - 1],
                        ), pushes)));
                        pushes += 1;
                    }
                }
                for (index, param) in params.iter().enumerate() {
                    if index < saved_regs {
                        block.push(X64BlockElem::Mov(
                            RegAddr::Reg(reg_order[index]),
                            push_adjust_reg_addr_imm64(ae_to_reg_addr_imm64(param), pushes),
                        ));
                    }
                }
                block.push(X64BlockElem::Call(func));
                if params.len() > saved_regs {
                    block.push(X64BlockElem::Binop(
                        OP::Plus,
                        RegAddr::Reg(Reg::RSP),
                        RegAddrImm64::Imm((((params.len() - saved_regs + 1) / 2) * 2) as i64),
                    ));
                }
                for i in 0..saved_regs {
                    block.push(X64BlockElem::Pop(reg_order[saved_regs - i - 1]));
                }
                if let Some(r) = rv {
                    block.push(X64BlockElem::Mov(
                        var_to_reg_addr(r),
                        RegAddrImm64::Reg(Reg::RAX),
                    ));
                }
                elems = es;
            }
            [BlockElem::BoundCheck(_, _), es @ ..] => {
                elems = es;
            }
            [BlockElem::NullCheck(_), es @ ..] => {
                elems = es;
            }
            [] => panic!("wat"),
        }
    }
    match transition {
        AsmTransitionX64::Cont => {}
        AsmTransitionX64::J(target) => {
            block.push(X64BlockElem::Branch(JmpX64::J(*target)));
        }
        AsmTransitionX64::JCCont(condition, cmp) => {
            block.push(X64BlockElem::Branch(JmpX64::JC(*condition, *cmp)));
        }
        AsmTransitionX64::JC(condition, cmp, target) => {
            block.push(X64BlockElem::Branch(JmpX64::JC(*condition, *cmp)));
            block.push(X64BlockElem::Branch(JmpX64::J(*target)));
        }
        AsmTransitionX64::RetV(val) => {
            block.push(X64BlockElem::Mov(
                RegAddr::Reg(Reg::RAX),
                var_to_reg_addr_imm64(*val),
            ));
            block.push(X64BlockElem::Binop(
                OP::Plus,
                RegAddr::Reg(Reg::RSP),
                RegAddrImm64::Imm((stack_space * 8 + 8) as i64),
            ));
            block.push(X64BlockElem::Pop(Reg::R15));
            block.push(X64BlockElem::Pop(Reg::R14));
            block.push(X64BlockElem::Pop(Reg::R13));
            block.push(X64BlockElem::Pop(Reg::R12));
            block.push(X64BlockElem::Pop(Reg::RBX));
            block.push(X64BlockElem::Ret);
        }
        AsmTransitionX64::Ret => {
            block.push(X64BlockElem::Binop(
                OP::Plus,
                RegAddr::Reg(Reg::RSP),
                RegAddrImm64::Imm((stack_space * 8 + 8) as i64),
            ));
            block.push(X64BlockElem::Pop(Reg::R15));
            block.push(X64BlockElem::Pop(Reg::R14));
            block.push(X64BlockElem::Pop(Reg::R13));
            block.push(X64BlockElem::Pop(Reg::R12));
            block.push(X64BlockElem::Pop(Reg::RBX));
            block.push(X64BlockElem::Ret);
        }
    };
    block
}

fn cfg_to_pres<'a>(
    cfg: &[(&'a CFGEntry, AsmTransitionX64<'a>)],
    stack_space: usize,
) -> Vec<X64BlockElem<'a>> {
    cfg.iter()
        .map(|(elem, trans)| cfg_block_to_pres(*elem, trans, stack_space))
        .flatten()
        .collect()
}

fn compile_block_elem<'a>(elem: &X64BlockElem<'a>) -> Vec<Instruction_x64<'a>> {
    match elem {
        X64BlockElem::Label(ind) => vec![Instruction_x64::ZLabel(*ind)],
        X64BlockElem::Binop(op, v1, v2) => match op_to_inst_type(*op) {
            OpType::Binop => match (v1, v2) {
                (RegAddr::Reg(r1), RegAddrImm64::Reg(r2)) => {
                    vec![Instruction_x64::ZBinInst(
                        op_to_bininst(*op),
                        Binop::RR(RegAddr::Reg(*r1), RegImm32::Reg(*r2)),
                    )]
                }
                (RegAddr::Reg(r), RegAddrImm64::Addr(a)) => {
                    vec![Instruction_x64::ZBinInst(
                        op_to_bininst(*op),
                        Binop::RA(*r, *a),
                    )]
                }
                (RegAddr::Reg(r), RegAddrImm64::Imm(i)) => {
                    if *i as i32 as i64 == *i {
                        vec![Instruction_x64::ZBinInst(
                            op_to_bininst(*op),
                            Binop::RR(RegAddr::Reg(*r), RegImm32::Imm(*i as i32)),
                        )]
                    } else {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Imm(*i),
                            )),
                            Instruction_x64::ZBinInst(
                                op_to_bininst(*op),
                                Binop::RR(RegAddr::Reg(*r), RegImm32::Reg(temp0)),
                            ),
                        ]
                    }
                }
                (RegAddr::Addr(a), RegAddrImm64::Reg(r)) => {
                    if op == &OP::Times {
                        vec![
                            Instruction_x64::Zmov(Movop::RA(temp0, *a)),
                            Instruction_x64::ZBinInst(
                                op_to_bininst(*op),
                                Binop::RR(RegAddr::Reg(temp0), RegImm32::Reg(*r))),
                            Instruction_x64::Zmov(Movop::RR(RegAddr::Addr(*a), RegImm64::Reg(temp0))),
                        ]
                    }
                    else {
                        vec![Instruction_x64::ZBinInst(
                            op_to_bininst(*op),
                            Binop::RR(RegAddr::Addr(*a), RegImm32::Reg(*r)),
                        )]
                    }
                }
                (RegAddr::Addr(a1), RegAddrImm64::Addr(a2)) => {
                    if op == &OP::Times {
                        vec![
                            Instruction_x64::Zmov(Movop::RA(temp0, *a1)),
                            Instruction_x64::ZBinInst(
                                op_to_bininst(*op),
                                Binop::RA(temp0, *a2),
                            ),
                            Instruction_x64::Zmov(Movop::RR(RegAddr::Addr(*a1), RegImm64::Reg(temp0))),
                        ]
                    }
                    else {
                        vec![
                            Instruction_x64::Zmov(Movop::RA(temp0, a2.clone())),
                            Instruction_x64::ZBinInst(
                                op_to_bininst(*op),
                                Binop::RR(RegAddr::Addr(a1.clone()), RegImm32::Reg(temp0)),
                            ),
                            Instruction_x64::Zmov(Movop::RR(RegAddr::Addr(a2.clone()), RegImm64::Reg(temp0)))
                        ]
                    }
                }
                (RegAddr::Addr(a), RegAddrImm64::Imm(i)) => {
                    if op == &OP::Times {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Imm(*i),
                            )),
                            Instruction_x64::Zmov(Movop::RA(
                                temp1,
                                *a,
                            )),
                            Instruction_x64::ZBinInst(
                                op_to_bininst(*op),
                                Binop::RR(RegAddr::Reg(temp1), RegImm32::Reg(temp0)),
                            ),
                            Instruction_x64::Zmov(Movop::RR(RegAddr::Addr(*a), RegImm64::Reg(temp1))),
                        ]
                    }
                    else {
                        if *i as i32 as i64 == *i {
                            vec![Instruction_x64::ZBinInst(
                                op_to_bininst(*op),
                                Binop::RR(RegAddr::Addr(*a), RegImm32::Imm(*i as i32)),
                            )]
                        } else {
                            vec![
                                Instruction_x64::Zmov(Movop::RR(
                                    RegAddr::Reg(temp0),
                                    RegImm64::Imm(*i),
                                )),
                                Instruction_x64::ZBinInst(
                                    op_to_bininst(*op),
                                    Binop::RR(RegAddr::Addr(*a), RegImm32::Reg(temp0)),
                                ),
                            ]
                        }
                    }
                }
            },
            OpType::Shift => match (v1, v2) {
                (RegAddr::Reg(r1), RegAddrImm64::Reg(r2)) => {
                    if r2 == &Reg::RCX {
                        vec![Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::ACL(RegAddr::Reg(*r1)),
                        )]
                    } else if r1 == &Reg::RCX {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Reg(*r1),
                            )),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(Reg::RCX),
                                RegImm64::Reg(*r2),
                            )),
                            Instruction_x64::ZShift(
                                ShiftInst::Shl,
                                Shiftop::ACL(RegAddr::Reg(temp0)),
                            ),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Reg(*r1),
                            )),
                        ]
                    } else {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Reg(Reg::RCX),
                            )),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(Reg::RCX),
                                RegImm64::Reg(*r2),
                            )),
                            Instruction_x64::ZShift(
                                ShiftInst::Shl,
                                Shiftop::ACL(RegAddr::Reg(*r1)),
                            ),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(Reg::RCX),
                                RegImm64::Reg(temp0),
                            )),
                        ]
                    }
                }
                (RegAddr::Reg(r), RegAddrImm64::Addr(a)) => {
                    if r == &Reg::RCX {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Reg(*r),
                            )),
                            Instruction_x64::Zmov(Movop::RA(Reg::RCX, *a)),
                            Instruction_x64::ZShift(
                                ShiftInst::Shl,
                                Shiftop::ACL(RegAddr::Reg(temp0)),
                            ),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(*r),
                                RegImm64::Reg(temp0),
                            )),
                        ]
                    } else {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Reg(Reg::RCX),
                            )),
                            Instruction_x64::Zmov(Movop::RA(Reg::RCX, *a)),
                            Instruction_x64::ZShift(ShiftInst::Shl, Shiftop::ACL(RegAddr::Reg(*r))),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(Reg::RCX),
                                RegImm64::Reg(temp0),
                            )),
                        ]
                    }
                }
                (RegAddr::Reg(r), RegAddrImm64::Imm(i)) => {
                    if *i > 63 {
                        vec![Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(*r),
                            RegImm64::Imm(0),
                        ))]
                    } else {
                        vec![Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::AI(RegAddr::Reg(*r), *i as u8),
                        )]
                    }
                }
                (RegAddr::Addr(a), RegAddrImm64::Reg(r)) => {
                    if r == &Reg::RCX {
                        vec![Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::ACL(RegAddr::Addr(*a)),
                        )]
                    } else {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Reg(Reg::RCX),
                            )),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(Reg::RCX),
                                RegImm64::Reg(*r),
                            )),
                            Instruction_x64::ZShift(
                                ShiftInst::Shl,
                                Shiftop::ACL(RegAddr::Addr(*a)),
                            ),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(Reg::RCX),
                                RegImm64::Reg(temp0),
                            )),
                        ]
                    }
                }
                (RegAddr::Addr(a1), RegAddrImm64::Addr(a2)) => {
                    vec![
                        Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(temp0),
                            RegImm64::Reg(Reg::RCX),
                        )),
                        Instruction_x64::Zmov(Movop::RA(Reg::RCX, *a2)),
                        Instruction_x64::ZShift(ShiftInst::Shl, Shiftop::ACL(RegAddr::Addr(*a1))),
                        Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(Reg::RCX),
                            RegImm64::Reg(temp0),
                        )),
                    ]
                }
                (RegAddr::Addr(a), RegAddrImm64::Imm(i)) => {
                    if *i > 63 {
                        vec![Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Addr(*a),
                            RegImm64::Imm(0),
                        ))]
                    } else {
                        vec![Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::AI(RegAddr::Addr(*a), *i as u8),
                        )]
                    }
                }
            },
            OpType::Div => todo!(),
        },
        X64BlockElem::Trinop(op, v1, v2, v3) => match op_to_inst_type(*op) {
            OpType::Binop => {
                let mut asm = Vec::new();
                let dst = if let RegAddr::Reg(r) = v1 { *r } else { temp0 };
                match v2 {
                    RegAddrImm64::Reg(r) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(dst),
                            RegImm64::Reg(*r),
                        )));
                    }
                    RegAddrImm64::Addr(a) => {
                        asm.push(Instruction_x64::Zmov(Movop::RA(dst, *a)));
                    }
                    RegAddrImm64::Imm(imm) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(dst),
                            RegImm64::Imm(*imm),
                        )));
                    }
                }
                let src2 = match v3 {
                    RegAddrImm64::Reg(r) => *r,
                    RegAddrImm64::Addr(a) => {
                        asm.push(Instruction_x64::Zmov(Movop::RA(temp2, *a)));
                        temp2
                    }
                    RegAddrImm64::Imm(imm) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(temp2),
                            RegImm64::Imm(*imm),
                        )));
                        temp2
                    }
                };
                asm.push(Instruction_x64::ZBinInst(
                    op_to_bininst(*op),
                    Binop::RR(RegAddr::Reg(dst), RegImm32::Reg(src2)),
                ));
                match v1 {
                    RegAddr::Reg(_) => {}
                    RegAddr::Addr(a) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Addr(*a), RegImm64::Reg(dst)
                        )));
                    }
                }
                asm
            }
            OpType::Shift => {
                let mut asm = Vec::new();
                let dst = if let RegAddr::Reg(r) = v1 {
                    if r == &Reg::RCX {
                        temp0
                    } else {
                        *r
                    }
                } else {
                    temp0
                };
                match v2 {
                    RegAddrImm64::Reg(r) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(dst),
                            RegImm64::Reg(*r),
                        )));
                    }
                    RegAddrImm64::Addr(a) => {
                        asm.push(Instruction_x64::Zmov(Movop::RA(dst, *a)));
                    }
                    RegAddrImm64::Imm(imm) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(dst),
                            RegImm64::Imm(*imm),
                        )));
                    }
                };
                let src2 = match v3 {
                    RegAddrImm64::Reg(Reg::RCX) => {
                        asm.push(Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::ACL(RegAddr::Reg(dst)),
                        ));
                    }
                    RegAddrImm64::Reg(r) => {
                        asm.push(Instruction_x64::Xchg(Reg::RCX, *r));
                        asm.push(Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::ACL(RegAddr::Reg(dst)),
                        ));
                        asm.push(Instruction_x64::Xchg(Reg::RCX, *r));
                    }
                    RegAddrImm64::Addr(a) => {
                        asm.push(Instruction_x64::Zmov(Movop::RA(temp2, *a)));
                        asm.push(Instruction_x64::Xchg(Reg::RCX, temp2));
                        asm.push(Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::ACL(RegAddr::Reg(dst)),
                        ));
                        asm.push(Instruction_x64::Xchg(Reg::RCX, temp2));
                    }
                    RegAddrImm64::Imm(imm) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(temp2),
                            RegImm64::Imm(*imm),
                        )));
                        asm.push(Instruction_x64::Xchg(Reg::RCX, temp2));
                        asm.push(Instruction_x64::ZShift(
                            ShiftInst::Shl,
                            Shiftop::ACL(RegAddr::Reg(dst)),
                        ));
                        asm.push(Instruction_x64::Xchg(Reg::RCX, temp2));
                    }
                };
                match v1 {
                    RegAddr::Reg(Reg::RCX) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Reg(Reg::RCX),
                            RegImm64::Reg(dst),
                        )));
                    }
                    RegAddr::Reg(r) => {}
                    RegAddr::Addr(a) => {
                        asm.push(Instruction_x64::Zmov(Movop::RR(
                            RegAddr::Addr(*a),
                            RegImm64::Reg(dst),
                        )));
                    }
                }
                asm
            }
            OpType::Div => todo!(),
        },
        X64BlockElem::Monop(_, _) => todo!(),
        X64BlockElem::Mov(v1, v2) => match (v1, v2) {
            (RegAddr::Reg(r1), RegAddrImm64::Reg(r2)) => {
                if r1 == r2 {
                    vec![]
                } else {
                    vec![Instruction_x64::Zmov(Movop::RR(
                        RegAddr::Reg(*r1),
                        RegImm64::Reg(*r2),
                    ))]
                }
            }
            (RegAddr::Reg(r), RegAddrImm64::Addr(a)) => {
                vec![Instruction_x64::Zmov(Movop::RA(*r, *a))]
            }
            (RegAddr::Reg(r), RegAddrImm64::Imm(i)) => {
                vec![Instruction_x64::Zmov(Movop::RR(
                    RegAddr::Reg(*r),
                    RegImm64::Imm(*i),
                ))]
            }
            (RegAddr::Addr(a), RegAddrImm64::Reg(r)) => {
                vec![Instruction_x64::Zmov(Movop::RR(
                    RegAddr::Addr(*a),
                    RegImm64::Reg(*r),
                ))]
            }
            (RegAddr::Addr(a1), RegAddrImm64::Addr(a2)) => {
                vec![
                    Instruction_x64::Zmov(Movop::RA(temp1, *a2)),
                    Instruction_x64::Zmov(Movop::RR(RegAddr::Addr(*a1), RegImm64::Reg(temp1))),
                ]
            }
            (RegAddr::Addr(a), RegAddrImm64::Imm(i)) => {
                vec![Instruction_x64::Zmov(Movop::RR(
                    RegAddr::Addr(*a),
                    RegImm64::Imm(*i),
                ))]
            }
        },
        X64BlockElem::BoundCheck(_, _) => todo!(),
        X64BlockElem::Branch(j) => match j {
            JmpX64::J(d) => {
                vec![Instruction_x64::Jmp(*d)]
            }
            JmpX64::JC(c, CmpArgs { a, b, target }) => {
                let mut cmp = match (a, b) {
                    (RegAddrImm64::Reg(r1), RegAddrImm64::Reg(r2)) => {
                        vec![Instruction_x64::ZBinInst(
                            BinInst::Cmp,
                            Binop::RR(RegAddr::Reg(*r1), RegImm32::Reg(*r2)),
                        )]
                    }
                    (RegAddrImm64::Reg(r), RegAddrImm64::Addr(a)) => {
                        vec![Instruction_x64::ZBinInst(BinInst::Cmp, Binop::RA(*r, *a))]
                    }
                    (RegAddrImm64::Reg(r), RegAddrImm64::Imm(i)) => {
                        if *i as i32 as i64 == *i {
                            vec![Instruction_x64::ZBinInst(
                                BinInst::Cmp,
                                Binop::RR(RegAddr::Reg(*r), RegImm32::Imm(*i as i32)),
                            )]
                        } else {
                            vec![
                                Instruction_x64::Zmov(Movop::RR(
                                    RegAddr::Reg(temp0),
                                    RegImm64::Imm(*i),
                                )),
                                Instruction_x64::ZBinInst(
                                    BinInst::Cmp,
                                    Binop::RR(RegAddr::Reg(*r), RegImm32::Reg(temp0)),
                                ),
                            ]
                        }
                    }
                    (RegAddrImm64::Addr(a), RegAddrImm64::Reg(r)) => {
                        vec![Instruction_x64::ZBinInst(
                            BinInst::Cmp,
                            Binop::RR(RegAddr::Addr(*a), RegImm32::Reg(*r)),
                        )]
                    }
                    (RegAddrImm64::Addr(a1), RegAddrImm64::Addr(a2)) => {
                        vec![
                            Instruction_x64::Zmov(Movop::RA(temp0, a2.clone())),
                            Instruction_x64::ZBinInst(
                                BinInst::Cmp,
                                Binop::RR(RegAddr::Addr(a1.clone()), RegImm32::Reg(temp0)),
                            ),
                        ]
                    }
                    (RegAddrImm64::Addr(a), RegAddrImm64::Imm(i)) => {
                        if *i as i32 as i64 == *i {
                            vec![Instruction_x64::ZBinInst(
                                BinInst::Cmp,
                                Binop::RR(RegAddr::Addr(*a), RegImm32::Imm(*i as i32)),
                            )]
                        } else {
                            vec![
                                Instruction_x64::Zmov(Movop::RR(
                                    RegAddr::Reg(temp0),
                                    RegImm64::Imm(*i),
                                )),
                                Instruction_x64::ZBinInst(
                                    BinInst::Cmp,
                                    Binop::RR(RegAddr::Addr(*a), RegImm32::Reg(temp0)),
                                ),
                            ]
                        }
                    }
                    (RegAddrImm64::Imm(i), RegAddrImm64::Reg(r)) => {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Imm(*i),
                            )),
                            Instruction_x64::ZBinInst(
                                BinInst::Cmp,
                                Binop::RR(RegAddr::Reg(temp0), RegImm32::Reg(*r)),
                            ),
                        ]
                    }
                    (RegAddrImm64::Imm(i), RegAddrImm64::Addr(a)) => {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Imm(*i),
                            )),
                            Instruction_x64::ZBinInst(BinInst::Cmp, Binop::RA(temp0, *a)),
                        ]
                    }
                    (RegAddrImm64::Imm(i1), RegAddrImm64::Imm(i2)) => {
                        vec![
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp0),
                                RegImm64::Imm(*i1),
                            )),
                            Instruction_x64::Zmov(Movop::RR(
                                RegAddr::Reg(temp1),
                                RegImm64::Imm(*i2),
                            )),
                            Instruction_x64::ZBinInst(
                                BinInst::Cmp,
                                Binop::RR(RegAddr::Reg(temp0), RegImm32::Reg(temp1)),
                            ),
                        ]
                    }
                };
                cmp.push(Instruction_x64::Branch(*c, *target));
                cmp
            }
        },
        X64BlockElem::Call(fname) => {
            vec![Instruction_x64::Call(*fname)]
        }
        X64BlockElem::Push(r) => match r {
            RegAddrImm64::Reg(r) => {
                vec![Instruction_x64::Push(*r)]
            }
            RegAddrImm64::Addr(a) => {
                vec![
                    Instruction_x64::Zmov(Movop::RA(temp0, *a)),
                    Instruction_x64::Push(temp0),
                ]
            }
            RegAddrImm64::Imm(i) => {
                vec![
                    Instruction_x64::Zmov(Movop::RR(RegAddr::Reg(temp0), RegImm64::Imm(*i))),
                    Instruction_x64::Push(temp0),
                ]
            }
        },
        X64BlockElem::Pop(r) => {
            vec![Instruction_x64::Pop(*r)]
        }
        X64BlockElem::Ret => {
            vec![Instruction_x64::Ret]
        }
    }
}

pub fn cfg_to_x64<'a>(cfg: &[&'a CFGEntry], stack_space: usize) -> Vec<Instruction_x64<'a>> {
    let block_order = order_cfg(cfg);
    let pre_compile = cfg_to_pres(&block_order, stack_space);
    pre_compile
        .iter()
        .map(|block| compile_block_elem(block))
        .flatten()
        .collect()
}
