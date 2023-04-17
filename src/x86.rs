pub enum Reg {
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

pub enum Displacement {
    ConcreteDisp(i64),
    LabelDisp(String),
}

pub enum Rm {
    Zr(Reg),
    Zm(Option<(i64, Reg)>, Option<Reg>, Option<Displacement>),
}

pub enum DestSrc {
    ZrmI(Rm, i64),
    ZrmR(Rm, Reg),
    ZrRm(Reg, Rm),
}

pub enum BinopName {
    Zadc,
    Zadd,
    Zand,
    Zcmp,
    Zor,
    Zshl,
    Zshr,
    Zsar,
    Zsub,
    Zsbb,
    Ztest,
    Zxor,
}

pub enum Instruction_x64 {
    Zlabel(String),
    Zbinop(BinopName, DestSrc),
}
