

use crate::source_ast::Scope;

pub enum Var {
    Vreg(i64),
    Stack(i64),
    Global(String),
    NamedSource(String, Scope),
    NamedTmp(String, i64)
}
