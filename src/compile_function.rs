use std::collections::{HashMap, HashSet};

use crate::block_structure;

use crate::block_structure::CFGEntry;
use crate::block_structure::Var;
use crate::const_prop;
use crate::live_var_analysis;
use crate::live_var_analysis::CFGAnnot;
use crate::shrink_imm::shrink_imm;
use crate::source_ast::Typ;
use crate::source_ast::TypedExp;
use crate::source_ast::{Exp, Func, Stmt, VarDec, ID};
use crate::unnest_exp;
use crate::x86::Instruction_x64;

fn init_var_dec_to_0(dec: &VarDec) -> Stmt {
    Stmt::Assign(
        dec.var_name.clone(),
        Vec::new(),
        TypedExp {
            exp: Exp::Num(0),
            typ: Some(Typ::Int),
        },
    )
}

fn var_dec_to_stmt(dec: &VarDec) -> Stmt {
    Stmt::Assign(dec.var_name.clone(), Vec::new(), dec.init.clone())
}

pub fn build_ast(func: &Func) -> Vec<Stmt> {
    func.locals
        .iter()
        .map(|local: &VarDec| init_var_dec_to_0(&local))
        .chain(func.locals.iter().map(|local| var_dec_to_stmt(&local)))
        .chain(func.body.iter().map(|stmt| stmt.clone()))
        .collect::<Vec<Stmt>>()
}

pub fn prop_stmts(func: &Func) -> Vec<Stmt> {
    let ast = build_ast(func);
    let (_, stmts) = const_prop::prop_stmts(HashMap::new(), &ast);
    stmts
}

pub fn unnest_stmts(func: &Func) -> Vec<Stmt> {
    let stmts = prop_stmts(func);
    unnest_exp::unnest(&stmts)
}

pub fn build_cfg(func: &Func) -> Vec<CFGEntry> {
    let stmts = unnest_stmts(func);
    block_structure::ast_to_cfg(&stmts)
}

pub fn clean_cfg(func: &Func) -> Vec<CFGEntry> {
    let mut cfg = build_cfg(func);
    block_structure::clean_cfg(&mut cfg);
    cfg
}

pub fn build_annot_cfg(globals: &HashSet<Var>, func: &Func) -> Vec<(CFGEntry, CFGAnnot)> {
    let cfg = clean_cfg(func);
    live_var_analysis::lva(globals, &cfg)
        .iter()
        .map(|(e, a)| ((*e).clone(), a.clone()))
        .collect()
}

pub fn compile_fun_x64(
    safe: bool,
    globals: &HashSet<Var>,
    func: &Func,
) -> (ID, Vec<Instruction_x64>) {
    //shrink_imm(&mut cfg);
    let lva_cfg = build_annot_cfg(globals, func);
    (func.fun_name.clone(), Vec::new())
    //todo!();
}
