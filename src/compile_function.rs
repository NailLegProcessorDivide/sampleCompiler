use std::collections::{HashSet, HashMap};

use crate::block_structure;

use crate::const_prop;
use crate::block_structure::Var;
use crate::source_ast::{ID, Exp, Stmt, VarDec, Func};
use crate::x86::Instruction_x64;
use crate::unnest_exp;

fn init_var_dec_to_0(dec: &VarDec) -> Stmt {
    Stmt::Assign(dec.var_name.clone(), Vec::new(), Exp::Num(0))
}

fn var_dec_to_stmt(dec: &VarDec) -> Stmt {
    Stmt::Assign(dec.var_name.clone(), Vec::new(), dec.init.clone())
}

pub fn compile_fun_x64(safe: bool, globals: &HashSet<Var>, func: &Func) -> (ID, Vec<Instruction_x64>){
    let ast: Vec<Stmt> = func.locals.iter().map(|local: &VarDec| init_var_dec_to_0(&local)).chain(
              func.locals.iter().map(|local| var_dec_to_stmt(&local))).chain(
              func.body.iter().map(|stmt| stmt.clone())).collect::<Vec<Stmt>>();
    let (_, stmts) = const_prop::prop_stmts(HashMap::new(), &ast);
    let no_nest_ast = unnest_exp::unnest(&stmts);
    let cfg = block_structure::build_cfg(&no_nest_ast);
    todo!();
}
