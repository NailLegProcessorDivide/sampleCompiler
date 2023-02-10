use std::collections::HashMap;

use crate::source_ast::{Exp, Stmt, ID};

pub fn prop_stmts(env: HashMap<ID, Exp>, stmts: &Vec<Stmt>) -> (HashMap<ID, Exp>, Vec<Stmt>) {
    todo!();
}