use std::collections::HashMap;

use crate::source_ast::{Exp, Stmt, ID};

fn prop_stmt(env : &mut HashMap<ID, Exp>, stmt : &Stmt) -> Stmt {
    match stmt {
        Stmt::Assign(x, inds, e) if inds.len() == 0 => {
            panic!("unimplemented")
            //let o1 = prop_exp(env, e);
        }
        Stmt::Assign(x, inds, e) => {
            panic!("unimplemented")
        }
        _ => {
            panic!("unimplemented")
        }
    }
}

pub fn prop_stmts(env: HashMap<ID, Exp>, stmts: &[Stmt]) -> (HashMap<ID, Exp>, Vec<Stmt>) {
    let mut stmts_out : Vec<Stmt> = Vec::new();
    let mut env_new = env.clone();
    for stmt in stmts.iter() {
        stmts_out.push(prop_stmt(&mut env_new, stmt));
    }
    
    (env_new, stmts_out)
}
