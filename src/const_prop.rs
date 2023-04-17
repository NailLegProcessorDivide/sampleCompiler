use std::collections::HashMap;

use crate::{
    source_ast::{Exp, Stmt, TypedExp, ID},
    tokens::OP,
};

fn might_have_effect(e: &Exp) -> bool {
    match e {
        Exp::Ident(_, ex) => ex.len() != 0,
        Exp::Call(_, _) => true,
        Exp::Num(_) => false,
        Exp::Bool(_) => false,
        Exp::Op(_, OP::Div, _) => true,
        Exp::Op(e1, _, e2) => might_have_effect(&e1.exp) || might_have_effect(&e2.exp),
        Exp::Uop(_, e1) => might_have_effect(&e1.exp),
        Exp::Array(es) => es.iter().any(|e| might_have_effect(&e.exp)),
    }
}

fn fold_exp(env: &mut HashMap<ID, TypedExp>, e: &TypedExp) -> TypedExp {
    TypedExp {
        exp: match &e.exp {
            Exp::Ident(id, idxs) if idxs.len() == 0 => env.get(id).unwrap_or(e).clone().exp,
            Exp::Ident(id, idxs) => Exp::Ident(
                id.clone(),
                idxs.iter().map(|idx| fold_exp(env, idx)).collect(),
            ),
            Exp::Num(n) => Exp::Num(*n),
            Exp::Bool(n) => Exp::Bool(*n),
            Exp::Op(e1, op, e2) => {
                let o1: TypedExp = fold_exp(env, e1);
                let o2: TypedExp = fold_exp(env, e2);
                match (&o1.exp, op, &o2.exp) {
                    (n1, OP::Plus, Exp::Num(0)) => n1.clone(),
                    (Exp::Num(0), OP::Plus, n2) => n2.clone(),
                    (Exp::Num(n1), OP::Plus, Exp::Num(n2)) => Exp::Num(*n1 + *n2),
                    (Exp::Num(n1), OP::Minus, Exp::Num(n2)) => Exp::Num(*n1 - *n2),

                    (Exp::Num(0), OP::Times, e2) if !might_have_effect(e2) => Exp::Num(0),
                    (_, OP::Times, Exp::Num(0)) => Exp::Num(0),
                    (Exp::Num(1), OP::Times, n2) => n2.clone(),
                    (n1, OP::Times, Exp::Num(1)) => n1.clone(),
                    (Exp::Num(n1), OP::Times, Exp::Num(n2)) => Exp::Num(*n1 * *n2),

                    (Exp::Num(_), OP::Div, Exp::Num(0)) => {
                        panic!("divide by zero, make runtime later")
                    }
                    (n1, OP::Div, Exp::Num(1)) => n1.clone(),
                    (Exp::Num(n1), OP::Div, Exp::Num(n2)) => Exp::Num(*n1 / *n2),
                    _ => Exp::Op(Box::new(o1), *op, Box::new(o2)),
                }
            }
            Exp::Uop(uop, e) => Exp::Uop(*uop, e.clone()),
            Exp::Array(es) => Exp::Array(es.iter().map(|e| fold_exp(env, e)).collect()),
            Exp::Call(f, es) => Exp::Call(f.clone(), es.iter().map(|e| fold_exp(env, e)).collect()),
            _ => panic!("unimplemented"),
        },
        typ: e.typ,
    }
}

fn prop_stmt(env: &mut HashMap<ID, TypedExp>, stmt: &Stmt) -> Stmt {
    match stmt {
        Stmt::Assign(x, inds, e) if inds.len() == 0 => {
            let o1 = fold_exp(env, e);
            panic!("unimplemented")
        }
        Stmt::Assign(x, inds, e) => {
            panic!("unimplemented")
        }
        _ => {
            panic!("unimplemented")
        }
    }
}

pub fn prop_stmts(
    env: HashMap<ID, TypedExp>,
    stmts: &[Stmt],
) -> (HashMap<ID, TypedExp>, Vec<Stmt>) {
    (env, Vec::from(stmts))
    /*
    let mut stmts_out : Vec<Stmt> = Vec::new();
    let mut env_new = env.clone();
    for stmt in stmts.iter() {
        stmts_out.push(prop_stmt(&mut env_new, stmt));
    }

    (env_new, stmts_out)
    */
}
