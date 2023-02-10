
use crate::source_ast::{Exp, Stmt, ID, stmts_to_stmt};
use crate::tokens::OP;

fn is_atomic(e: &Exp) -> bool{
    match e {
        Exp::Num(_) => true,
        Exp::Bool(_) => true,
        Exp::Ident(_, xs) => xs.is_empty(),
        Exp::Call(_, _) => false,
        Exp::Op(_, _, _) => false,
        Exp::Uop(_, _) => false,
        Exp::Array(_) => false,
    }
}

fn unnest_indices(arr: ID, indicies: &Vec<Exp>, nextIdent: &mut i64) -> (Vec<Stmt>, Exp) {
    if indicies.len() < 1 {
        (Vec::new(), Exp::Ident (arr, indicies.clone()))
    }
    else {
        let mut res_vec = Vec::new();
        let mut pid = arr;
        for ex in indicies.iter() {
            let id = ID::Temp("UE".to_string(), *nextIdent);
            *nextIdent += 1;
            res_vec.push(Stmt::Assign(id.clone(), Vec::new(), Exp::Ident(pid, vec![ex.clone()])));
            pid = id;
        }
        (res_vec, indicies.first().unwrap().clone())
    }
}

fn unnest_exp_atomic(e: Exp, nextIdent: &mut i64) -> (Vec<Stmt>, Exp) {
    let (mut s, f) = unnest_exp(e, nextIdent);
    if is_atomic(&f) {
        (s, f)
    }
    else {
        let id = ID::Temp("UE".to_string(), *nextIdent);
        *nextIdent += 1;
        s.push(Stmt::Assign(id.clone(), Vec::new(), f));
        (s, Exp::Ident(id, Vec::new()))
    }
}

fn unnest_exp_for_test(e: Exp, nextIdent: &mut i64) -> (Vec<Stmt>, Exp) {
    let (mut s, f) = unnest_exp(e, nextIdent);
    match f {
        Exp::Ident(fid, l) => {
            if l.len() == 1 {
                let id = ID::Temp("UE".to_string(), *nextIdent);
                *nextIdent += 1;
                s.push(Stmt::Assign(id.clone(), Vec::new(), Exp::Ident(fid, l)));
                (s, Exp::Ident(id, Vec::new()))
            }
            else {
                (s, Exp::Ident(fid, l))
            }
        }
        _ => (s,f)
    }
}

fn unnest_exp(e: Exp, nextIdent: &mut i64) -> (Vec<Stmt>, Exp) {
    match e {
        Exp::Num(i) => (Vec::new(), Exp::Num(i)),
        Exp::Bool(b) => (Vec::new(), Exp::Bool(b)),
        Exp::Ident(i, es) => {
            let (s_list, aes): (Vec<Vec<Stmt>>, Vec<Exp>) = es.into_iter().map(|x| unnest_exp_atomic(x, nextIdent)).unzip();
            let (s_list2, ae): (Vec<Stmt>, Exp) = unnest_indices(i, &aes, nextIdent);
            (s_list.into_iter().flatten().chain(s_list2.into_iter()).collect(), ae)
        },
        Exp::Op(e1, OP::And, e2) => {
            let (mut s1, f1) = unnest_exp(*e1, nextIdent);
            let (mut s2, f2) = unnest_exp(*e2, nextIdent);
            let id = ID::Temp("UE".to_string(), *nextIdent);
            *nextIdent += 1;
            s2.push(Stmt::Assign (id.clone(), Vec::new(), f2));
            s1.push(Stmt::Ite (f1, Box::new(Stmt::Stmts(s2)), Box::new(Stmt::Assign(id.clone(), Vec::new(), Exp::Bool(false)))));
            (s1, Exp::Ident (id, Vec::new()))
        },
        Exp::Op(e1, OP::And, e2) => {
            let (mut s1, f1) = unnest_exp(*e1, nextIdent);
            let (mut s2, f2) = unnest_exp(*e2, nextIdent);
            let id = ID::Temp("UE".to_string(), *nextIdent);
            *nextIdent += 1;
            s2.push(Stmt::Assign(id.clone(), Vec::new(), f2));
            s1.push(Stmt::Ite(f1, Box::new(Stmt::Assign(id.clone(), Vec::new(), Exp::Bool(true))), Box::new(Stmt::Stmts(s2))));
            (s1, Exp::Ident (id, Vec::new()))
        }
        Exp::Op(e1, op, e2) => {
            let (s1, a1) = unnest_exp_atomic(*e1, nextIdent);
            let (s2, a2) = unnest_exp_atomic(*e2, nextIdent);
            (s1.into_iter().chain(s2.into_iter()).collect(), Exp::Op(Box::new(a1), op, Box::new(a2)))
        }
        Exp::Uop(uop, e) => {
            let (s, a) = unnest_exp_atomic(*e, nextIdent);
            (s, Exp::Uop(uop, Box::new(a)))
        },
        Exp::Array(es) => {
            let (s_list, aes): (Vec<Vec<Stmt>>, Vec<Exp>) = es.into_iter().map(|part| unnest_exp_atomic(part, nextIdent)).unzip();
            (s_list.into_iter().flatten().collect(), Exp::Array(aes))
        },
        Exp::Call(f, es) => {
            let (s_list, aes): (Vec<Vec<Stmt>>, Vec<Exp>) = es.into_iter().map(|part| unnest_exp_atomic(part, nextIdent)).unzip();
            (s_list.into_iter().flatten().collect(), Exp::Call(f, aes))
        }
    }
}

fn unnest_stmt(s: Stmt, nextIdent: &mut i64) -> Vec<Stmt> {
    match s {
        Stmt::Assign(x, es, e) => {
            if (es.is_empty()) {
                let (mut s, f) = unnest_exp(e, nextIdent);
                s.push(Stmt::Assign(x, Vec::new(), f));
                s
            }
            else {
                let (s_list, aes): (Vec<Vec<Stmt>>, Vec<Exp>) = es.iter().map(|e| unnest_exp_atomic(e.clone(), nextIdent)).unzip();
                let (s, f) = unnest_exp_atomic(e, nextIdent);
                match unnest_indices(x, &aes, nextIdent) {
                    (sn, Exp::Ident(i, v)) => {
                        let mut sns: Vec<Stmt> = s_list.iter().flatten().chain(s.iter()).chain(sn.iter()).cloned().collect();
                        sns.push(Stmt::Assign(i, v, f));
                        sns
                    },
                    _ => todo!()
                } 
            }
        }
        Stmt::DoWhile(s0, e, s1) => {
            let s0s = unnest_stmt(*s0, nextIdent);
            let (se1, es) = unnest_exp_for_test(e, nextIdent);
            let s1s = unnest_stmt(*s1, nextIdent);
            vec![Stmt::DoWhile(Box::new(stmts_to_stmt(s0s.into_iter().chain(se1.into_iter()).collect())), es, Box::new(stmts_to_stmt(s1s)))]
        }
        Stmt::Ite(e, s1, s2) => {
            let (mut se, e1) = unnest_exp_for_test(e, nextIdent);
            let s1s = unnest_stmt(*s1, nextIdent);
            let s2s = unnest_stmt(*s2, nextIdent);
            se.push(Stmt::Ite(e1, Box::new(stmts_to_stmt(s1s)), Box::new(stmts_to_stmt(s2s))));
            se
        }
        Stmt::Stmts(s_list) => {
            s_list.into_iter().map(|stmt| unnest_stmt(stmt, nextIdent)).flatten().collect()
        }
        Stmt::Loc(stmt, _) => unnest_stmt(*stmt, nextIdent),
        stmt => vec![stmt] 
    }
}

pub fn unnest(stmts: &Vec<Stmt>) -> Vec<Stmt> {
    let mut nextIdent: i64 = 0;
    stmts.iter().map(|stmt| unnest_stmt(stmt.clone(), &mut nextIdent)).flatten().collect()
}