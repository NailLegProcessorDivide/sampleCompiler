
use crate::source_ast::{Exp, Stmt, ID, stmts_to_stmt, Typ, TypedExp};
use crate::tokens::OP;

fn is_atomic(e: &TypedExp) -> bool{
    match &e.exp {
        Exp::Num(_) => true,
        Exp::Bool(_) => true,
        Exp::Ident(_, xs) => xs.is_empty(),
        Exp::Call(_, _) => false,
        Exp::Op(_, _, _) => false,
        Exp::Uop(_, _) => false,
        Exp::Array(_) => false,
    }
}

fn unnest_indices(arr: &ID, indicies: &Vec<TypedExp>, next_ident: &mut usize) -> (Vec<Stmt>, TypedExp) {
    if indicies.len() < 2 {
        (Vec::new(), TypedExp{exp: Exp::Ident (arr.clone(), indicies.clone()), typ: Some(Typ::Int)})
    }
    else {
        let mut res_vec = Vec::new();
        let mut pid = arr.clone();
        for ex in &indicies[..indicies.len()-1] {
            let id = ID::Temp("UE".to_string(), *next_ident);
            *next_ident += 1;
            res_vec.push(Stmt::Assign(id.clone(), Vec::new(), TypedExp{exp: Exp::Ident(pid, vec![ex.clone()]), typ: Some(Typ::Array(1))}));
            pid = id;
        }
        (res_vec, TypedExp{exp: Exp::Ident(pid, vec![indicies.last().unwrap().clone()]), typ: Some(Typ::Int)})
    }
}

fn unnest_exp_atomic(e: &TypedExp, next_ident: &mut usize) -> (Vec<Stmt>, TypedExp) {
    let (mut s, f) = unnest_exp(e, next_ident);
    if is_atomic(&f) {
        (s, f)
    }
    else {
        let id = ID::Temp("UE".to_string(), *next_ident);
        *next_ident += 1;
        s.push(Stmt::Assign(id.clone(), Vec::new(), f));
        (s, TypedExp{exp: Exp::Ident(id, Vec::new()), typ: Some(Typ::Int)})
    }
}

fn unnest_exp_for_test(e: &TypedExp, next_ident: &mut usize) -> (Vec<Stmt>, TypedExp) {
    let (mut s, f) = unnest_exp(e, next_ident);
    match f.exp {
        Exp::Ident(fid, l) => {
            if l.len() == 1 {
                let id = ID::Temp("UE".to_string(), *next_ident);
                *next_ident += 1;
                s.push(Stmt::Assign(id.clone(), Vec::new(), TypedExp{exp: Exp::Ident(fid, l), typ: e.typ}));
                (s, TypedExp{exp: Exp::Ident(id, Vec::new()), typ: e.typ})
            }
            else {
                (s, TypedExp{exp: Exp::Ident(fid, l), typ: e.typ})
            }
        }
        _ => (s,f)
    }
}

fn unnest_exp(e: &TypedExp, next_ident: &mut usize) -> (Vec<Stmt>, TypedExp) {
    match &e.exp {
        Exp::Num(i) => (Vec::new(), TypedExp{exp: Exp::Num(*i), typ: Some(Typ::Int)}),
        Exp::Bool(b) => (Vec::new(), TypedExp{exp: Exp::Bool(*b), typ: Some(Typ::Bool)}),
        Exp::Ident(i, es) => {
            let (s_list, aes): (Vec<Vec<Stmt>>, Vec<TypedExp>) = es.into_iter().map(|x| unnest_exp_atomic(x, next_ident)).unzip();
            let (s_list2, ae): (Vec<Stmt>, TypedExp) = unnest_indices(&*i, &aes, next_ident);
            (s_list.into_iter().flatten().chain(s_list2.into_iter()).collect(), ae)
        },
        Exp::Op(e1, OP::And, e2) => {
            let (mut s1, f1) = unnest_exp(&**e1, next_ident);
            let (mut s2, f2) = unnest_exp(&**e2, next_ident);
            let id = ID::Temp("UE".to_string(), *next_ident);
            *next_ident += 1;
            s2.push(Stmt::Assign (id.clone(), Vec::new(), f2));
            s1.push(Stmt::Ite (f1, Box::new(Stmt::Stmts(s2)), Box::new(Stmt::Assign(id.clone(), Vec::new(), TypedExp {exp: Exp::Bool(false), typ : Some(Typ::Bool)}))));
            (s1, TypedExp{exp: Exp::Ident (id, Vec::new()), typ: e.typ})
        },
        Exp::Op(e1, OP::And, e2) => {
            let (mut s1, f1) = unnest_exp(&**e1, next_ident);
            let (mut s2, f2) = unnest_exp(&**e2, next_ident);
            let id = ID::Temp("UE".to_string(), *next_ident);
            *next_ident += 1;
            s2.push(Stmt::Assign(id.clone(), Vec::new(), f2));
            s1.push(Stmt::Ite(f1, Box::new(Stmt::Assign(id.clone(), Vec::new(), TypedExp {exp: Exp::Bool(true), typ : Some(Typ::Bool)})), Box::new(Stmt::Stmts(s2))));
            (s1, TypedExp{exp: Exp::Ident (id, Vec::new()), typ: e.typ})
        }
        Exp::Op(e1, op, e2) => {
            let (s1, a1) = unnest_exp_atomic(&**e1, next_ident);
            let (s2, a2) = unnest_exp_atomic(&**e2, next_ident);
            (s1.into_iter().chain(s2.into_iter()).collect(), TypedExp{exp: Exp::Op(Box::new(a1), *op, Box::new(a2)), typ: e.typ})
        }
        Exp::Uop(uop, e) => {
            let (s, a) = unnest_exp_atomic(&**e, next_ident);
            (s, TypedExp{exp: Exp::Uop(*uop, Box::new(a)), typ: e.typ})
        },
        Exp::Array(es) => {
            let (s_list, aes): (Vec<Vec<Stmt>>, Vec<TypedExp>) = es.into_iter().map(|part| unnest_exp_atomic(part, next_ident)).unzip();
            (s_list.into_iter().flatten().collect(), TypedExp{exp: Exp::Array(aes), typ: e.typ})
        },
        Exp::Call(f, es) => {
            let (s_list, aes): (Vec<Vec<Stmt>>, Vec<TypedExp>) = es.into_iter().map(|part| unnest_exp_atomic(part, next_ident)).unzip();
            (s_list.into_iter().flatten().collect(), TypedExp{exp: Exp::Call(f.clone(), aes), typ: e.typ})
        }
    }
}

fn unnest_stmt(s: &Stmt, next_ident: &mut usize) -> Vec<Stmt> {
    match s {
        Stmt::Assign(x, es, e) => {
            if es.is_empty() {
                let (mut s, f) = unnest_exp(e, next_ident);
                s.push(Stmt::Assign(x.clone(), Vec::new(), f));
                s
            }
            else {
                let (s_list, aes): (Vec<Vec<Stmt>>, Vec<TypedExp>) = es.iter().map(|e| unnest_exp_atomic(e, next_ident)).unzip();
                let (s, f) = unnest_exp_atomic(e, next_ident);
                match unnest_indices(x, &aes, next_ident) {
                    (sn, TypedExp{exp: Exp::Ident(i, v), typ: _}) => {
                        let mut sns: Vec<Stmt> = s_list.iter().flatten().chain(s.iter()).chain(sn.iter()).cloned().collect();
                        sns.push(Stmt::Assign(i, v, f));
                        sns
                    },
                    _ => todo!()
                } 
            }
        }
        Stmt::DoWhile(s0, e, s1) => {
            let s0s = unnest_stmt(s0, next_ident);
            let (se1, es) = unnest_exp_for_test(e, next_ident);
            let s1s = unnest_stmt(s1, next_ident);
            vec![Stmt::DoWhile(Box::new(stmts_to_stmt(s0s.into_iter().chain(se1.into_iter()).collect())), es, Box::new(stmts_to_stmt(s1s)))]
        }
        Stmt::Ite(e, s1, s2) => {
            let (mut se, e1) = unnest_exp_for_test(e, next_ident);
            let s1s = unnest_stmt(s1, next_ident);
            let s2s = unnest_stmt(s2, next_ident);
            se.push(Stmt::Ite(e1, Box::new(stmts_to_stmt(s1s)), Box::new(stmts_to_stmt(s2s))));
            se
        }
        Stmt::Stmts(s_list) => {
            s_list.into_iter().map(|stmt| unnest_stmt(stmt, next_ident)).flatten().collect()
        }
        Stmt::Loc(stmt, _) => unnest_stmt(stmt, next_ident),
        _ => vec![s.clone()] 
    }
}

pub fn unnest(stmts: &Vec<Stmt>) -> Vec<Stmt> {
    let mut next_ident: usize = 0;
    stmts.iter().map(|stmt| unnest_stmt(stmt, &mut next_ident)).flatten().collect()
}