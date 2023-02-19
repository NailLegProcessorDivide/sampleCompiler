use std::collections::HashMap;
use crate::{source_ast::{Prog, VarDec, Typ, ID, Scope, Func, Exp}, tokens::{OP, UOP}};

struct Env {
    funcs : HashMap<ID, (Vec<Typ>, Typ)>,
    vars: HashMap<ID, (Typ, Scope)>,
}

fn add_scope(id: &mut ID, scope : Scope) {
    match id {
        ID::Source(name, None) => {*id = ID::Source(name.clone(), Some(scope));},
        ID::Source(_, Some(_)) => {},
        ID::Temp(name, _) => {},
    }
}

fn type_exp(ln: Option<usize>, env: &Env, exp: &mut Exp) -> Typ{
    match exp {
        Exp::Ident(id, es) => {
            let (t, scope) = env.vars.get(&id).expect("unknown identifyer");
            let ts : Vec<Typ> = es.iter_mut().map(|exp| type_exp(ln, env, exp)).collect();
            add_scope(id, *scope);//mutates the exp
            if ts.len() == 0 {
                *t
            }
            else{
                match t {
                    Typ::Array(l) => {
                        if *l == ts.len() && ts.iter().all(|lt| *lt == Typ::Int) {
                            Typ::Int
                        }
                        else {
                            panic!("array parameter missmatch")
                        }
                    }
                    _ => panic!("non array with array args")
                }
            }
        }
        Exp::Call(f, args) => {
            let (pTyps, rTyp) = env.funcs.get(&f).expect("function doesnt exist");
            let ts : Vec<Typ> = args.iter_mut().map(|exp| type_exp(ln, env, exp)).collect();
            if ts.len() == args.len() && ts.iter().zip(pTyps.iter()).all(|(t, a)| t==a) {
                rTyp.clone()
            }
            else {
                panic!("many args");
            }
        }
        Exp::Num(_) => Typ::Int,
        Exp::Bool(_) => Typ::Bool,
        Exp::Op(e1, op, e2) => {
            let t1 = type_exp(ln, env, e1);
            let t2 = type_exp(ln, env, e2);
            match (t1, op, t2) {
                (Typ::Bool, (OP::And | OP::Or | OP::Eq), Typ::Bool) => Typ::Bool,
                (Typ::Int, (OP::Plus | OP::Minus | OP::Times | OP::Div | OP::Lshift | OP::BitOr |
                    OP::BitAnd), Typ::Int) => Typ::Int,
                (Typ::Int, (OP::Lt | OP::Eq | OP::Gt), Typ::Int) => Typ::Bool,
                (t1, _, t2) => panic!("no opetator matching op")
            }
        }
        Exp::Uop(uop, exp) => {
            let t = type_exp(ln, env, exp);
            match (uop, t) {
                (UOP::Not, Typ::Bool) => Typ::Bool,
                _ => panic!("undefined uop")
            }
        }
        Exp::Array(es) => {
            let ts : Vec<Typ> = es.iter_mut().map(|exp| type_exp(ln, env, exp)).collect();
            if ts.len() > 0 && ts.iter().all(|t| *t == Typ::Int) {
                Typ::Array(ts.len())
            }
            else {
                panic!("array length issue")
            }
        }
    }
}

fn get_var_types(scope: &Scope, vars: &[VarDec]) -> HashMap<ID, (Typ, Scope)> {
    let mut var_types : HashMap<ID, (Typ, Scope)> = HashMap::new();
    for var in vars.iter() {
        if var_types.contains_key(&var.var_name) {
            panic!("duplicate function")
        }
        else {
            var_types.insert(var.var_name.clone(), (var.typ, *scope));
        }
    }
    var_types
}

fn type_var_dec(sc: &Scope, env: &Env, dec: &mut VarDec) {
    let typ = type_exp(dec.loc, env, &mut dec.init);
    if typ != dec.typ {
        match dec.loc {
            Some(loc) => match dec.var_name.clone() {
                ID::Source(name, _) => panic!("type mismatch var '{}' on line '{}'", name, loc),
                ID::Temp(name, _) => panic!("type mismatch var '{}' on line '{}'", name, loc),
            },
            None => match dec.var_name.clone() {
                ID::Source(name, _) => panic!("type mismatch var '{}'", name),
                ID::Temp(name, _) => panic!("type mismatch var '{}'", name),
            }
        }
    }
}

fn get_function_types(funcs : &[Func]) -> HashMap<ID, (Vec<Typ>, Typ)> {
    let mut func_types : HashMap<ID, (Vec<Typ>, Typ)> = HashMap::new();
    for func in funcs.iter() {
        if func_types.contains_key(&func.fun_name) {
            panic!("duplicate function")
        }
        else {
            func_types.insert(func.fun_name.clone(), (func.params.iter().map(|(_, typ)| *typ).collect(), func.ret));
        }
    }
    func_types
}

pub fn type_prog(prog : &mut Prog) {
    let env = Env{funcs: get_function_types(&prog.funcs),
                       vars: get_var_types(&Scope::Global, &prog.var_dec)};
    for var_dec in prog.var_dec.iter_mut() {
        type_var_dec(&Scope::Global, &env, var_dec);
    }
}