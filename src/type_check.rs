use std::collections::HashMap;
use crate::{source_ast::{Prog, VarDec, Typ, ID, Scope, Func, Exp, Stmt}, tokens::{OP, UOP}};

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

fn type_ident(ln: Option<usize>, env: &Env, id: &mut ID, es : &mut [Exp]) -> Typ {
    let (t, scope) = env.vars.get(&id).unwrap_or_else(|| panic!("unknown identifyer line: {}", ln.unwrap()));
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
                    panic!("array parameter missmatch line: {}", ln.unwrap())
                }
            }
            _ => panic!("non array with array args")
        }
    }
}

fn type_exp(ln: Option<usize>, env: &Env, exp: &mut Exp) -> Typ{
    match exp {
        Exp::Ident(id, es) => {
            type_ident(ln, env, id, es)
        }
        Exp::Call(f, args) => {
            let (p_typs, r_typ) = env.funcs.get(&f).unwrap_or_else(|| panic!("function doesnt exist on line: {}", ln.unwrap()));
            let ts : Vec<Typ> = args.iter_mut().map(|exp| type_exp(ln, env, exp)).collect();
            if ts.len() == args.len() && ts.iter().zip(p_typs.iter()).all(|(t, a)| t==a) {
                r_typ.clone()
            }
            else {
                panic!("many args line: {}", ln.unwrap());
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
                (_, _, _2) => panic!("no opetator matching op on line: {}", ln.unwrap())
            }
        }
        Exp::Uop(uop, exp) => {
            let t = type_exp(ln, env, exp);
            match (uop, t) {
                (UOP::Not, Typ::Bool) => Typ::Bool,
                _ => panic!("undefined uop on line: {}", ln.unwrap())
            }
        }
        Exp::Array(es) => {
            let ts : Vec<Typ> = es.iter_mut().map(|exp| type_exp(ln, env, exp)).collect();
            if ts.len() > 0 && ts.iter().all(|t| *t == Typ::Int) {
                Typ::Array(ts.len())
            }
            else {
                panic!("array length issue on line: {}", ln.unwrap())
            }
        }
    }
}

fn get_var_types(scope: Scope, vars: &mut [VarDec]) -> HashMap<ID, (Typ, Scope)> {
    let mut var_types : HashMap<ID, (Typ, Scope)> = HashMap::new();
    for var in vars.iter_mut() {
        if var_types.contains_key(&var.var_name) {
            panic!("duplicate function")
        }
        else {
            var_types.insert(var.var_name.clone(), (var.typ, scope));
            add_scope(&mut var.var_name, scope)
        }
    }
    var_types
}

fn type_var_dec(sc: &Scope, env: &Env, dec: &mut VarDec) {
    let typ = type_exp(dec.loc, env, &mut dec.init);
    if typ != dec.typ {
        match dec.var_name.clone() {
            ID::Source(name, _) => panic!("type mismatch var '{}' on line '{}'", name, dec.loc.unwrap()),
            ID::Temp(name, _) => panic!("type mismatch var '{}' on line '{}'", name, dec.loc.unwrap()),
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

fn get_param_types(ln: Option<usize>, params : &[(ID, Typ)]) -> HashMap<ID, (Typ, Scope)> {
    let mut param_typs : HashMap<ID, (Typ, Scope)> = HashMap::new();
    for param in params.iter() {
        if param_typs.contains_key(&param.0) {
            panic!("duplicate parameter in function header line {}", ln.unwrap())
        }
        else {
            param_typs.insert(param.0.clone(), (param.1, Scope::Parameter));
        }
    }
    param_typs
}

fn type_stmt(ln: Option<usize>, env: &Env, ret : Typ, stmt : &mut Stmt) {
    
    match stmt {
        Stmt::In(id) => {
            if type_ident(ln, env, id, &mut []) != Typ::Int {
                panic!("in identifyer not type int line: {}", ln.unwrap())
            }
        },
        Stmt::Out(id) => {
            if type_ident(ln, env, id, &mut []) != Typ::Int {
                panic!("in identifyer not type int line: {}", ln.unwrap())
            }
        },
        Stmt::Return(Some(id)) => {
            if type_ident(ln, env, id, &mut []) != ret {
                panic!("in identifyer not type int line: {}", ln.unwrap())
            }
        },
        Stmt::Return(_) => {
            panic!("wut")
        },
        Stmt::Assign(x, es, e) => {
            if type_ident(ln, env, x, es) != type_exp(ln, env, e) {
                panic!("type missmatch line {}", ln.unwrap())
            }
        },
        Stmt::DoWhile(s1, e, s2) => {
            if type_exp(ln, env, e) != Typ::Bool {
                panic!("condition not type bool {}", ln.unwrap())
            }
            type_stmt(ln, env, ret, s1);
            type_stmt(ln, env, ret, s2);
        },
        Stmt::Ite(e, s1, s2) => {
            if type_exp(ln, env, e) != Typ::Bool {
                panic!("condition not type bool {}", ln.unwrap())
            }
            type_stmt(ln, env, ret, s1);
            type_stmt(ln, env, ret, s2);
        },
        Stmt::Stmts(stmts) => {
            for stmt in stmts.iter_mut() {
                type_stmt(ln, env, ret, stmt);
            }
        },
        Stmt::Loc(s, ln) => {
            type_stmt(Some(*ln), env, ret, s);
        },
    }
}

fn check_return_paths_stmt(stmt : &Stmt) -> bool {
    match stmt {
        Stmt::Assign(_, _, _) => false,
        Stmt::DoWhile(s1, _, _) => check_return_paths_stmt(s1),
        Stmt::Ite(_, s1, s2) => check_return_paths_stmt(s1) && check_return_paths_stmt(s2),
        Stmt::Stmts(stmts) => check_return_paths(stmts),
        Stmt::In(_) => false,
        Stmt::Out(_) => false,
        Stmt::Return(_) => true,
        Stmt::Loc(s, _) => check_return_paths_stmt(s),
    }

}

fn check_return_paths(stmts : &[Stmt]) -> bool{
    stmts.iter().any(|stmt| check_return_paths_stmt(stmt))
}

fn type_fun(env: &Env, func: &mut Func) {
    let param_env = get_param_types(func.loc, &func.params);
    let local_env = get_var_types(Scope::Local, &mut func.locals);
    let mut vars = env.vars.clone();
    for (param_id, param_val) in param_env.iter() {
        vars.insert(param_id.clone(), param_val.clone());
    }
    for (loc_id, loc_val) in local_env.iter() {
        vars.insert(loc_id.clone(), loc_val.clone());
    }
    let new_env = Env {vars : vars, funcs : env.funcs.clone()};
    for var_dec in func.locals.iter_mut() {
        type_var_dec(&Scope::Local, env, var_dec);
    }
    for stmt in func.body.iter_mut() {
        type_stmt(None, &new_env, func.ret, stmt);
    }
    if !check_return_paths(&func.body) {
        panic!("function may not return");
    }
    for (param, _) in func.params.iter_mut() {
        add_scope(param, Scope::Parameter);
    }
}

pub fn type_prog(prog : &mut Prog) {
    let env = Env{funcs: get_function_types(&prog.funcs),
                       vars: get_var_types(Scope::Global, &mut prog.globals)};
    for var_dec in prog.globals.iter_mut() {
        type_var_dec(&Scope::Global, &env, var_dec);
    }
    for func in prog.funcs.iter_mut() {
        type_fun(&env, func);
    }
}