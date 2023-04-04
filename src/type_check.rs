use std::collections::HashMap;
use crate::{source_ast::{Prog, VarDec, Typ, ID, Scope, Func, Exp, Stmt, TypedExp}, tokens::{OP, UOP}};

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

fn type_ident(ln: Option<usize>, env: &Env, id: &mut ID, es : &mut [TypedExp]) -> Result<Typ, String> {
    let (t, scope) = match env.vars.get(&id) {
        Some(v) => v,
        None => return Err(format!("unknown identifyer line: {}", ln.unwrap()))
    };
    let mut ts : Vec<Typ> = Vec::new();
    for exp in es {
        ts.push(type_exp(ln, env, exp)?);
    }
    add_scope(id, *scope);//mutates the exp
    if ts.len() == 0 {
        Ok(*t)
    }
    else{
        match t {
            Typ::Array(l) => {
                if *l == ts.len() && ts.iter().all(|lt| *lt == Typ::Int) {
                    Ok(Typ::Int)
                }
                else {
                    Err(format!("array parameter missmatch line: {}", ln.unwrap()))
                }
            }
            _ => Err(format!("non array with array args"))
        }
    }
}

fn type_exp(ln: Option<usize>, env: &Env, exp: &mut TypedExp) -> Result<Typ, String>{
    let typ : Typ = match &mut exp.exp {
        Exp::Ident(id, es) => {
            type_ident(ln, env, id, es)?
        }
        Exp::Call(f, args) => {
            let (p_typs, r_typ) = match env.funcs.get(&f) {
                Some(v) => v,
                None => return Err(format!("function doesnt exist on line: {}", ln.unwrap()))
            };
            let mut ts = Vec::new();
            for exp in args.iter_mut() {
                ts.push(type_exp(ln, env, exp)?);
            }
            if ts.len() == args.len() && ts.iter().zip(p_typs.iter()).all(|(t, a)| t==a) {
                r_typ.clone()
            }
            else {
                return Err(format!("many args line: {}", ln.unwrap()))
            }
        }
        Exp::Num(_) => Typ::Int,
        Exp::Bool(_) => Typ::Bool,
        Exp::Op(e1, op, e2) => {
            let t1 = type_exp(ln, env, &mut *e1)?;
            let t2 = type_exp(ln, env, &mut *e2)?;
            match (t1, op, t2) {
                (Typ::Bool, (OP::And | OP::Or | OP::Eq), Typ::Bool) => Typ::Bool,
                (Typ::Int, (OP::Plus | OP::Minus | OP::Times | OP::Div | OP::Lshift | OP::BitOr |
                    OP::BitAnd), Typ::Int) => Typ::Int,
                (Typ::Int, (OP::Lt | OP::Eq | OP::Gt), Typ::Int) => Typ::Bool,
                (_, _, _) => return Err(format!("no opetator matching op on line: {}", ln.unwrap()))
            }
        }
        Exp::Uop(uop, exp) => {
            let t = type_exp(ln, env, &mut *exp)?;
            match (uop, t) {
                (UOP::Not, Typ::Bool) => Typ::Bool,
                _ => return Err(format!("undefined uop on line: {}", ln.unwrap()))
            }
        }
        Exp::Array(es) => {
            let mut ts : Vec<Typ> = Vec::new();
            for exp in es.iter_mut() {
                ts.push(type_exp(ln, env, exp)?);
            }
            if ts.len() > 0 && ts.iter().all(|t| *t == Typ::Int) {
                Typ::Array(ts.len())
            }
            else {
                return Err(format!("array length issue on line: {}", ln.unwrap()))
            }
        }
    };
    exp.typ = Some(typ);
    Ok(typ)
}

fn get_var_types(scope: Scope, vars: &mut [VarDec]) -> Result<HashMap<ID, (Typ, Scope)>, String> {
    let mut var_types : HashMap<ID, (Typ, Scope)> = HashMap::new();
    for var in vars.iter_mut() {
        if var_types.contains_key(&var.var_name) {
            return Err(format!("duplicate function"))
        }
        else {
            var_types.insert(var.var_name.clone(), (var.typ, scope));
            add_scope(&mut var.var_name, scope)
        }
    }
    Ok(var_types)
}

fn type_var_dec(sc: &Scope, env: &Env, dec: &mut VarDec) -> Option<String>{
    let typ = match type_exp(dec.loc, env, &mut dec.init) {
        Ok(v) => v,
        Err(err) => return Some(err),
    };
    if typ != dec.typ {
        match dec.var_name.clone() {
            ID::Source(name, _) => Some(format!("type mismatch var '{}' on line '{}'", name, dec.loc.unwrap())),
            ID::Temp(name, _) => Some(format!("type mismatch var '{}' on line '{}'", name, dec.loc.unwrap())),
            _ => None
        }
    }
    else {
        None
    }
}

fn get_function_types(funcs : &[Func]) -> Result<HashMap<ID, (Vec<Typ>, Typ)>, String> {
    let mut func_types : HashMap<ID, (Vec<Typ>, Typ)> = HashMap::new();
    for func in funcs.iter() {
        if func_types.contains_key(&func.fun_name) {
            return Err(format!("duplicate function"))
        }
        else {
            func_types.insert(func.fun_name.clone(), (func.params.iter().map(|(_, typ)| *typ).collect(), func.ret));
        }
    }
    Ok(func_types)
}

fn get_param_types(ln: Option<usize>, params : &[(ID, Typ)]) -> Result<HashMap<ID, (Typ, Scope)>, String> {
    let mut param_typs : HashMap<ID, (Typ, Scope)> = HashMap::new();
    for param in params.iter() {
        if param_typs.contains_key(&param.0) {
            return Err(format!("duplicate parameter in function header line {}", ln.unwrap()))
        }
        else {
            param_typs.insert(param.0.clone(), (param.1, Scope::Parameter));
        }
    }
    Ok(param_typs)
}

fn type_stmt(ln: Option<usize>, env: &Env, ret : Typ, stmt : &mut Stmt) -> Option<String> {
    
    match stmt {
        Stmt::In(id) => {
            if type_ident(ln, env, id, &mut []) != Ok(Typ::Int) {
                return Some(format!("in identifyer not type int line: {}", ln.unwrap()))
            }
            None
        },
        Stmt::Out(id) => {
            if type_ident(ln, env, id, &mut []) != Ok(Typ::Int) {
                return Some(format!("in identifyer not type int line: {}", ln.unwrap()))
            }
            None
        },
        Stmt::Return(Some(id)) => {
            if type_ident(ln, env, id, &mut []) != Ok(ret) {
                return Some(format!("in identifyer not type int line: {}", ln.unwrap()))
            }
            None
        },
        Stmt::Return(_) => {
            return Some(format!("wut"))
        },
        Stmt::Assign(x, es, e) => {
            let ident = match type_ident(ln, env, x, es) {
                Ok(v) => v,
                Err(e) => return Some(e)
            };
            let t_exp = match type_exp(ln, env, e) {
                Ok(v) => v,
                Err(e) => return Some(e)
            };
            if ident != t_exp {
                return Some(format!("type missmatch line {}", ln.unwrap()))
            }
            None
        },
        Stmt::DoWhile(s1, e, s2) => {
            if type_exp(ln, env, e) != Ok(Typ::Bool) {
                return Some(format!("condition not type bool {}", ln.unwrap()))
            }
            if let Some(x) = type_stmt(ln, env, ret, s1) {
                return Some(x)
            };
            if let Some(x) = type_stmt(ln, env, ret, s2) {
                return Some(x)
            };
            None
        },
        Stmt::Ite(e, s1, s2) => {
            if type_exp(ln, env, e) != Ok(Typ::Bool) {
                return Some(format!("condition not type bool {}", ln.unwrap()))
            }
            if let Some(x) = type_stmt(ln, env, ret, s1) {
                return Some(x)
            }
            if let Some(x) = type_stmt(ln, env, ret, s2) {
                return Some(x)
            }
            None
        },
        Stmt::Stmts(stmts) => {
            for stmt in stmts.iter_mut() {
                if let Some(x) = type_stmt(ln, env, ret, stmt) {
                    return Some(x)
                }
            }
            None
        },
        Stmt::Loc(s, ln) => {
            if let Some(x) = type_stmt(Some(*ln), env, ret, s) {
                return Some(x)
            }
            None
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

fn type_fun(env: &Env, func: &mut Func) -> Option<String> {
    let param_env = match get_param_types(func.loc, &func.params) {
        Ok(v) => v,
        Err(e) => return Some(e),
    };
    let local_env = match get_var_types(Scope::Local, &mut func.locals) {
        Ok(v) => v,
        Err(e) => return Some(e),
    };
    let mut vars = env.vars.clone();
    for (param_id, param_val) in param_env {
        vars.insert(param_id.clone(), param_val.clone());
    }
    for (loc_id, loc_val) in local_env {
        vars.insert(loc_id.clone(), loc_val.clone());
    }
    let new_env = Env {vars : vars, funcs : env.funcs.clone()};
    for var_dec in func.locals.iter_mut() {
        if let Some(x) = type_var_dec(&Scope::Local, env, var_dec) {
            return Some(x)
        }
    }
    for stmt in func.body.iter_mut() {
        if let Some(x) = type_stmt(None, &new_env, func.ret, stmt) {
            return Some(x)
        }
    }
    if !check_return_paths(&func.body) {
        return Some(format!("function may not return"));
    }
    for (param, _) in func.params.iter_mut() {
        add_scope(param, Scope::Parameter);
    }
    None
}

pub fn type_prog(prog : &mut Prog) -> Option<String> {
    let env = Env{funcs: match get_function_types(&prog.funcs) {
        Ok(v) => v,
        Err(e) => return Some(e)
    },
        vars: match get_var_types(Scope::Global, &mut prog.globals)  {
        Ok(v) => v,
        Err(e) => return Some(e)
    }};
    for var_dec in prog.globals.iter_mut() {
        type_var_dec(&Scope::Global, &env, var_dec);
    }
    for func in prog.funcs.iter_mut() {
        if let Some(x) = type_fun(&env, func) {
            return Some(x)
        }
    }
    None
}