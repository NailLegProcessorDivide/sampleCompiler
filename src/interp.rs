use std::collections::HashMap;
use std::rc::Rc;

use crate::source_ast::{Prog, Func, ID, VarDec, Typ, Exp, Stmt};

struct ArrayVal {
    sizes : Vec<usize>,
    data : Vec<i64>
}

#[derive(Clone)]
enum Val {
    VInt(i64),
    VArray(Rc<ArrayVal>)
}

fn val_to_int(val : &Val) -> i64 {
    match val {
        Val::VInt(i) => *i,
        _ => panic!("expected int")
    }
}

#[derive(Clone)]
struct InterpEnv<'a> {
     funs : HashMap<ID, &'a Func>,
     vars : HashMap<ID, Val>
}

fn init_val(val_type: Typ) -> Val {
    match val_type {
        Typ::Array(dims) => Val::VArray(Rc::new(ArrayVal{sizes : vec![0; dims], data : Vec::new()})),
        _ => Val::VInt(0),
    }
}

fn interp_stmt(env : &mut InterpEnv, stmt: &Stmt) -> Option<Val> {
    match stmt {
        _ => panic!("unimplemented")
    }
}

fn interp_exp(env : &mut InterpEnv, exp: &Exp) -> Val {
    println!("exp");
    match exp {
        Exp::Ident(i, inds) if inds.len() == 0 => env.vars.get(i).unwrap().clone(),
        Exp::Ident(i, inds) => {
            match env.vars.get(i).unwrap().clone() {
                Val::VArray(arr) => {
                    if inds.len() != (*arr).sizes.len() {
                        panic!("array dimension miss match")
                    }
                    let inds : Vec<Val> = inds.iter().map(|ind| interp_exp(env, ind)).collect();
                    if !inds.iter().zip((*arr).sizes.iter()).all(|(ind, dim)| (val_to_int(&ind) as usize) < *dim) {
                        panic!("array index out of bounds")
                    }
                    let mut idx : usize = 0;
                    for i in 0..inds.len() {
                        idx = idx * (*arr).sizes[i] + (val_to_int(&inds[i]) as usize);
                    }
                    Val::VInt((*arr).data[idx])
                },
                _ => panic!("type error")
            }
        }
        Exp::Call(f, args) => {
            let func : &Func = env.funs.get(f).unwrap();
            let mut new_env : InterpEnv = env.clone();
            for (val, (id, _)) in  args.iter().map(|arg| interp_exp(env, arg)).zip(func.params.iter()) {
                new_env.vars.insert(id.clone(), val);
            }
            interp_var_decs(& mut new_env, &func.locals);
            for stmt in func.body.iter() {
                if let Some(rv) = interp_stmt(&mut new_env, stmt) {
                    return rv;
                }
            }
            panic!("function didnt return")
        }
        _ => todo!("impl")
    }
}

fn interp_var_decs(env : &mut InterpEnv, var_decs : &[VarDec]) {
    for var_dec in var_decs.iter() {
        *(env.vars.get_mut(&var_dec.var_name).unwrap()) = interp_exp(env, &var_dec.init);
    }
}

pub fn interp(prog : &Prog) {
    let mut func_env : HashMap<ID, &Func> = HashMap::new();
    for fun in prog.funcs.iter() {
        func_env.insert(fun.fun_name.clone(), fun);
    }
    let mut var_env : HashMap<ID, Val> = HashMap::new();
    for glob in prog.globals.iter() {
        var_env.insert(glob.var_name.clone(), init_val(glob.typ));
    }
    let mut interp_env : InterpEnv = InterpEnv{funs: func_env, vars : var_env};
    interp_var_decs(&mut interp_env, &prog.globals);
}
