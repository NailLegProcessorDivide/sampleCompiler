use std::collections::HashMap;

use crate::source_ast::{Prog, Func, ID, VarDec, Typ, Exp};

#[derive(Clone)]
enum Val {
    VInt(i64),
    VArray(Vec<usize>, Vec<i64>)
}

fn val_to_int(val : &Val) -> i64 {
    match val {
        Val::VInt(i) => *i,
        _ => panic!("expected int")
    }
}

struct InterpEnv<'a> {
     funs : HashMap<ID, &'a Func>,
     vars : HashMap<ID, Box<Val>>
}

fn init_val(val_type: Typ) -> Val {
    match val_type {
        Typ::Array(dims) => Val::VArray(vec![0; dims], Vec::new()),
        _ => Val::VInt(0),
    }
}

fn interp_exp(env : &mut InterpEnv, exp: &Exp) -> Val {
    println!("exp");
    match exp {
        Exp::Ident(i, inds) if inds.len() > 0 => *env.vars.get(i).unwrap().clone(),
        Exp::Ident(i, inds) => {
            match *env.vars.get(i).unwrap().clone() {
                Val::VArray(dims, data) => {
                    if inds.len() != dims.len() {
                        panic!("array dimension miss match")
                    }
                    let inds : Vec<Val> = inds.iter().map(|ind| interp_exp(env, ind)).collect();
                    if !inds.iter().zip(dims.iter()).all(|(ind, dim)| (val_to_int(&ind) as usize) < *dim) {
                        panic!("array index out of bounds")
                    }
                    let mut idx : usize = 0;
                    for i in 0..inds.len() {
                        idx = idx * dims[i] + (val_to_int(&inds[i]) as usize);
                    }
                    Val::VInt(data[idx])
                },
                _ => panic!("type error")
            }
        }
        _ => todo!("impl")
    }

}

fn interp_var_decs(env : &mut InterpEnv, var_decs : &[VarDec]) {
    for var_dec in var_decs.iter() {
        **(env.vars.get_mut(&var_dec.var_name).unwrap()) = interp_exp(env, &var_dec.init);
    }
}

pub fn interp(prog : &Prog) {
    let mut func_env : HashMap<ID, &Func> = HashMap::new();
    for fun in prog.funcs.iter() {
        func_env.insert(fun.fun_name.clone(), fun);
    }
    let mut var_env : HashMap<ID, Box<Val>> = HashMap::new();
    for glob in prog.globals.iter() {
        var_env.insert(glob.var_name.clone(), Box::new(init_val(glob.typ)));
    }
    let mut interp_env : InterpEnv = InterpEnv{funs: func_env, vars : var_env};
    interp_var_decs(&mut interp_env, &prog.globals);
}
