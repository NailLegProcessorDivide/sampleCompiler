use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::rc::Rc;

use crate::source_ast::{Exp, Func, Prog, Stmt, Typ, TypedExp, VarDec, ID};
use crate::tokens::{OP, UOP};

#[derive(Clone)]
struct ArrayVal {
    sizes: Vec<usize>,
    data: Vec<i64>,
}

#[derive(Clone)]
enum Val {
    VInt(i64),
    VArray(Rc<RefCell<ArrayVal>>),
}

fn val_to_int(val: &Val) -> i64 {
    match val {
        Val::VInt(i) => *i,
        _ => panic!("expected int"),
    }
}

#[derive(Clone)]
struct InterpEnv<'a> {
    funs: HashMap<ID, &'a Func>,
    vars: HashMap<ID, Val>,
}

fn init_val(val_type: Typ) -> Val {
    match val_type {
        Typ::Array(dims) => Val::VArray(Rc::new(RefCell::new(ArrayVal {
            sizes: vec![0; dims],
            data: Vec::new(),
        }))),
        _ => Val::VInt(0),
    }
}

fn do_op(op: OP, v1: Val, v2: Val) -> Val {
    match (v1, v2) {
        (Val::VInt(i1), Val::VInt(i2)) => Val::VInt(match op {
            OP::Plus => i1 + i2,
            OP::Minus => i1 - i2,
            OP::Times => i1 * i2,
            OP::Div if i2 == 0 => panic!("div by 0"),
            OP::Div => i1 / i2,
            OP::Lt => (i1 < i2) as i64,
            OP::Gt => (i1 > i2) as i64,
            OP::Eq => (i1 == i2) as i64,
            OP::Lshift => i1 << i2,
            OP::BitOr => i1 | i2,
            OP::BitAnd => i1 & i2,
            _ => panic!("unimplemented"),
        }),
        _ => panic!("type error"),
    }
}

fn do_uop(uop: UOP, v1: Val) -> Val {
    match (uop, v1) {
        (UOP::Not, Val::VInt(0)) => Val::VInt(1),
        (UOP::Not, Val::VInt(_)) => Val::VInt(0),
        _ => panic!("unimplemented"),
    }
}

fn get_array_address(env: &mut InterpEnv, arr: &ArrayVal, exps: &[TypedExp]) -> usize {
    if exps.len() != (*arr).sizes.len() {
        panic!("array dimension miss match")
    }
    let inds: Vec<Val> = exps.iter().map(|ind| interp_exp(env, ind)).collect();
    if !inds
        .iter()
        .zip((*arr).sizes.iter())
        .all(|(ind, dim)| (val_to_int(&ind) as usize) < *dim)
    {
        panic!("array index out of bounds")
    }
    let mut idx: usize = 0;
    for i in 0..inds.len() {
        idx = idx * (*arr).sizes[i] + (val_to_int(&inds[i]) as usize);
    }
    idx
}

fn interp_stmt(env: &mut InterpEnv, stmt: &Stmt) -> Option<Val> {
    match stmt {
        Stmt::Assign(i, exps, e) if exps.len() == 0 => {
            *env.vars.get_mut(i).unwrap() = interp_exp(env, e);
            None
        }
        Stmt::Assign(i, exps, e) => {
            let v = interp_exp(env, e);
            match env.vars.get_mut(i).unwrap().clone() {
                Val::VArray(arr) => {
                    let idx: usize = get_array_address(env, &*arr.borrow(), exps);
                    let val: i64 = val_to_int(&v);
                    arr.borrow_mut().data[idx] = val;
                }
                _ => panic!("type error"),
            };
            None
        }
        Stmt::DoWhile(head, e, body) => match interp_stmt(env, &*head) {
            None => {
                while val_to_int(&interp_exp(env, e)) == 1 {
                    match interp_stmt(env, &*body) {
                        Some(v) => return Some(v),
                        _ => {}
                    }
                }
                None
            }
            Some(v) => Some(v),
        },
        Stmt::Ite(e, s1, s2) => {
            if val_to_int(&interp_exp(env, e)) == 1 {
                interp_stmt(env, &*s1)
            } else {
                interp_stmt(env, &*s2)
            }
        }
        Stmt::Stmts(ss) => {
            for s in ss.iter() {
                if let Some(v) = interp_stmt(env, s) {
                    return Some(v);
                }
            }
            None
        }
        Stmt::Out(i) => {
            println!("{}", val_to_int(env.vars.get(i).unwrap()));
            None
        }
        Stmt::In(i) => {
            //https://users.rust-lang.org/t/how-to-read-an-integer-from-stdin/57538
            let mut input_line = String::new();
            io::stdin()
                .read_line(&mut input_line)
                .expect("Failed to read line");
            let v: i64 = input_line.trim().parse().expect("non integer input");
            *env.vars.get_mut(i).unwrap() = Val::VInt(v);
            None
        }
        Stmt::Return(Some(i)) => Some(env.vars.get(i).unwrap().clone()),
        Stmt::Loc(stmt, _) => interp_stmt(env, &*stmt),
        _ => panic!("unimplemented"),
    }
}

fn interp_exp(env: &mut InterpEnv, exp: &TypedExp) -> Val {
    match &exp.exp {
        Exp::Ident(i, inds) if inds.len() == 0 => env.vars.get(i).unwrap().clone(),
        Exp::Ident(i, inds) => match env.vars.get(i).unwrap().clone() {
            Val::VArray(arr) => {
                Val::VInt(arr.borrow().data[get_array_address(env, &*arr.borrow(), inds)])
            }
            _ => panic!("type error"),
        },
        Exp::Call(f, args) => {
            let func: &Func = env.funs.get(f).unwrap();
            let mut new_env: InterpEnv = env.clone();
            for (val, (id, _)) in args
                .iter()
                .map(|arg| interp_exp(env, arg))
                .zip(func.params.iter())
            {
                new_env.vars.insert(id.clone(), val);
            }
            interp_var_decs(&mut new_env, &func.locals);
            for stmt in func.body.iter() {
                if let Some(rv) = interp_stmt(&mut new_env, stmt) {
                    return rv;
                }
            }
            panic!("function didnt return")
        }
        Exp::Num(n) => Val::VInt(*n),
        Exp::Bool(b) => Val::VInt(*b as i64),
        Exp::Op(e1, OP::And, e2) => {
            let v1: Val = interp_exp(env, e1);
            match v1 {
                Val::VInt(1) => interp_exp(env, e2),
                Val::VInt(_) => Val::VInt(0),
                _ => panic!("type error"),
            }
        }
        Exp::Op(e1, OP::Or, e2) => {
            let v1: Val = interp_exp(env, e1);
            match v1 {
                Val::VInt(0) => interp_exp(env, e2),
                Val::VInt(_) => Val::VInt(1),
                _ => panic!("type error"),
            }
        }
        Exp::Op(e1, op, e2) => {
            let v1 = interp_exp(env, e1);
            let v2 = interp_exp(env, e2);
            do_op(*op, v1, v2)
        }
        Exp::Uop(uop, e) => do_uop(*uop, interp_exp(env, e)),
        Exp::Array(iexps) => {
            let sizes: Vec<usize> = iexps
                .iter()
                .map(|exp| match interp_exp(env, exp) {
                    Val::VInt(i) => i as usize,
                    _ => panic!("type error"),
                })
                .collect();
            let data = vec![0; sizes.iter().copied().reduce(|a, b| a * b).unwrap() as usize];
            Val::VArray(Rc::new(RefCell::new(ArrayVal { sizes, data })))
        }
        _ => todo!("impl"),
    }
}

fn interp_var_decs(env: &mut InterpEnv, var_decs: &[VarDec]) {
    for var_dec in var_decs.iter() {
        env.vars
            .insert(var_dec.var_name.clone(), init_val(var_dec.typ));
    }
    for var_dec in var_decs.iter() {
        *(env.vars.get_mut(&var_dec.var_name).unwrap()) = interp_exp(env, &var_dec.init);
    }
}

pub fn interp(prog: &Prog) {
    let mut func_env: HashMap<ID, &Func> = HashMap::new();
    for fun in prog.funcs.iter() {
        func_env.insert(fun.fun_name.clone(), fun);
    }
    let mut var_env: HashMap<ID, Val> = HashMap::new();
    for glob in prog.globals.iter() {
        var_env.insert(glob.var_name.clone(), init_val(glob.typ));
    }
    let mut interp_env: InterpEnv = InterpEnv {
        funs: func_env,
        vars: var_env,
    };
    interp_var_decs(&mut interp_env, &prog.globals);
}
