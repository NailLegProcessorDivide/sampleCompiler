use std::collections::{HashMap, HashSet};

use crate::{
    block_structure::{AtomicExp, BlockElem, CFGEntry, NextBlock, Test, Var},
    source_ast::{Scope, Typ, ID},
};

fn add_atomic_exp(ae: &AtomicExp, vars: &mut HashSet<Var>) {
    if let AtomicExp::Ident(v) = ae {
        vars.insert(v.clone());
    }
}

fn get_vars_from_ret(next: &NextBlock, vars: &mut HashSet<Var>) {
    match next {
        NextBlock::Return(Some(r)) => {
            vars.insert(r.clone());
        },
        NextBlock::Branch(Test { exp1, op: _, exp2 }, _, _) => {
            add_atomic_exp(exp1, vars);
            add_atomic_exp(exp2, vars);
        },
        _ => {},
    }
}

fn get_vars_from_block(block: &Vec<BlockElem>, vars: &mut HashSet<Var>) {
    for elem in block {
        match elem {
            BlockElem::AssignOp(r, ae1, _, ae2) => {
                vars.insert(r.clone());
                add_atomic_exp(ae1, vars);
                add_atomic_exp(ae2, vars);
            }
            BlockElem::AssignAtom(r, ae) => {
                vars.insert(r.clone());
                add_atomic_exp(ae, vars);
            }
            BlockElem::Ld(r1, r2, ae) => {
                vars.insert(r1.clone());
                vars.insert(r2.clone());
                add_atomic_exp(ae, vars);
            }
            BlockElem::St(r, ae1, ae2) => {
                vars.insert(r.clone());
                add_atomic_exp(ae1, vars);
                add_atomic_exp(ae2, vars);
            }
            BlockElem::Call(r, _, aes) => {
                if let Some(r) = r {
                    vars.insert(r.clone());
                }
                for ae in aes {
                    add_atomic_exp(ae, vars);
                }
            }
            BlockElem::BoundCheck(ae1, v) => {
                add_atomic_exp(ae1, vars);
                vars.insert(v.clone());
            }
            BlockElem::NullCheck(r) => {
                vars.insert(r.clone());
            }
        }
    }
}

fn get_vars(cfg: &Vec<&mut CFGEntry>) -> HashSet<Var> {
    let mut vars = HashSet::new();
    for block in cfg {
        get_vars_from_block(&block.elems, &mut vars);
        get_vars_from_ret(&block.next, &mut vars)
    }
    vars
}

fn is_local(v: &Var) -> bool {
    match v {
        Var::NamedSource(_, Scope::Local) | Var::NamedTmp(_, _) => true,
        _ => false,
    }
}

fn is_global(v: &Var) -> bool {
    match v {
        Var::NamedSource(_, Scope::Global) => true,
        _ => false,
    }
}

fn is_param(v: &Var) -> bool {
    match v {
        Var::NamedSource(_, Scope::Parameter) => true,
        _ => false,
    }
}

fn map_var(v: &mut Var, var_map: &HashMap<Var, Var>) {
    *v = var_map.get(v).expect("found var not in map").clone();
}

fn map_atomic_exp(ae: &mut AtomicExp, var_map: &HashMap<Var, Var>) {
    if let AtomicExp::Ident(v) = ae {
        map_var(v, var_map);
    }
}

fn map_elem(elem: &mut BlockElem, var_map: &HashMap<Var, Var>) {
    match elem {
        BlockElem::AssignOp(v, ae1, op, ae2) => {
            map_var(v, var_map);
            map_atomic_exp(ae1, var_map);
            map_atomic_exp(ae2, var_map);
        }
        BlockElem::AssignAtom(v, ae) => {
            map_var(v, var_map);
            map_atomic_exp(ae, var_map);
        }
        BlockElem::Ld(v1, v2, ae) => {
            map_var(v1, var_map);
            map_var(v2, var_map);
            map_atomic_exp(ae, var_map);
        }
        BlockElem::St(v, ae1, ae2) => {
            map_var(v, var_map);
            map_atomic_exp(ae1, var_map);
            map_atomic_exp(ae2, var_map);
        }
        BlockElem::Call(v, _, aes) => {
            if let Some(v) = v {
                map_var(v, var_map);
            }
            for ae in aes {
                map_atomic_exp(ae, var_map);
            }
        }
        BlockElem::BoundCheck(ae1, v) => {
            map_atomic_exp(ae1, var_map);
            map_var(v, var_map);
        }
        BlockElem::NullCheck(v) => {
            map_var(v, var_map);
        }
    }
}

fn map_next(next: &mut NextBlock, var_map: &HashMap<Var, Var>) {
    match next {
        NextBlock::Return(Some(v)) => map_var(v, var_map),
        NextBlock::Return(None) => {}
        NextBlock::Next(_) => {}
        NextBlock::Branch(Test { exp1, op, exp2 }, _, _) => {
            map_atomic_exp(exp1, var_map);
            map_atomic_exp(exp2, var_map);
        }
    }
}

pub fn reg_alloc(
    cfg: &mut Vec<&mut CFGEntry>,
    params: &Vec<(ID, Typ)>,
    param_regs: usize,
    alocatable_regs: usize,
) -> (usize, HashMap<Var, Var>) {
    let vars = get_vars(&cfg);
    let local_map: HashMap<Var, Var> = vars
        .iter()
        .filter(|v| is_local(*v))
        .enumerate()
        .map(|(n, v)| (v.clone(), Var::Stack(n as isize)))
        .collect();
    let global_map: HashMap<Var, Var> = vars
        .iter()
        .filter_map(|v| match v {
            Var::NamedSource(s, Scope::Global) => {
                Some((v.clone(), Var::Global(format!("{}_global", s))))
            }
            _ => None,
        })
        .collect();
    let mut overflow_count: isize = param_regs as isize;
    let mut param_map: HashMap<Var, Var> = HashMap::new();
    for (id, _) in params {
        let name = match id {
            ID::Source(n, _) => n,
            _ => panic!("parameter is temp value"),
        };
        overflow_count -= 1;
        if overflow_count >= 0 {
            param_map.insert(
                Var::NamedSource(name.clone(), Scope::Parameter),
                Var::Vreg(param_regs - 1 - (overflow_count as usize)),
            );
        } else {
            param_map.insert(
                Var::NamedSource(name.clone(), Scope::Parameter),
                Var::Stack(overflow_count),
            );
        }
    }
    let var_map: HashMap<Var, Var> = local_map
        .into_iter()
        .chain(param_map.into_iter())
        .chain(global_map.into_iter())
        .collect();
    for block in cfg {
        for elem in &mut block.elems {
            map_elem(elem, &var_map);
        }
        map_next(&mut block.next, &var_map);
    }
    return (vars.iter().filter(|v| is_local(*v)).count(), var_map);
}
