use std::hash::{Hash, Hasher};

use crate::tokens::{self, print_token_list};
use tokens::{show_token, TokLoc, Token, OP, UOP};

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub enum Scope {
    Global,
    Parameter,
    Local,
}

#[derive(Clone)]
pub enum ID {
    Source(String, Option<Scope>),
    Temp(String, usize),
}

//doesnt check scope
impl Hash for ID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            ID::Source(n, _) => {
                n.hash(state);
            }
            ID::Temp(n, idx) => {
                n.hash(state);
                idx.hash(state);
            }
        }
    }
}

//doesnt check scope
impl PartialEq for ID {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ID::Source(n, _) => match other {
                ID::Source(on, _) => n == on,
                _ => false,
            },
            ID::Temp(n, idx) => match other {
                ID::Temp(on, oidx) => n == on && idx == oidx,
                _ => false,
            },
        }
    }
}
impl Eq for ID {}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Typ {
    Int,
    Bool,
    Array(usize),
}

#[derive(Clone, Eq, PartialEq)]
pub enum Exp {
    Ident(ID, Vec<TypedExp>),
    Call(ID, Vec<TypedExp>),
    Num(i64),
    Bool(bool),
    Op(Box<TypedExp>, OP, Box<TypedExp>),
    Uop(UOP, Box<TypedExp>),
    Array(Vec<TypedExp>),
}

#[derive(Clone, Eq, PartialEq)]
pub struct TypedExp {
    pub exp: Exp,
    pub typ: Option<Typ>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct VarDec {
    pub var_name: ID,
    pub typ: Typ,
    pub init: TypedExp,
    pub loc: Option<usize>,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Stmt {
    Assign(ID, Vec<TypedExp>, TypedExp),
    DoWhile(Box<Stmt>, TypedExp, Box<Stmt>),
    Ite(TypedExp, Box<Stmt>, Box<Stmt>),
    Stmts(Vec<Stmt>),
    In(ID),
    Out(ID),
    Return(Option<ID>),
    Loc(Box<Stmt>, usize),
}

#[derive(Clone, Eq, PartialEq)]
pub struct Func {
    pub fun_name: ID,
    pub params: Vec<(ID, Typ)>,
    pub ret: Typ,
    pub locals: Vec<VarDec>,
    pub body: Vec<Stmt>,
    pub loc: Option<usize>,
}

#[derive(Eq, PartialEq)]
pub struct Prog {
    pub globals: Vec<VarDec>,
    pub funcs: Vec<Func>,
}

pub fn scope_to_string(scope: &Option<Scope>) -> &str {
    match scope {
        Some(Scope::Global) => "global",
        Some(Scope::Parameter) => "param",
        Some(Scope::Local) => "local",
        None => "none",
    }
}

pub fn id_to_string(id: &ID) -> String {
    match id {
        ID::Source(s, scope) => s.clone() + "_" + scope_to_string(scope),
        ID::Temp(s, id) => format!("{}_{}", s.clone(), id),
    }
}

pub fn typ_to_string(t: &Typ) -> String {
    match t {
        Typ::Int => "int".to_string(),
        Typ::Bool => "bool".to_string(),
        Typ::Array(n) => format!("array {}", n),
    }
}

fn parse_typ(toks: &[TokLoc]) -> Result<(Typ, &[TokLoc]), String> {
    match toks {
        [TokLoc {
            tok: Token::Int,
            loc: _,
        }, toks @ ..] => Ok((Typ::Int, toks)),
        [TokLoc {
            tok: Token::Bool,
            loc: _,
        }, toks @ ..] => Ok((Typ::Bool, toks)),
        [TokLoc {
            tok: Token::Array,
            loc: _,
        }, TokLoc {
            tok: Token::Num(array_size),
            loc: _,
        }, toks @ ..] => Ok((Typ::Array(*array_size as usize), toks)),
        _ => Err("type error at".to_string()),
    }
}

fn parse_args(mut toks: &[TokLoc]) -> Result<(Vec<TypedExp>, &[TokLoc]), String> {
    let mut args = Vec::new();
    //while true feels scuffed but its probably fine
    loop {
        let (exp, rem_toks) = parse_exp(toks)?;
        toks = rem_toks;
        args.push(exp);
        match toks {
            [] => return Err("out of tokens!".to_string()),
            [TokLoc {
                tok: Token::Comma,
                loc: _,
            }, rem_toks @ ..] => {
                toks = rem_toks;
            }
            [TokLoc {
                tok: Token::Rparen,
                loc: _,
            }, toks @ ..] => return Ok((args, toks)),
            _ => {
                return Err(format!(
                    "expected ',' or ')' found {}",
                    show_token(&(toks.get(0).unwrap()).tok)
                ))
            }
        }
    }
}

fn parse_indices(mut toks: &[TokLoc]) -> Result<(Vec<TypedExp>, &[TokLoc]), String> {
    let mut inds = Vec::new();
    loop {
        match toks {
            [] => return Err("eof parsing indicies".to_string()),
            [TokLoc {
                tok: Token::Lbrac,
                loc: _,
            }, rem_toks @ ..] => match parse_exp(rem_toks)? {
                (_, []) => return Err("eof in indicies".to_string()),
                (
                    exp,
                    [TokLoc {
                        tok: Token::Rbrac,
                        loc: _,
                    }, rem_toks @ ..],
                ) => {
                    toks = rem_toks;
                    inds.push(exp);
                }
                _ => {
                    return Err(format!(
                        "unexpected token {} while parsing indicies",
                        show_token(&(rem_toks.get(0).unwrap()).tok)
                    ))
                }
            },
            _ => return Ok((inds, toks)),
        }
    }
}

fn parse_atomic_exp(toks: &[TokLoc]) -> Result<(TypedExp, &[TokLoc]), String> {
    match toks {
        [] => Err("eof parsing expr".to_string()),
        [TokLoc {
            tok: Token::Ident(ident),
            loc: _,
        }, TokLoc {
            tok: Token::Lparen,
            loc: _,
        }, toks @ ..] => {
            let (args, toks) = parse_args(toks)?;
            Ok((
                TypedExp {
                    exp: Exp::Call(ID::Source(ident.clone(), None), args),
                    typ: None,
                },
                toks,
            ))
        }
        [TokLoc {
            tok: Token::Ident(ident),
            loc: _,
        }, toks @ ..] => {
            let (indicies, toks) = parse_indices(toks)?;
            Ok((
                TypedExp {
                    exp: Exp::Ident(ID::Source(ident.to_string(), None), indicies),
                    typ: None,
                },
                toks,
            ))
        }
        [TokLoc {
            tok: Token::Num(n),
            loc: _,
        }, toks @ ..] => Ok((
            TypedExp {
                exp: Exp::Num(*n),
                typ: None,
            },
            toks,
        )),
        [TokLoc {
            tok: Token::True,
            loc: _,
        }, toks @ ..] => Ok((
            TypedExp {
                exp: Exp::Bool(true),
                typ: None,
            },
            toks,
        )),
        [TokLoc {
            tok: Token::False,
            loc: _,
        }, toks @ ..] => Ok((
            TypedExp {
                exp: Exp::Bool(false),
                typ: None,
            },
            toks,
        )),
        [TokLoc {
            tok: Token::Op(OP::Minus),
            loc: _,
        }, toks @ ..] => {
            let (exp, toks) = parse_atomic_exp(toks)?;
            Ok((
                TypedExp {
                    exp: Exp::Op(
                        Box::new(TypedExp {
                            exp: Exp::Num(0),
                            typ: None,
                        }),
                        OP::Minus,
                        Box::new(exp),
                    ),
                    typ: None,
                },
                toks,
            ))
        }
        [TokLoc {
            tok: Token::Uop(uop),
            loc: _,
        }, toks @ ..] => {
            let (exp, toks) = parse_atomic_exp(toks)?;
            Ok((
                TypedExp {
                    exp: Exp::Uop(uop.clone(), Box::new(exp)),
                    typ: None,
                },
                toks,
            ))
        }
        [TokLoc {
            tok: Token::Array,
            loc: _,
        }, toks @ ..] => {
            let (inds, toks) = parse_indices(toks)?;
            Ok((
                TypedExp {
                    exp: Exp::Array(inds),
                    typ: None,
                },
                toks,
            ))
        }
        [TokLoc {
            tok: Token::Lparen,
            loc: _,
        }, toks @ ..] => match parse_exp(toks)? {
            (
                exp,
                [TokLoc {
                    tok: Token::Rparen,
                    loc: _,
                }, toks @ ..],
            ) => Ok((exp, toks)),
            (_, []) => return Err("eof parsing bracketed expression".to_string()),
            _ => {
                return Err(format!(
                    "expected ')' found {}",
                    show_token(&(toks.get(0).unwrap()).tok)
                ))
            }
        },
        _ => {
            return Err(format!(
                "unimplemented exp starting with '{}' within '{}'",
                show_token(&(toks.get(0).unwrap()).tok),
                &(toks.get(0).unwrap()).loc
            ))
        }
    }
}

fn parse_exp(toks: &[TokLoc]) -> Result<(TypedExp, &[TokLoc]), String> {
    match parse_atomic_exp(toks)? {
        (
            e1,
            [TokLoc {
                tok: Token::Op(o),
                loc: _,
            }, toks @ ..],
        ) => {
            let (e2, toks) = parse_atomic_exp(toks)?;
            Ok((
                TypedExp {
                    exp: Exp::Op(Box::new(e1), o.clone(), Box::new(e2)),
                    typ: None,
                },
                toks,
            ))
        }
        (exp, toks) => Ok((exp, toks)),
    }
}

fn parse_var_dec(mut toks: &[TokLoc]) -> Result<(VarDec, &[TokLoc]), String> {
    match toks {
        [TokLoc {
            tok: Token::Let,
            loc: line_number,
        }, TokLoc {
            tok: Token::Ident(x),
            loc: _,
        }, TokLoc {
            tok: Token::Colon,
            loc: _,
        }, rem_toks @ ..] => {
            toks = rem_toks;
            let (typ, rem_toks) = parse_typ(toks)?;
            toks = rem_toks;
            match (typ, toks) {
                (
                    t,
                    [TokLoc {
                        tok: Token::Op(OP::Eq),
                        loc: _,
                    }, rem_toks @ ..],
                ) => {
                    toks = rem_toks;
                    let (exp, rem_toks) = parse_exp(toks)?;
                    toks = rem_toks;
                    Ok((
                        VarDec {
                            var_name: ID::Source(x.to_string(), None),
                            typ: t,
                            init: exp,
                            loc: Some(*line_number),
                        },
                        toks,
                    ))
                }
                _ => return Err(format!("invalid vardec line {}", line_number)),
            }
        }
        _ => {
            return Err(format!(
                "expected variable declaration {}: '{}'",
                toks.get(0).unwrap().loc,
                show_token(&(toks.get(0).unwrap()).tok)
            ))
        }
    }
}

fn parse_param(toks: &[TokLoc]) -> Result<((ID, Typ), &[TokLoc]), String> {
    match toks {
        [] => panic!("eof parsing function parameters"),
        [TokLoc {
            tok: Token::Lparen,
            loc: _,
        }, TokLoc {
            tok: Token::Ident(x),
            loc: _,
        }, TokLoc {
            tok: Token::Colon,
            loc: _,
        }, toks @ ..] => match parse_typ(toks)? {
            (_, []) => panic!("eof parsing function parameters"),
            (
                t,
                [TokLoc {
                    tok: Token::Rparen,
                    loc: _,
                }, toks @ ..],
            ) => Ok(((ID::Source(x.to_string(), None), t), toks)),
            (_, [TokLoc { tok: _, loc }, ..]) => {
                Err(format!("error parsing function parameter on line {}", *loc))
            }
        },
        [TokLoc { tok, loc }, ..] => Err(format!(
            "error parsing function parameter on line {} starting '{}'",
            *loc,
            show_token(tok)
        )),
    }
}

fn parse_param_list(mut toks: &[TokLoc]) -> Result<(Vec<(ID, Typ)>, &[TokLoc]), String> {
    let mut params = Vec::new();
    loop {
        match toks {
            [TokLoc {
                tok: Token::Lparen,
                loc: _,
            }, ..] => {
                let (param, rem_toks) = parse_param(toks)?;
                params.push(param);
                toks = rem_toks;
            }
            _ => return Ok((params, toks)),
        }
    }
}

fn parse_stmt(toks: &[TokLoc]) -> Result<(Stmt, &[TokLoc]), String> {
    match toks {
        [] => panic!("eof parsing statement"),
        [TokLoc {
            tok: Token::Ident(x),
            loc: ln,
        }, toks @ ..] => match parse_indices(toks)? {
            (_, []) => Err(format!("eof parsing statement")),
            (
                indices,
                [TokLoc {
                    tok: Token::Assign,
                    loc: _,
                }, toks @ ..],
            ) => {
                let (exp, toks) = parse_exp(toks)?;
                Ok((
                    Stmt::Loc(
                        Box::new(Stmt::Assign(ID::Source(x.to_string(), None), indices, exp)),
                        *ln,
                    ),
                    toks,
                ))
            }
            (_, [TokLoc { tok, loc }, ..]) => Err(format!(
                "error parsing function parameter on line {} starting '{}'",
                *loc,
                show_token(tok)
            )),
        },
        [TokLoc {
            tok: Token::While,
            loc: ln,
        }, toks @ ..] => {
            let (e, toks) = parse_exp(toks)?;
            let (s, toks) = parse_stmt(toks)?;
            Ok((
                Stmt::Loc(
                    Box::new(Stmt::DoWhile(
                        Box::new(Stmt::Stmts(Vec::new())),
                        e,
                        Box::new(s),
                    )),
                    *ln,
                ),
                toks,
            ))
        }
        [TokLoc {
            tok: Token::Do,
            loc: _,
        }, toks @ ..] => match parse_stmt(toks)? {
            (_, []) => Err("eof in do statement".to_string()),
            (
                s,
                [TokLoc {
                    tok: Token::While,
                    loc: ln,
                }, toks @ ..],
            ) => {
                let (e, toks) = parse_exp(toks)?;
                Ok((
                    Stmt::Loc(
                        Box::new(Stmt::DoWhile(
                            Box::new(s),
                            e,
                            Box::new(Stmt::Stmts(Vec::new())),
                        )),
                        *ln,
                    ),
                    toks,
                ))
            }
            (_, [TokLoc { tok, loc }, ..]) => Err(format!(
                "error parsing do statement on line {} starting '{}'",
                *loc,
                show_token(tok)
            )),
        },
        [TokLoc {
            tok: Token::If,
            loc: ln,
        }, toks @ ..] => match parse_exp(toks)? {
            (_, []) => Err("eof in if statement".to_string()),
            (
                cond,
                [TokLoc {
                    tok: Token::Then,
                    loc: ln,
                }, toks @ ..],
            ) => match parse_stmt(toks)? {
                (_, []) => Err("eof in if statement".to_string()),
                (
                    then_exp,
                    [TokLoc {
                        tok: Token::Else,
                        loc: ln,
                    }, toks @ ..],
                ) => {
                    let (else_exp, toks) = parse_stmt(toks)?;
                    Ok((
                        Stmt::Loc(
                            Box::new(Stmt::Ite(cond, Box::new(then_exp), Box::new(else_exp))),
                            *ln,
                        ),
                        toks,
                    ))
                }
                (_, [TokLoc { tok, loc }, ..]) => Err(format!(
                    "error parsing else on line {} starting '{}'",
                    *loc,
                    show_token(tok)
                )),
            },
            (_, [TokLoc { tok, loc }, ..]) => Err(format!(
                "error parsing then parameter on line {} starting '{}'",
                *loc,
                show_token(tok)
            )),
        },
        [TokLoc {
            tok: Token::Lcurly,
            loc: ln,
        }, toks @ ..] => {
            let (s_list, toks) = parse_stmt_list(toks)?;
            Ok((Stmt::Loc(Box::new(Stmt::Stmts(s_list)), *ln), toks))
        }
        [TokLoc {
            tok: Token::Input,
            loc: ln,
        }, TokLoc {
            tok: Token::Ident(x),
            loc: _,
        }, toks @ ..] => Ok((
            Stmt::Loc(Box::new(Stmt::In(ID::Source(x.to_string(), None))), *ln),
            toks,
        )),
        [TokLoc {
            tok: Token::Output,
            loc: ln,
        }, TokLoc {
            tok: Token::Ident(x),
            loc: _,
        }, toks @ ..] => Ok((
            Stmt::Loc(Box::new(Stmt::Out(ID::Source(x.to_string(), None))), *ln),
            toks,
        )),
        [TokLoc {
            tok: Token::Return,
            loc: ln,
        }, TokLoc {
            tok: Token::Ident(x),
            loc: _,
        }, toks @ ..] => Ok((
            Stmt::Loc(
                Box::new(Stmt::Return(Some(ID::Source(x.to_string(), None)))),
                *ln,
            ),
            toks,
        )),
        [TokLoc { tok, loc }, ..] => Err(format!(
            "error parsing statement on line {} starting '{}'",
            *loc,
            show_token(tok)
        )),
    }
}

fn parse_stmt_list(mut toks: &[TokLoc]) -> Result<(Vec<Stmt>, &[TokLoc]), String> {
    let mut stmts = Vec::new();
    loop {
        match toks {
            [TokLoc {
                tok: Token::Rcurly,
                loc: _,
            }, rem_toks @ ..] => return Ok((stmts, rem_toks)),
            _ => {
                let (stmt, rem_toks) = parse_stmt(toks)?;
                toks = rem_toks;
                stmts.push(stmt);
            }
        }
    }
}

fn parse_var_dec_list(mut toks: &[TokLoc]) -> Result<(Vec<VarDec>, &[TokLoc]), String> {
    let mut var_decs = Vec::new();
    while let Some(TokLoc {
        tok: Token::Let,
        loc: _,
    }) = toks.get(0)
    {
        let (dec, rem_toks) = parse_var_dec(toks)?;
        toks = rem_toks;
        var_decs.push(dec);
    }
    Ok((var_decs, toks))
}

fn parse_func(toks: &[TokLoc]) -> Result<(Func, &[TokLoc]), String> {
    match toks {
        [] => return Err("end of file looking for function".to_string()),
        [TokLoc {
            tok: Token::Function,
            loc: func_loc,
        }, TokLoc {
            tok: Token::Ident(func_name),
            loc: _,
        }, toks @ ..] => match parse_param_list(toks)? {
            (_, []) => return Err("eof parsing function".to_string()),
            (
                params,
                [TokLoc {
                    tok: Token::Colon,
                    loc: _,
                }, toks @ ..],
            ) => {
                if params.len() != 0 {
                    match parse_typ(toks)? {
                        (_, []) => return Err("eof parsing function".to_string()),
                        (
                            t,
                            [TokLoc {
                                tok: Token::Lcurly,
                                loc: _,
                            }, toks @ ..],
                        ) => {
                            let (var_decs, toks) = parse_var_dec_list(toks)?;
                            let (stmts, toks) = parse_stmt_list(toks)?;
                            return Ok((
                                Func {
                                    fun_name: ID::Source(func_name.to_string(), None),
                                    params: params,
                                    ret: t,
                                    locals: var_decs,
                                    body: stmts,
                                    loc: Some(*func_loc),
                                },
                                toks,
                            ));
                        }
                        (_, [TokLoc { tok: _, loc }, ..]) => {
                            return Err(format!(
                                "error parsing function expected '{{' on line {}",
                                *loc
                            ))
                        }
                    }
                } else {
                    return Err("error parsing function".to_string());
                }
            }
            (_, [TokLoc { tok: _, loc }, ..]) => {
                return Err(format!("error parsing function on line {}", *loc))
            }
        },
        [TokLoc { tok: _, loc }, ..] => {
            return Err(format!("error parsing function on line {}", *loc))
        }
    }
}

fn parse_funcs(mut toks: &[TokLoc]) -> Result<Vec<Func>, String> {
    let mut funcs = Vec::new();
    loop {
        match toks {
            [] => return Ok(funcs),
            _ => {
                let (func, rem_toks) = parse_func(toks)?;
                funcs.push(func);
                toks = rem_toks;
            }
        }
    }
}

pub fn parse_program(toks: &[TokLoc]) -> Result<Prog, String> {
    let (globals, toks) = parse_var_dec_list(toks)?;
    let funcs = parse_funcs(toks)?;
    Ok(Prog {
        globals: globals,
        funcs: funcs,
    })
}

pub fn stmts_to_stmt(stmts: Vec<Stmt>) -> Stmt {
    if stmts.len() == 1 {
        stmts[0].clone()
    } else {
        Stmt::Stmts(stmts)
    }
}
