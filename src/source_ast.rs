use crate::tokens::{self, print_token_list};
use tokens::{OP, UOP, Token, TokLoc, show_token};

#[derive(Clone)]
pub enum Scope {
    Global,
    Parameter,
    Local
}

#[derive(Clone)]
pub enum ID {
    Source(String, Option<Scope>),
    Temp(String, i64)
}

pub enum Typ {
    Int,
    Bool,
    Array(i64)
}

#[derive(Clone)]
pub enum Exp {
    Ident(ID, Vec<Exp>),
    Call(ID, Vec<Exp>),
    Num(i64),
    Bool(bool),
    Op(Box<Exp>, OP, Box<Exp>),
    Uop(UOP, Box<Exp>),
    Array(Vec<Exp>)
}

pub struct VarDec {
    pub var_name : ID,
    pub typ : Typ,
    pub init : Exp,
    pub loc : Option<usize>
}

#[derive(Clone)]
pub enum Stmt {
    Assign(ID, Vec<Exp>, Exp),
    DoWhile(Box<Stmt>, Exp, Box<Stmt>),
    Ite(Exp, Box<Stmt>, Box<Stmt>),
    Stmts(Vec<Stmt>),
    In(ID),
    Out(ID),
    Return(Option<ID>),
    Loc(Box<Stmt>, usize)
}

pub struct Func {
    pub fun_name: ID,
    pub params: Vec<(ID, Typ)>,
    pub ret: Typ,
    pub locals: Vec<VarDec>,
    pub body: Vec<Stmt>,
    pub loc: Option<usize>,
}

pub struct Prog {
    pub var_dec : Vec<VarDec>,
    pub funcs : Vec<Func>,
}

fn parse_typ (toks: &[TokLoc]) -> (Typ, &[TokLoc]) {
    match toks {
        [TokLoc {tok: Token::Int, loc: _}, toks @ ..] => (Typ::Int, toks),
        [TokLoc {tok: Token::Bool, loc: _}, toks @ ..]=> (Typ::Bool, toks),
        [TokLoc {tok: Token::Array, loc: _}, TokLoc {tok: Token::Num(array_size), loc: _}, toks @ ..] => (Typ::Array(*array_size), toks),
        _ => panic!("type error at")
    }
}

fn parse_args(mut toks: &[TokLoc]) -> (Vec<Exp>, &[TokLoc]) {
    let mut args = Vec::new();
    println!("args");
    //while true feels scuffed but its probably fine
    loop {
        let (exp, rem_toks) = parse_exp(toks);
        toks = rem_toks;
        args.push(exp);
        match toks {
            [] => panic!("out of tokens!"),
            [TokLoc{tok: Token::Comma, loc: _}, rem_toks @ ..] => {
                toks = rem_toks;
            }
            [TokLoc{tok: Token::Rparen, loc: _}, toks @ ..] => {
                return (args, toks)
            }
            _ => panic!("expected ',' or ')' found {}", show_token(&(toks.get(0).unwrap()).tok))
        }
    }
}

fn parse_indices(mut toks: &[TokLoc]) -> (Vec<Exp>, &[TokLoc]) {
    let mut inds = Vec::new();
    loop {
        match toks {
            [] => panic!("eof parsing indicies"),
            [TokLoc{tok: Token::Lbrac, loc: _}, rem_toks @ ..] => {
                match parse_exp(rem_toks) {
                    (_, []) => panic!("eof in indicies"),
                    (exp, [TokLoc{tok: Token::Rbrac, loc: _}, rem_toks @ ..]) => {
                        toks = rem_toks;
                        inds.push(exp);
                    },
                    _ => panic!("unexpected token {} while parsing indicies", show_token(&(rem_toks.get(0).unwrap()).tok))
                }
            },
            _ => return (inds, toks)
        }
    }
}

fn parse_atomic_exp(toks: &[TokLoc]) -> (Exp, &[TokLoc]) {
    match toks {
        [] => panic!("eof parsing expr"),
        [TokLoc{tok: Token::Ident(ident), loc: _}, TokLoc{tok: Token::Lparen, loc: _}, toks @ ..] => {
            let (args, toks) = parse_args(toks);
            (Exp::Call(ID::Source(ident.clone(), None), args), toks)
        },
        [TokLoc{tok: Token::Ident(ident), loc: _}, toks @ ..] => {
            let (indicies, toks) = parse_indices(toks);
            (Exp::Ident(ID::Source(ident.to_string(), None), indicies), toks)
        },
        [TokLoc{tok: Token::Num(n), loc: _}, toks @ ..] => (Exp::Num(*n), toks),
        [TokLoc{tok: Token::True, loc: _}, toks @ ..] => (Exp::Bool(true), toks),
        [TokLoc{tok: Token::False, loc: _}, toks @ ..] => (Exp::Bool(false), toks),
        [TokLoc{tok: Token::Op(OP::Minus), loc: _}, toks @ ..] => {
            let (exp, toks) = parse_atomic_exp(toks);
            (Exp::Op(Box::new(Exp::Num(0)), OP::Minus, Box::new(exp)), toks)
        },
        [TokLoc{tok: Token::Uop(uop), loc: _}, toks @ ..] => {
            let (exp, toks) = parse_atomic_exp(toks);
            (Exp::Uop(uop.clone(), Box::new(exp)), toks)
        },
        [TokLoc{tok: Token::Array, loc: _}, toks @ ..] => {
            let (inds, toks) = parse_indices(toks);
            (Exp::Array(inds), toks)
        },
        [TokLoc{tok: Token::Lparen, loc: _}, toks @ ..] => {
            match parse_exp(toks) {
                (exp, [TokLoc{tok:Token::Rparen, loc: _}, toks @ ..]) => (exp, toks),
                (_, []) => panic!("eof parsing bracketed expression"),
                _ => panic!("expected ')' found {}", show_token(&(toks.get(0).unwrap()).tok))
            }
        }
        _ => todo!("unimplemented exp starting with '{}' within '{}'", show_token(&(toks.get(0).unwrap()).tok), &(toks.get(0).unwrap()).loc)
    }

}

fn parse_exp(toks: &[TokLoc]) -> (Exp, &[TokLoc]) {
    match parse_atomic_exp(toks) {
        (e1, [TokLoc{tok: Token::Op(o), loc: _}, toks @ ..]) => {
            let (e2, toks) = parse_atomic_exp(toks);
            (Exp::Op(Box::new(e1), o.clone(), Box::new(e2)), toks)
        },
        (exp, toks) => (exp, toks)
    }
}

fn parse_var_dec(mut toks: &[TokLoc]) -> (VarDec, &[TokLoc]) {
    match toks {
        [TokLoc {tok: Token::Let, loc: line_number},
         TokLoc {tok: Token::Ident(x), loc: _},
         TokLoc {tok: Token::Colon, loc: _}, rem_toks @ ..] => {
            toks = rem_toks;
            let (typ, rem_toks) = parse_typ(toks);
            toks = rem_toks;
            match (typ, toks) {
                (t, [TokLoc{tok: Token::Op(OP::Eq), loc: _}, rem_toks @ ..]) => {
                    toks = rem_toks;
                    let (exp, rem_toks) = parse_exp(toks);
                    toks = rem_toks;
                    (VarDec { var_name: ID::Source(x.to_string(), None), typ: t, init: exp, loc: Some(*line_number)}, toks)
                },
                _ => panic!("invalid vardec line {}", line_number)
            }
        }
        _ => panic!("expected variable declaration {}: '{}'", toks.get(0).unwrap().loc, show_token(&(toks.get(0).unwrap()).tok))
    }
}

fn parse_param(toks: &[TokLoc]) -> ((ID, Typ), &[TokLoc]) {
    match toks {
        [] => panic!("eof parsing function parameters"),
        [TokLoc{tok: Token::Lparen, loc: _},
         TokLoc{tok: Token::Ident(x), loc: _},
         TokLoc{tok: Token::Colon, loc: _},
         toks @ ..] => {
            match parse_typ(toks) {
                (_, []) => panic!("eof parsing function parameters"),
                (t, [TokLoc{tok: Token::Rparen, loc: _}, toks @ ..]) => {
                    ((ID::Source(x.to_string(), None), t), toks)
                },
                (_, [TokLoc{tok: _, loc}, ..]) => panic!("error parsing function parameter on line {}", *loc)
            }
        },
        [TokLoc{tok, loc}, ..] => panic!("error parsing function parameter on line {} starting '{}'", *loc, show_token(tok))
    }
}

fn parse_param_list(mut toks: &[TokLoc]) -> (Vec<(ID, Typ)>, &[TokLoc]) {
    let mut params = Vec::new();
    loop {
        match toks {
            [TokLoc{tok: Token::Lparen, loc: _}, ..] => {
                let (param, rem_toks) = parse_param(toks);
                params.push(param);
                toks = rem_toks;
            },
            _ => return (params, toks)
        }
    }
}

fn parse_stmt(toks: &[TokLoc]) -> (Stmt, &[TokLoc]) {
    match toks {
        [] => panic!("eof parsing statement"),
        [TokLoc{tok: Token::Ident(x), loc: ln}, toks @ ..] => {
            match parse_indices(toks) {
                (_, []) => panic!("eof parsing statement"),
                (indices, [TokLoc{tok: Token::Assign, loc: _}, toks @ ..]) => {
                    let (exp, toks) = parse_exp(toks);
                    (Stmt::Loc(Box::new(Stmt::Assign(ID::Source(x.to_string(), None), indices, exp)), *ln), toks)
                },
                (_, [TokLoc{tok, loc}, ..]) => panic!("error parsing function parameter on line {} starting '{}'", *loc, show_token(tok))
            }
        },
        [TokLoc{tok: Token::While, loc: ln}, toks @ ..] => {
            let (e, toks) = parse_exp(toks);
            let (s, toks) = parse_stmt(toks);
            (Stmt::Loc(Box::new(Stmt::DoWhile(Box::new(Stmt::Stmts(Vec::new())), e, Box::new(s))), *ln), toks)
        },
        [TokLoc{tok: Token::Do, loc: _}, toks @ ..] => {
            match parse_stmt(toks) {
                (_, []) => panic!("eof in do statement"),
                (s, [TokLoc{tok: Token::While, loc: ln}, toks @ ..]) => {
                    let (e, toks) = parse_exp(toks);
                    (Stmt::Loc(Box::new(Stmt::DoWhile(Box::new(s), e, Box::new(Stmt::Stmts(Vec::new())))), *ln), toks)
                }
                (_, [TokLoc{tok, loc}, ..]) => panic!("error parsing do statement on line {} starting '{}'", *loc, show_token(tok))
            }
        },
        [TokLoc{tok: Token::If, loc: ln}, toks @ ..] => {
            match parse_exp(toks) {
                (_, []) => panic!("eof in if statement"),
                (cond, [TokLoc{tok: Token::Then, loc: ln}, toks @ ..]) => {
                    match parse_stmt(toks) {
                        (_, []) => panic!("eof in if statement"),
                        (then_exp, [TokLoc{tok: Token::Else, loc: ln}, toks @ ..]) => {
                            let (else_exp, toks) = parse_stmt(toks);
                            (Stmt::Loc(Box::new(Stmt::Ite(cond, Box::new(then_exp), Box::new(else_exp))), *ln), toks)
                        },
                        (_, [TokLoc{tok, loc}, ..]) => panic!("error parsing else on line {} starting '{}'", *loc, show_token(tok))
                    }
                },
                (_, [TokLoc{tok, loc}, ..]) => panic!("error parsing then parameter on line {} starting '{}'", *loc, show_token(tok))
            }
        },
        [TokLoc{tok: Token::Lcurly, loc: ln}, toks @ ..] => {
            let (s_list, toks) = parse_stmt_list(toks);
            (Stmt::Loc(Box::new(Stmt::Stmts(s_list)), *ln), toks)
        },
        [TokLoc{tok: Token::Input, loc: ln},
         TokLoc{tok: Token::Ident(x), loc: _},
         toks @ ..] => 
             (Stmt::Loc(Box::new(Stmt::In(ID::Source(x.to_string(), None))), *ln), toks),
        [TokLoc{tok: Token::Output, loc: ln},
         TokLoc{tok: Token::Ident(x), loc: _},
         toks @ ..] =>
             (Stmt::Loc(Box::new(Stmt::Out(ID::Source(x.to_string(), None))), *ln), toks),
        [TokLoc{tok: Token::Return, loc: ln},
         TokLoc{tok: Token::Ident(x), loc: _},
         toks @ ..] =>
             (Stmt::Loc(Box::new(Stmt::Return(Some(ID::Source(x.to_string(), None)))), *ln), toks),
        [TokLoc{tok, loc}, ..] => panic!("error parsing statement on line {} starting '{}'", *loc, show_token(tok))
    }
}

fn parse_stmt_list(mut toks: &[TokLoc]) -> (Vec<Stmt>, &[TokLoc]) {
    let mut stmts = Vec::new();
    loop {
        match toks {
            [TokLoc{tok: Token::Rcurly, loc: _}, rem_toks @ ..] => return (stmts, rem_toks),
            _ => {
                let (stmt, rem_toks) = parse_stmt(toks);
                toks = rem_toks;
                stmts.push(stmt);
            }
        }
    }
}

fn parse_var_dec_list(mut toks: &[TokLoc]) -> (Vec<VarDec>, &[TokLoc]) {
    let mut var_decs = Vec::new();
    while let Some(TokLoc{tok: Token::Let, loc: _}) = toks.get(0) {
        let (dec, rem_toks) = parse_var_dec(toks);
        toks = rem_toks;
        var_decs.push(dec);
    }
    (var_decs, toks)
}

fn parse_func(toks: &[TokLoc]) -> (Func, &[TokLoc]) {
    match toks {
        [] => panic!("end of file looking for function"),
        [TokLoc{tok: Token::Function, loc: func_loc}, TokLoc{tok: Token::Ident(func_name), loc: _}, toks @ ..] => {
            match parse_param_list(toks) {
                (_, []) => panic!("eof parsing function"),
                (params, [TokLoc{tok: Token::Colon, loc: _}, toks @ ..]) => {
                    if params.len() != 0 {
                        match parse_typ(toks) {
                            (_, []) => panic!("eof parsing function"),
                            (t, [TokLoc{tok: Token::Lcurly, loc: _}, toks @ ..]) => {
                                let (var_decs, toks) = parse_var_dec_list(toks);
                                let (stmts, toks) = parse_stmt_list(toks);
                                return (Func{ fun_name: ID::Source(func_name.to_string(), None), params: params, ret: t, locals: var_decs, body: stmts, loc: Some(*func_loc) }, toks)
                            },
                            (_,  [TokLoc{tok: _, loc}, ..]) => panic!("error parsing function expected '{{' on line {}", *loc)
                        }
                    }
                    else {
                        panic!("error parsing function")
                    }
                },
                (_, [TokLoc{tok: _, loc}, ..]) => panic!("error parsing function on line {}", *loc)
            }
        },
        [TokLoc{tok: _, loc}, ..] => panic!("error parsing function on line {}", *loc)
    }
}

fn parse_funcs(mut toks: &[TokLoc]) -> Vec<Func> {
    let mut funcs = Vec::new();
    loop {
        match toks {
            [] => return funcs,
            _ => {
                let (func, rem_toks) = parse_func(toks);
                funcs.push(func);
                toks = rem_toks;
            }
        }
    }
}

pub fn parse_program(toks: &[TokLoc]) -> Prog {
    let (var_decs, toks) = parse_var_dec_list(toks);
    let funcs = parse_funcs(toks);
    Prog { var_dec: var_decs, funcs: funcs }
}

pub fn stmts_to_stmt(stmts: Vec<Stmt>) -> Stmt {
    if stmts.len() == 1 {
        stmts[0].clone()
    }
    else {
        Stmt::Stmts(stmts)
    }
}