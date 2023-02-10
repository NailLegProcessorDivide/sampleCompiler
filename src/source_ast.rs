

use crate::{block_structure::Var, tokens::TokLoc};

use super::tokens;
use regex::internal::Input;
use tokens::{OP, UOP, Token};

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
    pub loc : Option<i64>
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
    Loc(Box<Stmt>, i64)
}

pub struct Func {
    pub fun_name: ID,
    pub params: Vec<(ID, Typ)>,
    pub ret: Typ,
    pub locals: Vec<VarDec>,
    pub body: Vec<Stmt>,
    pub loc: Option<i64>,
}

pub struct Prog {
    pub var_dec : Vec<VarDec>,
    pub funcs : Vec<Func>,
}

fn parse_var_dec(toks: &Vec<TokLoc>, token_index: usize) -> (Vec<VarDec>, usize) {
    match &toks[token_index..] {
        [TokLoc {tok: Token::Let, loc: _},
         TokLoc {tok: Token::Ident(x), loc: _},
         TokLoc {tok: Token::Colon, loc: _}] => (Vec::new(), token_index + 3),
        _ => todo!()
    }
}

fn parse_var_dec_list(toks: &Vec<TokLoc>, start_index: usize) -> (Vec<VarDec>, usize) {
    let mut var_decs = Vec::new();
    let mut token_index = start_index;
    while let Some(TokLoc{tok: Token::Let, loc: line_num}) = toks.get(token_index) {

        token_index += 1;
    }
    (var_decs, token_index)
}

pub fn parse_program(toks: &Vec<TokLoc>) -> Prog {
    let (var_decs, toks) = parse_var_dec_list(toks, 0);
    todo!()
}

pub fn stmts_to_stmt(stmts: Vec<Stmt>) -> Stmt {
    if stmts.len() == 1 {
        stmts[0].clone()
    }
    else {
        Stmt::Stmts(stmts)
    }
}