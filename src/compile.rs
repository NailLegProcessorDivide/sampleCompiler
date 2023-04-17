use std::collections::HashSet;
use std::fs::File;
use std::io::Write;

use crate::{
    block_structure::Var,
    compile_function,
    source_ast::{Func, Prog, Scope, Stmt, Typ, ID},
    x86::Instruction_x64,
};

fn write_header(file: &mut File) {
    write!(file, "[section .text align=16]\n").ok();
    write!(file, "global main\n\n").ok();
    write!(file, "extern signal_error\n").ok();
    write!(file, "extern input\n").ok();
    write!(file, "extern output\n").ok();
    write!(file, "extern allocate1\n").ok();
    write!(file, "extern allocate2\n").ok();
    write!(file, "extern allocate3\n").ok();
    write!(file, "extern allocate4\n").ok();
    write!(file, "extern allocate5\n").ok();
    write!(file, "extern allocate6\n").ok();
    write!(file, "extern allocate7\n\n").ok();
}

pub fn id_to_var(global: &ID) -> Var {
    match global {
        ID::Source(name, Some(Scope::Global)) => Var::NamedSource(name.clone(), Scope::Global),
        _ => panic!("doesnt get here"),
    }
}

pub fn compile(prog: &mut Prog, target_filename: &str) {
    let mut main: Func = Func {
        fun_name: ID::Source("main".to_string(), None),
        params: Vec::new(),
        ret: Typ::Int,
        locals: Vec::new(),
        body: prog
            .globals
            .iter()
            .map(|var| Stmt::Assign(var.var_name.clone(), Vec::new(), var.init.clone()))
            .collect(),
        loc: None,
    };
    main.body.push(Stmt::Return(None));
    prog.funcs.push(main);

    let mut file = File::create(target_filename).unwrap();
    write_header(&mut file);
    let globals: HashSet<Var> =
        HashSet::from_iter(prog.globals.iter().map(|glob| id_to_var(&glob.var_name)));
    println!("made it to start of funcs");
    let functions: Vec<(ID, Vec<Instruction_x64>)> = prog
        .funcs
        .iter()
        .map(|func| compile_function::compile_fun_x64(false, &globals, func))
        .collect();
    println!("made it to end of funcs");
}
